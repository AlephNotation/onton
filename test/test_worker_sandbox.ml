open Base
open Onton
open Onton_core

let remove_tree path =
  let rec remove path =
    match Unix.lstat path with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | stat -> (
        match stat.Unix.st_kind with
        | Unix.S_DIR ->
            Stdlib.Sys.readdir path
            |> Array.iter ~f:(fun child ->
                remove (Stdlib.Filename.concat path child));
            Unix.rmdir path
        | Unix.S_REG | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
        | Unix.S_SOCK ->
            Unix.unlink path)
  in
  remove path

let write path contents =
  let channel = Stdlib.Out_channel.open_text path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.Out_channel.close channel)
    (fun () -> Stdlib.Out_channel.output_string channel contents)

let read_file path =
  let channel = Stdlib.open_in_bin path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_in_noerr channel)
    (fun () -> Stdlib.In_channel.input_all channel)

let write_executable path contents =
  write path contents;
  Unix.chmod path 0o700

let environment_value name environment =
  Array.find_map environment ~f:(fun entry ->
      match String.lsplit2 entry ~on:'=' with
      | Some (key, value) when String.equal key name -> Some value
      | Some _ | None -> None)

let with_environment overrides f =
  let previous =
    List.map overrides ~f:(fun (name, _) -> (name, Stdlib.Sys.getenv_opt name))
  in
  Stdlib.Fun.protect
    ~finally:(fun () ->
      List.iter previous ~f:(fun (name, value) ->
          Unix.putenv name (Option.value value ~default:"")))
    (fun () ->
      List.iter overrides ~f:(fun (name, value) -> Unix.putenv name value);
      f ())

let run process_mgr ~cwd ~env ~profile script =
  let stderr = Buffer.create 512 in
  try
    Eio.Process.run process_mgr ~cwd ~env
      ~stderr:(Eio.Flow.buffer_sink stderr)
      [ "/usr/bin/sandbox-exec"; "-p"; profile; "/bin/sh"; "-c"; script ];
    Ok ()
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | _ -> Error (Buffer.contents stderr)

let require_run = function
  | Ok () -> ()
  | Error message -> failwith ("sandbox command failed: " ^ message)

let test_packaged_setsid_resolution () =
  let package_dir =
    Stdlib.Filename.temp_dir "onton-package-layout-" "" |> Unix.realpath
  in
  Stdlib.Fun.protect ~finally:(fun () -> remove_tree package_dir) @@ fun () ->
  let executable = Stdlib.Filename.concat package_dir "lo" in
  let helper = Stdlib.Filename.concat package_dir "lo-setsid-exec" in
  write_executable executable "#!/bin/sh\nexit 0\n";
  write_executable helper "#!/bin/sh\nexit 0\n";
  assert (
    Result.equal String.equal String.equal
      (Worker_sandbox.resolve_setsid_exec ~executable_name:executable
         ~override:None)
      (Ok (Unix.realpath helper)));
  with_environment
    [ ("PATH", package_dir ^ ":/usr/bin:/bin") ]
    (fun () ->
      assert (
        Result.equal String.equal String.equal
          (Worker_sandbox.resolve_setsid_exec ~executable_name:"lo"
             ~override:None)
          (Ok (Unix.realpath helper))));
  assert (
    Result.is_error
      (Worker_sandbox.resolve_setsid_exec ~executable_name:executable
         ~override:(Some "")))

let () =
  test_packaged_setsid_resolution ();
  match Worker_sandbox.preflight () with
  | Error message -> Stdlib.Printf.eprintf "SKIP: %s\n" message
  | Ok () ->
      Eio_main.run @@ fun eio ->
      let process_mgr = Eio.Stdenv.process_mgr eio in
      let fs = Eio.Stdenv.fs eio in
      let root =
        Stdlib.Filename.temp_dir "onton-worker-sandbox-" "" |> Unix.realpath
      in
      Stdlib.Fun.protect ~finally:(fun () -> remove_tree root) @@ fun () ->
      let worktree = Stdlib.Filename.concat root "worktree" in
      let sibling = Stdlib.Filename.concat root "sibling" in
      let state = Stdlib.Filename.concat root "state" in
      let context = Stdlib.Filename.concat root "plan.md" in
      List.iter [ worktree; sibling; state ] ~f:(fun path ->
          Unix.mkdir path 0o700);
      let declared = Stdlib.Filename.concat worktree "owned.txt" in
      let undeclared = Stdlib.Filename.concat worktree "other.txt" in
      let secret = Stdlib.Filename.concat sibling "secret.txt" in
      write declared "before";
      write secret "controller-secret";
      write context "read-only-plan";
      let policy =
        Worker_sandbox_policy.create ~worktree ~read_only_paths:[ context ]
          ~read_only_dirs:[] ~writable_files:[ declared ] ~writable_dirs:[]
          ~creatable_dirs:[] ~runtime_files:[] ~runtime_roots:[]
          ~state_dir:state ~network:Worker_sandbox_policy.Https_only
        |> Result.ok_or_failwith
      in
      let profile = Worker_sandbox_policy.macos_profile policy in
      let environment =
        Worker_sandbox_policy.environment
          ~allowed_provider_names:[ "OPENAI_API_KEY" ]
          ~base:
            [|
              "PATH=/usr/bin:/bin";
              "GITHUB_TOKEN=must-not-leak";
              "OPENAI_API_KEY=provider-only";
            |]
          ~overrides:[ ("HOME", state); ("TMPDIR", state) ]
        |> Result.ok_or_failwith
      in
      let cwd = Eio.Path.(fs / worktree) in
      run process_mgr ~cwd ~env:environment ~profile
        "test -z \"$GITHUB_TOKEN\" && test \"$OPENAI_API_KEY\" = provider-only"
      |> require_run;
      run process_mgr ~cwd ~env:environment ~profile
        (Printf.sprintf "cat %s >/dev/null && printf after > %s"
           (Stdlib.Filename.quote context)
           (Stdlib.Filename.quote declared))
      |> require_run;
      run process_mgr ~cwd ~env:environment ~profile
        "/usr/bin/curl -fsS https://example.com >/dev/null"
      |> require_run;
      assert (
        Result.is_error
          (run process_mgr ~cwd ~env:environment ~profile
             (Printf.sprintf "cat %s >/dev/null" (Stdlib.Filename.quote secret))));
      assert (
        Result.is_error
          (run process_mgr ~cwd ~env:environment ~profile
             (Printf.sprintf "printf escaped > %s"
                (Stdlib.Filename.quote undeclared))));
      assert (
        Result.is_error
          (run process_mgr ~cwd ~env:environment ~profile
             (Printf.sprintf "/bin/sh -c 'cat %s >/dev/null'"
                (Stdlib.Filename.quote secret))));
      assert (
        Result.is_error
          (run process_mgr ~cwd ~env:environment ~profile
             (Printf.sprintf "kill -0 %d" (Unix.getpid ()))));
      let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Stdlib.Fun.protect ~finally:(fun () -> Unix.close listener) @@ fun () ->
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 1;
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      assert (
        Result.is_error
          (run process_mgr ~cwd ~env:environment ~profile
             (Printf.sprintf "/usr/bin/nc -z 127.0.0.1 %d" port)));
      let escape = Stdlib.Filename.concat worktree "escape" in
      Unix.symlink sibling escape;
      let patch_id = Types.Patch_id.of_string "sandbox-test" in
      let patch : Types.Patch.t =
        {
          Types.Patch.id = patch_id;
          goal = "test escape rejection";
          branch = Types.Branch.of_string "test/sandbox";
          dependencies = [];
          files = [ "escape/secret.txt" ];
          checks = [];
          agent = None;
        }
      in
      let gameplan : Types.Gameplan.t =
        {
          Types.Gameplan.project_name = "sandbox-test";
          repo_owner = "test";
          repo_name = "test";
          patches = [ patch ];
        }
      in
      assert (
        Result.is_error
          (Worker_sandbox.create ~backend:"codex" ~provider:"codex"
             ~project_name:"sandbox-test" ~worktree ~patch ~gameplan
             ~operation:None));
      Unix.unlink escape;
      let runtime_dir = Stdlib.Filename.concat root "runtime-bin" in
      let data_dir = Stdlib.Filename.concat root "data" in
      List.iter [ runtime_dir; data_dir ] ~f:(fun path -> Unix.mkdir path 0o700);
      let fake_codex = Stdlib.Filename.concat runtime_dir "codex" in
      let runtime_secret = Stdlib.Filename.concat runtime_dir "secret.txt" in
      write runtime_secret "runtime-secret";
      write_executable fake_codex
        {|#!/bin/sh
case "$1" in
  write)
    if cat "$2" >/dev/null 2>&1; then exit 91; fi
    mkdir -p nested/deep
    printf new > new.txt
    printf nested > nested/deep/file.txt
    ;;
  probe)
    if cat "$2" >/dev/null 2>&1; then exit 92; fi
    ;;
  *) exit 93 ;;
esac
|};
      let new_patch : Types.Patch.t =
        {
          Types.Patch.id = patch_id;
          goal = "create exact declared files";
          branch = Types.Branch.of_string "test/sandbox";
          dependencies = [];
          files = [ "new.txt"; "nested/deep/file.txt" ];
          checks = [];
          agent = None;
        }
      in
      let new_gameplan =
        { gameplan with Types.Gameplan.patches = [ new_patch ] }
      in
      let setsid_exec =
        Stdlib.Sys.getenv_opt "ONTON_SETSID_EXEC"
        |> Option.value_exn |> Unix.realpath
      in
      with_environment
        [
          ("PATH", runtime_dir ^ ":/usr/bin:/bin:/usr/sbin:/sbin");
          ("ONTON_DATA_DIR", data_dir);
          ("CODEX_API_KEY", "selected-codex");
          ("OPENAI_API_KEY", "selected-openai");
          ("ANTHROPIC_API_KEY", "selected-anthropic");
          ("CLAUDE_CONFIG_DIR", "/ambient/claude");
        ]
        (fun () ->
          let codex_sandbox =
            Worker_sandbox.create ~backend:"codex" ~provider:"codex"
              ~project_name:"sandbox-test" ~worktree ~patch:new_patch
              ~gameplan:new_gameplan ~operation:None
            |> Result.ok_or_failwith
          in
          assert (
            Result.is_error
              (Worker_sandbox.prepare_spawn codex_sandbox ~overrides:[]
                 ~setsid_exec:None [ "codex" ]));
          assert (
            Result.is_error
              (Worker_sandbox.prepare_spawn codex_sandbox ~overrides:[]
                 ~setsid_exec:(Some setsid_exec) [ "claude" ]));
          let codex_spawn =
            Worker_sandbox.prepare_spawn codex_sandbox ~overrides:[]
              ~setsid_exec:(Some setsid_exec)
              [ "codex"; "write"; runtime_secret ]
            |> Result.ok_or_failwith
          in
          assert (
            Option.equal String.equal
              (environment_value "CODEX_API_KEY"
                 codex_spawn.Worker_sandbox.environment)
              (Some "selected-codex"));
          assert (
            Option.is_none
              (environment_value "OPENAI_API_KEY"
                 codex_spawn.Worker_sandbox.environment));
          assert (
            Option.is_none
              (environment_value "ANTHROPIC_API_KEY"
                 codex_spawn.Worker_sandbox.environment));
          assert (
            Option.is_none
              (environment_value "CLAUDE_CONFIG_DIR"
                 codex_spawn.Worker_sandbox.environment));
          assert (
            Option.equal String.equal
              (environment_value "SSL_CERT_FILE"
                 codex_spawn.Worker_sandbox.environment)
              (Some "/etc/ssl/cert.pem"));
          Eio.Process.run process_mgr ~cwd
            ~env:codex_spawn.Worker_sandbox.environment
            codex_spawn.Worker_sandbox.argv;
          assert (
            String.equal
              (read_file (Stdlib.Filename.concat worktree "new.txt"))
              "new");
          assert (
            String.equal
              (read_file
                 (Stdlib.Filename.concat worktree "nested/deep/file.txt"))
              "nested");
          let provider_state_secret =
            Stdlib.Filename.concat
              (Worker_sandbox.state_dir codex_sandbox)
              "provider-secret.txt"
          in
          write provider_state_secret "codex-state-secret";
          let anthropic_sandbox =
            Worker_sandbox.create ~backend:"codex" ~provider:"anthropic"
              ~project_name:"sandbox-test" ~worktree ~patch:new_patch
              ~gameplan:new_gameplan ~operation:None
            |> Result.ok_or_failwith
          in
          assert (
            not
              (String.equal
                 (Worker_sandbox.state_dir codex_sandbox)
                 (Worker_sandbox.state_dir anthropic_sandbox)));
          let anthropic_spawn =
            Worker_sandbox.prepare_spawn anthropic_sandbox ~overrides:[]
              ~setsid_exec:(Some setsid_exec)
              [ "codex"; "probe"; provider_state_secret ]
            |> Result.ok_or_failwith
          in
          assert (
            Option.equal String.equal
              (environment_value "ANTHROPIC_API_KEY"
                 anthropic_spawn.Worker_sandbox.environment)
              (Some "selected-anthropic"));
          assert (
            Option.is_none
              (environment_value "OPENAI_API_KEY"
                 anthropic_spawn.Worker_sandbox.environment));
          Eio.Process.run process_mgr ~cwd
            ~env:anthropic_spawn.Worker_sandbox.environment
            anthropic_spawn.Worker_sandbox.argv);
      with_environment
        [ ("ONTON_DATA_DIR", data_dir) ]
        (fun () ->
          List.iter
            [
              ("claude", "claude");
              ("codex", "codex");
              ("gemini", "google");
              ("opencode", "openai");
              ("pi", "anthropic");
              ("patch-agent", "anthropic");
            ]
            ~f:(fun (backend, provider) ->
              let available =
                Stdlib.Sys.command
                  (Printf.sprintf "command -v %s >/dev/null 2>&1"
                     (Stdlib.Filename.quote backend))
                = 0
              in
              if available then
                let sandbox =
                  Worker_sandbox.create ~backend ~provider
                    ~project_name:"sandbox-test" ~worktree ~patch:new_patch
                    ~gameplan:new_gameplan ~operation:None
                  |> Result.ok_or_failwith
                in
                let spawn =
                  Worker_sandbox.prepare_spawn sandbox ~overrides:[]
                    ~setsid_exec:(Some setsid_exec) [ backend; "--version" ]
                  |> Result.ok_or_failwith
                in
                Eio.Process.run process_mgr ~cwd
                  ~env:spawn.Worker_sandbox.environment
                  spawn.Worker_sandbox.argv))
