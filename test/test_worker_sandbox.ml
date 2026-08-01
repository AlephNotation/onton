(* @archlint.module test
   @archlint.domain worker-sandbox *)

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

let () =
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
          ~runtime_roots:[] ~state_dir:state
          ~network:Worker_sandbox_policy.Https_only
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
          (Worker_sandbox.create ~backend:"codex" ~provider:"openai"
             ~project_name:"sandbox-test" ~worktree ~patch ~gameplan
             ~operation:None))
