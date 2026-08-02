open Base
open Onton
open Onton_core

let rec remove_tree path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stat -> (
      match stat.Unix.st_kind with
      | Unix.S_DIR ->
          Stdlib.Sys.readdir path
          |> Array.iter ~f:(fun child ->
              remove_tree (Stdlib.Filename.concat path child));
          Unix.rmdir path
      | Unix.S_REG | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
      | Unix.S_SOCK ->
          Unix.unlink path)

let write_executable path contents =
  let channel = Stdlib.Out_channel.open_text path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.Out_channel.close channel)
    (fun () -> Stdlib.Out_channel.output_string channel contents);
  Unix.chmod path 0o700

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

let terminal_payload = function
  | "claude" -> {|{"type":"result","result":"done","stop_reason":"end_turn"}|}
  | "codex" -> {|{"type":"turn.completed"}|}
  | "gemini" -> {|{"type":"result","status":"success","stats":{}}|}
  | "opencode" ->
      {|{"type":"step_finish","part":{"type":"step-finish","reason":"stop"}}|}
  | "pi" -> {|{"type":"agent_end","messages":[]}|}
  | backend -> failwith ("unsupported fake backend: " ^ backend)

let install_ephemeral runtime_dir backend =
  let path = Stdlib.Filename.concat runtime_dir backend in
  write_executable path
    (Printf.sprintf "#!/bin/sh\nprintf '%%s\\n' '%s'\n"
       (terminal_payload backend))

let install_patch_agent runtime_dir =
  let path = Stdlib.Filename.concat runtime_dir "patch-agent" in
  write_executable path
    {|#!/bin/sh
printf '%s\n' '{"type":"session_init","session_id":"fake-session","model_id":"fake-model","provider":"anthropic"}'
while IFS= read -r request; do
  case "$request" in
    *'"type":"prompt"'*)
      printf '%s\n' '{"type":"turn_started","turn_index":1}'
      printf '%s\n' '{"type":"text_delta","delta":"sandboxed"}'
      printf '%s\n' '{"type":"done","stop_reason":"end_turn","final_text":"done"}'
      ;;
    *'"type":"shutdown"'*) exit 0 ;;
  esac
done
|}

let make_patch () : Types.Patch.t =
  {
    Types.Patch.id = Types.Patch_id.of_string "backend-sandbox";
    goal = "exercise every worker backend launch";
    branch = Types.Branch.of_string "test/backend-sandbox";
    dependencies = [];
    files = [];
    checks = [];
  }

let make_gameplan patch : Types.Gameplan.t =
  {
    Types.Gameplan.project_name = "backend-sandbox";
    repo_owner = "test";
    repo_name = "test";
    patches = [ patch ];
  }

let assert_success backend result events =
  if result.Llm_backend.timed_out then failwith (backend ^ " timed out");
  if not result.Llm_backend.got_events then
    failwith (backend ^ " emitted no events");
  if not result.Llm_backend.saw_final_result then
    failwith (backend ^ " emitted no final result");
  if
    not
      (List.exists !events ~f:(function
        | Types.Stream_event.Final_result _ -> true
        | Types.Stream_event.Turn_started | Types.Stream_event.Text_delta _
        | Types.Stream_event.Tool_use _ | Types.Stream_event.Error _
        | Types.Stream_event.Session_init _ ->
            false))
  then failwith (backend ^ " callback received no final result")

let run_ephemeral ~cwd ~patch ~gameplan ~backend_name ~provider backend =
  let sandbox =
    Worker_sandbox.create ~backend:backend_name ~provider
      ~project_name:"backend-sandbox" ~worktree:(snd cwd) ~patch ~gameplan
      ~operation:None
    |> Result.ok_or_failwith
  in
  let events = ref [] in
  let result =
    backend.Llm_backend.run_streaming ~sandbox ~project_name:"backend-sandbox"
      ~cwd ~patch_id:patch.Types.Patch.id ~prompt:"work" ~resume_session:None
      ~session_uuid:(backend_name ^ "-session") ~on_event:(fun event ->
        events := event :: !events)
  in
  assert_success backend_name result events

let run_long_lived ~process_mgr ~clock ~cwd ~setsid_exec ~patch ~gameplan =
  let sandbox =
    Worker_sandbox.create ~backend:"patch-agent" ~provider:"anthropic"
      ~project_name:"backend-sandbox" ~worktree:(snd cwd) ~patch ~gameplan
      ~operation:None
    |> Result.ok_or_failwith
  in
  let backend =
    Patch_agent_backend.create ~process_mgr ~clock ~timeout:10.0
      ~binary_path:"patch-agent" ~setsid_exec:(Some setsid_exec)
  in
  let events = ref [] in
  Eio.Switch.run @@ fun sw ->
  let (Llm_backend_long_lived.T { start; prompt; shutdown; _ }) = backend in
  let handle =
    start ~sw
      {
        Llm_backend_long_lived.project_name = "backend-sandbox";
        worktree = cwd;
        patch_id = patch.Types.Patch.id;
        sandbox;
        provider = "anthropic";
        model = "fake-model";
        effort = "medium";
        gameplan_prompt = "gameplan";
        patch_prompt = "patch";
      }
  in
  let result =
    Stdlib.Fun.protect
      ~finally:(fun () -> shutdown handle)
      (fun () ->
        prompt handle ~prompt:"work" ~timeout:10.0 ~on_event:(fun event ->
            events := event :: !events))
  in
  assert_success "patch-agent" result events

let () =
  match Worker_sandbox.preflight () with
  | Error message -> Stdlib.Printf.eprintf "SKIP: %s\n" message
  | Ok () ->
      let root =
        Stdlib.Filename.temp_dir "onton-worker-backends-" "" |> Unix.realpath
      in
      Stdlib.Fun.protect ~finally:(fun () -> remove_tree root) @@ fun () ->
      let runtime_dir = Stdlib.Filename.concat root "runtime" in
      let worktree = Stdlib.Filename.concat root "worktree" in
      let data_dir = Stdlib.Filename.concat root "data" in
      List.iter [ runtime_dir; worktree; data_dir ] ~f:(fun path ->
          Unix.mkdir path 0o700);
      List.iter
        [ "claude"; "codex"; "gemini"; "opencode"; "pi" ]
        ~f:(install_ephemeral runtime_dir);
      install_patch_agent runtime_dir;
      let setsid_exec =
        Stdlib.Sys.getenv_opt "ONTON_SETSID_EXEC"
        |> Option.value_exn |> Unix.realpath
      in
      with_environment
        [
          ("PATH", runtime_dir ^ ":/usr/bin:/bin:/usr/sbin:/sbin");
          ("ONTON_DATA_DIR", data_dir);
        ]
        (fun () ->
          Eio_main.run @@ fun environment ->
          let process_mgr = Eio.Stdenv.process_mgr environment in
          let clock = Eio.Stdenv.clock environment in
          let fs = Eio.Stdenv.fs environment in
          let cwd = Eio.Path.(fs / worktree) in
          let patch = make_patch () in
          let gameplan = make_gameplan patch in
          let ephemeral =
            [
              ( "claude",
                "claude",
                Claude_backend.create ~name:"Claude" ~model:None ~process_mgr
                  ~clock ~timeout:10.0 ~setsid_exec:(Some setsid_exec) );
              ( "codex",
                "openai",
                Codex_backend.create ~model:None ~process_mgr ~clock
                  ~timeout:10.0 ~setsid_exec:(Some setsid_exec) );
              ( "gemini",
                "google",
                Gemini_backend.create ~model:None ~process_mgr ~clock
                  ~timeout:10.0 ~setsid_exec:(Some setsid_exec) );
              ( "opencode",
                "openai",
                Opencode_backend.create ~model:None ~process_mgr ~clock
                  ~timeout:10.0 ~setsid_exec:(Some setsid_exec) );
              ( "pi",
                "anthropic",
                Pi_backend.create ~model:None ~process_mgr ~clock ~timeout:10.0
                  ~setsid_exec:(Some setsid_exec) );
            ]
          in
          List.iter ephemeral ~f:(fun (backend_name, provider, backend) ->
              run_ephemeral ~cwd ~patch ~gameplan ~backend_name ~provider
                backend);
          run_long_lived ~process_mgr ~clock ~cwd ~setsid_exec ~patch ~gameplan)
