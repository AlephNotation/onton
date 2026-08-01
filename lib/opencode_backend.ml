(* @archlint.module shell
   @archlint.domain pi-event-parser *)

open Base

(* Pure parser, tool-name normalization, and CLI-arg builder live in
   [Opencode_event_parser] (lib_core/). This file is the effectful streaming
   driver. *)

let build_args = Opencode_event_parser.build_args
let parse_event = Opencode_event_parser.parse_event

let run_streaming ~model ~process_mgr ~clock ~timeout ~setsid_exec ~sandbox
    ~project_name ~cwd ~patch_id ~prompt ~resume_session ~session_uuid ~on_event
    =
  let cwd_path = snd cwd in
  let args = build_args ~model ~cwd_path ~prompt ~resume_session in
  let overrides =
    Spawn_env.per_patch_env ~backend:"opencode" ~project_name ~patch_id
  in
  let process_line line =
    let trimmed = String.strip line in
    if String.is_empty trimmed then [] else parse_event trimmed
  in
  match Worker_sandbox.prepare_spawn sandbox ~overrides ~setsid_exec args with
  | Error message -> Llm_backend.sandbox_failure ~on_event message
  | Ok spawn ->
      let args = spawn.Worker_sandbox.argv in
      let env = spawn.Worker_sandbox.environment in
      Llm_backend.emit_spawn_started ~patch_id ~session_uuid ~prompt ~args ~env;
      Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout ~cwd ~spawn
        ~session_uuid:(Some session_uuid) ~patch_id ~process_line ~on_event

let create ~model ~process_mgr ~clock ~timeout ~setsid_exec : Llm_backend.t =
  {
    name = "OpenCode";
    run_streaming =
      (fun ~sandbox
        ~project_name
        ~cwd
        ~patch_id
        ~prompt
        ~resume_session
        ~session_uuid
        ~on_event
      ->
        run_streaming ~model ~process_mgr ~clock ~timeout ~setsid_exec ~sandbox
          ~cwd ~project_name ~patch_id ~prompt ~resume_session ~session_uuid
          ~on_event);
  }
