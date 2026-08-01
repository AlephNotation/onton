(* @archlint.module interface
   @archlint.domain llm-backend *)

open Base

(** Generic LLM backend interface.

    A backend wraps an LLM CLI's streaming invocation behind a common type. The
    process manager, clock, and timeout are captured at construction time so
    that the record type avoids Eio's unquantifiable row variables. *)

type result = {
  exit_code : int;
  stdout : string;
      (** Bounded raw stdout capture from streaming backends. This is retained
          for diagnostics when a backend exits cleanly without parseable events;
          it may be truncated. *)
  stderr : string;
  got_events : bool;
  saw_final_result : bool;
  timed_out : bool;
}
[@@deriving show, eq, sexp_of, compare]

val redact_env : string array -> string array

val emit_spawn_started :
  patch_id:Types.Patch_id.t ->
  session_uuid:string ->
  prompt:string ->
  args:string list ->
  env:string array ->
  unit

val spawn_and_stream :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  spawn:Worker_sandbox.spawn ->
  session_uuid:string option ->
  patch_id:Types.Patch_id.t ->
  process_line:(string -> Types.Stream_event.t list) ->
  on_event:(Types.Stream_event.t -> unit) ->
  result
(** Spawn a subprocess, read NDJSON lines from stdout, and stream parsed events.
    Each stdout line is passed to [process_line] which returns events to forward
    to [on_event]. Handles pipe setup, stdin EOF, stderr capture, and exit code
    extraction. Stdout allows large single-line JSON events from CLIs such as
    Codex; stderr is capped and truncated. The process is killed after [timeout]
    seconds.

    [spawn] can only be produced by {!Worker_sandbox.prepare_spawn}; this is the
    single production process-launch boundary. Teardown signals the isolated
    process group so tool-call grandchildren are reaped. *)

module For_test : sig
  val spawn_and_stream_raw :
    process_mgr:_ Eio.Process.mgr ->
    clock:_ Eio.Time.clock ->
    timeout:float ->
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    env:string array ->
    setsid_exec:string option ->
    process_group:bool ->
    args:string list ->
    session_uuid:string option ->
    patch_id:Types.Patch_id.t ->
    process_line:(string -> Types.Stream_event.t list) ->
    on_event:(Types.Stream_event.t -> unit) ->
    result
  (** Unsandboxed subprocess primitive exposed only for parser and teardown
      smoke tests. *)
end

type t = {
  name : string;
  run_streaming :
    sandbox:Worker_sandbox.t ->
    project_name:string ->
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    patch_id:Types.Patch_id.t ->
    prompt:string ->
    resume_session:string option ->
    session_uuid:string ->
    on_event:(Types.Stream_event.t -> unit) ->
    result;
}

val sandbox_failure :
  on_event:(Types.Stream_event.t -> unit) -> string -> result
