(* @archlint.module interface
   @archlint.domain worker-sandbox *)

open Base

type t

type spawn = private {
  argv : string list;
  environment : string array;
  process_group : bool;
}

val preflight : unit -> (unit, string) Result.t
(** Verify that the host has the one supported enforcement mechanism. This is
    intentionally fail-closed; there is no unsandboxed fallback. *)

val resolve_setsid_exec :
  executable_name:string -> override:string option -> (string, string) Result.t
(** Resolve the installed sibling process-group helper, with the dune build
    layout as the only development fallback. Empty overrides disable worker
    launch and missing paths fail closed. *)

val create :
  backend:string ->
  provider:string ->
  project_name:string ->
  worktree:string ->
  patch:Types.Patch.t ->
  gameplan:Types.Gameplan.t ->
  operation:Types.Operation_kind.t option ->
  (t, string) Result.t
(** Resolve and validate one worker's filesystem and network capabilities.
    Symlinked or escaping declared write paths are rejected. Missing declared
    parent directories become exact create-only capabilities. The selected
    backend is resolved to an absolute executable, package-scoped runtime, and
    exact non-system linked libraries; ambient PATH directories are not made
    readable. *)

val prepare_spawn :
  t ->
  overrides:(string * string) list ->
  setsid_exec:string option ->
  string list ->
  (spawn, string) Result.t
(** Build the only permitted worker launch pair: wrapped argv plus scrubbed
    environment. *)

val profile : t -> string
(** Exposed for focused policy tests and diagnostics. Contains paths but never
    environment values or credentials. *)

val state_dir : t -> string
(** Private writable state root granted only to this patch/backend/provider
    combination. Controller-created backend inputs belong here, never in the Git
    worktree. *)
