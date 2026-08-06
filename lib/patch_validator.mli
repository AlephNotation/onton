open Onton_core.Types

type command_failure = {
  command : string;
  exit_code : int option;
  stdout : string;
  stderr : string;
}
[@@deriving show, eq]

type failure =
  | Scope_read_failed of command_failure
  | Outside_scope of string list
  | Check_failed of Check.t * command_failure
  | Check_modified_repository of Check.t * string list
[@@deriving show, eq]

type preparation = No_changes | Committed | Rebase_continued
[@@deriving show, eq]

type prepare_failure =
  | Validation_failed of failure
  | Git_failed of command_failure
  | Rebase_conflict_remaining of string list * command_failure
[@@deriving show, eq]

val run_checks :
  process_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:float Eio.Time.clock_ty Eio.Time.clock ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cwd:string ->
  Check.t list ->
  (unit, failure) result
(** Run only the declared checks. Exposed independently for focused effect
    tests; normal orchestration uses {!run}. *)

val run :
  process_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:float Eio.Time.clock_ty Eio.Time.clock ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cwd:string ->
  base_branch:Branch.t ->
  Patch.t ->
  (unit, failure) result
(** Verify that the patch changes only its declared files, then run its checks
    sequentially. Commands have a ten-minute timeout; validation stops on the
    first failure. *)

val commit_subject : project_name:string -> Patch.t -> string
(** Concise controller-owned commit subject retaining the project and patch
    markers used by stacked-rebase ancestry classification. *)

val pr_title : Patch.t -> string
(** Concise human-facing pull-request title. The full goal belongs in the body.
*)

val prepare :
  process_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:float Eio.Time.clock_ty Eio.Time.clock ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cwd:string ->
  base_branch:Branch.t ->
  project_name:string ->
  rebase_in_progress:bool ->
  Patch.t ->
  (preparation, prepare_failure) result
(** Validate scope, stage the complete worker result, finish any in-progress
    rebase, then run each declared check exactly once against the tree that will
    be published. A check that changes HEAD, the index, or non-ignored worktree
    files is rejected. Finally create the controller-owned commit when needed.
    If continuing a rebase exposes another conflict, returns
    [Rebase_conflict_remaining] so the controller can schedule another bounded
    worker resolution without misclassifying expected Git state as an
    infrastructure failure. No worker needs write access to Git metadata. *)
