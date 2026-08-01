(* @archlint.module shell
   @archlint.domain patch-validator *)

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
[@@deriving show, eq]

val outside_scope : allowed:string list -> changed:string list -> string list
(** Pure exact-path scope check, with duplicates removed from the result. *)

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
