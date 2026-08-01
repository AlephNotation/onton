(* @archlint.module interface
   @archlint.domain github-effect *)

open Base

(** A durable command for the GitHub adapter. Commands carry every input the
    adapter needs; it never consults policy or mutates orchestrator state. *)

type action =
  | Set_pr_draft of { pr_number : Types.Pr_number.t; draft : bool }
  | Set_pr_base of { pr_number : Types.Pr_number.t; base : Types.Branch.t }
  | Request_review of {
      pr_number : Types.Pr_number.t;
      team_slug : string;
      head_oid : string;
    }
  | Direct_merge of { pr_number : Types.Pr_number.t }
  | Enqueue of { pr_number : Types.Pr_number.t }
  | Dequeue of { pr_number : Types.Pr_number.t; entry_id : string }
[@@deriving show, eq, sexp_of, compare]

type status = Pending | Running | Retry_at of float | Failed
[@@deriving show, eq, sexp_of, compare]

type t = private {
  id : Types.Effect_id.t;
  patch_id : Types.Patch_id.t;
  action : action;
  status : status;
  attempts : int;
  last_error : string option;
}
[@@deriving show, eq, sexp_of, compare]

val create : patch_id:Types.Patch_id.t -> action -> t
(** Construct a command with a deterministic ID derived from its patch and exact
    action. Reconciliation can therefore enqueue the same intent repeatedly
    without creating duplicate external mutations. *)

val restore :
  id:Types.Effect_id.t ->
  patch_id:Types.Patch_id.t ->
  action:action ->
  status:status ->
  attempts:int ->
  last_error:string option ->
  t
(** Restore persisted state. A persisted [Running] command becomes [Pending]:
    the process that owned the call no longer exists, so at-least-once replay is
    the only honest recovery rule. *)

val runnable : now:float -> t -> bool
val claim : now:float -> t -> t option
val retry : now:float -> delay:float -> error:string -> t -> t
val fail : error:string -> t -> t
val identity : patch_id:Types.Patch_id.t -> action -> Types.Effect_id.t
