(* @archlint.module core
   @archlint.domain github-effect *)

open Base
open Types

type action =
  | Set_pr_draft of { pr_number : Pr_number.t; draft : bool }
  | Set_pr_base of { pr_number : Pr_number.t; base : Branch.t }
  | Request_review of {
      pr_number : Pr_number.t;
      team_slug : string;
      head_oid : string;
    }
  | Direct_merge of { pr_number : Pr_number.t }
  | Enqueue of { pr_number : Pr_number.t }
  | Dequeue of { pr_number : Pr_number.t; entry_id : string }
[@@deriving show, eq, sexp_of, compare]

type status = Pending | Running | Retry_at of float | Failed
[@@deriving show, eq, sexp_of, compare]

type t = {
  id : Effect_id.t;
  patch_id : Patch_id.t;
  action : action;
  status : status;
  attempts : int;
  last_error : string option;
}
[@@deriving show, eq, sexp_of, compare]

let identity ~patch_id action =
  let raw =
    Sexp.to_string_mach
      (Sexp.List
         [
           Sexp.Atom "github-effect-v1";
           Sexp.Atom (Patch_id.to_string patch_id);
           sexp_of_action action;
         ])
  in
  let digest = Digestif.SHA256.digest_string raw |> Digestif.SHA256.to_hex in
  Effect_id.of_string
    (Printf.sprintf "%s:%s" (Patch_id.to_string patch_id) digest)

let create ~patch_id action =
  {
    id = identity ~patch_id action;
    patch_id;
    action;
    status = Pending;
    attempts = 0;
    last_error = None;
  }

let restore ~id ~patch_id ~action ~status ~attempts ~last_error =
  {
    id;
    patch_id;
    action;
    status =
      (match status with
      | Running -> Pending
      | Pending -> Pending
      | Retry_at at -> Retry_at at
      | Failed -> Failed);
    attempts = Int.max 0 attempts;
    last_error;
  }

let runnable ~now t =
  match t.status with
  | Pending -> true
  | Retry_at at -> Float.(now >= at)
  | Running | Failed -> false

let claim ~now t =
  if runnable ~now t then Some { t with status = Running } else None

let retry ~now ~delay ~error t =
  {
    t with
    status = Retry_at (now +. Float.max 0.0 delay);
    attempts = t.attempts + 1;
    last_error = Some error;
  }

let fail ~error t =
  { t with status = Failed; attempts = t.attempts + 1; last_error = Some error }
