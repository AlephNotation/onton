(* @archlint.module interface
   @archlint.domain patch-pr-status *)

open Types

(** Whether a planned patch currently has a tracked pull request. A PR that no
    longer exists is cleared to [Absent] and recreated from the plan. *)
type t = Absent | Present of Pr_number.t
[@@deriving show, eq, sexp_of, compare]

val has_pr : t -> bool
val pr_number : t -> Pr_number.t option

type set_present_decision = Preserve_existing | Adopt_new
[@@deriving show, eq]

val classify_set_present : t -> Pr_number.t -> set_present_decision
(** Preserve state only when the same PR is observed again. A first or changed
    number starts a new PR lifecycle. *)

val set_present : t -> Pr_number.t -> t

val clear : t -> t
(** Clear a present PR. Raises [Invalid_argument] when already absent. *)

val yojson_of_t : t -> Yojson.Safe.t

val t_of_yojson : Yojson.Safe.t -> (t, string) Result.t
(** Strict tagged-object codec. *)
