open Types

type target = Outside_scope | Check of Check.t
[@@deriving show, eq, sexp_of, compare, yojson]

type t [@@deriving show, eq, sexp_of, compare, yojson]

type decision = Continue | Restart_session | Exhausted
[@@deriving show, eq, sexp_of]

val empty : t
val total_attempts : t -> int
val is_exhausted : t -> bool

val record_failure : t -> target -> t * decision
(** Record one controller rejection for [target]. A target gets three repair
    turns in its current model session, then three more in one fresh session.
    The complete repair lifecycle is also bounded across alternating targets.
    [Restart_session] means the caller must discard the current resume id before
    delivering the next diagnostic. [Exhausted] is sticky. *)

val of_legacy_count : int -> t
(** Recover the former unstructured validation counter. Counts at its terminal
    threshold remain terminal; lower counts consume the corresponding portion of
    the new global budget without inventing a failure target. *)
