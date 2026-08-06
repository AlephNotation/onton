type t = Complete | Blocked of string [@@deriving show, eq, sexp_of]

val of_yojson : Yojson.Safe.t -> (t, string) result
(** Strict, total decoder for the worker's per-turn completion claim. Accepts
    exactly [{"status":"complete"}] or [{"status":"blocked","reason":"..."}].
    Unknown/duplicate fields and empty reasons are rejected. *)
