open Base
open Types
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type target = Outside_scope | Check of Check.t
[@@deriving show, eq, sexp_of, compare, yojson]

type target_state = {
  target : target;
  attempts_in_session : int;
  fresh_session_used : bool;
}
[@@deriving show, eq, sexp_of, compare, yojson]

type t = { total_attempts : int; targets : target_state list }
[@@deriving show, eq, sexp_of, compare, yojson]

type decision = Continue | Restart_session | Exhausted
[@@deriving show, eq, sexp_of]

let attempts_per_session = 3
let total_attempt_limit = 12
let empty = { total_attempts = 0; targets = [] }
let total_attempts t = t.total_attempts

let target_exhausted state =
  state.fresh_session_used && state.attempts_in_session >= attempts_per_session

let is_exhausted t =
  t.total_attempts >= total_attempt_limit
  || List.exists t.targets ~f:target_exhausted

let record_failure t target =
  if is_exhausted t then (t, Exhausted)
  else
    let prior, others =
      List.partition_tf t.targets ~f:(fun state ->
          equal_target state.target target)
    in
    let state =
      match prior with
      | state :: _ -> state
      | [] -> { target; attempts_in_session = 0; fresh_session_used = false }
    in
    let attempts_in_session = state.attempts_in_session + 1 in
    let total_attempts = t.total_attempts + 1 in
    let state, target_decision =
      if attempts_in_session < attempts_per_session then
        ({ state with attempts_in_session }, Continue)
      else if state.fresh_session_used then
        ({ state with attempts_in_session }, Exhausted)
      else
        ( { state with attempts_in_session = 0; fresh_session_used = true },
          Restart_session )
    in
    let next = { total_attempts; targets = state :: others } in
    if total_attempts >= total_attempt_limit then (next, Exhausted)
    else (next, target_decision)

let of_legacy_count count =
  let count = Int.max 0 count in
  if count >= 3 then { empty with total_attempts = total_attempt_limit }
  else { empty with total_attempts = count }
