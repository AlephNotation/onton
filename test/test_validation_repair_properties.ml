open Base
open Onton_core.Types
module Repair = Onton_core.Validation_repair

let check run = Repair.Check { Check.run; proves = "the declared gate passes" }
let record state target = Repair.record_failure state target

let exact_gate_windows =
  QCheck2.Test.make
    ~name:"one gate gets one resumed window and one fresh window"
    QCheck2.Gen.unit (fun () ->
      let target = check "dune build" in
      let state, first = record Repair.empty target in
      let state, second = record state target in
      let state, third = record state target in
      let state, fourth = record state target in
      let state, fifth = record state target in
      let state, sixth = record state target in
      Repair.equal_decision first Repair.Continue
      && Repair.equal_decision second Repair.Continue
      && Repair.equal_decision third Repair.Restart_session
      && Repair.equal_decision fourth Repair.Continue
      && Repair.equal_decision fifth Repair.Continue
      && Repair.equal_decision sixth Repair.Exhausted
      && Repair.total_attempts state = 6
      && Repair.is_exhausted state)

let gates_do_not_share_session_windows =
  QCheck2.Test.make
    ~name:"distinct validation gates do not consume each other's repair window"
    QCheck2.Gen.unit (fun () ->
      let a = check "dune build @fmt" in
      let b = check "dune build" in
      let state, _ = record Repair.empty a in
      let state, _ = record state a in
      let state, first_b = record state b in
      let state, second_b = record state b in
      let state, third_b = record state b in
      let state, third_a = record state a in
      Repair.equal_decision first_b Repair.Continue
      && Repair.equal_decision second_b Repair.Continue
      && Repair.equal_decision third_b Repair.Restart_session
      && Repair.equal_decision third_a Repair.Restart_session
      && Repair.total_attempts state = 6
      && not (Repair.is_exhausted state))

let alternating_failures_are_globally_bounded =
  QCheck2.Test.make
    ~name:"alternating validation gates cannot evade the patch-wide ceiling"
    QCheck2.Gen.unit (fun () ->
      let rec loop state index =
        if index = 12 then state
        else
          let state, _ = record state (check (Int.to_string index)) in
          loop state (index + 1)
      in
      let state = loop Repair.empty 0 in
      Repair.total_attempts state = 12 && Repair.is_exhausted state)

let exhausted_is_sticky =
  QCheck2.Test.make ~name:"terminal validation repair state is sticky"
    QCheck2.Gen.(list_size (int_range 12 40) nat_small)
    (fun targets ->
      let final =
        List.fold targets ~init:Repair.empty ~f:(fun state value ->
            record state (check (Int.to_string value)) |> fst)
      in
      if not (Repair.is_exhausted final) then true
      else
        let after, decision = record final Repair.Outside_scope in
        Repair.equal final after
        && Repair.equal_decision decision Repair.Exhausted)

let legacy_terminal_state_stays_terminal =
  QCheck2.Test.make
    ~name:"legacy three-strike snapshots remain terminal after migration"
    QCheck2.Gen.unit (fun () ->
      (not (Repair.is_exhausted (Repair.of_legacy_count 2)))
      && Repair.is_exhausted (Repair.of_legacy_count 3))

let json_roundtrip =
  QCheck2.Test.make ~name:"validation repair JSON round-trip preserves state"
    QCheck2.Gen.(list_size (int_range 0 20) nat_small)
    (fun targets ->
      let state =
        List.fold targets ~init:Repair.empty ~f:(fun state value ->
            record state (check (Int.to_string value)) |> fst)
      in
      match
        Onton_core.Json.try_of_yojson Repair.t_of_yojson
          (Repair.yojson_of_t state)
      with
      | Ok restored -> Repair.equal state restored
      | Error _ -> false)

let () =
  QCheck_base_runner.run_tests_main
    [
      exact_gate_windows;
      gates_do_not_share_session_windows;
      alternating_failures_are_globally_bounded;
      exhausted_is_sticky;
      legacy_terminal_state_stays_terminal;
      json_roundtrip;
    ]
