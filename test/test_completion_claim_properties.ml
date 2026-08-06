open Base
module Claim = Onton_core.Completion_claim

let strict_examples =
  QCheck2.Test.make ~name:"completion claim accepts only the strict schema"
    QCheck2.Gen.unit (fun () ->
      (match Claim.of_yojson (`Assoc [ ("status", `String "complete") ]) with
        | Ok Claim.Complete -> true
        | Ok (Claim.Blocked _) | Error _ -> false)
      && (match
            Claim.of_yojson
              (`Assoc
                 [
                   ("status", `String "blocked");
                   ("reason", `String "  scope is incomplete  ");
                 ])
          with
        | Ok (Claim.Blocked "scope is incomplete") -> true
        | Ok (Claim.Blocked _ | Claim.Complete) | Error _ -> false)
      && Result.is_error
           (Claim.of_yojson
              (`Assoc [ ("status", `String "complete"); ("extra", `Bool true) ]))
      && Result.is_error
           (Claim.of_yojson
              (`Assoc
                 [
                   ("status", `String "complete"); ("status", `String "blocked");
                 ])))

let arbitrary_status_is_total =
  QCheck2.Test.make
    ~name:"completion claim decoder is total for arbitrary status"
    QCheck2.Gen.string (fun status ->
      match Claim.of_yojson (`Assoc [ ("status", `String status) ]) with
      | Ok _ | Error _ -> true)

let blocked_requires_nonempty_reason =
  QCheck2.Test.make ~name:"blocked completion requires a nonempty reason"
    QCheck2.Gen.string (fun reason ->
      match
        Claim.of_yojson
          (`Assoc [ ("status", `String "blocked"); ("reason", `String reason) ])
      with
      | Ok (Claim.Blocked decoded) ->
          (not (String.is_empty decoded))
          && String.equal decoded (String.strip reason)
      | Error _ -> String.is_empty (String.strip reason)
      | Ok Claim.Complete -> false)

let () =
  QCheck_base_runner.run_tests_main
    [
      strict_examples;
      arbitrary_status_is_total;
      blocked_requires_nonempty_reason;
    ]
