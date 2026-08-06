open Base
open Onton_core.Types
module Github_effect = Onton_core.Github_effect
module Runtime = Onton.Runtime

let check condition message = if not condition then failwith message
let patch_id = Patch_id.of_string "outbox-patch"
let branch = Branch.of_string "onton/outbox-patch"
let main = Branch.of_string "main"
let pr_number = Pr_number.of_int 42

let patch =
  Patch.
    {
      id = patch_id;
      goal = "prove the durable GitHub outbox";
      branch;
      dependencies = [];
      files = [];
      checks = [];
      agent = None;
    }

let gameplan =
  Gameplan.
    {
      project_name = "outbox";
      repo_owner = "owner";
      repo_name = "repo";
      patches = [ patch ];
    }

let empty_orchestrator () =
  Onton.Orchestrator.create ~patches:[ patch ] ~main_branch:main

let snapshot orchestrator =
  {
    Onton.Runtime.orchestrator;
    activity_log = Onton_core.Activity_log.empty;
    gameplan;
    transcripts = Hashtbl.create (module Patch_id);
  }

let command action = Onton_core.Github_effect.create ~patch_id action

let find_command orchestrator id =
  match Onton.Orchestrator.find_github_effect orchestrator id with
  | Some command -> command
  | None -> failwith "expected GitHub command"

let roundtrip snap =
  match
    snap |> Onton.Persistence.snapshot_to_yojson
    |> Onton.Persistence.snapshot_of_yojson
  with
  | Ok restored -> restored
  | Error message -> failwith message

let property_controller_owned_outbox_surface () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:"controller-owned GitHub outbox transitions compose" ~count:300
       QCheck2.Gen.(pair (int_range 1 10000) bool)
       (fun (number, permanent) ->
         let generated_pr = Pr_number.of_int number in
         let generated_base =
           Branch.of_string (Printf.sprintf "base-%d" number)
         in
         let orch = empty_orchestrator () in
         let orch =
           Onton.Patch_controller.apply_replacement_pr orch patch_id
             ~pr_number:generated_pr ~base_branch:generated_base ~merged:false
         in
         let orch =
           Onton.Orchestrator.set_base_branch orch patch_id generated_base
         in
         let orch =
           Onton.Orchestrator.record_delivered_ci_run_ids orch patch_id
             [ number ]
         in
         let orch =
           Onton.Orchestrator.reset_pr_body_artifact_miss_count orch patch_id
         in
         let agent_exists =
           Option.is_some (Onton.Orchestrator.find_agent orch patch_id)
         in
         let first =
           command (Github_effect.Direct_merge { pr_number = generated_pr })
         in
         let second =
           command (Github_effect.Enqueue { pr_number = generated_pr })
         in
         let orch = Onton.Orchestrator.enqueue_github_effect orch first in
         let orch = Onton.Orchestrator.enqueue_github_effect orch second in
         let found =
           Onton.Orchestrator.find_github_effect orch first.Github_effect.id
         in
         let runnable =
           Onton.Orchestrator.runnable_github_effects orch ~now:0.0
         in
         let orch, claimed =
           Onton.Orchestrator.claim_github_effect orch ~now:0.0
             first.Github_effect.id
         in
         match claimed with
         | None -> false
         | Some claimed ->
             let orch =
               Onton.Patch_controller.finish_github_failure orch ~now:0.0
                 claimed ~permanent ~error:"generated failure"
             in
             let updated =
               Github_effect.retry ~now:1.0 ~delay:1.0 ~error:"retry" second
             in
             let orch = Onton.Orchestrator.update_github_effect orch updated in
             let orch =
               Onton.Orchestrator.remove_github_effects_for_patch orch patch_id
                 ~f:(fun candidate ->
                   Effect_id.equal candidate.Github_effect.id
                     updated.Github_effect.id)
             in
             let orch =
               Onton.Orchestrator.remove_github_effect orch
                 first.Github_effect.id
             in
             let _rebase_orch, rebase_resolution =
               Onton.Orchestrator.reject_rebase_publication orch patch_id
                 (Onton.Orchestrator.Repair_required
                    {
                      target = Onton_core.Validation_repair.Outside_scope;
                      detail = "generated contract rejection";
                    })
             in
             let _conflict_orch, conflict_resolution =
               Onton.Orchestrator.reject_conflict_publication orch patch_id
             in
             agent_exists && Option.is_some found
             && List.length runnable = 2
             && List.is_empty (Onton.Orchestrator.all_github_effects orch)
             && Onton.Orchestrator.equal_rebase_push_resolution
                  rebase_resolution
                  Onton.Orchestrator.Rebase_publication_rejected
             && Onton.Orchestrator.equal_conflict_resolution conflict_resolution
                  Onton.Orchestrator.Conflict_retry_push))

let test_identity_and_claim () =
  let action = Onton_core.Github_effect.Direct_merge { pr_number } in
  let first = command action in
  let second = command action in
  check
    (Effect_id.equal first.Github_effect.id second.Github_effect.id)
    "identity is not deterministic";
  let different = command (Onton_core.Github_effect.Enqueue { pr_number }) in
  check
    (not (Effect_id.equal first.Github_effect.id different.Github_effect.id))
    "different actions share an identity";
  let ambiguous_left =
    command
      (Github_effect.Request_review
         { pr_number; team_slug = "a:b"; head_oid = "c" })
  in
  let ambiguous_right =
    command
      (Github_effect.Request_review
         { pr_number; team_slug = "a"; head_oid = "b:c" })
  in
  check
    (not
       (Effect_id.equal ambiguous_left.Github_effect.id
          ambiguous_right.Github_effect.id))
    "identity encoding is ambiguous across action fields";
  let orch = empty_orchestrator () in
  let orch = Onton.Orchestrator.enqueue_github_effect orch first in
  let orch = Onton.Orchestrator.enqueue_github_effect orch second in
  check
    (List.length (Onton.Orchestrator.all_github_effects orch) = 1)
    "deterministic enqueue was not idempotent";
  let orch, claimed =
    Onton.Orchestrator.claim_github_effect orch ~now:0.0 first.Github_effect.id
  in
  check (Option.is_some claimed) "pending command was not claimable";
  let _orch, claimed_again =
    Onton.Orchestrator.claim_github_effect orch ~now:0.0 first.Github_effect.id
  in
  check (Option.is_none claimed_again) "running command was claimed twice"

let test_restart_replays_running () =
  let original =
    command (Onton_core.Github_effect.Direct_merge { pr_number })
  in
  let orch =
    empty_orchestrator () |> fun orch ->
    Onton.Orchestrator.enqueue_github_effect orch original
  in
  let orch, claimed =
    Onton.Orchestrator.claim_github_effect orch ~now:0.0
      original.Github_effect.id
  in
  check (Option.is_some claimed) "failed to create a running claim";
  let restored = roundtrip (snapshot orch) in
  let replayed =
    find_command restored.Runtime.orchestrator original.Github_effect.id
  in
  check
    (Github_effect.equal_status replayed.Github_effect.status
       Onton_core.Github_effect.Pending)
    "a crashed running command did not restore as pending";
  check
    (List.length
       (Onton.Orchestrator.runnable_github_effects restored.Runtime.orchestrator
          ~now:0.0)
    = 1)
    "restored command is not runnable"

let test_all_actions_and_statuses_roundtrip () =
  let actions =
    [
      Github_effect.Set_pr_draft { pr_number; draft = false };
      Github_effect.Set_pr_base { pr_number; base = main };
      Github_effect.Request_review
        { pr_number; team_slug = "reviewers"; head_oid = "abc123" };
      Github_effect.Direct_merge { pr_number };
      Github_effect.Enqueue { pr_number };
      Github_effect.Dequeue { pr_number; entry_id = "entry-42" };
    ]
  in
  let commands =
    List.mapi actions ~f:(fun index action ->
        let created = command action in
        match index with
        | 0 | 4 -> created
        | 1 -> (
            match Github_effect.claim ~now:0.0 created with
            | Some claimed -> claimed
            | None -> failwith "new command was not claimable")
        | 2 | 5 ->
            Github_effect.retry ~now:10.0 ~delay:5.0 ~error:"later" created
        | 3 -> Github_effect.fail ~error:"terminal" created
        | _ -> failwith "unreachable action index")
  in
  let orch =
    List.fold commands ~init:(empty_orchestrator ())
      ~f:Onton.Orchestrator.enqueue_github_effect
  in
  let restored = roundtrip (snapshot orch) in
  List.iter commands ~f:(fun before ->
      let after =
        find_command restored.Runtime.orchestrator before.Github_effect.id
      in
      check
        (Github_effect.equal_action before.Github_effect.action
           after.Github_effect.action)
        "GitHub action payload changed across persistence";
      let expected_status =
        match before.Github_effect.status with
        | Github_effect.Running -> Github_effect.Pending
        | Github_effect.Pending -> Github_effect.Pending
        | Github_effect.Retry_at at -> Github_effect.Retry_at at
        | Github_effect.Failed -> Github_effect.Failed
      in
      check
        (Github_effect.equal_status after.Github_effect.status expected_status)
        "GitHub command status changed unexpectedly across persistence";
      check
        (after.Github_effect.attempts = before.Github_effect.attempts)
        "GitHub command attempts changed across persistence")

let test_corrupt_identity_is_rejected () =
  let original = command (Github_effect.Direct_merge { pr_number }) in
  let orch =
    empty_orchestrator () |> fun orch ->
    Onton.Orchestrator.enqueue_github_effect orch original
  in
  let json = Onton.Persistence.snapshot_to_yojson (snapshot orch) in
  let corrupt = function
    | `Assoc top_fields ->
        `Assoc
          (List.map top_fields ~f:(fun (top_key, top_value) ->
               if not (String.equal top_key "orchestrator") then
                 (top_key, top_value)
               else
                 match top_value with
                 | `Assoc orchestrator_fields ->
                     ( top_key,
                       `Assoc
                         (List.map orchestrator_fields ~f:(fun (key, value) ->
                              if not (String.equal key "github_outbox") then
                                (key, value)
                              else
                                match value with
                                | `Assoc ((_, payload) :: rest) ->
                                    ( key,
                                      `Assoc (("corrupt-id", payload) :: rest)
                                    )
                                | _ -> (key, value))) )
                 | _ -> (top_key, top_value)))
    | other -> other
  in
  check
    (Result.is_error (Onton.Persistence.snapshot_of_yojson (corrupt json)))
    "persistence accepted an outbox key/payload identity mismatch"

let test_command_for_unknown_patch_is_rejected () =
  let unknown = Patch_id.of_string "unknown-patch" in
  let orphan =
    Github_effect.create ~patch_id:unknown
      (Github_effect.Direct_merge { pr_number })
  in
  let orch =
    empty_orchestrator () |> fun orch ->
    Onton.Orchestrator.enqueue_github_effect orch orphan
  in
  let json = Onton.Persistence.snapshot_to_yojson (snapshot orch) in
  check
    (Result.is_error (Onton.Persistence.snapshot_of_yojson json))
    "persistence silently discarded a command outside the plan"

let test_retry_is_bounded_and_terminal () =
  let original =
    command (Onton_core.Github_effect.Set_pr_draft { pr_number; draft = false })
  in
  let rec fail_attempt orch now remaining =
    let current = find_command orch original.Github_effect.id in
    let orch, claimed =
      Onton.Orchestrator.claim_github_effect orch ~now current.Github_effect.id
    in
    let claimed =
      match claimed with
      | Some value -> value
      | None -> failwith "retry not runnable"
    in
    let orch =
      Onton.Patch_controller.finish_github_failure orch ~now claimed
        ~permanent:false ~error:"temporary"
    in
    if remaining = 1 then orch
    else
      fail_attempt orch
        (now +. Onton.Patch_controller.github_retry_delay)
        (remaining - 1)
  in
  let orch =
    empty_orchestrator () |> fun orch ->
    Onton.Orchestrator.enqueue_github_effect orch original
  in
  let orch = fail_attempt orch 0.0 Onton.Patch_controller.github_max_attempts in
  let failed = find_command orch original.Github_effect.id in
  check
    (Github_effect.equal_status failed.Github_effect.status Github_effect.Failed)
    "retry limit did not persist a terminal command";
  check
    (failed.Github_effect.attempts = Onton.Patch_controller.github_max_attempts)
    "terminal attempt count is wrong";
  check
    (List.is_empty (Onton.Orchestrator.runnable_github_effects orch ~now:1e9))
    "terminal command remained runnable"

let test_automerge_failure_stays_in_outbox () =
  let original =
    command (Onton_core.Github_effect.Direct_merge { pr_number })
  in
  let rec fail_attempt orch now remaining =
    let orch, claimed =
      Onton.Orchestrator.claim_github_effect orch ~now original.Github_effect.id
    in
    let claimed =
      match claimed with
      | Some value -> value
      | None -> failwith "automerge retry not runnable"
    in
    let orch =
      Onton.Patch_controller.finish_github_failure orch ~now claimed
        ~permanent:false ~error:"merge blocked"
    in
    if remaining = 1 then orch
    else
      fail_attempt orch
        (now +. Onton.Patch_controller.automerge_idle_timeout)
        (remaining - 1)
  in
  let orch =
    empty_orchestrator () |> fun orch ->
    Onton.Orchestrator.set_automerge_enabled orch patch_id true |> fun orch ->
    Onton.Orchestrator.enqueue_github_effect orch original
  in
  let orch =
    fail_attempt orch 0.0 Onton.Patch_controller.automerge_max_failures
  in
  let failed = find_command orch original.Github_effect.id in
  let agent = Onton.Orchestrator.agent orch patch_id in
  check
    (Github_effect.equal_status failed.Github_effect.status Github_effect.Failed)
    "automerge exhausted retries without retaining a failed command";
  check
    (Onton_core.Patch_agent.automerge_failure_count agent
    = Onton.Patch_controller.automerge_max_failures)
    "automerge policy failure count drifted from command retries"

let test_automerge_policy_cancels_only_safe_claims () =
  let created = command (Github_effect.Direct_merge { pr_number }) in
  let retrying =
    Github_effect.retry ~now:0.0 ~delay:300.0 ~error:"temporary" created
  in
  let disabled =
    empty_orchestrator () |> fun orch ->
    Onton.Orchestrator.enqueue_github_effect orch retrying
  in
  let disabled, _ =
    Onton.Patch_controller.reconcile_automerge disabled ~now:1.0
  in
  check
    (Option.is_none
       (Onton.Orchestrator.find_github_effect disabled created.Github_effect.id))
    "obsolete retry survived lost automerge eligibility";
  let failed = Github_effect.fail ~error:"terminal" created in
  let enabled =
    empty_orchestrator () |> fun orch ->
    Onton.Orchestrator.set_automerge_enabled orch patch_id true |> fun orch ->
    Onton.Orchestrator.enqueue_github_effect orch failed
  in
  let reconciled, _ =
    Onton.Patch_controller.reconcile_automerge enabled ~now:1.0
  in
  check
    (Option.is_some
       (Onton.Orchestrator.find_github_effect reconciled
          created.Github_effect.id))
    "ordinary reconciliation discarded a terminal failure";
  let toggled =
    Onton.Patch_controller.set_automerge_enabled reconciled patch_id false
  in
  check
    (Option.is_none
       (Onton.Orchestrator.find_github_effect toggled created.Github_effect.id))
    "explicit automerge reset did not clear its terminal command";
  let running =
    match Github_effect.claim ~now:0.0 created with
    | Some command -> command
    | None -> failwith "new command was not claimable"
  in
  let enabled =
    empty_orchestrator () |> fun orch ->
    Onton.Orchestrator.set_automerge_enabled orch patch_id true |> fun orch ->
    Onton.Orchestrator.enqueue_github_effect orch running
  in
  let toggled =
    Onton.Patch_controller.set_automerge_enabled enabled patch_id false
  in
  let retained = find_command toggled created.Github_effect.id in
  check
    (Github_effect.equal_status retained.Github_effect.status
       Github_effect.Running)
    "automerge toggle discarded an in-progress external call"

let test_merge_observation_retains_running_and_failed_commands () =
  let pending = command (Github_effect.Direct_merge { pr_number }) in
  let running =
    command (Github_effect.Enqueue { pr_number }) |> fun created ->
    match Github_effect.claim ~now:0.0 created with
    | Some claimed -> claimed
    | None -> failwith "new enqueue command was not claimable"
  in
  let failed =
    command (Github_effect.Dequeue { pr_number; entry_id = "entry-42" })
    |> Github_effect.fail ~error:"terminal"
  in
  let orch =
    empty_orchestrator () |> fun orch ->
    Onton.Orchestrator.set_automerge_enabled orch patch_id true |> fun orch ->
    Onton.Orchestrator.enqueue_github_effect orch pending |> fun orch ->
    Onton.Orchestrator.enqueue_github_effect orch running |> fun orch ->
    Onton.Orchestrator.enqueue_github_effect orch failed |> fun orch ->
    Onton.Orchestrator.mark_merged orch patch_id
  in
  let reconciled, _ =
    Onton.Patch_controller.reconcile_automerge orch ~now:1.0
  in
  check
    (Option.is_none
       (Onton.Orchestrator.find_github_effect reconciled
          pending.Github_effect.id))
    "merged reconciliation retained an obsolete waiting command";
  let retained_running = find_command reconciled running.Github_effect.id in
  check
    (Github_effect.equal_status retained_running.Github_effect.status
       Github_effect.Running)
    "merged reconciliation discarded an in-progress external call";
  let retained_failed = find_command reconciled failed.Github_effect.id in
  check
    (Github_effect.equal_status retained_failed.Github_effect.status
       Github_effect.Failed)
    "merged reconciliation discarded a terminal failure"

let test_review_reconcile_replaces_stale_retry () =
  let old =
    command
      (Github_effect.Request_review
         { pr_number; team_slug = "old-team"; head_oid = "old-head" })
    |> Github_effect.retry ~now:0.0 ~delay:60.0 ~error:"temporary"
  in
  let orch =
    empty_orchestrator () |> fun orch ->
    Onton.Orchestrator.set_pr_number orch patch_id pr_number |> fun orch ->
    Onton.Orchestrator.set_base_branch orch patch_id main |> fun orch ->
    Onton.Orchestrator.set_head_oid orch patch_id (Some "new-head")
    |> fun orch ->
    Onton.Orchestrator.set_review_decision orch patch_id
      (Some "REVIEW_REQUIRED")
    |> fun orch ->
    Onton.Orchestrator.set_unresolved_comment_count orch patch_id 0
    |> fun orch ->
    Onton.Orchestrator.set_checks_passing orch patch_id true |> fun orch ->
    Onton.Orchestrator.set_is_draft orch patch_id false |> fun orch ->
    Onton.Orchestrator.enqueue_github_effect orch old
  in
  let reconciled, emitted =
    Onton.Patch_controller.reconcile_review_requests orch ~team_slug:"new-team"
  in
  check
    (Option.is_none
       (Onton.Orchestrator.find_github_effect reconciled old.Github_effect.id))
    "stale review retry survived a head/team change";
  match emitted with
  | [ replacement ] -> (
      match replacement.Github_effect.action with
      | Github_effect.Request_review { team_slug; head_oid; _ } ->
          check
            (String.equal team_slug "new-team"
            && String.equal head_oid "new-head")
            "review replacement did not capture current intent"
      | Github_effect.Set_pr_draft _ | Github_effect.Set_pr_base _
      | Github_effect.Direct_merge _ | Github_effect.Enqueue _
      | Github_effect.Dequeue _ ->
          failwith "review reconcile emitted a non-review command")
  | _ -> failwith "review reconcile did not emit exactly one replacement"

let test_failed_writes_do_not_expose_transitions () =
  let original =
    command (Onton_core.Github_effect.Direct_merge { pr_number })
  in
  let saw_prospective = ref false in
  let durable_store proposed =
    saw_prospective :=
      List.length
        (Onton.Orchestrator.all_github_effects proposed.Runtime.orchestrator)
      = 1;
    Error "disk full"
  in
  let runtime =
    Onton.Runtime.create ~gameplan ~main_branch:main ~durable_store ()
  in
  let result =
    Onton.Runtime.commit_orchestrator runtime (fun orch ->
        Onton.Orchestrator.enqueue_github_effect orch original)
  in
  check !saw_prospective "durable store did not receive the prospective command";
  check (Result.is_error result) "failed durable write reported success";
  check
    (Onton.Runtime.read runtime (fun current ->
         List.is_empty
           (Onton.Orchestrator.all_github_effects current.Runtime.orchestrator)))
    "failed durable write changed in-memory state"

let test_failed_outcome_write_keeps_claim () =
  let original =
    command (Onton_core.Github_effect.Direct_merge { pr_number })
  in
  let stores = ref 0 in
  let reject_outcome = ref false in
  let durable_store _ =
    Int.incr stores;
    if !reject_outcome then Error "disk full" else Ok ()
  in
  let runtime =
    Onton.Runtime.create ~gameplan ~main_branch:main ~durable_store ()
  in
  let enqueue_result =
    Onton.Runtime.commit_orchestrator runtime (fun orch ->
        Onton.Orchestrator.enqueue_github_effect orch original)
  in
  check (Result.is_ok enqueue_result) "could not persist command";
  let claim_result =
    Onton.Runtime.commit_orchestrator_returning runtime (fun orch ->
        Onton.Orchestrator.claim_github_effect orch ~now:0.0
          original.Github_effect.id)
  in
  let claimed =
    match claim_result with
    | Ok (Some value) -> value
    | Ok None -> failwith "persisted command was not claimable"
    | Error message -> failwith message
  in
  reject_outcome := true;
  let outcome_result =
    Onton.Runtime.commit_orchestrator runtime (fun orch ->
        Onton.Patch_controller.finish_github_success orch ~now:1.0 claimed
          Onton.Patch_controller.Merge_succeeded)
  in
  check (Result.is_error outcome_result) "failed outcome write reported success";
  let current =
    Onton.Runtime.read runtime (fun snap ->
        ( find_command snap.Runtime.orchestrator original.Github_effect.id,
          Onton.Orchestrator.agent snap.Runtime.orchestrator patch_id ))
  in
  let retained, agent = current in
  check
    (Github_effect.equal_status retained.Github_effect.status
       Github_effect.Running)
    "failed outcome write removed or rewound the claim";
  check
    (not agent.Onton_core.Patch_agent.merged)
    "failed outcome write exposed domain success";
  reject_outcome := false;
  let persisted_outcome =
    Onton.Runtime.commit_orchestrator runtime (fun orch ->
        Onton.Patch_controller.finish_github_success orch ~now:1.0 claimed
          Onton.Patch_controller.Merge_succeeded)
  in
  check (Result.is_ok persisted_outcome) "outcome retry did not persist";
  Onton.Runtime.read runtime (fun snap ->
      check
        (List.is_empty
           (Onton.Orchestrator.all_github_effects snap.Runtime.orchestrator))
        "persisted success did not remove the command";
      check
        (Onton.Orchestrator.agent snap.Runtime.orchestrator patch_id)
          .Onton_core.Patch_agent.merged
        "persisted success did not expose the domain outcome");
  check (!stores = 4) "unexpected number of durable writes"

let () =
  property_controller_owned_outbox_surface ();
  test_identity_and_claim ();
  test_restart_replays_running ();
  test_all_actions_and_statuses_roundtrip ();
  test_corrupt_identity_is_rejected ();
  test_command_for_unknown_patch_is_rejected ();
  test_retry_is_bounded_and_terminal ();
  test_automerge_failure_stays_in_outbox ();
  test_automerge_policy_cancels_only_safe_claims ();
  test_merge_observation_retains_running_and_failed_commands ();
  test_review_reconcile_replaces_stale_retry ();
  test_failed_writes_do_not_expose_transitions ();
  test_failed_outcome_write_keeps_claim ();
  Stdlib.print_endline "GitHub outbox durability tests passed"
