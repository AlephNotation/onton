(* @archlint.module stateTest
   @archlint.domain orchestrator *)

open Base
open Onton
open Onton_core
open Onton_core.Types

(** Random, plan-reachable controller interleavings. The model deliberately has
    no command for adding or removing patches: the parsed plan is the complete
    patch universe. *)

let main = Branch.of_string "main"
let make_patches = Onton_test_support.Test_generators.mk_linear_patches
let make_gameplan = Onton_test_support.Test_generators.make_test_gameplan

let patch_id patches index =
  Onton_test_support.Test_generators.pid_of_idx patches index

type poll = Healthy | Conflict | Ci_failed | Review_feedback | Merged

type session =
  | Session_ok
  | Session_failed
  | Session_context_exhausted
  | Session_no_commits
  | Session_give_up

type rebase = Rebase_ok | Rebase_noop | Rebase_conflict | Rebase_error

type command =
  | Controller_tick
  | Discover_pr of int
  | Observe of int * poll
  | Complete_session of int * session
  | Complete_rebase of int * rebase
  | Human_message of int
  | Reset_intervention of int
  | Clear_closed_pr of int

let show_poll = function
  | Healthy -> "healthy"
  | Conflict -> "conflict"
  | Ci_failed -> "ci-failed"
  | Review_feedback -> "review-feedback"
  | Merged -> "merged"

let show_session = function
  | Session_ok -> "ok"
  | Session_failed -> "failed"
  | Session_context_exhausted -> "context-exhausted"
  | Session_no_commits -> "no-commits"
  | Session_give_up -> "give-up"

let show_rebase = function
  | Rebase_ok -> "ok"
  | Rebase_noop -> "noop"
  | Rebase_conflict -> "conflict"
  | Rebase_error -> "error"

let show_command = function
  | Controller_tick -> "tick"
  | Discover_pr index -> Printf.sprintf "discover-pr(%d)" index
  | Observe (index, poll) -> Printf.sprintf "poll(%d,%s)" index (show_poll poll)
  | Complete_session (index, result) ->
      Printf.sprintf "session(%d,%s)" index (show_session result)
  | Complete_rebase (index, result) ->
      Printf.sprintf "rebase(%d,%s)" index (show_rebase result)
  | Human_message index -> Printf.sprintf "human(%d)" index
  | Reset_intervention index -> Printf.sprintf "reset(%d)" index
  | Clear_closed_pr index -> Printf.sprintf "closed-pr(%d)" index

let branch_of patches =
  let branches =
    List.fold patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun result (patch : Patch.t) ->
        Map.set result ~key:patch.id ~data:patch.branch)
  in
  fun id -> Option.value (Map.find branches id) ~default:main

let poll_result = function
  | Healthy -> ([], false, false, Pr_state.Mergeable, true, true)
  | Conflict ->
      ( [ Operation_kind.Merge_conflict ],
        false,
        false,
        Pr_state.Conflicting,
        false,
        false )
  | Ci_failed ->
      ([ Operation_kind.Ci ], false, false, Pr_state.Mergeable, false, false)
  | Review_feedback ->
      ( [ Operation_kind.Review_comments ],
        false,
        false,
        Pr_state.Mergeable,
        false,
        true )
  | Merged -> ([], true, false, Pr_state.Mergeable, false, false)

let observation kind =
  let queue, merged, closed, merge_state, merge_ready, checks_passing =
    poll_result kind
  in
  let poll_result : Poller.t =
    {
      queue;
      merged;
      closed;
      is_draft = false;
      merge_state;
      merge_ready;
      head_oid = Some "head";
      review_decision = None;
      unresolved_comment_count =
        (if
           List.mem queue Operation_kind.Review_comments
             ~equal:Operation_kind.equal
         then 1
         else 0);
      merge_queue_required = false;
      merge_queue_entry = None;
      checks_passing;
      ci_checks = [];
      merge_commit_sha = (if merged then Some "merge" else None);
    }
  in
  Patch_controller.
    {
      poll_result;
      base_branch = Some main;
      branch_in_root = false;
      worktree_path = Some "/tmp/onton-property-worktree";
    }

let session_result = function
  | Session_ok -> Orchestrator.Session_ok
  | Session_failed ->
      Orchestrator.Session_failed { is_fresh = false; detail = None }
  | Session_context_exhausted -> Orchestrator.Session_context_exhausted
  | Session_no_commits -> Orchestrator.Session_no_commits
  | Session_give_up -> Orchestrator.Session_give_up

let rebase_result = function
  | Rebase_ok -> Worktree.Ok
  | Rebase_noop -> Worktree.Noop
  | Rebase_conflict ->
      Worktree.Conflict
        {
          target = Branch.to_string main;
          old_base = "";
          unique_commits = [];
          strategy = Worktree.Plain;
          orig_head = "";
        }
  | Rebase_error -> Worktree.Error "simulated rebase failure"

let apply patches orchestrator = function
  | Controller_tick ->
      let orchestrator, _actions =
        Patch_controller.tick orchestrator ~project_name:"property"
          ~gameplan:(make_gameplan patches)
      in
      orchestrator
  | Discover_pr index ->
      let id = patch_id patches index in
      let agent = Orchestrator.agent orchestrator id in
      if
        Patch_agent.is_busy agent
        && (not (Patch_agent.has_pr agent))
        && Option.is_none (Patch_agent.current_op agent)
      then
        orchestrator |> fun state ->
        Orchestrator.set_pr_number state id (Pr_number.of_int (index + 1))
        |> fun state -> Orchestrator.complete state id
      else orchestrator
  | Observe (index, kind) ->
      let id = patch_id patches index in
      let agent = Orchestrator.agent orchestrator id in
      if
        Patch_agent.has_pr agent
        && (not (Patch_agent.is_busy agent))
        && not agent.merged
      then
        let orchestrator, _logs, _blocked =
          Patch_controller.apply_poll_result orchestrator id (observation kind)
        in
        orchestrator
      else orchestrator
  | Complete_session (index, result) ->
      let id = patch_id patches index in
      let agent = Orchestrator.agent orchestrator id in
      if
        Patch_agent.is_busy agent && Patch_agent.has_pr agent
        && not
             (Option.equal Operation_kind.equal
                (Patch_agent.current_op agent)
                (Some Operation_kind.Rebase))
      then
        Orchestrator.apply_session_result orchestrator id
          (session_result result)
      else orchestrator
  | Complete_rebase (index, result) ->
      let id = patch_id patches index in
      let agent = Orchestrator.agent orchestrator id in
      if
        Patch_agent.is_busy agent
        && Option.equal Operation_kind.equal
             (Patch_agent.current_op agent)
             (Some Operation_kind.Rebase)
      then
        let has_merged dependency =
          (Orchestrator.agent orchestrator dependency).merged
        in
        let base =
          Graph.initial_base
            (Orchestrator.graph orchestrator)
            id ~has_merged ~branch_of:(branch_of patches) ~main
        in
        fst
          (Orchestrator.apply_rebase_result orchestrator id
             (rebase_result result) base)
      else orchestrator
  | Human_message index ->
      let id = patch_id patches index in
      let agent = Orchestrator.agent orchestrator id in
      if agent.merged then orchestrator
      else Orchestrator.send_human_message orchestrator id "operator guidance"
  | Reset_intervention index ->
      let id = patch_id patches index in
      Orchestrator.reset_intervention_state orchestrator id
  | Clear_closed_pr index ->
      let id = patch_id patches index in
      let agent = Orchestrator.agent orchestrator id in
      if
        Patch_agent.has_pr agent
        && (not (Patch_agent.is_busy agent))
        && not agent.merged
      then Orchestrator.clear_pr orchestrator id
      else orchestrator

let unique_operation_queue agent =
  List.length agent.Patch_agent.queue
  = List.length
      (List.dedup_and_sort agent.queue ~compare:Operation_kind.compare)

let plan_ids patches =
  List.map patches ~f:(fun (patch : Patch.t) -> patch.id)
  |> Set.of_list (module Patch_id)

let agent_ids orchestrator =
  Orchestrator.all_agents orchestrator
  |> List.map ~f:(fun (agent : Patch_agent.t) -> agent.patch_id)
  |> Set.of_list (module Patch_id)

let graph_ids orchestrator =
  Graph.all_patch_ids (Orchestrator.graph orchestrator)
  |> Set.of_list (module Patch_id)

let counters_nonnegative (agent : Patch_agent.t) =
  List.for_all
    [
      agent.ci_failure_count;
      agent.start_attempts_without_pr;
      agent.conflict_noop_count;
      agent.no_commits_push_count;
      agent.context_exhaustion_count;
      agent.push_failure_count;
      agent.rebase_failure_count;
      agent.pr_body_artifact_miss_count;
      agent.review_unresolved_cycle_count;
      Patch_agent.automerge_failure_count agent;
    ]
    ~f:(fun count -> count >= 0)

let check_state patches orchestrator =
  let expected = plan_ids patches in
  Set.equal expected (agent_ids orchestrator)
  && Set.equal expected (graph_ids orchestrator)
  && List.for_all (Orchestrator.all_agents orchestrator)
       ~f:(fun (agent : Patch_agent.t) ->
         ((not (Patch_agent.is_busy agent)) || Patch_agent.has_session agent)
         && ((not agent.merged) || not (Patch_agent.is_busy agent))
         && unique_operation_queue agent
         && counters_nonnegative agent)
  && List.for_all (Orchestrator.all_messages orchestrator) ~f:(fun message ->
      Set.mem expected (Orchestrator.message_patch_id message))

let merged_is_absorbing before after =
  List.for_all (Orchestrator.all_agents before)
    ~f:(fun (agent : Patch_agent.t) ->
      (not agent.merged)
      || (Orchestrator.agent after agent.patch_id).Patch_agent.merged)

let newly_started_respects_dependencies patches before after =
  let graph = Orchestrator.graph before in
  List.for_all patches ~f:(fun (patch : Patch.t) ->
      let prior = Orchestrator.agent before patch.id in
      let next = Orchestrator.agent after patch.id in
      if Patch_agent.has_session prior || not (Patch_agent.has_session next)
      then true
      else
        Graph.deps_satisfied graph patch.id
          ~has_merged:(fun id -> (Orchestrator.agent before id).merged)
          ~has_pr:(fun id -> Patch_agent.has_pr (Orchestrator.agent before id)))

let run_sequence patches commands =
  let initial = Orchestrator.create ~patches ~main_branch:main in
  let rec loop state = function
    | [] -> check_state patches state
    | command :: rest -> (
        try
          let next = apply patches state command in
          if
            check_state patches next
            && merged_is_absorbing state next
            && newly_started_respects_dependencies patches state next
          then loop next rest
          else false
        with Invalid_argument _ -> false)
  in
  loop initial commands

let gen_poll =
  QCheck2.Gen.oneof_list
    [ Healthy; Conflict; Ci_failed; Review_feedback; Merged ]

let gen_session =
  QCheck2.Gen.oneof_list
    [
      Session_ok;
      Session_failed;
      Session_context_exhausted;
      Session_no_commits;
      Session_give_up;
    ]

let gen_rebase =
  QCheck2.Gen.oneof_list
    [ Rebase_ok; Rebase_noop; Rebase_conflict; Rebase_error ]

let gen_command patch_count =
  let open QCheck2.Gen in
  let index = int_range 0 (patch_count - 1) in
  oneof
    [
      return Controller_tick;
      map (fun i -> Discover_pr i) index;
      map2 (fun i p -> Observe (i, p)) index gen_poll;
      map2 (fun i result -> Complete_session (i, result)) index gen_session;
      map2 (fun i result -> Complete_rebase (i, result)) index gen_rebase;
      map (fun i -> Human_message i) index;
      map (fun i -> Reset_intervention i) index;
      map (fun i -> Clear_closed_pr i) index;
    ]

let scenario =
  let open QCheck2.Gen in
  let* patch_count = int_range 1 5 in
  let* commands = list_size (int_range 1 250) (gen_command patch_count) in
  return (make_patches patch_count, commands)

let print_scenario (_patches, commands) =
  commands |> List.map ~f:show_command |> String.concat ~sep:"; "

let () =
  let property =
    QCheck2.Test.make
      ~name:"plan-reachable interleavings preserve lifecycle invariants"
      ~count:2_000 ~print:print_scenario scenario (fun (patches, commands) ->
        run_sequence patches commands)
  in
  let exit_code = QCheck_base_runner.run_tests ~verbose:true [ property ] in
  if exit_code <> 0 then Stdlib.exit exit_code
