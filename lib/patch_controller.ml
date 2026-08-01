(* @archlint.module core
   @archlint.domain orchestrator *)

open Base
open Types

let action_identity = function
  | Orchestrator.Start (patch_id, base) ->
      Printf.sprintf "start:%s:%s"
        (Patch_id.to_string patch_id)
        (Branch.to_string base)
  | Orchestrator.Respond (patch_id, kind) ->
      Printf.sprintf "respond:%s:%s"
        (Patch_id.to_string patch_id)
        (Operation_kind.to_label kind)
  | Orchestrator.Rebase (patch_id, base) ->
      Printf.sprintf "rebase:%s:%s"
        (Patch_id.to_string patch_id)
        (Branch.to_string base)

let message_of_action (patch_agent : Patch_agent.t) action =
  let identity =
    Printf.sprintf "%d:%s" patch_agent.generation (action_identity action)
  in
  let digest = Stdlib.Digest.to_hex (Stdlib.Digest.string identity) in
  let msg_id =
    Message_id.of_string
      (Printf.sprintf "%s:%s" (Patch_id.to_string patch_agent.patch_id) digest)
  in
  Orchestrator.
    {
      message_id = msg_id;
      patch_id = patch_agent.patch_id;
      generation = patch_agent.generation;
      action;
      payload_hash = digest;
      status = Pending;
    }

type poll_log_entry = { message : string; patch_id : Patch_id.t }
[@@deriving show, eq]

type poll_observation = {
  poll_result : Poller.t;
  base_branch : Branch.t option;
  branch_in_root : bool;
  worktree_path : string option;
}

let discovery_intents orch =
  Orchestrator.all_agents orch
  |> List.filter_map ~f:(fun (agent : Patch_agent.t) ->
      if
        Patch_agent.has_session agent
        && (not (Patch_agent.has_pr agent))
        && not agent.merged
      then Some (agent.patch_id, agent.branch)
      else None)

let enqueue_pr_body_if_needed t patch_id (agent : Patch_agent.t) =
  (* [needs_intervention] guard: without it, after two Respond_pr_body_miss
     outcomes push [pr_body_artifact_miss_count] to the cap, the reconciler
     would re-enqueue Pr_body on every tick and immediately override the
     escalation — defeating the retry-once-then-intervene contract. *)
  if
    (not (Patch_agent.has_pr agent))
    || agent.merged || agent.pr_body_delivered
    || Patch_agent.needs_intervention agent
  then t
  else
    let already_queued =
      List.mem agent.queue Operation_kind.Pr_body ~equal:Operation_kind.equal
      || Option.equal Operation_kind.equal
           (Patch_agent.current_op agent)
           (Some Operation_kind.Pr_body)
    in
    if already_queued then t
    else Orchestrator.enqueue t patch_id Operation_kind.Pr_body

let apply_poll_result ?(merge_queue_ejection_confirmed = false) t patch_id
    ({ poll_result; base_branch; branch_in_root; worktree_path } :
      poll_observation) =
  let logs = ref [] in
  let log message = logs := { message; patch_id } :: !logs in
  let poll_result, merge_queue_failure =
    let agent = Orchestrator.agent t patch_id in
    let application =
      Merge_queue_decision.apply ~previous_entry:agent.merge_queue_entry
        ~ejection_confirmed:merge_queue_ejection_confirmed poll_result
    in
    if application.merge_queue_ejected then
      log "Merge queue removed PR after checks failed — treating as CI failure";
    (application.poll_result, application.merge_queue_ejected)
  in
  let t =
    if poll_result.merged then (
      let agent = Orchestrator.agent t patch_id in
      if not agent.Patch_agent.merged then log "Merged";
      let t = Orchestrator.mark_merged t patch_id in
      (* Record the squash/merge commit SHA so dependents' base-containment gate
         can ancestry-check it. Only overwrite when present, so a late/absent
         [mergeCommit.oid] does not clobber a previously recorded SHA. Merged
         agents that still lack a SHA are kept in the poll loop (see
         [poller_fiber]) until it arrives. *)
      match poll_result.Poller.merge_commit_sha with
      | Some _ as sha -> Orchestrator.set_merge_commit_sha t patch_id sha
      | None -> t)
    else t
  in
  let t =
    if Poller.has_conflict poll_result then (
      let agent = Orchestrator.agent t patch_id in
      if not agent.Patch_agent.has_conflict then log "Merge conflict detected";
      Orchestrator.set_has_conflict t patch_id)
    else
      let agent = Orchestrator.agent t patch_id in
      if agent.Patch_agent.has_conflict then
        if Patch_decision.should_clear_conflict agent then (
          log "Conflict cleared — no longer detected";
          let t = Orchestrator.clear_has_conflict t patch_id in
          Orchestrator.reset_conflict_noop_count t patch_id)
        else (
          log "Conflict flag retained — resolution in flight";
          t)
      else t
  in
  let t = Orchestrator.set_ci_checks t patch_id poll_result.ci_checks in
  let agent_before = Orchestrator.agent t patch_id in
  let t =
    List.fold poll_result.queue ~init:t ~f:(fun acc kind ->
        let current_agent = Orchestrator.agent acc patch_id in
        let already_queued =
          List.mem current_agent.Patch_agent.queue kind
            ~equal:Operation_kind.equal
        in
        let is_new =
          (not already_queued)
          && not
               (Option.equal Operation_kind.equal
                  (Patch_agent.current_op agent_before)
                  (Some kind))
        in
        match kind with
        | Operation_kind.Ci -> (
            match Patch_decision.on_ci_failure agent_before with
            | Patch_decision.Enqueue_ci ->
                if is_new then
                  log
                    (Printf.sprintf "Enqueued %s"
                       (Operation_kind.to_label kind));
                Orchestrator.enqueue acc patch_id kind
            | Patch_decision.Ci_already_queued ->
                log "CI already queued — skipping";
                acc
            | Patch_decision.Ci_fix_in_progress ->
                log "CI fix in progress — skipping";
                acc
            | Patch_decision.Ci_already_delivered ->
                log "CI failure already delivered — skipping";
                acc
            | Patch_decision.Cap_reached ->
                log
                  (Printf.sprintf
                     "CI failure cap reached (>=%d) — skipping CI enqueue"
                     agent_before.Patch_agent.max_ci_failures);
                acc)
        | Operation_kind.Review_comments | Operation_kind.Findings ->
            if is_new then
              log (Printf.sprintf "Enqueued %s" (Operation_kind.to_label kind));
            Orchestrator.enqueue acc patch_id kind
        | Operation_kind.Merge_conflict ->
            if is_new then
              log (Printf.sprintf "Enqueued %s" (Operation_kind.to_label kind));
            Orchestrator.enqueue acc patch_id kind
        | Operation_kind.Rebase | Operation_kind.Human | Operation_kind.Pr_body
          ->
            if is_new then
              log (Printf.sprintf "Enqueued %s" (Operation_kind.to_label kind));
            Orchestrator.enqueue acc patch_id kind)
  in
  let t = Orchestrator.set_merge_ready t patch_id poll_result.merge_ready in
  let t = Orchestrator.set_head_oid t patch_id poll_result.head_oid in
  let t =
    Orchestrator.set_review_decision t patch_id poll_result.review_decision
  in
  let t =
    Orchestrator.set_unresolved_comment_count t patch_id
      poll_result.unresolved_comment_count
  in
  let t =
    Orchestrator.set_mergeability_unknown t patch_id
      (Poller.mergeability_unknown poll_result)
  in
  let t =
    Orchestrator.observe_merge_queue t patch_id
      ~required:poll_result.merge_queue_required
      ~entry:poll_result.merge_queue_entry
  in
  let t =
    let was_draft = (Orchestrator.agent t patch_id).Patch_agent.is_draft in
    let t = Orchestrator.set_is_draft t patch_id poll_result.is_draft in
    if (not was_draft) && poll_result.is_draft then log "Marked as draft"
    else if was_draft && not poll_result.is_draft then
      log "Marked as ready for review";
    t
  in
  let t =
    Orchestrator.set_checks_passing t patch_id poll_result.checks_passing
  in
  let t =
    if merge_queue_failure then Orchestrator.clear_automerge_deadline t patch_id
    else t
  in
  let t =
    let agent = Orchestrator.agent t patch_id in
    match
      Patch_decision.on_checks_passing agent
        ~checks_passing:poll_result.checks_passing
    with
    | Patch_decision.Reset_ci_failure_count ->
        log "CI checks now passing — cleared failure history";
        Orchestrator.reset_ci_failure_count t patch_id
    | Patch_decision.No_ci_reset -> t
  in
  let t, newly_blocked =
    if branch_in_root then
      let agent = Orchestrator.agent t patch_id in
      if
        (not agent.Patch_agent.branch_blocked)
        && Option.is_none agent.Patch_agent.worktree_path
      then (
        log "Branch checked out in repo root — blocking worktree operations";
        (Orchestrator.set_branch_blocked t patch_id, true))
      else if
        (not agent.Patch_agent.branch_blocked)
        && Option.is_some agent.Patch_agent.worktree_path
      then (t, false)
      else (Orchestrator.set_branch_blocked t patch_id, false)
    else
      let agent = Orchestrator.agent t patch_id in
      if agent.Patch_agent.branch_blocked then (
        log "Branch no longer in repo root — unblocked";
        (Orchestrator.clear_branch_blocked t patch_id, false))
      else (t, false)
  in
  let t =
    match base_branch with
    | Some branch -> Orchestrator.set_base_branch t patch_id branch
    | None -> t
  in
  let t =
    let agent = Orchestrator.agent t patch_id in
    match worktree_path with
    | Some path when Option.is_none agent.Patch_agent.worktree_path ->
        let t = Orchestrator.set_worktree_path t patch_id path in
        let agent = Orchestrator.agent t patch_id in
        if agent.Patch_agent.branch_blocked then (
          log "Worktree path discovered — unblocking branch operations";
          Orchestrator.clear_branch_blocked t patch_id)
        else t
    | _ -> t
  in
  (t, List.rev !logs, newly_blocked)

let apply_replacement_pr t patch_id ~pr_number ~base_branch ~merged =
  let t = Orchestrator.set_pr_number t patch_id pr_number in
  let t = Orchestrator.set_base_branch t patch_id base_branch in
  if merged then Orchestrator.mark_merged t patch_id else t

(* The mark-for-review "ready for review" fixpoint for a patch, read from
   orchestrator state. A PR has reached it when its expected base is [main]
   (every dependency merged), its PR body is delivered, no merge conflict is
   active, its local branch is actually rebased onto that base, and CI is green.
   [branch_rebased_onto] lags [expected_base] until the Rebase action lands,
   and [has_conflict] is set the moment a rebase or push surfaces a conflict —
   both must clear before the fixpoint holds. Gates the draft flip in
   [reconcile_pr_settings].

   With more than one open dep the expected base is not yet a single branch
   ([Graph.initial_base] would raise), so the fixpoint is unreachable: [false]. *)
let ready_for_review t patch_id =
  let agent = Orchestrator.agent t patch_id in
  let graph = Orchestrator.graph t in
  let has_merged pid = (Orchestrator.agent t pid).Patch_agent.merged in
  List.length (Graph.open_pr_deps graph patch_id ~has_merged) <= 1
  &&
  let branch_of pid = (Orchestrator.agent t pid).Patch_agent.branch in
  let expected_base =
    Graph.initial_base graph patch_id ~has_merged ~branch_of
      ~main:(Orchestrator.main_branch t)
  in
  let rebase_pending =
    match agent.Patch_agent.branch_rebased_onto with
    | Some b -> not (Branch.equal b expected_base)
    | None -> true
  in
  Branch.equal expected_base (Orchestrator.main_branch t)
  && agent.Patch_agent.pr_body_delivered
  && (not agent.Patch_agent.has_conflict)
  && (not rebase_pending) && agent.Patch_agent.checks_passing

(* The gate a child's Start waits on for its sole open-PR dependency: the
   dependency's PR body is delivered, no merge conflict is active, and CI is
   green. This is the [ready_for_review] set MINUS its "targets main / rebased
   onto main" conjuncts — a child may legitimately start stacked on a
   dependency that is itself still mid-chain (not yet drained to main), so
   requiring the dependency to be on main would needlessly serialize deep
   stacks. It strengthens the older deps-notes-ready gate ([pr_body_delivered]
   alone) with the dependency's mergeability ([not has_conflict]) and CI health.

   Deadlock-free: every conjunct is a property of the dependency itself, never
   of the dependent, so gating introduces no wait cycle. Merged deps are exempt
   at the call site ([Graph.open_pr_deps] filters them out), so a human-merged
   parent that never delivered notes / went green cannot strand its child. *)
let open_dep_review_ready t pid =
  let a = Orchestrator.agent t pid in
  a.Patch_agent.pr_body_delivered
  && (not a.Patch_agent.has_conflict)
  && a.Patch_agent.checks_passing

(* Reconcile the PR settings owned by the plan executor. A PR left targeting a merged
   dependency's branch diffs against frozen pre-merge history, so GitHub
   reports conflicts that no rebase onto main can clear (the local rebase
   noops, the poller re-reads the stale base, and the two spin forever). *)
let reconcile_pr_settings t patch_id =
  let agent = Orchestrator.agent t patch_id in
  if agent.Patch_agent.merged then []
  else begin
    let commands = ref [] in
    (* Gate on [is_pr_present]: emitting Set_pr_draft / Set_pr_base against a
       [Missing] PR would 404 against GitHub. A gameplan patch can in
       principle reach Missing if the poller ever wires that for gameplan
       patches in the future; defensively skip it here either way. *)
    (match Patch_agent.pr_number agent with
    | Some pr_number when Patch_agent.has_pr agent -> (
        match agent.base_branch with
        | Some actual_base ->
            let has_merged pid =
              (Orchestrator.agent t pid).Patch_agent.merged
            in
            let open_deps =
              Graph.open_pr_deps (Orchestrator.graph t) patch_id ~has_merged
            in
            if List.length open_deps <= 1 then begin
              let branch_of pid =
                (Orchestrator.agent t pid).Patch_agent.branch
              in
              let expected_base =
                Graph.initial_base (Orchestrator.graph t) patch_id ~has_merged
                  ~branch_of
                  ~main:(Orchestrator.main_branch t)
              in
              (* Mark-for-review is a one-way ratchet: we only emit
                 [draft = false]. Re-drafting a ready PR is disruptive to
                 reviewers, so the controller defers the transition until the
                 patch reaches the [ready_for_review] fixpoint and then never
                 flips back. *)
              if agent.is_draft && ready_for_review t patch_id then
                commands :=
                  Github_effect.create ~patch_id
                    (Github_effect.Set_pr_draft { pr_number; draft = false })
                  :: !commands;
              if not (Branch.equal actual_base expected_base) then
                commands :=
                  Github_effect.create ~patch_id
                    (Github_effect.Set_pr_base
                       { pr_number; base = expected_base })
                  :: !commands
            end
        | None -> ())
    | Some _ | None -> ());
    List.rev !commands
  end

let reconcile_patch t ~project_name:_ ~gameplan:_ ~(patch : Patch.t) =
  let patch_id = patch.id in
  let agent = Orchestrator.agent t patch_id in
  if agent.Patch_agent.merged then t
  else
    let t = enqueue_pr_body_if_needed t patch_id agent in
    let commands = reconcile_pr_settings t patch_id in
    List.fold commands ~init:t ~f:Orchestrator.enqueue_github_effect

let reconcile_all t ~project_name ~gameplan =
  List.fold gameplan.Gameplan.patches ~init:t ~f:(fun orch patch ->
      reconcile_patch orch ~project_name ~gameplan ~patch)

let branch_map_of_patches patches =
  List.fold patches
    ~init:(Map.empty (module Patch_id))
    ~f:(fun acc (p : Patch.t) ->
      match Map.add acc ~key:p.Patch.id ~data:p.Patch.branch with
      | `Ok m -> m
      | `Duplicate ->
          invalid_arg
            (Printf.sprintf
               "Patch_controller.plan_actions: duplicate patch id %s"
               (Patch_id.to_string p.Patch.id)))

let plan_action_for_patch t ~branch_map patch_id =
  let agent = Orchestrator.agent t patch_id in
  let has_merged pid = (Orchestrator.agent t pid).Patch_agent.merged in
  let has_pr pid =
    (* Dependency satisfaction: a child cannot rebase onto a [Missing] parent
       — the parent's branch may not exist on the remote. Gate on
       is_pr_present rather than has_pr (which is true for [Missing] too). *)
    Patch_agent.has_pr (Orchestrator.agent t pid)
  in
  if
    (not (Patch_agent.has_pr agent))
    && (not (Patch_agent.is_busy agent))
    && (not agent.Patch_agent.merged)
    && (not (Patch_agent.needs_intervention agent))
    && Graph.deps_satisfied (Orchestrator.graph t) patch_id ~has_merged ~has_pr
    (* A child may only Start once its sole open-PR dependency is itself
       review-ready ([open_dep_review_ready]): PR body delivered, no conflict,
       CI green. This subsumes the older deps-notes-ready gate
       ([pr_body_delivered] is one of its conjuncts). Merged deps are exempt:
       [open_pr_deps] filters them out, so a human-merged parent that never
       delivered notes can never strand its child. *)
    && List.for_all
         (Graph.open_pr_deps (Orchestrator.graph t) patch_id ~has_merged)
         ~f:(fun dep -> open_dep_review_ready t dep)
  then
    let branch_of pid =
      match Map.find branch_map pid with
      | Some b -> b
      | None ->
          invalid_arg
            (Printf.sprintf
               "Patch_controller.plan_actions: no branch for patch %s"
               (Patch_id.to_string pid))
    in
    let base =
      Graph.initial_base (Orchestrator.graph t) patch_id ~has_merged ~branch_of
        ~main:(Orchestrator.main_branch t)
    in
    Some (Orchestrator.Start (patch_id, base))
  else if
    (* Rebase is exempt from [needs_intervention] because it is
       orchestrator-executed (no LLM session) and does not mutate the
       counters that triggered intervention. Otherwise a [needs-help] patch
       would sit on a stale base until a human message happened to land and
       flip the Human exemption inside [Patch_agent.needs_intervention]. A
       conflict outcome still enqueues [Merge_conflict], which keeps the
       patch blocked until an LLM session can run. Gate on [is_pr_present]
       (not [has_pr]) so a [Missing] PR is excluded — rebase pushes against
       a vanished PR would 404. *)
    Patch_agent.has_pr agent
    && (not agent.Patch_agent.merged)
    && (not (Patch_agent.is_busy agent))
    && (not agent.Patch_agent.branch_blocked)
    && List.mem agent.Patch_agent.queue Operation_kind.Rebase
         ~equal:Operation_kind.equal
  then
    match Patch_agent.highest_priority agent with
    | Some highest when Operation_kind.equal highest Operation_kind.Rebase ->
        let branch_of dep_pid =
          match Map.find branch_map dep_pid with
          | Some b -> b
          | None ->
              Option.value
                (Orchestrator.agent t dep_pid).Patch_agent.base_branch
                ~default:(Orchestrator.main_branch t)
        in
        let new_base =
          Graph.initial_base (Orchestrator.graph t) patch_id ~has_merged
            ~branch_of
            ~main:(Orchestrator.main_branch t)
        in
        Some (Orchestrator.Rebase (patch_id, new_base))
    | _ -> None
  else if
    Patch_agent.has_pr agent
    && (not agent.Patch_agent.merged)
    && (not (Patch_agent.is_busy agent))
    && (not (Patch_agent.needs_intervention agent))
    && not agent.Patch_agent.branch_blocked
  then
    Patch_agent.highest_priority agent
    |> Option.bind ~f:(fun kind ->
        if Priority.is_feedback kind then
          Some (Orchestrator.Respond (patch_id, kind))
        else None)
  else None

let reconcile_action_message t action =
  let patch_id =
    match action with
    | Orchestrator.Start (patch_id, _)
    | Orchestrator.Respond (patch_id, _)
    | Orchestrator.Rebase (patch_id, _) ->
        patch_id
  in
  let agent = Orchestrator.agent t patch_id in
  let msg = message_of_action agent action in
  let t = Orchestrator.reconcile_message t msg in
  let msg = Option.value_exn (Orchestrator.find_message t msg.message_id) in
  (t, msg)

let reconcile_messages t ~patches =
  let branch_map = branch_map_of_patches patches in
  let patch_ids = Graph.all_patch_ids (Orchestrator.graph t) in
  let t =
    List.fold patch_ids ~init:t ~f:(fun acc patch_id ->
        let keep =
          match Orchestrator.current_message acc patch_id with
          | Some msg when Orchestrator.equal_message_status msg.status Acked ->
              [ msg.message_id ]
          | _ -> []
        in
        Orchestrator.mark_patch_pending_messages_obsolete_except acc patch_id
          ~keep)
  in
  let t, desired_ids =
    List.fold patch_ids ~init:(t, []) ~f:(fun (acc, ids) patch_id ->
        match Orchestrator.current_message acc patch_id with
        | Some msg when Orchestrator.equal_message_status msg.status Acked ->
            (acc, msg.message_id :: ids)
        | _ -> (
            match plan_action_for_patch acc ~branch_map patch_id with
            | None -> (acc, ids)
            | Some action ->
                let acc, msg = reconcile_action_message acc action in
                (acc, msg.message_id :: ids)))
  in
  let t =
    List.fold patch_ids ~init:t ~f:(fun acc patch_id ->
        let keep =
          List.filter desired_ids ~f:(fun message_id ->
              match Orchestrator.find_message acc message_id with
              | Some msg when Patch_id.equal msg.patch_id patch_id -> true
              | _ -> false)
        in
        Orchestrator.mark_patch_pending_messages_obsolete_except acc patch_id
          ~keep)
  in
  (t, Orchestrator.runnable_messages t)

let plan_messages t ~patches = snd (reconcile_messages t ~patches)

let plan_actions t ~patches =
  plan_messages t ~patches
  |> List.map ~f:(fun (msg : Orchestrator.patch_agent_message) -> msg.action)

let plan_tick_messages t ~project_name ~gameplan =
  let t = reconcile_all t ~project_name ~gameplan in
  let t, messages = reconcile_messages t ~patches:gameplan.Gameplan.patches in
  (t, messages)

let plan_tick t ~project_name ~gameplan =
  let t, messages = plan_tick_messages t ~project_name ~gameplan in
  let actions =
    List.map messages ~f:(fun (msg : Orchestrator.patch_agent_message) ->
        msg.action)
  in
  (t, actions)

let tick t ~project_name ~gameplan =
  let t, actions = plan_tick t ~project_name ~gameplan in
  let t =
    List.fold actions ~init:t ~f:(fun acc action ->
        Orchestrator.fire acc action)
  in
  (t, actions)

let automerge_idle_timeout = 300.0
let automerge_max_failures = 3

(** Pure predicate: a patch is a candidate to start a new automerge command when
    it is not already merged, automerge is enabled, the PR is approved, CI is
    passing, the queue is empty, and the consecutive failure count is under
    [automerge_max_failures]. New feedback (Review_comments, Human, Ci,
    Merge_conflict, Pr_body) enqueues an operation, which fails this check and
    so resets the deadline. [merge_ready] is now component-derived
    ([Pr_state.merge_ready_of]: mergeable + CI passing + non-blocking review),
    not GitHub's stale [mergeStateStatus]; the merge *attempt* is the final
    authority, so a PR deemed ready that GitHub still blocks is rejected by the
    merge call and backs off via the failure counter. [checks_passing] is
    retained as a guard alongside the [merge_ready] CI term.

    The [not merged] guard is included here because a merged PR can retain a
    stale [merge_ready = true] in the orchestrator snapshot (e.g. the poller
    hasn't yet observed the merge or the PR was merged out-of-band); without
    this guard such a PR would re-qualify as a candidate. The outbox command,
    checked separately by [reconcile_automerge], is the sole execution claim. *)
let is_automerge_candidate (agent : Patch_agent.t) ~main_branch =
  let enqueued_and_still_approved =
    Option.is_some agent.Patch_agent.merge_queue_entry
    && Patch_agent.is_approved agent ~main_branch
  in
  (not agent.Patch_agent.merged)
  && Patch_agent.automerge_enabled agent
  && Patch_agent.is_approved agent ~main_branch
  && (not enqueued_and_still_approved)
  && agent.Patch_agent.checks_passing
  && List.is_empty agent.Patch_agent.queue
  && Patch_agent.automerge_failure_count agent < automerge_max_failures

(** [true] when a patch has momentarily lost [merge_ready] *only* because GitHub
    is recomputing mergeability ([mergeability_unknown], i.e.
    [Pr_state.merge_state = Unknown]), while every real precondition for
    automerge still holds. This is the benign flap caused by the base branch
    advancing — e.g. a sibling patch merging invalidates mergeability on every
    other open PR at once, so they all read [Unknown] for a poll or two before
    settling back to [Mergeable].

    When this holds, [reconcile_automerge] preserves the existing
    [automerge_deadline] instead of clearing it, so the idle window measures
    real elapsed wall-clock rather than restarting on every sibling merge. We
    never *fire* a merge in this state (the decision branches require
    [merge_ready], which is component-derived and false while mergeability is
    [Unknown]); the hold only protects a timer that is already armed.

    Keyed on the live mergeability component ([mergeability_unknown]), not
    GitHub's stale [mergeStateStatus] rollup — that field no longer exists on
    the agent. Deliberately scoped to the direct-merge path
    ([merge_queue_entry = None]) and to [Unknown] alone — a real conflict
    ([has_conflict]), a failing check, new feedback (non-empty queue), or a hit
    failure cap all fall through to the normal clear. [checks_passing] is
    required so a genuine CI regression that coincides with an [Unknown] window
    still resets the timer. *)
let automerge_transient_hold (agent : Patch_agent.t) ~main_branch =
  agent.Patch_agent.mergeability_unknown
  && (not agent.Patch_agent.merged)
  && Patch_agent.automerge_enabled agent
  && Patch_agent.is_approved_modulo_merge_ready agent ~main_branch
  && agent.Patch_agent.checks_passing
  && List.is_empty agent.Patch_agent.queue
  && Patch_agent.automerge_failure_count agent < automerge_max_failures
  && Option.is_none agent.Patch_agent.merge_queue_entry

type merge_action = Direct_merge | Enqueue | Dequeue of string
[@@deriving show, eq, sexp_of]

let is_automerge_github_action = function
  | Github_effect.Direct_merge _ | Github_effect.Enqueue _
  | Github_effect.Dequeue _ ->
      true
  | Github_effect.Set_pr_draft _ | Github_effect.Set_pr_base _
  | Github_effect.Request_review _ ->
      false

let has_automerge_command t patch_id =
  Orchestrator.github_effects_for_patch t patch_id
  |> List.exists ~f:(fun command ->
      is_automerge_github_action command.Github_effect.action)

let set_automerge_enabled t patch_id enabled =
  let agent = Orchestrator.agent t patch_id in
  if Bool.equal (Patch_agent.automerge_enabled agent) enabled then t
  else
    let t = Orchestrator.set_automerge_enabled t patch_id enabled in
    Orchestrator.remove_github_effects_for_patch t patch_id ~f:(fun command ->
        is_automerge_github_action command.Github_effect.action
        &&
        match command.Github_effect.status with
        | Github_effect.Pending | Github_effect.Retry_at _
        | Github_effect.Failed ->
            true
        | Github_effect.Running -> false)

let merge_queue_entry_unmergeable (entry : Pr_state.merge_queue_entry) =
  Pr_state.equal_merge_queue_entry_state entry.state Pr_state.Mq_unmergeable

let merge_queue_alarm (agent : Patch_agent.t) =
  agent.Patch_agent.has_conflict
  || List.exists agent.Patch_agent.ci_checks ~f:Ci_check.is_failure
  || Option.exists agent.Patch_agent.merge_queue_entry
       ~f:merge_queue_entry_unmergeable

let merge_queue_transient_unknown_hold (agent : Patch_agent.t) ~main_branch =
  agent.Patch_agent.mergeability_unknown
  && Patch_agent.is_approved_modulo_merge_ready agent ~main_branch
  && agent.Patch_agent.checks_passing
  && List.is_empty agent.Patch_agent.queue

let should_dequeue_merge_queue agent ~main_branch ~entry_id =
  match agent.Patch_agent.merge_queue_entry with
  | Some entry when String.equal entry.Pr_state.id entry_id ->
      merge_queue_alarm agent
      || (not (Patch_agent.is_approved agent ~main_branch))
         && not (merge_queue_transient_unknown_hold agent ~main_branch)
  | Some _ | None -> false

let automerge_command_still_desired agent ~main_branch = function
  | Github_effect.Direct_merge _ ->
      is_automerge_candidate agent ~main_branch
      && (not agent.Patch_agent.merge_queue_required)
      && Option.is_none agent.Patch_agent.merge_queue_entry
  | Github_effect.Enqueue _ ->
      is_automerge_candidate agent ~main_branch
      && agent.Patch_agent.merge_queue_required
      && Option.is_none agent.Patch_agent.merge_queue_entry
  | Github_effect.Dequeue { entry_id; _ } ->
      Patch_agent.automerge_enabled agent
      && Patch_agent.automerge_failure_count agent < automerge_max_failures
      && should_dequeue_merge_queue agent ~main_branch ~entry_id
  | Github_effect.Set_pr_draft _ | Github_effect.Set_pr_base _
  | Github_effect.Request_review _ ->
      false

let automerge_action (agent : Patch_agent.t) ~main_branch =
  let approved = Patch_agent.is_approved agent ~main_branch in
  match (agent.Patch_agent.merge_queue_required, agent.merge_queue_entry) with
  | true, Some entry
    when should_dequeue_merge_queue agent ~main_branch ~entry_id:entry.id ->
      Some (Dequeue entry.id)
  | true, Some _ -> None
  | true, None when approved -> Some Enqueue
  | true, None -> None
  | false, Some entry
    when should_dequeue_merge_queue agent ~main_branch ~entry_id:entry.id ->
      Some (Dequeue entry.id)
  | false, Some _ -> None
  | false, None -> Some Direct_merge

let apply_automerge_failure_state t ~now patch_id =
  Orchestrator.apply_automerge_failure_state t patch_id
    ~retry_deadline:(now +. automerge_idle_timeout)
    ~max_failures:automerge_max_failures

(** Reconcile the automerge deadline for every agent and durably stage commands
    whose deadline has elapsed.

    For each agent:
    - If the patch is [merged], clear any stale deadline and automerge command —
      PRs merged outside [apply_automerge_success] (manual merge, replacement
      PR, etc.) must not keep a leftover timer in persisted state or the UI.
    - If an automerge command already exists, no-op. Its outbox status is the
      sole claim across pending, running, retrying, and terminal states.
    - If [automerge_enabled = false], [is_automerge_candidate] returns false,
      which funnels into the non-candidate branches below (clearing any stale
      deadline). No separate early-return is needed.
    - If the patch is a candidate and has no deadline, set one at
      [now +. automerge_idle_timeout].
    - If the patch is not a candidate and has a deadline, clear it — any
      feedback resets the timer, so enabling again must wait out a fresh
      5-minute window.
    - If the patch is a candidate and the deadline has elapsed, enqueue one
      deterministic outbox command. The deadline is left in place; success or
      failure owns clearing/rearming it and advancing the failure counter. *)
let reconcile_automerge t ~now =
  let agents = Orchestrator.all_agents t in
  let main_branch = Orchestrator.main_branch t in
  List.fold agents ~init:(t, []) ~f:(fun (t, commands) agent ->
      let patch_id = agent.Patch_agent.patch_id in
      if agent.Patch_agent.merged then
        let t =
          if Option.is_some (Patch_agent.automerge_deadline agent) then
            Orchestrator.clear_automerge_deadline t patch_id
          else t
        in
        let t =
          Orchestrator.remove_github_effects_for_patch t patch_id
            ~f:(fun command ->
              is_automerge_github_action command.Github_effect.action
              &&
              match command.Github_effect.status with
              | Github_effect.Pending | Github_effect.Retry_at _ -> true
              | Github_effect.Running | Github_effect.Failed -> false)
        in
        (t, commands)
      else
        let t =
          Orchestrator.remove_github_effects_for_patch t patch_id
            ~f:(fun command ->
              is_automerge_github_action command.Github_effect.action
              &&
              match command.Github_effect.status with
              | Github_effect.Pending | Github_effect.Retry_at _ ->
                  not
                    (automerge_command_still_desired agent ~main_branch
                       command.action)
              | Github_effect.Running | Github_effect.Failed -> false)
        in
        if has_automerge_command t patch_id then
          (* The outbox command is the sole claim. Pending, running, retrying,
           and terminally failed commands all suppress duplicate mutations. *)
          (t, commands)
        else
          let dequeue_action =
            Option.bind agent.Patch_agent.merge_queue_entry ~f:(fun entry ->
                if
                  should_dequeue_merge_queue agent ~main_branch
                    ~entry_id:entry.Pr_state.id
                then Some (Dequeue entry.id)
                else None)
          in
          let emit_automerge_command t action =
            match Patch_agent.pr_number agent with
            | Some pr_number when Patch_agent.has_pr agent ->
                let github_action =
                  match action with
                  | Direct_merge -> Github_effect.Direct_merge { pr_number }
                  | Enqueue -> Github_effect.Enqueue { pr_number }
                  | Dequeue entry_id ->
                      Github_effect.Dequeue { pr_number; entry_id }
                in
                let command = Github_effect.create ~patch_id github_action in
                ( Orchestrator.enqueue_github_effect t command,
                  command :: commands )
            | Some _ | None ->
                (* Defensive clear so a future predicate change can't leave a
                 patch with a permanently-elapsed deadline that fires every
                 tick. *)
                (Orchestrator.clear_automerge_deadline t patch_id, commands)
          in
          match dequeue_action with
          | Some action
            when Patch_agent.automerge_enabled agent
                 && Patch_agent.automerge_failure_count agent
                    < automerge_max_failures -> (
              (* Dequeue queue-alarmed PRs immediately on first observation. If a
               previous dequeue call failed, [runner_fiber_impl] increments the
               automerge failure count; honor the same cap used for merge and
               enqueue failures. *)
              match Patch_agent.automerge_deadline agent with
              | Some deadline when Float.(now < deadline) -> (t, commands)
              | None | Some _ -> emit_automerge_command t action)
          | Some _ | None -> (
              let candidate = is_automerge_candidate agent ~main_branch in
              match (candidate, Patch_agent.automerge_deadline agent) with
              | false, Some _ when automerge_transient_hold agent ~main_branch
                ->
                  (* merge_ready dropped only because GitHub is recomputing
               mergeability (mergeStateStatus = UNKNOWN) after the base
               advanced — typically a sibling patch merging. Hold the existing
               deadline so the idle window keeps counting real elapsed time
               instead of restarting on every sibling merge. We do not fire
               here (firing requires merge_ready / CLEAN); the next CLEAN poll
               re-qualifies the candidate and the preserved deadline elapses. *)
                  (t, commands)
              | false, Some _ ->
                  (* Feedback arrived, lost approval, CI flipped, or failure cap
               hit: drop the deadline so the next reconcile re-arms a fresh
               idle window once the patch becomes a candidate again. *)
                  (Orchestrator.clear_automerge_deadline t patch_id, commands)
              | false, None -> (t, commands)
              | true, None ->
                  let deadline = now +. automerge_idle_timeout in
                  ( Orchestrator.set_automerge_deadline t patch_id deadline,
                    commands )
              | true, Some deadline ->
                  if Float.( >= ) now deadline then
                    match automerge_action agent ~main_branch with
                    | None ->
                        ( Orchestrator.clear_automerge_deadline t patch_id,
                          commands )
                    | Some action -> emit_automerge_command t action
                  else (t, commands)))
  |> fun (t, commands) -> (t, List.rev commands)

let reconcile_review_requests t ~team_slug =
  let agents = Orchestrator.all_agents t in
  let main_branch = Orchestrator.main_branch t in
  List.fold agents ~init:(t, []) ~f:(fun (t, commands) agent ->
      let patch_id = agent.Patch_agent.patch_id in
      let review_is_desired =
        Patch_agent.should_request_review agent ~main_branch
      in
      let t =
        Orchestrator.remove_github_effects_for_patch t patch_id
          ~f:(fun command ->
            match (command.Github_effect.action, command.status) with
            | ( Github_effect.Request_review
                  { team_slug = command_team; head_oid; _ },
                (Github_effect.Pending | Github_effect.Retry_at _) ) ->
                (not review_is_desired)
                || (not (String.equal command_team team_slug))
                || not
                     (Option.equal String.equal agent.head_oid (Some head_oid))
            | ( Github_effect.Request_review _,
                (Github_effect.Running | Github_effect.Failed) )
            | ( ( Github_effect.Set_pr_draft _ | Github_effect.Set_pr_base _
                | Github_effect.Direct_merge _ | Github_effect.Enqueue _
                | Github_effect.Dequeue _ ),
                _ ) ->
                false)
      in
      if review_is_desired then
        match (Patch_agent.pr_number agent, agent.Patch_agent.head_oid) with
        | Some pr_number, Some head_oid when Patch_agent.has_pr agent ->
            let command =
              Github_effect.create ~patch_id
                (Github_effect.Request_review { pr_number; team_slug; head_oid })
            in
            (Orchestrator.enqueue_github_effect t command, command :: commands)
        | _ ->
            (* Defensive: [should_request_review] currently requires [has_pr],
               and [head_oid], but a Missing PR or absent head must not produce
               a GitHub mutation. *)
            (t, commands)
      else (t, commands))
  |> fun (t, commands) -> (t, List.rev commands)

let apply_automerge_success t patch_id =
  let t = Orchestrator.mark_merged t patch_id in
  let t = Orchestrator.clear_automerge_deadline t patch_id in
  Orchestrator.reset_automerge_failure_count t patch_id

let apply_merge_queue_entered t patch_id entry =
  Orchestrator.entered_merge_queue t patch_id entry

let apply_merge_queue_dequeued t ~now patch_id =
  let t = Orchestrator.set_merge_queue_entry t patch_id None in
  let t = Orchestrator.reset_automerge_failure_count t patch_id in
  Orchestrator.set_automerge_deadline t patch_id (now +. automerge_idle_timeout)

(** Apply the policy state that follows a failed automerge command. Increments
    the failure counter and pushes the deadline out by [automerge_idle_timeout].
    The durable outbox command remains the execution claim; this deadline is
    only the user-visible policy/backoff state.

    Skip the deadline re-arm in two terminal cases:
    - The failure cap has now been reached: reconciliation will no longer issue
      merge calls for this patch, so writing a deadline that the next reconcile
      will immediately clear is wasted work and confusing in traces.
    - Automerge has been disabled (the user toggled it off while the merge was
      in flight): writing a deadline would contradict the disabled state and
      stick around until the next tick cleared it. *)
let apply_automerge_failure t ~now patch_id =
  apply_automerge_failure_state t ~now patch_id

type github_success =
  | Mutation_applied
  | Merge_succeeded
  | Merge_pending
  | Queue_entered of Pr_state.merge_queue_entry
[@@deriving show, eq, sexp_of]

let github_retry_delay = 60.0
let github_max_attempts = 5

let finish_github_success t ~now (command : Github_effect.t) success =
  let patch_id = command.patch_id in
  let applied =
    match (command.action, success) with
    | Github_effect.Set_pr_draft { draft; _ }, Mutation_applied ->
        Some (Orchestrator.set_is_draft t patch_id draft)
    | Github_effect.Set_pr_base { base; _ }, Mutation_applied ->
        Some (Orchestrator.set_base_branch t patch_id base)
    | Github_effect.Request_review { head_oid; _ }, Mutation_applied ->
        Some (Orchestrator.mark_review_requested t patch_id head_oid)
    | Github_effect.Dequeue _, Mutation_applied ->
        Some (apply_merge_queue_dequeued t ~now patch_id)
    | Github_effect.Direct_merge _, Merge_succeeded ->
        Some (apply_automerge_success t patch_id)
    | Github_effect.Direct_merge _, Merge_pending ->
        Some
          (Orchestrator.set_automerge_deadline t patch_id
             (now +. automerge_idle_timeout))
    | Github_effect.Enqueue _, Queue_entered entry ->
        Some (apply_merge_queue_entered t patch_id entry)
    | ( Github_effect.Set_pr_draft _,
        (Merge_succeeded | Merge_pending | Queue_entered _) )
    | ( Github_effect.Set_pr_base _,
        (Merge_succeeded | Merge_pending | Queue_entered _) )
    | ( Github_effect.Request_review _,
        (Merge_succeeded | Merge_pending | Queue_entered _) )
    | Github_effect.Direct_merge _, (Mutation_applied | Queue_entered _)
    | ( Github_effect.Enqueue _,
        (Mutation_applied | Merge_succeeded | Merge_pending) )
    | ( Github_effect.Dequeue _,
        (Merge_succeeded | Merge_pending | Queue_entered _) ) ->
        None
  in
  match applied with
  | Some t -> Orchestrator.remove_github_effect t command.id
  | None ->
      let failed =
        Github_effect.fail ~error:"adapter returned an invalid result" command
      in
      Orchestrator.update_github_effect t failed

let finish_github_failure t ~now (command : Github_effect.t) ~permanent ~error =
  match command.action with
  | Github_effect.Direct_merge _ | Github_effect.Enqueue _
  | Github_effect.Dequeue _ ->
      let t = apply_automerge_failure t ~now command.patch_id in
      let agent = Orchestrator.agent t command.patch_id in
      let terminal =
        permanent
        || command.attempts + 1 >= github_max_attempts
        || Patch_agent.automerge_failure_count agent >= automerge_max_failures
      in
      if terminal then
        Orchestrator.update_github_effect t (Github_effect.fail ~error command)
      else
        Orchestrator.update_github_effect t
          (Github_effect.retry ~now ~delay:automerge_idle_timeout ~error command)
  | Github_effect.Request_review { head_oid; _ } ->
      let terminal = permanent || command.attempts + 1 >= github_max_attempts in
      if terminal then
        let t =
          Orchestrator.mark_review_failed t command.patch_id ~head_oid ~error
        in
        Orchestrator.update_github_effect t (Github_effect.fail ~error command)
      else
        Orchestrator.update_github_effect t
          (Github_effect.retry ~now ~delay:github_retry_delay ~error command)
  | Github_effect.Set_pr_draft _ | Github_effect.Set_pr_base _ ->
      let terminal = permanent || command.attempts + 1 >= github_max_attempts in
      let command =
        if terminal then Github_effect.fail ~error command
        else Github_effect.retry ~now ~delay:github_retry_delay ~error command
      in
      Orchestrator.update_github_effect t command

let make_orchestrator ~patch_id ~main_branch =
  let patch =
    {
      Patch.id = patch_id;
      goal = "test";
      branch = Branch.of_string "test-branch";
      dependencies = [];
      files = [];
      checks = [];
    }
  in
  (patch, Orchestrator.create ~patches:[ patch ] ~main_branch)

let pid = Patch_id.of_string "p1"
let main = Branch.of_string "main"

let test_gameplan patches =
  Gameplan.{ project_name = "proj"; repo_owner = ""; repo_name = ""; patches }

let%test "reconcile_patch escalates repeated start discovery failures" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t =
    let t = Orchestrator.increment_start_attempts_without_pr t pid in
    Orchestrator.increment_start_attempts_without_pr t pid
  in
  let t =
    reconcile_patch t ~project_name:"proj" ~gameplan:(test_gameplan [ patch ])
      ~patch
  in
  Patch_agent.needs_intervention (Orchestrator.agent t pid)

let%test "reconcile_patch enqueues pr_body after PR creation" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.complete t pid in
  let gp = test_gameplan [ patch ] in
  let t = reconcile_patch t ~project_name:"proj" ~gameplan:gp ~patch in
  let queue = (Orchestrator.agent t pid).Patch_agent.queue in
  List.mem queue Operation_kind.Pr_body ~equal:Operation_kind.equal

let%test "reconcile_patch requests ready-for-review after pr_body on main" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.set_pr_body_delivered t pid true in
  let t = Orchestrator.set_checks_passing t pid true in
  let t = Orchestrator.complete t pid in
  let t =
    reconcile_patch t ~project_name:"proj" ~gameplan:(test_gameplan [ patch ])
      ~patch
  in
  let effects = Orchestrator.all_github_effects t in
  List.exists effects ~f:(fun command ->
      Patch_id.equal command.Github_effect.patch_id pid
      &&
      match command.action with
      | Github_effect.Set_pr_draft { draft = false; _ } -> true
      | Github_effect.Set_pr_draft _ | Github_effect.Set_pr_base _
      | Github_effect.Request_review _ | Github_effect.Direct_merge _
      | Github_effect.Enqueue _ | Github_effect.Dequeue _ ->
          false)

let%test "reconcile_patch keeps PR draft while CI is not green" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.set_pr_body_delivered t pid true in
  let t = Orchestrator.complete t pid in
  (* checks_passing left at its default [false] — CI hasn't reported green. *)
  let t =
    reconcile_patch t ~project_name:"proj" ~gameplan:(test_gameplan [ patch ])
      ~patch
  in
  let effects = Orchestrator.all_github_effects t in
  List.is_empty effects

let%test "reconcile_patch never re-drafts a PR once it is ready for review" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.set_pr_body_delivered t pid true in
  let t = Orchestrator.complete t pid in
  (* Simulate GitHub having observed the PR as ready-for-review. *)
  let t = Orchestrator.set_is_draft t pid false in
  (* Now CI flaps red and a fresh conflict arrives. *)
  let t = Orchestrator.set_checks_passing t pid false in
  let t = Orchestrator.set_has_conflict t pid in
  let t =
    reconcile_patch t ~project_name:"proj" ~gameplan:(test_gameplan [ patch ])
      ~patch
  in
  let effects = Orchestrator.all_github_effects t in
  not
    (List.exists effects ~f:(fun command ->
         match command.Github_effect.action with
         | Github_effect.Set_pr_draft _ -> true
         | Github_effect.Set_pr_base _ | Github_effect.Request_review _
         | Github_effect.Direct_merge _ | Github_effect.Enqueue _
         | Github_effect.Dequeue _ ->
             false))

let%test "reconcile_patch keeps PR draft while merge conflict is active" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.set_pr_body_delivered t pid true in
  let t = Orchestrator.set_checks_passing t pid true in
  let t = Orchestrator.complete t pid in
  let t = Orchestrator.set_has_conflict t pid in
  let t =
    reconcile_patch t ~project_name:"proj" ~gameplan:(test_gameplan [ patch ])
      ~patch
  in
  let effects = Orchestrator.all_github_effects t in
  not
    (List.exists effects ~f:(fun command ->
         match command.Github_effect.action with
         | Github_effect.Set_pr_draft { draft = false; _ } -> true
         | Github_effect.Set_pr_draft _ | Github_effect.Set_pr_base _
         | Github_effect.Request_review _ | Github_effect.Direct_merge _
         | Github_effect.Enqueue _ | Github_effect.Dequeue _ ->
             false))

let%test
    "reconcile_patch keeps child PR draft until rebased onto main after parent \
     merges" =
  let parent_id = Patch_id.of_string "parent" in
  let child_id = Patch_id.of_string "child" in
  let parent_branch = Branch.of_string "parent-branch" in
  let child_branch = Branch.of_string "child-branch" in
  let mk_patch id branch deps =
    {
      Patch.id;
      goal = "test";
      branch;
      dependencies = deps;
      files = [];
      checks = [];
    }
  in
  let parent_patch = mk_patch parent_id parent_branch [] in
  let child_patch = mk_patch child_id child_branch [ parent_id ] in
  let t =
    Orchestrator.create ~patches:[ parent_patch; child_patch ] ~main_branch:main
  in
  let t = Orchestrator.fire t (Orchestrator.Start (parent_id, main)) in
  let t = Orchestrator.set_pr_number t parent_id (Pr_number.of_int 41) in
  let t = Orchestrator.complete t parent_id in
  let t = Orchestrator.mark_merged t parent_id in
  (* Child started on the parent's branch — branch_rebased_onto records
     [parent_branch], NOT main. Parent has since merged, so expected_base
     flips to main, but the local branch has not yet been rebased. *)
  let t = Orchestrator.fire t (Orchestrator.Start (child_id, parent_branch)) in
  let t = Orchestrator.set_pr_number t child_id (Pr_number.of_int 42) in
  let t = Orchestrator.set_pr_body_delivered t child_id true in
  let t = Orchestrator.set_checks_passing t child_id true in
  let t = Orchestrator.complete t child_id in
  let gp = test_gameplan [ parent_patch; child_patch ] in
  let t =
    reconcile_patch t ~project_name:"proj" ~gameplan:gp ~patch:child_patch
  in
  let effects = Orchestrator.all_github_effects t in
  not
    (List.exists effects ~f:(fun command ->
         Patch_id.equal command.Github_effect.patch_id child_id
         &&
         match command.action with
         | Github_effect.Set_pr_draft { draft = false; _ } -> true
         | Github_effect.Set_pr_draft _ | Github_effect.Set_pr_base _
         | Github_effect.Request_review _ | Github_effect.Direct_merge _
         | Github_effect.Enqueue _ | Github_effect.Dequeue _ ->
             false))

(* Shared fixture for the deps-notes-ready Start gate tests: a parent with an
   open PR and a child depending on it. *)
let make_notes_gate_fixture () =
  let parent_id = Patch_id.of_string "parent" in
  let child_id = Patch_id.of_string "child" in
  let mk_patch id branch deps =
    {
      Patch.id;
      goal = "test";
      branch;
      dependencies = deps;
      files = [];
      checks = [];
    }
  in
  let parent_patch = mk_patch parent_id (Branch.of_string "parent-branch") [] in
  let child_patch =
    mk_patch child_id (Branch.of_string "child-branch") [ parent_id ]
  in
  let patches = [ parent_patch; child_patch ] in
  let t = Orchestrator.create ~patches ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (parent_id, main)) in
  let t = Orchestrator.set_pr_number t parent_id (Pr_number.of_int 41) in
  let t = Orchestrator.complete t parent_id in
  (parent_id, child_id, patches, t)

let plans_child_start ~child_id ~patches t =
  List.exists (plan_actions t ~patches) ~f:(function
    | Orchestrator.Start (p, _) -> Patch_id.equal p child_id
    | Orchestrator.Respond _ | Orchestrator.Rebase _ -> false)

let%test
    "plan_actions defers child Start until open dep reaches ready-for-review" =
  let parent_id, child_id, patches, t = make_notes_gate_fixture () in
  (* The parent (started on main, rebased, no conflict) has neither delivered
     its notes nor gone CI-green, so it has not reached the ready-for-review
     fixpoint: the child must not start yet. *)
  (not (plans_child_start ~child_id ~patches t))
  (* Notes alone are no longer sufficient — the shared fixpoint also requires
     the dep's CI to be green. *)
  && (not
        (plans_child_start ~child_id ~patches
           (Orchestrator.set_pr_body_delivered t parent_id true)))
  (* Once the dep is both notes-delivered and CI-green it is ready for review,
     and the child becomes startable. *)
  &&
  let t = Orchestrator.set_pr_body_delivered t parent_id true in
  let t = Orchestrator.set_checks_passing t parent_id true in
  plans_child_start ~child_id ~patches t

let%test "plan_actions does not gate child Start on a merged dep" =
  let parent_id, child_id, patches, t = make_notes_gate_fixture () in
  (* Parent merges without ever delivering notes (e.g. merged by a human
     before the Pr_body step ran). A merged dep can never deliver, so the
     gate must exempt it rather than deadlock the child. *)
  let t = Orchestrator.mark_merged t parent_id in
  plans_child_start ~child_id ~patches t

let%test "reconcile_patch enqueues no commands for merged agent" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.complete t pid in
  let t = Orchestrator.mark_merged t pid in
  let t =
    reconcile_patch t ~project_name:"proj" ~gameplan:(test_gameplan [ patch ])
      ~patch
  in
  List.is_empty (Orchestrator.all_github_effects t)

let%test "no merge-conflict re-enqueue after noop" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.complete t pid in
  (* First poll: conflict detected, enqueues Merge_conflict *)
  let poll_conflict =
    Poller.
      {
        queue = [ Operation_kind.Merge_conflict ];
        merged = false;
        closed = false;
        is_draft = false;
        merge_state = Pr_state.Conflicting;
        merge_ready = false;
        head_oid = None;
        review_decision = None;
        unresolved_comment_count = 0;
        merge_queue_required = false;
        merge_queue_entry = None;
        checks_passing = false;
        ci_checks = [];
        merge_commit_sha = None;
      }
  in
  let obs =
    {
      poll_result = poll_conflict;
      base_branch = Some main;
      branch_in_root = false;
      worktree_path = None;
    }
  in
  let t, _, _ = apply_poll_result t pid obs in
  let agent = Orchestrator.agent t pid in
  assert (
    List.mem agent.Patch_agent.queue Operation_kind.Merge_conflict
      ~equal:Operation_kind.equal);
  (* Fire the Merge_conflict, then simulate Noop via apply_conflict_rebase_result *)
  let t =
    Orchestrator.fire t
      (Orchestrator.Respond (pid, Operation_kind.Merge_conflict))
  in
  let t, decision, _effects =
    Orchestrator.apply_conflict_rebase_result t pid Worktree.Noop main
  in
  (* Noop -> Conflict_resolved, agent completed (not busy) *)
  Orchestrator.equal_conflict_rebase_decision decision
    Orchestrator.Conflict_resolved
  && not (Patch_agent.is_busy (Orchestrator.agent t pid))

(* -- Automerge reconciliation tests -- *)

let make_approved_agent t =
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.complete t pid in
  let t = Orchestrator.set_is_draft t pid false in
  let t = Orchestrator.set_merge_ready t pid true in
  let t = Orchestrator.set_checks_passing t pid true in
  let t = Orchestrator.set_pr_body_delivered t pid true in
  Orchestrator.set_automerge_enabled t pid true

let%test "apply_poll_result turns merge queue ejection into CI feedback" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let t = Orchestrator.set_merge_queue_required t pid true in
  let t =
    Orchestrator.set_merge_queue_entry t pid
      (Some Pr_state.{ id = "MQE_1"; state = Mq_awaiting_checks; position = 1 })
  in
  let poll_result =
    Poller.
      {
        queue = [];
        merged = false;
        closed = false;
        is_draft = false;
        merge_state = Pr_state.Mergeable;
        merge_ready = true;
        head_oid = None;
        review_decision = Some "APPROVED";
        unresolved_comment_count = 0;
        merge_queue_required = true;
        merge_queue_entry = None;
        checks_passing = true;
        ci_checks = [];
        merge_commit_sha = None;
      }
  in
  let t, logs, _ =
    apply_poll_result ~merge_queue_ejection_confirmed:true t pid
      {
        poll_result;
        base_branch = Some main;
        branch_in_root = false;
        worktree_path = None;
      }
  in
  let agent = Orchestrator.agent t pid in
  List.mem agent.queue Operation_kind.Ci ~equal:Operation_kind.equal
  && (not agent.checks_passing) && (not agent.merge_ready)
  && Patch_agent.automerge_failure_count agent = 0
  && Option.is_none (Patch_agent.automerge_deadline agent)
  && List.exists agent.ci_checks ~f:Ci_check.is_merge_queue_failure
  && List.exists logs ~f:(fun { message; _ } ->
      String.is_substring message ~substring:"Merge queue removed PR")

(* An [Mq_unmergeable] entry that is still in the queue is, in a stacked merge
   queue, almost always a conflict with the sibling ahead of it — not a check
   failure. It must not synthesize a CI failure; the PR stays untouched and the
   conflict surfaces later through [merge_state = Conflicting]. *)
let%test "apply_poll_result leaves an unmergeable in-queue entry untouched" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let poll_result =
    Poller.
      {
        queue = [];
        merged = false;
        closed = false;
        is_draft = false;
        merge_state = Pr_state.Mergeable;
        merge_ready = true;
        head_oid = None;
        review_decision = Some "APPROVED";
        unresolved_comment_count = 0;
        merge_queue_required = true;
        merge_queue_entry =
          Some Pr_state.{ id = "MQE_2"; state = Mq_unmergeable; position = 4 };
        checks_passing = true;
        ci_checks = [];
        merge_commit_sha = None;
      }
  in
  (* Irrelevant verdict on purpose: an in-queue entry is not an ejection, so
     even a [true] confirmation must not produce synthesis. *)
  let t, _, _ =
    apply_poll_result ~merge_queue_ejection_confirmed:true t pid
      {
        poll_result;
        base_branch = Some main;
        branch_in_root = false;
        worktree_path = None;
      }
  in
  let agent = Orchestrator.agent t pid in
  (not (List.mem agent.queue Operation_kind.Ci ~equal:Operation_kind.equal))
  && agent.checks_passing && agent.merge_ready
  && Patch_agent.automerge_failure_count agent = 0
  && not (List.exists agent.ci_checks ~f:Ci_check.is_merge_queue_failure)

(* The user-facing regression: a PR ejected from the queue while still green and
   mergeable, but ejected because of a conflict with the sibling ahead of it.
   The poller's removal-event fetch found no failing merge-group checks, so
   [merge_queue_ejection_confirmed = false]. We must NOT report a CI failure. *)
let%test "apply_poll_result leaves an unconfirmed (conflict) ejection alone" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_merge_queue_required t pid true in
  let t =
    Orchestrator.set_merge_queue_entry t pid
      (Some Pr_state.{ id = "MQE_1"; state = Mq_awaiting_checks; position = 1 })
  in
  let poll_result =
    Poller.
      {
        queue = [];
        merged = false;
        closed = false;
        is_draft = false;
        merge_state = Pr_state.Mergeable;
        merge_ready = true;
        head_oid = None;
        review_decision = Some "APPROVED";
        unresolved_comment_count = 0;
        merge_queue_required = true;
        merge_queue_entry = None;
        checks_passing = true;
        ci_checks = [];
        merge_commit_sha = None;
      }
  in
  let t, _, _ =
    apply_poll_result ~merge_queue_ejection_confirmed:false t pid
      {
        poll_result;
        base_branch = Some main;
        branch_in_root = false;
        worktree_path = None;
      }
  in
  let agent = Orchestrator.agent t pid in
  (not (List.mem agent.queue Operation_kind.Ci ~equal:Operation_kind.equal))
  && agent.checks_passing && agent.merge_ready
  && Patch_agent.automerge_failure_count agent = 0
  && not (List.exists agent.ci_checks ~f:Ci_check.is_merge_queue_failure)

let%test "reconcile_automerge clears deadline on merged agent" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let t = Orchestrator.mark_merged t pid in
  let t, decisions = reconcile_automerge t ~now:100.0 in
  List.is_empty decisions
  && Option.is_none (Patch_agent.automerge_deadline (Orchestrator.agent t pid))

let%test "reconcile_automerge removes stale command on merged agent" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let command =
    Github_effect.create ~patch_id:pid
      (Github_effect.Direct_merge { pr_number = Pr_number.of_int 17 })
  in
  let t = Orchestrator.enqueue_github_effect t command in
  let t = Orchestrator.mark_merged t pid in
  let t, decisions = reconcile_automerge t ~now:100.0 in
  List.is_empty decisions
  && Option.is_none (Orchestrator.find_github_effect t command.id)

let%test "reconcile_automerge (false, None) is a stable no-op" =
  (* Complement to the (false, Some _) clearing test below: when the patch is
     not a candidate AND has no deadline, reconcile must not arm one or emit a
     decision. [make_approved_agent] enables automerge but does not set a
     deadline, so disabling checks_passing lands us on the no-op branch. *)
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_checks_passing t pid false in
  let t_before = t in
  let t, decisions = reconcile_automerge t ~now:1000.0 in
  List.is_empty decisions
  && Patch_agent.equal
       (Orchestrator.agent t_before pid)
       (Orchestrator.agent t pid)

let%test "reconcile_automerge clears deadline when checks_passing flips false" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  (* Pre-arm a deadline so the (false, Some _) clearing branch actually
     fires. Without this the test passes trivially via (false, None). *)
  let t = Orchestrator.set_automerge_deadline t pid 1300.0 in
  let t = Orchestrator.set_checks_passing t pid false in
  let t, decisions = reconcile_automerge t ~now:1000.0 in
  List.is_empty decisions
  && Option.is_none (Patch_agent.automerge_deadline (Orchestrator.agent t pid))

let%test "reconcile_automerge persists the command it emits" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let t, decisions = reconcile_automerge t ~now:100.0 in
  match decisions with
  | [ command ] -> Option.is_some (Orchestrator.find_github_effect t command.id)
  | _ -> false

let%test "reconcile_automerge skips when an outbox command exists" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let t, first = reconcile_automerge t ~now:100.0 in
  let t, decisions = reconcile_automerge t ~now:100.0 in
  let a = Orchestrator.agent t pid in
  List.length first = 1
  && List.is_empty decisions
  && List.length (Orchestrator.github_effects_for_patch t pid) = 1
  && Option.is_some (Patch_agent.automerge_deadline a)

let%test "reconcile_automerge skips when failure cap is hit" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  (* Arm a deadline before hitting the cap so the (false, Some _) branch
     actually fires and we verify the deadline gets cleared — otherwise the
     test passes trivially via (false, None). *)
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let t =
    Fn.apply_n_times ~n:automerge_max_failures
      (fun t -> Orchestrator.increment_automerge_failure_count t pid)
      t
  in
  let t, decisions = reconcile_automerge t ~now:1000.0 in
  let a = Orchestrator.agent t pid in
  List.is_empty decisions && Option.is_none (Patch_agent.automerge_deadline a)

let%test "apply_automerge_success resets failures" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.increment_automerge_failure_count t pid in
  let t = apply_automerge_success t pid in
  let a = Orchestrator.agent t pid in
  a.Patch_agent.merged
  && Patch_agent.automerge_failure_count a = 0
  && Option.is_none (Patch_agent.automerge_deadline a)

let%test "apply_automerge_failure bumps deadline one idle window out" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let now = 1000.0 in
  let t = apply_automerge_failure t ~now pid in
  let a = Orchestrator.agent t pid in
  Patch_agent.automerge_failure_count a = 1
  &&
  match Patch_agent.automerge_deadline a with
  | Some d -> Float.( = ) d (now +. automerge_idle_timeout)
  | None -> false

let%test "apply_automerge_failure clears deadline once cap is reached" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  (* Prime the counter so this failure hits the cap. *)
  let t =
    Fn.apply_n_times
      ~n:(automerge_max_failures - 1)
      (fun t -> Orchestrator.increment_automerge_failure_count t pid)
      t
  in
  (* Pre-arm a deadline so we can verify it's actively cleared, not just
     skipped. *)
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let t = apply_automerge_failure t ~now:1000.0 pid in
  let a = Orchestrator.agent t pid in
  Patch_agent.automerge_failure_count a = automerge_max_failures
  && Option.is_none (Patch_agent.automerge_deadline a)

let%test "apply_automerge_failure does not re-arm when automerge is disabled" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  (* User disables automerge while the command is outstanding. *)
  let t = Orchestrator.set_automerge_enabled t pid false in
  let t = apply_automerge_failure t ~now:1000.0 pid in
  let a = Orchestrator.agent t pid in
  (* The counter IS incremented (0 → 1) by [apply_automerge_failure] even
     though automerge is now disabled — the implementation bumps the counter
     unconditionally and only skips the deadline re-arm when disabled.
     [set_automerge_enabled true] on a later re-enable resets the counter,
     so this increment is invisible across a disable/enable cycle. The key
     invariant the test guards is that no deadline is re-armed. *)
  (not (Patch_agent.automerge_enabled a))
  && Option.is_none (Patch_agent.automerge_deadline a)
  && Patch_agent.automerge_failure_count a = 0

let%test "toggling automerge resets failure count" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.increment_automerge_failure_count t pid in
  let t = Orchestrator.increment_automerge_failure_count t pid in
  (* Disable → enable *)
  let t = Orchestrator.set_automerge_enabled t pid false in
  let t = Orchestrator.set_automerge_enabled t pid true in
  let a = Orchestrator.agent t pid in
  Patch_agent.automerge_enabled a && Patch_agent.automerge_failure_count a = 0
