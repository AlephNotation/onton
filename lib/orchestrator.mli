open Base
open Types

(** Top-level orchestrator wiring.

    Manages the dependency graph and per-patch agent state. Scheduling and
    reconciliation are owned by [Patch_controller]; this module provides the
    durable state and primitive state transitions. *)

type t

(** {2 Construction} *)

val create : patches:Patch.t list -> main_branch:Branch.t -> t
(** Build orchestrator state from a list of patches and a main branch. *)

type action =
  | Start of Patch_id.t * Branch.t
  | Respond of Patch_id.t * Operation_kind.t
  | Rebase of Patch_id.t * Branch.t
[@@deriving sexp_of, show, eq]

type message_status = Pending | Acked | Completed | Obsolete
[@@deriving sexp_of, show, eq]

type patch_agent_message = {
  message_id : Message_id.t;
  patch_id : Patch_id.t;
  generation : int;
  action : action;
  payload : string list;
  payload_hash : string;
  status : message_status;
}
[@@deriving sexp_of, show, eq]

val fire : t -> action -> t
(** Apply a single action to the orchestrator state. *)

val accept_message : t -> Message_id.t -> t * action option
(** Durably accept a pending message and fire its action exactly once. Returns
    [Some action] only on first acceptance. Duplicate acceptance is a no-op. *)

val resume_message : t -> Message_id.t -> t * action option
(** Resume execution of an already accepted but incomplete message. Returns
    [Some action] only when the message is still the patch's current message. *)

val reconcile_message : t -> patch_agent_message -> t
(** Insert or refresh a desired pending message. Existing equivalent messages
    are preserved; other pending messages for the same patch are marked
    obsolete. *)

val mark_message_obsolete : t -> Message_id.t -> t

val mark_patch_pending_messages_obsolete_except :
  t -> Patch_id.t -> keep:Message_id.t list -> t

val find_message : t -> Message_id.t -> patch_agent_message option
val all_messages : t -> patch_agent_message list
val current_message : t -> Patch_id.t -> patch_agent_message option
val runnable_messages : t -> patch_agent_message list
val message_id : patch_agent_message -> Message_id.t
val message_patch_id : patch_agent_message -> Patch_id.t
val message_action : patch_agent_message -> action
val message_payload : patch_agent_message -> string list
val message_status : patch_agent_message -> message_status

(** {2 Durable GitHub command outbox} *)

val enqueue_github_effect : t -> Github_effect.t -> t
val find_github_effect : t -> Effect_id.t -> Github_effect.t option
val all_github_effects : t -> Github_effect.t list
val runnable_github_effects : t -> now:float -> Github_effect.t list

val claim_github_effect :
  t -> now:float -> Effect_id.t -> t * Github_effect.t option
(** Mark a runnable command as [Running]. The returned command may be passed to
    the GitHub adapter only after the enclosing runtime transition has been
    durably committed. *)

val update_github_effect : t -> Github_effect.t -> t
val remove_github_effect : t -> Effect_id.t -> t
val github_effects_for_patch : t -> Patch_id.t -> Github_effect.t list

val remove_github_effects_for_patch :
  t -> Patch_id.t -> f:(Github_effect.t -> bool) -> t

(** {2 External event application} *)

val complete : t -> Patch_id.t -> t
val enqueue : t -> Patch_id.t -> Operation_kind.t -> t
val mark_merged : t -> Patch_id.t -> t

val send_human_message : t -> Patch_id.t -> string -> t
(** Append a human message to the agent's [human_messages] queue, reset
    intervention state, and enqueue [Operation_kind.Human]. *)

val set_pr_number : t -> Patch_id.t -> Pr_number.t -> t
val clear_pr : t -> Patch_id.t -> t
val set_session_failed : t -> Patch_id.t -> t

val set_branch_rebased_onto_sha : t -> Patch_id.t -> string option -> t
(** Record the SHA the base ref resolved to at the moment of a successful rebase
    / start. Called by the runner fiber after [W.read_branch_sha] captures the
    new value. [None] clears the field. *)

val set_tried_fresh : t -> Patch_id.t -> t
val clear_session_fallback : t -> Patch_id.t -> t
val on_session_failure : t -> Patch_id.t -> is_fresh:bool -> t
val on_pr_discovery_failure : t -> Patch_id.t -> t
val set_has_conflict : t -> Patch_id.t -> t
val clear_has_conflict : t -> Patch_id.t -> t
val reset_conflict_noop_count : t -> Patch_id.t -> t
val set_base_branch : t -> Patch_id.t -> Branch.t -> t
val set_notified_base_branch : t -> Patch_id.t -> Branch.t -> t
val increment_ci_failure_count : t -> Patch_id.t -> t
val reset_ci_failure_count : t -> Patch_id.t -> t
val set_ci_checks : t -> Patch_id.t -> Ci_check.t list -> t

val record_delivered_ci_run_ids : t -> Patch_id.t -> int list -> t
(** Record CheckRun [databaseId]s as delivered to the agent for this patch so
    subsequent CI deliveries do not re-deliver the same runs. See
    {!Patch_agent.record_delivered_ci_run_ids}. *)

val set_checks_passing : t -> Patch_id.t -> bool -> t
val set_merge_ready : t -> Patch_id.t -> bool -> t
val set_head_oid : t -> Patch_id.t -> string option -> t
val set_review_decision : t -> Patch_id.t -> string option -> t
val set_unresolved_comment_count : t -> Patch_id.t -> int -> t
val set_mergeability_unknown : t -> Patch_id.t -> bool -> t
val set_merge_queue_required : t -> Patch_id.t -> bool -> t

val set_merge_queue_entry :
  t -> Patch_id.t -> Pr_state.merge_queue_entry option -> t

val observe_merge_queue :
  t ->
  Patch_id.t ->
  required:bool ->
  entry:Pr_state.merge_queue_entry option ->
  t
(** Apply a poll observation of merge-queue state. Delegates the agent-level
    state rule to {!Automerge_state.observe_merge_queue}, including clearing any
    stale automerge deadline once a queue entry is present. *)

val set_merge_commit_sha : t -> Patch_id.t -> string option -> t
val set_base_contains_merged_siblings : t -> Patch_id.t -> bool -> t
val set_is_draft : t -> Patch_id.t -> bool -> t
val set_pr_body_delivered : t -> Patch_id.t -> bool -> t
val reset_pr_body_artifact_miss_count : t -> Patch_id.t -> t
val increment_start_attempts_without_pr : t -> Patch_id.t -> t
val reset_intervention_state : t -> Patch_id.t -> t
val set_branch_blocked : t -> Patch_id.t -> t
val clear_branch_blocked : t -> Patch_id.t -> t
val reset_busy : t -> Patch_id.t -> t

val mark_running : t -> Patch_id.t -> t
(** Transition the patch's [current_op_state] from [Queued] to [Running]. Called
    from action fibers right after the Claude semaphore has been acquired so the
    TUI can distinguish queued (waiting on slot) from running. No-op if the
    agent is no longer busy. *)

val set_worktree_path : t -> Patch_id.t -> string -> t
val set_llm_session_id : t -> Patch_id.t -> string option -> t

val mark_inflight_human_messages_delivered : t -> Patch_id.t -> t
(** Clear inflight Human messages after backend turn acceptance. See
    {!Patch_agent.mark_inflight_human_messages_delivered}. *)

val set_automerge_enabled : t -> Patch_id.t -> bool -> t
(** Primitive agent-state update. Interactive policy changes must use
    {!Patch_controller.set_automerge_enabled} so obsolete durable commands are
    reset in the same transition. *)

val set_automerge_deadline : t -> Patch_id.t -> float -> t
(** Arm an automerge deadline via {!Automerge_state.arm_deadline}. If the patch
    is already known to be in the merge queue, this clears the deadline instead
    of recording an incoherent timer. *)

val clear_automerge_deadline : t -> Patch_id.t -> t
val mark_review_requested : t -> Patch_id.t -> string -> t
val mark_review_failed : t -> Patch_id.t -> head_oid:string -> error:string -> t
val increment_automerge_failure_count : t -> Patch_id.t -> t
val reset_automerge_failure_count : t -> Patch_id.t -> t

val entered_merge_queue : t -> Patch_id.t -> Pr_state.merge_queue_entry -> t
(** Record that the PR is now in GitHub's merge queue, regardless of whether
    that was learned from an automerge enqueue response, manual enqueue, or
    polling. Delegates to {!Automerge_state.entered_merge_queue}. *)

val apply_automerge_failure_state :
  t -> Patch_id.t -> retry_deadline:float -> max_failures:int -> t
(** Apply the pure automerge failure/backoff transition from
    {!Automerge_state.merge_call_failed}. *)

(** {2 Queries} *)

val agent : t -> Patch_id.t -> Patch_agent.t
val find_agent : t -> Patch_id.t -> Patch_agent.t option
val all_agents : t -> Patch_agent.t list
val graph : t -> Graph.t
val main_branch : t -> Branch.t
val set_main_branch : t -> Branch.t -> t

val set_max_ci_failures : t -> max_ci_failures:int -> t
(** Stamp the per-project CI-failure cap onto every current agent. Called once
    at startup by [Runtime.create] with the resolved config value — both for
    fresh orchestrators and for snapshot restores, so a changed
    [--max-ci-failures] takes effect on resume. Does not bump agent
    [generation]s (config stamp, not a state transition). *)

val agents_map : t -> Patch_agent.t Map.M(Patch_id).t

(** {2 Persistence support} *)

(** [detail] carries the same formatted reason that [session_driver] writes to
    the activity log (truncated to ~500 chars) — backend, exit code, stderr
    excerpt. It surfaces in [events.jsonl] via [show_session_result] for
    postmortem diagnosis. *)
type session_result =
  | Session_ok
  | Session_process_error of { is_fresh : bool; detail : string option }
  | Session_no_resume
  | Session_failed of { is_fresh : bool; detail : string option }
  | Session_validation_failed of { detail : string }
  | Session_give_up
  | Session_worktree_missing
  | Session_push_failed of Push_reject_classify.rejection option
      (** [Some r] carries a classified server-side rejection; [None] reflects a
          transport / local [git push] failure ([Push_error]). *)
  | Session_no_commits
  | Session_context_exhausted
      (** The session exhausted the model's context window. Clears
          [llm_session_id] so the next session starts fresh and bumps
          [context_exhaustion_count]; at [>= 2] the agent surfaces for
          intervention. *)
[@@deriving show, eq, sexp_of]

val apply_session_result : t -> Patch_id.t -> session_result -> t
(** Apply a Claude session outcome to the orchestrator. Pure function.
    [Session_ok] -> clear_session_fallback. [Session_process_error] ->
    on_session_failure + on_pre_session_failure + complete_failed.
    [Session_failed] -> on_session_failure + complete_failed.
    [Session_validation_failed] -> preserve the healthy backend session, queue
    its actionable detail as a direct message, increment the bounded validation
    counter, and complete the current worker operation for ordinary replay.
    [Session_no_resume] -> on_session_failure (not fresh) + clear llm_session_id
    \+ complete_failed. [Session_give_up] -> set_session_failed +
    set_tried_fresh + clear llm_session_id + complete_failed.
    [Session_worktree_missing] -> on_pre_session_failure + complete_failed.

    {b Deferred completion}: [Session_push_failed] and [Session_no_commits] do
    NOT complete the agent — they only adjust state ([clear_session_fallback] in
    both cases; [increment_no_commits_push_count] for [Session_no_commits];
    [increment_push_failure_count] and possible escalation to [Given_up] for
    [Session_push_failed]). [busy] remains [true]. The caller MUST follow up in
    the same atomic snapshot write with either
    [apply_start_outcome _ Start_failed] (Start path) or
    [apply_respond_outcome _ _ Respond_no_commits] /
    [apply_respond_outcome _ _ Respond_retry_push] (Respond path) to clear
    [busy]. This two-phase design is deliberate: the LLM session itself
    succeeded (messages were delivered; commits were made locally), so any
    inflight human payload must be consumed by plain [complete] rather than
    restored by [complete_failed] — the latter would re-enqueue Human and cause
    an infinite re-delivery loop. After 2 consecutive [Session_no_commits]
    outcomes, [needs_intervention] fires via [no_commits_push_count >= 2]. After
    3 consecutive [Session_push_failed] outcomes (or a single one carrying a
    permanent rejection), [needs_intervention] fires via
    [push_failure_count >= 3] or [Given_up]. *)

val combine_session_and_push :
  branch_changed:bool ->
  session:session_result ->
  push:Worktree.push_result ->
  session_result
(** Pure: fold the LLM session outcome and the supervisor's post-session push
    outcome into a single [session_result] for [apply_session_result].
    - A pre-existing LLM failure ([Session_process_error], [Session_failed],
      [Session_no_resume], [Session_give_up], [Session_worktree_missing],
      [Session_push_failed _]) is preserved unchanged — the push outcome doesn't
      change anything.
    - [Session_ok] with [Push_ok] or [Push_up_to_date] stays [Session_ok] when
      [branch_changed] is true. When [branch_changed] is false, it becomes
      [Session_no_commits]: the agent turn finished but did not create a new
      commit, even if the branch already had older commits ahead of base.
    - [Session_ok] with [Push_rejected reason] becomes
      [Session_push_failed (Some reason)] — the LLM ran fine but the remote
      refused commits; the classified reason rides along so the orchestrator can
      escalate immediately on permanent rejections (workflow-scope,
      branch-protection, push-pattern, hook).
    - [Session_ok] with [Push_error _] becomes [Session_push_failed None] — a
      transport/local git error, always treated as transient.
    - [Session_ok] with [Push_no_commits] becomes [Session_no_commits] — the
      branch has no commits ahead of base, so the push was skipped (a base-equal
      branch can't become a PR). *)

type start_outcome = Start_ok | Start_failed | Start_stale
[@@deriving show, eq, sexp_of]

val apply_start_outcome : t -> Patch_id.t -> start_outcome -> t
(** Apply the outcome of a Start action fiber. [Start_failed] -> complete.
    [Start_ok] -> identity (caller must [complete] after PR discovery).
    [Start_stale] -> identity. *)

type respond_outcome =
  | Respond_ok
  | Respond_failed
  | Respond_retry_push
  | Respond_no_commits
  | Respond_stale
  | Respond_skip_empty
  | Respond_pr_body_miss
  | Respond_review_unresolved
[@@deriving show, eq, sexp_of]

val apply_respond_outcome :
  t -> Patch_id.t -> Operation_kind.t -> respond_outcome -> t
(** Apply the outcome of a Respond action fiber. [Respond_ok] -> complete +
    reset_no_commits_push_count + kind-specific transitions (Merge_conflict ->
    clear_has_conflict + reset_conflict_noop_count; Review_comments ->
    reset_review_unresolved_cycle_count so the cap counts only consecutive
    non-converged cycles; Pr_body -> set_pr_body_delivered +
    reset_pr_body_artifact_miss_count so the cap counts only consecutive
    misses). [Respond_failed] -> complete_failed (restores inflight human
    messages). [Respond_skip_empty], [Respond_retry_push], and
    [Respond_no_commits] -> complete. [Respond_stale] -> identity.
    [Respond_pr_body_miss] -> complete + increment_pr_body_artifact_miss_count
    (does NOT set_pr_body_delivered — the reconciler re-enqueues Pr_body
    naturally). [Respond_review_unresolved] -> complete +
    increment_review_unresolved_cycle_count — the still-unresolved threads
    re-deliver via the next poll until the cap (>=2) surfaces the agent through
    [needs_intervention]. *)

type force_complete_reason = Cancelled | Unexpected_exception
[@@deriving show, eq, sexp_of]

val apply_force_complete : t -> Patch_id.t -> force_complete_reason -> t
(** Pure applicator for runner fibers that exited abnormally while the agent was
    [busy]. The single source of truth for the [bin/main.ml] [with_busy_guard]
    finally and [mark_session_failed] sites that previously called [complete]
    directly — and so silently dropped [inflight_human_messages].

    Semantics:
    - Unknown patch: identity.
    - [Unexpected_exception]: always advances [session_fallback] via
      [set_session_failed] then [set_tried_fresh] (preserving the prior
      [mark_session_failed] semantics, which pushed [Fresh_available] all the
      way to [Given_up]). This still runs even when the agent is not busy.
    - [Cancelled]: leaves [session_fallback] alone — a clean cancel should not
      poison the fallback chain.
    - If [busy] AND [inflight_human_messages <> []]: routes through
      [complete_failed], which restores inflight back to [human_messages] and
      re-enqueues [Operation_kind.Human].
    - If [busy] AND inflight is empty: routes through plain [complete].
    - If not [busy]: skip the complete step. *)

(** Side effects emitted by rebase result application. The runner is responsible
    for executing these (e.g. force-pushing the branch to the remote). Modeled
    as data so property tests can assert on effect presence. *)
type rebase_effect = Push_branch [@@deriving show, eq, sexp_of]

val apply_rebase_result :
  t ->
  Patch_id.t ->
  Worktree.rebase_result ->
  Branch.t ->
  t * rebase_effect list
(** Apply a rebase outcome to the orchestrator state. Pure function. [Ok] ->
    set_base_branch + reset_rebase_failure_count + clear_has_conflict +
    reset_conflict_noop_count + rewrite cascade + complete + [[Push_branch]].
    [Noop] -> set_base_branch + reset_rebase_failure_count + complete +
    [[Push_branch]]. [Conflict] -> set_base_branch + reset_rebase_failure_count
    \+ set_has_conflict + enqueue Merge_conflict + complete. [Error _] ->
    increment_rebase_failure_count + complete.

    The {e rewrite cascade} on [Ok] is the dual of [mark_merged]'s eager
    enqueue: a completed rebase force-replaces the branch's commits, leaving
    stacked children (open dependents with PRs whose [branch_rebased_onto] names
    this branch) on dead history that every name-based detector reads as fresh.
    Each such child gets a [Rebase] enqueued; each child's own [Ok] re-fires the
    cascade, so the wave walks an open stack one layer per round, serialized
    bottom-up by the eligibility gate. [Noop] deliberately does not cascade —
    the branch was not rewritten. *)

val apply_rebase_with_anchor :
  t ->
  Patch_id.t ->
  Worktree.rebase_result ->
  Branch.t ->
  Worktree_plan.anchor_event list ->
  t * rebase_effect list
(** Atomic variant of {!apply_rebase_result} that also folds the executor's
    {!Worktree_plan.anchor_event} observations into the agent. The two
    transitions are combined so the orchestrator can never observe a half-
    applied state where (say) the rebase succeeded but the anchor was not
    recorded. Each [Anchor_recorded] event calls {!Patch_agent.record_anchor};
    [Anchor_capture_failed] events are ignored (the prior anchor is preserved).
*)

type rebase_push_resolution =
  | Rebase_push_ok
  | Rebase_push_failed
  | Rebase_push_queue_locked
  | Rebase_push_error
[@@deriving show, eq, sexp_of]

val apply_rebase_push_result :
  t -> Patch_id.t -> Worktree.push_result option -> t * rebase_push_resolution
(** Pure second stage of rebase handling. Takes the push outcome from executing
    the [Push_branch] effect ([None] when no push was requested). Returns the
    updated state and resolution.

    - [Rebase_push_ok]: push succeeded or was up-to-date; no further action.
    - [Rebase_push_failed]: push was rejected (lease failure); resets conflict
      state and enqueues [Merge_conflict] so the next poll cycle retries.
    - [Rebase_push_queue_locked]: push was rejected because the PR is queued in
      a merge queue ([Push_reject_classify.Merge_queue_locked]). Inert: no
      conflict state, no enqueue. The local rebase already landed so
      [branch_rebased_onto] is current; a merge from the queue needs nothing
      further, and an ejection-as-Conflicting surfaces through the poll path,
      whose conflict-resolution no-op + now-unlocked push syncs the remote.
    - [Rebase_push_error]: infrastructure error; enqueues [Rebase] to retry
      without entering the conflict path. *)

val reject_rebase_publication : t -> Patch_id.t -> t * rebase_push_resolution
(** Record that a rebased branch failed its declared contract before push. *)

type conflict_rebase_decision =
  | Conflict_resolved
  | Deliver_to_agent
  | Conflict_failed
[@@deriving show, eq, sexp_of]

val apply_conflict_rebase_result :
  t ->
  Patch_id.t ->
  Worktree.rebase_result ->
  Branch.t ->
  t * conflict_rebase_decision * rebase_effect list
(** Apply a rebase outcome during merge-conflict resolution. Pure function. [Ok]
    -> set_base_branch + reset_rebase_failure_count + clear_has_conflict +
    reset_conflict_noop_count + rewrite cascade (see {!apply_rebase_result} —
    conflict resolution rewrites the branch just like a clean rebase) + complete
    + [Conflict_resolved] + [[Push_branch]]. [Noop] -> set_base_branch +
      reset_rebase_failure_count + clear_has_conflict +
      increment_conflict_noop_count + complete + [Conflict_resolved] +
      [[Push_branch]] (local is correct; has_conflict cleared so it purely
      tracks GitHub state — the poller will re-set and re-enqueue if conflict
      persists; no cascade — the branch was not rewritten). [Conflict] ->
      set_base_branch + reset_rebase_failure_count + set_has_conflict +
      [Deliver_to_agent] + [[]]. [Error _] -> increment_rebase_failure_count +
      complete + [Conflict_failed]. *)

val apply_conflict_rebase_with_anchor :
  t ->
  Patch_id.t ->
  Worktree.rebase_result ->
  Branch.t ->
  Worktree_plan.anchor_event list ->
  t * conflict_rebase_decision * rebase_effect list
(** Atomic variant of {!apply_conflict_rebase_result} that also folds
    {!Worktree_plan.anchor_event} observations into the agent. Same policy as
    {!apply_rebase_with_anchor}: [Anchor_recorded] calls
    {!Patch_agent.record_anchor}; [Anchor_capture_failed] is ignored. *)

val apply_anchor_events :
  t -> Patch_id.t -> Worktree_plan.anchor_event list -> t
(** Fold {!Worktree_plan.anchor_event} observations into the agent without any
    other transition. Called by the runner Start path after executing
    {!Worktree_plan.for_start} to record the initial anchor for a
    freshly-branched-off-dep patch, before the LLM session begins. *)

type conflict_resolution =
  | Conflict_done
  | Conflict_retry_push
  | Conflict_needs_agent
  | Conflict_give_up
[@@deriving show, eq, sexp_of]

val apply_conflict_push_result :
  t ->
  Patch_id.t ->
  conflict_rebase_decision ->
  Worktree.push_result option ->
  t * conflict_resolution
(** Pure second stage of merge-conflict resolution. Takes the rebase decision
    from [apply_conflict_rebase_result] and the outcome of executing the
    [Push_branch] effect ([None] when no push was requested). Returns the final
    resolution and updated state.

    - [Conflict_done]: conflict fully resolved (rebase succeeded and push
      landed).
    - [Conflict_retry_push]: rebase succeeded locally but push failed;
      re-enqueues [Merge_conflict] so the next cycle retries.
    - [Conflict_needs_agent]: the coding agent must resolve the conflict
      manually (rebase was noop/conflict — push result is irrelevant because the
      rebase itself didn't resolve the conflict).
    - [Conflict_give_up]: unrecoverable rebase error. *)

val reject_conflict_publication : t -> Patch_id.t -> t * conflict_resolution
(** Record that a conflict-resolution branch failed its declared contract before
    push. *)

val restore :
  graph:Graph.t ->
  agents:Patch_agent.t Map.M(Patch_id).t ->
  outbox:patch_agent_message Map.M(Message_id).t ->
  ?github_outbox:Github_effect.t Map.M(Effect_id).t ->
  main_branch:Branch.t ->
  unit ->
  t
(** Reconstruct orchestrator from persisted components. *)

val start_eligibility :
  t ->
  base_contains_merged_siblings:bool ->
  Branch.t ->
  Start_eligibility.decision
(** [start_eligibility t ~base_contains_merged_siblings base] is the freshness
    verdict for a hypothetical [Start]/[Rebase] action whose base is [base],
    evaluated against [t]'s dependency graph and the base patch's recorded
    rebase anchor. [base_contains_merged_siblings] is the launching patch's
    poll-derived base-containment cache. Used to gate [Start] and [Rebase] in
    {!runnable_messages}; exposed for tests/TUI introspection.

    Returns [Allow] when the base is main, when the base patch is merged
    (effectively main), or when the base patch's local branch is rebased onto
    its structurally-correct base ([branch_rebased_onto = Graph.initial_base]),
    has no unresolved conflict ([has_conflict] — a conflicted rebase keeps the
    gate closed until the resolution force-pushes the rewritten tip), and
    contains the launching patch's merged siblings. Otherwise returns
    [Defer reason]. Freshness is dependency-scoped: an unrelated advance of
    [origin/main] never defers a [Start]. See {!Start_eligibility}. *)
