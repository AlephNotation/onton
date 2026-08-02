open Base

(** Per-patch agent state machine.

    Encodes the spec fragment for actions: Start, Respond, Complete. The type
    [t] is private — external code can inspect fields but must use smart
    constructors that enforce spec preconditions. *)

type session_fallback = Fresh_available | Tried_fresh | Given_up
[@@deriving show, eq, sexp_of, compare, yojson]

type session_state =
  | Not_started
  | Started of { resume_id : string option; fallback : session_fallback }
[@@deriving show, eq, sexp_of, compare]

type op_state = Queued | Running
[@@deriving show, eq, sexp_of, compare, yojson]

type activity =
  | Inactive
  | Interrupted of {
      operation : Types.Operation_kind.t option;
      message_id : Types.Message_id.t option;
    }
  | Active of {
      operation : Types.Operation_kind.t option;
      phase : op_state;
      message_id : Types.Message_id.t option;
    }
[@@deriving show, eq, sexp_of, compare]

type automerge_state =
  | Disabled
  | Enabled of { deadline : float option; failure_count : int }
[@@deriving show, eq, sexp_of, compare]

type review_state =
  | Review_not_requested
  | Review_requested of string
  | Review_failed of { head_oid : string; error : string }
[@@deriving show, eq, sexp_of, compare]

type t = private {
  patch_id : Types.Patch_id.t;
  branch : Types.Branch.t;
  pr_status : Patch_pr_status.t;  (** Lifecycle status of the patch's PR. *)
  session : session_state;
  activity : activity;
      (** Atomic execution lifecycle. [Inactive] carries no operation metadata;
          [Interrupted] retains an accepted delivery for crash replay, and
          [Active] carries a live fiber's queued/running phase. *)
  merged : bool;
  queue : Types.Operation_kind.t list;
  satisfies : bool;
  changed : bool;
  has_conflict : bool;
  base_branch : Types.Branch.t option;
  notified_base_branch : Types.Branch.t option;
  ci_failure_count : int;
  max_ci_failures : int;
      (** Per-project cap on consecutive CI-failure responses: at
          [ci_failure_count >= max_ci_failures] the agent stops enqueueing [Ci]
          feedback ({!Patch_decision.Cap_reached}) and contributes to
          [needs_intervention]. Configuration, not state — stamped at
          construction (and re-stamped on snapshot restore via
          {!Orchestrator.set_max_ci_failures}) from the [--max-ci-failures] flag
          / stored project config; defaults to {!default_max_ci_failures}. *)
  validation_failure_count : int;
      (** Consecutive controller validation failures before PR publication. At
          three failures the patch fails closed even while its last feedback is
          still queued. A human message resets the counter. *)
  human_messages : string list;
  inflight_human_messages : string list;
  ci_checks : Types.Ci_check.t list;
  merge_ready : bool;
      (** Component-derived merge readiness ([Pr_state.merge_ready_of]), not
          GitHub's [mergeStateStatus]. *)
  head_oid : string option;
  review_decision : string option;
  unresolved_comment_count : int;
  mergeability_unknown : bool;
      (** Poll-mirror of [Pr_state.merge_state = Unknown] (GitHub recomputing
          the test-merge, e.g. after a sibling merge advanced the base). Set
          every poll, no hysteresis. [reconcile_automerge] reads it via
          [Patch_controller.automerge_transient_hold] to hold the automerge idle
          timer through the recompute blip. Replaces the former
          [merge_state_status] string. [false] until first polled / after
          [clear_pr]. *)
  merge_queue_required : bool;
  merge_queue_entry : Pr_state.merge_queue_entry option;
  merge_commit_sha : string option;
      (** Squash/merge commit SHA once this patch's PR is merged (GitHub
          [mergeCommit.oid]). Persisted, because merged agents are not
          re-polled; dependents' base-containment gate ancestry-checks it. *)
  base_contains_merged_siblings : bool;
      (** Poll-derived cache (like [merge_ready]): whether this patch's resolved
          base branch already contains the squash commit of every *merged*
          dependency of this patch. Recomputed each poll tick; fail-closed to
          [false] until known. Read by the reconciler and the Start/Rebase
          eligibility gate. *)
  is_draft : bool;
  pr_body_delivered : bool;
  pr_body_artifact_miss_count : int;
      (** Consecutive Pr_body sessions that produced [Respond_pr_body_miss]:
          either the artifact was missing/empty AND a Write tool call did not
          complete (agent blocked mid-call), or the GitHub [update_pr_body]
          PATCH call failed ([`Patch_failed]). Contributes to
          [needs_intervention] at [>= 2]. Zero in fresh snapshots and older
          snapshots that didn't persist the field. Reset by
          [reset_intervention_state] and by [Respond_ok] for [Pr_body]. *)
  review_unresolved_cycle_count : int;
      (** Consecutive Review_comments sessions that completed cleanly
          ([Respond_ok]-shaped) but did not reply-and-resolve every delivered
          comment: missing response file, failed reply/resolve forge call, or an
          unaddressable comment (synthetic id, no thread id). The loop's only
          terminator — the agent cannot resolve threads itself, so a comment it
          never responds to would otherwise re-enqueue a session every poll
          forever. Contributes to [needs_intervention] at [>= 2]. Zero in fresh
          and older snapshots. Reset by a fully-converged review cycle
          ([Respond_ok] for [Review_comments]) and by
          [reset_intervention_state]. *)
  start_attempts_without_pr : int;
  conflict_noop_count : int;
  no_commits_push_count : int;
  context_exhaustion_count : int;
      (** Consecutive sessions that ended by exhausting the model's context
          window ([Run_classification.Context_exhausted]).
          [on_context_exhausted] bumps this and clears [llm_session_id] so the
          next session starts fresh (resuming the overflowed thread would
          re-overflow). At [>= 2] contributes to [needs_intervention] — a fresh
          session that still overflows means the task does not fit one context
          window. Reset on a successful session and by
          [reset_intervention_state]. *)
  push_failure_count : int;
      (** Consecutive [Session_push_failed] outcomes (Session_ok or session
          retry with [Push_rejected]/[Push_error]) since the last successful
          push. At [>= 3] contributes to [needs_intervention]. Reset on
          successful push ([Push_ok] / [Push_up_to_date]) and by
          [reset_intervention_state]. A {e permanent} rejection
          ([Push_reject_classify.is_permanent]) short-circuits this counter by
          setting [session_fallback = Given_up] directly — see
          {!Orchestrator.apply_session_result}. *)
  rebase_failure_count : int;
      (** Consecutive worktree rebase failures ([Worktree.Error]) since the last
          successful/noop rebase. At [>= 2] contributes to [needs_intervention].
          Kept separate from [session_fallback] so git fetch/ref-lock failures
          are not rendered as LLM session failures. *)
  branch_rebased_onto : Types.Branch.t option;
  branch_rebased_onto_sha : string option;
      (** SHA the base ref resolved to at the time of the most recent successful
          rebase / start. Used by [Worktree.rebase_onto] as the
          [--onto NEW_BASE OLD_SHA] anchor when the base transitions from one
          dep's branch to another (or to [main]): without it, the rebase falls
          back to plain [git rebase NEW_BASE] which replays {e all} commits
          between the local main and HEAD, leaving the old dep's commits on the
          branch even after the dep's squash-merge appears on origin/main.
          [None] when no rebase has succeeded yet, or when the orchestrator was
          restarted before this field was added (it is back-filled to [None] via
          [Patch_agent.restore], and old snapshots persist with [null]). *)
  anchor_history : Anchor_history.t;
      (** Newest-first log of {!Anchor.t} values recorded over this agent's
          lifetime. Updated via {!record_anchor}; mirrored into
          [branch_rebased_onto] and [branch_rebased_onto_sha] as a derived view.
          {!Rebase_decision.plan} consults the history when the newest anchor is
          unreachable from HEAD and walks back to the oldest still- reachable
          entry. Capped at {!Anchor_history.cap}. *)
  checks_passing : bool;
  generation : int;
  worktree_path : string option;
  branch_blocked : bool;
  automerge : automerge_state;
  review : review_state;
  delivered_ci_run_ids : int list;
      (** CheckRun [databaseId]s already delivered as CI feedback. Sorted and
          deduplicated. Drives per-run deduplication in the CI delivery path so
          a single failing run is never delivered twice. Cleared on [clear_pr].
      *)
}
[@@deriving show, eq, sexp_of, compare]

val default_max_ci_failures : int
(** Built-in default for [max_ci_failures] (3). Single source of truth: the CLI
    default, the stored-config fallback for legacy [config.json] files, and the
    constructor defaults all reference this constant. *)

val create :
  branch:Types.Branch.t -> ?max_ci_failures:int -> Types.Patch_id.t -> t
(** Initial state for a patch: no PR, not busy, empty queue. [max_ci_failures]
    defaults to {!default_max_ci_failures}. *)

(** {2 Derived predicates} *)

val has_pr : t -> bool
(** [true] when this planned patch has a tracked PR. *)

val pr_number : t -> Types.Pr_number.t option
(** The tracked PR number, if any. *)

val is_busy : t -> bool
val current_op : t -> Types.Operation_kind.t option
val current_op_state : t -> op_state option
val current_message_id : t -> Types.Message_id.t option
val has_session : t -> bool
val session_fallback : t -> session_fallback
val llm_session_id : t -> string option
val automerge_enabled : t -> bool
val automerge_deadline : t -> float option
val automerge_failure_count : t -> int
val review_requested_for_oid : t -> string option
val review_failure : t -> (string * string) option

val intervention_reason : t -> string option
(** [Some reason] when the agent needs intervention; [None] otherwise. Returns
    the first triggering condition's short label, in the same priority order as
    the predicate. The label strings — ["conflict_noop_count>=2"], etc. — are
    stable and intended to land verbatim in the event log so operators can grep
    "why is this patch stuck?" by reason. The CI label embeds the configured cap
    (["ci_failure_count>=3"] under the default) — match it by the
    ["ci_failure_count>="] prefix, not the full string. *)

val intervention_reason_of_fields :
  merged:bool ->
  has_pr:bool ->
  session_given_up:bool ->
  human_in_queue:bool ->
  ci_failure_count:int ->
  max_ci_failures:int ->
  validation_failure_count:int ->
  start_attempts_without_pr:int ->
  conflict_noop_count:int ->
  no_commits_push_count:int ->
  context_exhaustion_count:int ->
  push_failure_count:int ->
  rebase_failure_count:int ->
  pr_body_artifact_miss_count:int ->
  review_unresolved_cycle_count:int ->
  string option
(** Raw-field form of {!intervention_reason}. This is the canonical pure
    decision for callers that reconstruct agent status from persisted telemetry
    instead of holding a {!t}. *)

val needs_intervention : t -> bool
(** [Option.is_some (intervention_reason t)]. Derived predicate. True iff the
    agent is not [merged] AND any of:
    - [session_fallback = Given_up] (bypasses the Human exemption)
    - [Human] not in queue AND any of: [ci_failure_count >= max_ci_failures],
      [(not has_pr) && start_attempts_without_pr >= 2],
      [conflict_noop_count >= 2], [no_commits_push_count >= 2],
      [context_exhaustion_count >= 2], [push_failure_count >= 3],
      [rebase_failure_count >= 2], [pr_body_artifact_miss_count >= 2],
      [review_unresolved_cycle_count >= 2]. *)

val needs_intervention_of_fields :
  merged:bool ->
  has_pr:bool ->
  session_given_up:bool ->
  human_in_queue:bool ->
  ci_failure_count:int ->
  max_ci_failures:int ->
  validation_failure_count:int ->
  start_attempts_without_pr:int ->
  conflict_noop_count:int ->
  no_commits_push_count:int ->
  context_exhaustion_count:int ->
  push_failure_count:int ->
  rebase_failure_count:int ->
  pr_body_artifact_miss_count:int ->
  review_unresolved_cycle_count:int ->
  bool
(** [Option.is_some (intervention_reason_of_fields ...)]. *)

(** {2 Spec actions} *)

val start : t -> base_branch:Types.Branch.t -> t
(** [PatchCtx ~> Start] — begin work on a patch. Preconditions (checked):
    [~has_pr], [~busy]. Caller must verify [in_gameplan] and [deps_satisfied]
    externally. Postconditions: [has_session], [busy], [satisfies],
    [base_branch = Some base_branch]. Pending direct messages move atomically to
    [inflight_human_messages], and a queued [Human] operation is consumed by the
    Start turn so pre-PR feedback does not require a parallel operation. *)

val rebase : t -> base_branch:Types.Branch.t -> t
(** [PatchCtx ~> Rebase] — orchestrator-executed rebase. Preconditions:
    [has_pr], [~merged], [~busy], [Rebase] in [queue], [Rebase] is
    [highest_priority]. Postconditions: [busy]; dequeues [Rebase]; preserves
    [has_session] (does not force true); updates [base_branch]. *)

val respond : t -> Types.Operation_kind.t -> t
(** [PatchCtx, Comments ~> Respond] — respond to queued feedback. Preconditions
    (checked): [has_pr], [~merged], [~busy], [~needs_intervention], [k] in
    [queue], [k] is [highest_priority]. Postconditions per spec: sets
    [has_session], [busy]; dequeues [k]; conditionally updates [satisfies],
    [changed], [has_conflict], and resolves [pending_comments]. *)

val complete : t -> t
(** [PatchCtx ~> Complete] — session finished. Preconditions (checked): [busy].
    Postconditions: [~busy]. [needs_intervention] is derived automatically from
    [ci_failure_count], [session_fallback], [start_attempts_without_pr], and
    [Human] in queue. *)

(** {2 State mutation helpers} *)

val enqueue : t -> Types.Operation_kind.t -> t
(** Add an operation to the queue (idempotent). *)

val mark_merged : t -> t
(** Mark the patch as merged. *)

val add_human_message : t -> string -> t
(** Add a human message to the pending list. *)

val add_human_messages : t -> string list -> t
(** Prepend multiple messages to the pending list, preserving their order. *)

val set_session_failed : t -> t
(** Mark session fallback as [Given_up]. *)

val set_tried_fresh : t -> t
(** Advance session fallback to [Tried_fresh]. No-op if already [Tried_fresh] or
    [Given_up] — the fallback state only moves forward. *)

val clear_session_fallback : t -> t
(** Reset session fallback to [Fresh_available]. *)

val on_session_failure : t -> is_fresh:bool -> t
(** Handle a Claude session failure. Pure decision:
    - Start path (no PR) + fresh failure: reset to [Fresh_available] for retry
    - Resume failure: escalate to [Tried_fresh] (will try fresh next)
    - Respond path fresh failure: escalate to [Given_up] → needs_intervention *)

val on_pr_discovery_failure : t -> t
(** Handle a successful Claude run where PR discovery failed. Increments the
    durable attempt counter so [needs_intervention] fires after repeated
    failures. No-op when the agent already has a PR. *)

val on_pre_session_failure : t -> t
(** Handle a failure that occurs before a Claude session starts (worktree
    creation, process spawn error). Increments [start_attempts_without_pr] for
    no-PR agents so they hit [needs_intervention] after 2 failures instead of
    retrying indefinitely. No-op for agents that already have a PR. *)

val set_has_conflict : t -> t
(** Mark the patch as having a merge conflict. *)

val clear_has_conflict : t -> t
(** Clear the merge conflict flag. Does NOT reset [conflict_noop_count]; call
    [reset_conflict_noop_count] explicitly when a conflict is truly resolved
    (not just a noop). *)

val reset_conflict_noop_count : t -> t
(** Reset [conflict_noop_count] to 0. Call when a conflict is genuinely resolved
    (successful rebase, agent resolution, or poll no longer reports conflict).
*)

val set_base_branch : t -> Types.Branch.t -> t
(** Update the base branch. *)

val set_notified_base_branch : t -> Types.Branch.t -> t
(** Record that the agent session has been informed of this base branch. *)

val set_branch_rebased_onto : t -> Types.Branch.t -> t
(** Record that the local branch has been rebased onto this base (either by an
    explicit successful [Rebase] action, or by the initial [Start] which plants
    the branch on its base). Drives [detect_notified_base_drift]. *)

val set_branch_rebased_onto_sha : t -> string option -> t
(** Record the SHA the base ref resolved to at the moment of a successful rebase
    / start. [None] (or a whitespace-only string) clears the field — used when
    the SHA could not be read; the next rebase will then fall back to the legacy
    plain [git rebase target] semantics. *)

val record_anchor : t -> Anchor.t -> t
(** Push [anchor] onto {!field-anchor_history} (newest-first, deduped by
    [(base, sha)]) and update the legacy view fields [branch_rebased_onto] and
    [branch_rebased_onto_sha] to mirror the new anchor. Pure and total. *)

val anchor_history : t -> Anchor_history.t

val base_branch_changed : t -> bool
(** [true] when [base_branch] differs from [notified_base_branch], meaning the
    agent session has not yet been told about the current base branch. *)

val set_merge_ready : t -> bool -> t
(** Set the component-derived [merge_ready] flag ([Pr_state.merge_ready_of]). *)

val set_head_oid : t -> string option -> t
val set_review_decision : t -> string option -> t
val set_unresolved_comment_count : t -> int -> t

val set_mergeability_unknown : t -> bool -> t
(** Set the [mergeability_unknown] poll-mirror
    ([Pr_state.merge_state = Unknown]). *)

val set_merge_queue_required : t -> bool -> t
(** Set whether the patch's target branch is governed by a merge queue. *)

val set_merge_queue_entry : t -> Pr_state.merge_queue_entry option -> t
(** Set the current merge-queue entry, if any. *)

val in_merge_queue : t -> bool
(** [true] when the PR currently sits in a merge queue
    ([merge_queue_entry <> None], refreshed every poll via
    [Automerge_state.observe_merge_queue]). While true, the head branch is
    push-locked by GitHub ([Push_reject_classify.Merge_queue_locked]) and the
    queue itself validates against the latest base — so rebase/conflict demand
    for the patch is redundant and its pushes are guaranteed to be rejected.
    Enqueue-side gates ([Reconciler] detectors, [Orchestrator.mark_merged] and
    the stranded-dependent cascade) consult this predicate. *)

val set_merge_commit_sha : t -> string option -> t
(** Record the squash/merge commit SHA (GitHub [mergeCommit.oid]) when the PR is
    observed merged. *)

val set_base_contains_merged_siblings : t -> bool -> t
(** Set the poll-derived base-containment cache. *)

val set_is_draft : t -> bool -> t
(** Set the draft flag from GitHub PR state. *)

val set_pr_body_delivered : t -> bool -> t
(** Record whether the LLM-authored PR body has been written to the artifact and
    PATCHed onto the PR. Set to [true] on Pr_body Respond_ok regardless of
    whether the artifact existed (so we don't loop on missing artifacts —
    documented fallback is to keep the gameplan-derived body). *)

val increment_start_attempts_without_pr : t -> t
(** Record a successful Start run that still failed to discover a PR. *)

val increment_conflict_noop_count : t -> t
(** Record a conflict resolution attempt where rebase returned Noop (stale refs
    or no real diff). After 2 noop attempts, [needs_intervention] triggers. *)

val increment_no_commits_push_count : t -> t
(** Record a session that ended with no commits on the branch (HEAD == base).
    After 2 such sessions, [needs_intervention] triggers — the agent is not
    committing its work and further retries are wasted. *)

val reset_no_commits_push_count : t -> t
(** Reset [no_commits_push_count] to 0. Called on [Session_ok] with a successful
    push, because the agent has demonstrated it can commit. *)

val on_context_exhausted : t -> t
(** Record a session that exhausted the model's context window. Bumps
    [context_exhaustion_count] and clears [llm_session_id] so the next session
    starts fresh — resuming the overflowed thread would re-overflow. Leaves
    [session_fallback] untouched; exhaustion's intervention budget is the
    counter, not the resume/fresh ladder. *)

val reset_context_exhaustion_count : t -> t
(** Reset [context_exhaustion_count] to 0. Called on a successful session. *)

val increment_push_failure_count : t -> t
(** Record a [Session_push_failed] outcome. At [>= 3], [needs_intervention]
    triggers — the push has been refused by the remote three sessions in a row
    (e.g. lease races that don't resolve, or any transient server-side block). A
    {e permanent} rejection (workflow scope, branch protection, push rule)
    bypasses this counter entirely; see {!Orchestrator.apply_session_result}. *)

val reset_push_failure_count : t -> t
(** Reset [push_failure_count] to 0. Called on a successful push ([Push_ok] /
    [Push_up_to_date]) in [Orchestrator.apply_session_result]. *)

val increment_rebase_failure_count : t -> t
(** Record a worktree rebase failure. At [>= 2], [needs_intervention] triggers —
    repeated fetch/ref-lock/rebase setup failures require operator attention and
    should not be reported as LLM session failures. *)

val reset_rebase_failure_count : t -> t
(** Reset [rebase_failure_count] to 0. Called on successful/noop rebase paths.
    Leaves [session_fallback] untouched because LLM session-failure intervention
    state is independent from rebase/worktree recovery. *)

val increment_pr_body_artifact_miss_count : t -> t
(** Record a Pr_body session that ended without durable PR body delivery. Called
    from [Orchestrator.apply_respond_outcome] on [Respond_pr_body_miss], which
    covers two cases: (a) missing/empty artifact AND an observed non-completed
    Write tool call (agent blocked mid-call); (b)
    [artifact_outcome = `Patch_failed] (notes written but the GitHub
    [update_pr_body] call failed). After 2 such sessions, [needs_intervention]
    triggers. *)

val reset_pr_body_artifact_miss_count : t -> t
(** Reset [pr_body_artifact_miss_count] to 0. *)

val increment_review_unresolved_cycle_count : t -> t
(** Record a Review_comments session that completed cleanly but left at least
    one delivered comment without a posted reply-and-resolve. Called from
    [Orchestrator.apply_respond_outcome] on [Respond_review_unresolved]. After 2
    such consecutive cycles, [needs_intervention] triggers — the retry-once-
    then-intervene contract shared with [pr_body_artifact_miss_count]. *)

val reset_review_unresolved_cycle_count : t -> t
(** Reset [review_unresolved_cycle_count] to 0. *)

val set_checks_passing : t -> bool -> t
(** Set the checks_passing flag from GitHub CI status. *)

val set_worktree_path : t -> string -> t
(** Store the resolved worktree path for this patch. *)

val is_approved_modulo_merge_ready : t -> main_branch:Types.Branch.t -> bool
(** Every approval precondition except [merge_ready]. A patch satisfying this
    but with [merge_ready = false] is approval-ready and is only missing a
    [CLEAN] mergeability reading — the basis for holding the automerge timer
    through a transient [mergeStateStatus = UNKNOWN]. *)

val is_approved : t -> main_branch:Types.Branch.t -> bool
(** Derived predicate:
    [has_pr && merge_ready && not is_draft && not busy && not needs_intervention
     && base_branch = main_branch]. A patch is only approved when its PR targets
    [main_branch] directly and is no longer a draft. [merge_ready] is the
    component-derived readiness ([Pr_state.merge_ready_of]: mergeable + CI
    passing + non-blocking review), not GitHub's [mergeStateStatus]; the merge
    attempt is the final authority on branch-protection specifics. *)

val should_request_review : t -> main_branch:Types.Branch.t -> bool

val increment_ci_failure_count : t -> t
(** Increment the CI failure counter. Called from
    [Orchestrator.apply_respond_outcome] on [Respond_ok] for a Ci delivery, so
    the counter only reflects CI fix attempts that actually delivered a payload
    with failure conclusions. *)

val reset_ci_failure_count : t -> t
(** Reset [ci_failure_count] to 0. Called by the poller when CI checks pass
    after failures. [needs_intervention] is re-derived automatically. *)

val set_max_ci_failures : t -> max_ci_failures:int -> t
(** Stamp the per-project CI-failure cap onto the agent. Config, not a state
    transition: does {e not} bump [generation], so restamping restored agents at
    startup does not invalidate in-flight outbox messages. *)

val reset_intervention_state : t -> t
(** Reset [session_fallback] to [Fresh_available], [ci_failure_count] to 0,
    [start_attempts_without_pr] to 0, [conflict_noop_count] to 0,
    [no_commits_push_count] to 0, [context_exhaustion_count] to 0,
    [push_failure_count] to 0, [rebase_failure_count] to 0, and
    [pr_body_artifact_miss_count] to 0. Used after manual resolution (e.g.,
    sending a human message) to give the patch a fresh start. *)

val set_branch_blocked : t -> t
(** Set the branch-blocked flag (branch is checked out in repo root). *)

val clear_branch_blocked : t -> t
(** Clear the branch-blocked flag (branch is no longer in repo root). *)

val set_ci_checks : t -> Types.Ci_check.t list -> t
(** Replace the stored CI check details. *)

val record_delivered_ci_run_ids : t -> int list -> t
(** Mark the given CheckRun [databaseId]s as delivered so the CI feedback path
    will not re-deliver them. Merges with the existing set, sorts, and dedups.
    [ids] should contain only CheckRuns that carried an id (StatusContext
    entries without stable numeric ids cannot be deduped). *)

val reset_busy : t -> t
(** Reset a stale [busy] flag from a crashed session. If [busy], clears it.
    [needs_intervention] is derived automatically. No-op if not busy. *)

val set_current_message_id : t -> Types.Message_id.t option -> t
(** Track the currently accepted delivery message for this patch. *)

val bump_generation : t -> t
(** Advance the patch generation used for deterministic message IDs. *)

val set_llm_session_id : t -> string option -> t
(** Store the LLM backend's session ID for explicit session resumption.
    Preserved across fallback escalation so the operator can resume the session
    after intervention. Cleared on start-path fresh-failure reset (clean retry)
    and when the session is known dead (no-resume, give-up). *)

val mark_inflight_human_messages_delivered : t -> t
(** Clear [inflight_human_messages] once the backend has emitted evidence that
    it accepted the turn. Start and Human turns can both carry direct messages.
    Does not complete the session or change fallback state. *)

val increment_validation_failure_count : t -> t
val reset_validation_failure_count : t -> t

val set_automerge_enabled : t -> bool -> t
(** Enable or disable automerge for this patch. When the value actually changes,
    [automerge_failure_count] is reset; disabling additionally clears any
    pending deadline so the next enable starts a fresh timer. Calling with the
    current value is a no-op — the failure count and [automerge_deadline] are
    NOT reset in that case. If the preserved deadline has already elapsed, the
    next reconcile tick will fire immediately rather than waiting
    [automerge_idle_timeout]; callers that need a fresh timer must call
    [clear_automerge_deadline] explicitly after this function. *)

val set_automerge_deadline : t -> float -> t
(** Record the Unix timestamp at which the supervisor should merge this patch if
    it is still approved. *)

val clear_automerge_deadline : t -> t
(** Clear a pending automerge deadline without disabling automerge. *)

val mark_review_requested : t -> string -> t
val mark_review_failed : t -> head_oid:string -> error:string -> t

val increment_automerge_failure_count : t -> t
(** Record a failed automerge call. After [automerge_max_failures] consecutive
    failures the patch is no longer an automerge candidate. *)

val reset_automerge_failure_count : t -> t
(** Reset the consecutive-failure counter to zero. Called on a successful merge
    and whenever automerge is re-toggled. *)

val resume_current_message : t -> op:Types.Operation_kind.t option -> t
(** Resume execution of an already accepted message without reapplying its
    queue-consuming state transition. [~op] restores [current_op] from the
    outbox so that [complete] can clear [human_messages] correctly. Resets
    [current_op_state] to [Queued] — the resumed fiber must call [mark_running]
    when it actually begins work. *)

val mark_running : t -> t
(** Transition [current_op_state] from [Queued] to [Running]. Called from the
    action fiber once the Claude semaphore has been acquired and real work is
    about to begin. No-op when [busy] is false (fiber already exited). *)

(** {2 Queries} *)

val highest_priority : t -> Types.Operation_kind.t option
(** The highest-priority operation in the queue, or [None] if empty. *)

(** {2 Persistence support} *)

val set_pr_number : t -> Types.Pr_number.t -> t
(** Store [pr_number] (making [has_pr] true). Dispatches on
    {!Patch_pr_status.classify_set_present}:
    - [Preserve_existing] ([Present N] observed again): preserves world-state.
    - [Adopt_new] (prior was [Absent], or the number differs): resets
      PR-bootstrap fields ([is_draft = true], [pr_body_delivered = false],
      [start_attempts_without_pr = 0]) plus PR-keyed CI history that no longer
      matches the new PR's check runs ([ci_checks = []], [ci_failure_count = 0],
      [delivered_ci_run_ids = []]). Does NOT touch [base_branch] /
      [notified_base_branch] — those are owned by [start] during bootstrap and
      by the poller during renumbering. *)

val clear_pr : t -> t
(** Remove the PR number and reset PR-related state, returning the planned patch
    to the no-PR bootstrap path. Raises [Invalid_argument] when already absent.
*)

val restore :
  patch_id:Types.Patch_id.t ->
  branch:Types.Branch.t ->
  pr_status:Patch_pr_status.t ->
  session:session_state ->
  activity:activity ->
  merged:bool ->
  queue:Types.Operation_kind.t list ->
  satisfies:bool ->
  changed:bool ->
  has_conflict:bool ->
  base_branch:Types.Branch.t option ->
  notified_base_branch:Types.Branch.t option ->
  ci_failure_count:int ->
  ?max_ci_failures:int ->
  ?validation_failure_count:int ->
  human_messages:string list ->
  inflight_human_messages:string list ->
  ci_checks:Types.Ci_check.t list ->
  merge_ready:bool ->
  ?head_oid:string option ->
  ?review_decision:string option ->
  ?unresolved_comment_count:int ->
  mergeability_unknown:bool ->
  merge_queue_required:bool ->
  merge_queue_entry:Pr_state.merge_queue_entry option ->
  merge_commit_sha:string option ->
  base_contains_merged_siblings:bool ->
  is_draft:bool ->
  pr_body_delivered:bool ->
  pr_body_artifact_miss_count:int ->
  ?review_unresolved_cycle_count:int ->
  start_attempts_without_pr:int ->
  conflict_noop_count:int ->
  no_commits_push_count:int ->
  context_exhaustion_count:int ->
  push_failure_count:int ->
  rebase_failure_count:int ->
  branch_rebased_onto:Types.Branch.t option ->
  branch_rebased_onto_sha:string option ->
  anchor_history:Anchor_history.t ->
  checks_passing:bool ->
  generation:int ->
  worktree_path:string option ->
  branch_blocked:bool ->
  automerge:automerge_state ->
  ?review:review_state ->
  delivered_ci_run_ids:int list ->
  unit ->
  t
(** Reconstruct agent state from persisted field values. Bypasses precondition
    checks — use only for deserialization. *)
