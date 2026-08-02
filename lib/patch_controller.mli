open Types

type poll_log_entry = { message : string; patch_id : Patch_id.t }
[@@deriving show, eq]

type poll_observation = {
  poll_result : Poller.t;
  base_branch : Branch.t option;
  branch_in_root : bool;
  worktree_path : string option;
}

val discovery_intents : Orchestrator.t -> (Patch_id.t * Branch.t) list
(** Patches that have run at least once ([has_session]) but lack a PR and are
    not merged. Returns [(patch_id, branch)] pairs for tick-based PR discovery
    in the poller. *)

val reconcile_patch :
  Orchestrator.t ->
  project_name:string ->
  gameplan:Gameplan.t ->
  patch:Patch.t ->
  Orchestrator.t
(** Reconcile durable per-patch lifecycle facts into queue updates and GitHub
    outbox commands. The same snapshot always produces the same result. *)

val apply_poll_result :
  ?merge_queue_ejection_confirmed:bool ->
  Orchestrator.t ->
  Patch_id.t ->
  poll_observation ->
  Orchestrator.t * poll_log_entry list * bool
(** Apply a GitHub poll observation to durable state. Returns the updated
    orchestrator, log entries, and whether the patch became newly branch-
    blocked in this step. This is the controller-owned pure poll ingestion step.

    [merge_queue_ejection_confirmed] is the poller's verdict on a merge-queue
    ejection (default [false]): [true] when the removal event confirmed a real
    merge-group check failure — or the confirming fetch errored, erring toward
    surfacing it — and [false] for a conflict-driven ejection, which is left to
    surface through normal [Merge_conflict] handling. Consulted only when the PR
    actually left the queue while green and mergeable; ignored otherwise. *)

val apply_replacement_pr :
  Orchestrator.t ->
  Patch_id.t ->
  pr_number:Pr_number.t ->
  base_branch:Branch.t ->
  merged:bool ->
  Orchestrator.t
(** Apply replacement-PR discovery after a closed PR is re-mapped to a new open
    PR for the same patch. *)

val reconcile_all :
  Orchestrator.t -> project_name:string -> gameplan:Gameplan.t -> Orchestrator.t
(** Reconcile PR base, draft state, and PR-body delivery for every plan patch.
*)

val plan_actions :
  Orchestrator.t -> patches:Patch.t list -> Orchestrator.action list
(** Compute runnable actions from the current snapshot after reconciliation.
    This is the evergreen scheduler used by the main loop. *)

val plan_messages :
  Orchestrator.t ->
  patches:Patch.t list ->
  Orchestrator.patch_agent_message list
(** Compute durable runnable messages from the current snapshot after
    reconciliation. Accepted but incomplete messages are replayed; new desired
    actions become pending messages in the outbox. *)

val plan_tick_messages :
  Orchestrator.t ->
  project_name:string ->
  gameplan:Gameplan.t ->
  Orchestrator.t * Orchestrator.patch_agent_message list
(** Reconcile durable state, enqueue missing GitHub commands, and compute
    durable runnable patch-agent messages for the same snapshot. *)

val plan_tick :
  Orchestrator.t ->
  project_name:string ->
  gameplan:Gameplan.t ->
  Orchestrator.t * Orchestrator.action list
(** Reconcile durable state, enqueue missing GitHub commands, and compute
    runnable actions for the same snapshot. *)

val tick :
  Orchestrator.t ->
  project_name:string ->
  gameplan:Gameplan.t ->
  Orchestrator.t * Orchestrator.action list
(** Reconcile durable state, enqueue missing GitHub commands, and fire the
    planned actions into the orchestrator state. The returned action list is the
    set of actions that were fired. *)

val automerge_idle_timeout : float
(** Seconds of idle time after approval before automerge fires. *)

val automerge_max_failures : int
(** Hard cap on consecutive automerge call failures per patch. Once a patch hits
    this count it is no longer a candidate and reconciliation stops retrying
    until automerge is toggled off/on (or a successful merge resets the count —
    which cannot happen once the cap is hit, so the toggle is the only
    recovery). *)

type merge_action = Direct_merge | Enqueue | Dequeue of string
[@@deriving show, eq, sexp_of]

val is_automerge_candidate : Patch_agent.t -> main_branch:Branch.t -> bool
(** A patch is a candidate for a new automerge command when it is not already
    merged, automerge is enabled, the PR is approved, CI is passing, the queue
    is empty, and the consecutive failure count is under
    [automerge_max_failures]. Any queued feedback (Review_comments, Human, Ci,
    Merge_conflict, Pr_body) resets the deadline. *)

val automerge_transient_hold : Patch_agent.t -> main_branch:Branch.t -> bool
(** [true] when a direct-merge patch has lost [merge_ready] *only* because
    GitHub is transiently recomputing mergeability ([mergeability_unknown], i.e.
    [Pr_state.merge_state = Unknown]) while every other automerge precondition
    still holds. This is the benign flap a sibling merge causes: advancing the
    base invalidates mergeability on every other open PR at once, so they read
    [Unknown] for a poll or two before settling back to [Mergeable].
    [reconcile_automerge] preserves the existing deadline in this state instead
    of clearing it, so the idle window measures real elapsed time rather than
    restarting on every sibling merge. Scoped to the direct-merge path
    ([merge_queue_entry = None]) and to [Unknown] alone — a conflict, failing
    checks, queued feedback, or a hit failure cap all fall through to the normal
    clear. *)

val set_automerge_enabled :
  Orchestrator.t -> Patch_id.t -> bool -> Orchestrator.t
(** Change automerge policy and discard obsolete pending, retrying, or failed
    automerge commands. A running adapter call cannot be canceled; its durable
    outcome still wins. Calling with the current value is a no-op. *)

val should_dequeue_merge_queue :
  Patch_agent.t -> main_branch:Branch.t -> entry_id:string -> bool
(** [true] when an already-enqueued PR should be removed from GitHub's merge
    queue. This includes explicit queue alarms ([UNMERGEABLE] entries, conflict
    state, or visible failing checks) and lost approval. *)

val reconcile_automerge :
  Orchestrator.t -> now:float -> Orchestrator.t * Github_effect.t list
(** Reconcile the automerge deadline for every agent and enqueue durable
    commands. For each agent:
    - merged → clear any stale deadline and automerge commands.
    - existing automerge command → no-op; the outbox is the sole claim.
      Pending/retrying commands whose policy preconditions no longer hold are
      removed first; running and terminally failed commands are retained.
    - candidate + no deadline → set deadline at [now +. automerge_idle_timeout].
    - not candidate + deadline, but [automerge_transient_hold] → preserve the
      deadline unchanged and emit no decision (GitHub is recomputing
      mergeability after the base advanced; the idle window keeps counting).
    - not candidate + deadline (and not a transient hold) → clear deadline
      (feedback arrived, CI flipped, automerge disabled, or failure cap hit).
    - candidate + deadline elapsed → enqueue exactly one deterministic command.
      A persistent-failure PR retries once per idle window until the failure
      counter reaches [automerge_max_failures]. *)

val reconcile_review_requests :
  Orchestrator.t -> team_slug:string -> Orchestrator.t * Github_effect.t list
(** Enqueue a deterministic durable review request for each eligible patch. The
    GitHub adapter receives its exact PR, team, and head inputs from the
    command; no transient claim is stored on the patch agent. Pending or
    retrying requests are discarded when eligibility, team, or head changes;
    running and terminally failed commands are retained. *)

val apply_automerge_success : Orchestrator.t -> Patch_id.t -> Orchestrator.t
(** Mark the patch as merged, clear the automerge deadline, and reset the
    failure counter. *)

val apply_merge_queue_entered :
  Orchestrator.t -> Patch_id.t -> Pr_state.merge_queue_entry -> Orchestrator.t
(** Record that the PR is in GitHub's merge queue, regardless of whether that
    was learned from an automerge enqueue response, manual enqueue, or polling.
    Being in the merge queue is terminal for the automerge timer: the next
    useful transition comes from polling GitHub's queue state, not from another
    automerge fire. *)

val apply_merge_queue_dequeued :
  Orchestrator.t -> now:float -> Patch_id.t -> Orchestrator.t
(** Record that an automerge dequeue request succeeded. The PR is no longer
    known to be in GitHub's merge queue, the successful GitHub call clears the
    consecutive automerge API-failure counter, and the deadline is restarted so
    a still-ready PR can be enqueued again after the idle window. *)

val apply_automerge_failure :
  Orchestrator.t -> now:float -> Patch_id.t -> Orchestrator.t
(** Record a failed merge call by incrementing the consecutive failure counter.
    Push the deadline out to [now +. automerge_idle_timeout] so the retry is at
    least one idle window away (without this bound, a persistent GitHub failure
    could burst many merge calls per poll cycle since the runner re-reconciles
    every tick). The deadline is NOT re-armed when either (a) the failure cap
    has now been reached (reconciliation will no longer issue merge calls for
    this patch), or (b) the user disabled automerge while the command was
    outstanding. *)

val github_retry_delay : float
val github_max_attempts : int

type github_success =
  | Mutation_applied
  | Merge_succeeded
  | Merge_pending
  | Queue_entered of Pr_state.merge_queue_entry
[@@deriving show, eq, sexp_of]

val finish_github_success :
  Orchestrator.t ->
  now:float ->
  Github_effect.t ->
  github_success ->
  Orchestrator.t

val finish_github_failure :
  Orchestrator.t ->
  now:float ->
  Github_effect.t ->
  permanent:bool ->
  error:string ->
  Orchestrator.t
(** Resolve a durable GitHub command. Success applies the corresponding domain
    transition and removes the command in one state change. Failure either
    schedules a bounded retry or records a terminal failed command; review
    failures also enter the agent's fail-closed review state. *)
