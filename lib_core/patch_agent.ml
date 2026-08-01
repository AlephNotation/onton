(* @archlint.module core
   @archlint.domain patch-agent *)

open Base
open Types
open Operation_kind

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
      operation : Operation_kind.t option;
      message_id : Message_id.t option;
    }
  | Active of {
      operation : Operation_kind.t option;
      phase : op_state;
      message_id : Message_id.t option;
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

type t = {
  patch_id : Patch_id.t;
  branch : Branch.t;
  pr_status : Patch_pr_status.t;
  session : session_state;
  activity : activity;
  merged : bool;
  queue : Operation_kind.t list;
  satisfies : bool;
  changed : bool;
  has_conflict : bool;
  base_branch : Branch.t option;
  notified_base_branch : Branch.t option;
  ci_failure_count : int;
  max_ci_failures : int;
  human_messages : string list;
  inflight_human_messages : string list;
  ci_checks : Ci_check.t list;
  merge_ready : bool;
  head_oid : string option;
  review_decision : string option;
  unresolved_comment_count : int;
  mergeability_unknown : bool;
      (** Poll-mirror of [Pr_state.merge_state = Unknown]: GitHub is currently
          recomputing the test-merge (e.g. a sibling patch merged and advanced
          the base). Set every poll like [merge_ready]/[checks_passing], with no
          hysteresis. [reconcile_automerge] reads it via
          [Patch_controller.automerge_transient_hold] to *hold* the automerge
          idle timer through the recompute blip instead of resetting it.
          Replaces the former [merge_state_status] string — the hold no longer
          reads GitHub's [mergeStateStatus] at all. [false] until first polled
          and after [clear_pr]. *)
  merge_queue_required : bool;
  merge_queue_entry : Pr_state.merge_queue_entry option;
  merge_commit_sha : string option;
      (** Squash/merge commit SHA once this patch's PR is merged (GitHub
          [mergeCommit.oid]). Persisted, because merged agents are not
          re-polled; dependents' base-containment gate ancestry-checks it. *)
  base_contains_merged_siblings : bool;
      (** Poll-derived cache (like [merge_ready]): whether this patch's resolved
          base branch already contains the squash commit of every *merged*
          dependency of this patch. Recomputed each poll tick by [poller_fiber]
          via [git merge-base --is-ancestor]; fail-closed to [false] until
          known. Read by the reconciler ([detect_sibling_stale_bases]) and the
          Start/Rebase eligibility gate. *)
  is_draft : bool;
  pr_body_delivered : bool;
  pr_body_artifact_miss_count : int;
      (** Consecutive Pr_body sessions that ended with evidence the agent was
          blocked mid-write (see [Orchestrator.Respond_pr_body_miss]). At >=2
          contributes to [needs_intervention]. Reset by
          [reset_intervention_state]. *)
  review_unresolved_cycle_count : int;
      (** Consecutive Review_comments sessions that completed cleanly but did
          not reply-and-resolve every delivered comment (missing response file,
          failed reply/resolve call, or an unaddressable comment — see
          [Orchestrator.Respond_review_unresolved]). Without this cap the loop
          has no terminator: the agent cannot resolve threads itself, so a
          comment it never responds to re-enqueues a session every poll,
          forever. At [>= 2] contributes to [needs_intervention]. Reset by a
          fully-converged review cycle and by [reset_intervention_state]. *)
  start_attempts_without_pr : int;
  conflict_noop_count : int;
  no_commits_push_count : int;
  context_exhaustion_count : int;
      (** Consecutive sessions that ended by exhausting the model's context
          window ([Run_classification.Context_exhausted]). Each one clears
          [llm_session_id] so the next session starts fresh (resuming the
          overflowed thread would re-overflow). At [>= 2] contributes to
          [needs_intervention]: a *fresh* session that still overflows means the
          task does not fit one context window and needs a human. Reset on a
          successful session. *)
  push_failure_count : int;
  rebase_failure_count : int;
      (** Consecutive worktree rebase failures ([Worktree.Error]) since the last
          successful/noop rebase. At [>= 2] contributes to [needs_intervention].
          Kept separate from [session_fallback] so git fetch/ref-lock failures
          are not rendered as LLM session failures. *)
  branch_rebased_onto : Branch.t option;
  branch_rebased_onto_sha : string option;
  anchor_history : Anchor_history.t;
      (** Newest-first log of {!Anchor.t} values recorded over this agent's
          lifetime. The newest entry mirrors
          [(branch_rebased_onto, branch_rebased_onto_sha)] as a derived view;
          older entries serve as divergence fallbacks for
          {!Rebase_decision.plan}. Capped at {!Anchor_history.cap}. *)
  checks_passing : bool;
  generation : int;
  worktree_path : string option;
  branch_blocked : bool;
  automerge : automerge_state;
  review : review_state;
  delivered_ci_run_ids : int list;
      (** CheckRun [databaseId]s that have already been delivered to the agent
          as CI feedback. Maintained sorted and deduplicated. Used so a single
          failing run is only delivered once, even if [generation] bumps or
          other state changes cause the Ci payload to be recomposed with the
          same underlying failures. Cleared on [clear_pr]. Checks without a
          stable id (StatusContext entries) bypass this dedup. *)
}
[@@deriving eq, sexp_of, compare]

let pp fmt t = Sexp.pp_hum fmt (sexp_of_t t)
let show t = Sexp.to_string_hum (sexp_of_t t)
let has_pr t = Patch_pr_status.has_pr t.pr_status
let pr_number t = Patch_pr_status.pr_number t.pr_status

let is_busy t =
  match t.activity with Inactive | Interrupted _ -> false | Active _ -> true

let current_op t =
  match t.activity with
  | Inactive -> None
  | Interrupted interrupted -> interrupted.operation
  | Active active -> active.operation

let current_op_state t =
  match t.activity with
  | Inactive -> None
  | Interrupted _ -> Some Queued
  | Active active -> Some active.phase

let current_message_id t =
  match t.activity with
  | Inactive -> None
  | Interrupted interrupted -> interrupted.message_id
  | Active active -> active.message_id

let has_session t =
  match t.session with Not_started -> false | Started _ -> true

let session_fallback t =
  match t.session with
  | Not_started -> Fresh_available
  | Started session -> session.fallback

let llm_session_id t =
  match t.session with
  | Not_started -> None
  | Started session -> session.resume_id

let automerge_enabled t =
  match t.automerge with Disabled -> false | Enabled _ -> true

let automerge_deadline t =
  match t.automerge with Disabled -> None | Enabled state -> state.deadline

let automerge_failure_count t =
  match t.automerge with Disabled -> 0 | Enabled state -> state.failure_count

let review_requested_for_oid t =
  match t.review with
  | Review_requested oid -> Some oid
  | Review_not_requested | Review_failed _ -> None

let review_failure t =
  match t.review with
  | Review_failed { head_oid; error } -> Some (head_oid, error)
  | Review_not_requested | Review_requested _ -> None

let start_session = function
  | Not_started -> Started { resume_id = None; fallback = Fresh_available }
  | Started _ as session -> session

let default_max_ci_failures = 3

(* Single source of truth for the needs-intervention reason. Returns the
   first triggering condition's short label, or [None] when the predicate
   is false. [needs_intervention] is then defined as
   [Option.is_some (intervention_reason t)], so the two functions cannot
   drift. The label strings are stable and intended to land verbatim in the
   event log so operators can grep for "why is this patch stuck?" by
   reason. *)
let intervention_reason_of_fields ~merged ~has_pr ~session_given_up
    ~human_in_queue ~ci_failure_count ~max_ci_failures
    ~start_attempts_without_pr ~conflict_noop_count ~no_commits_push_count
    ~context_exhaustion_count ~push_failure_count ~rebase_failure_count
    ~pr_body_artifact_miss_count ~review_unresolved_cycle_count =
  if merged then None
  else if session_given_up then Some "session_fallback=given_up"
    (* The Human exemption lets a newly-arrived human message be delivered
       even to an agent with a high ci_failure_count or other failure state.
       However, the exemption does NOT apply when session_fallback = Given_up:
       a Given_up agent cannot start any session, so the delivery attempt
       immediately fails at the Give_up check and complete_failed re-enqueues
       Human — creating an infinite loop. Override the exemption so the
       reconciler stops scheduling actions and the agent surfaces for
       manual intervention.

       [merged] is terminal — a merged agent never needs intervention, so
       short-circuit on it to keep the predicate self-consistent even for
       callers that don't pre-filter by [merged]. *)
  else if human_in_queue then None
  else if ci_failure_count >= max_ci_failures then
    Some (Printf.sprintf "ci_failure_count>=%d" max_ci_failures)
  else if (not has_pr) && start_attempts_without_pr >= 2 then
    Some "start_attempts_without_pr>=2"
  else if conflict_noop_count >= 2 then Some "conflict_noop_count>=2"
  else if no_commits_push_count >= 2 then Some "no_commits_push_count>=2"
  else if context_exhaustion_count >= 2 then Some "context_exhaustion_count>=2"
  else if push_failure_count >= 3 then Some "push_failure_count>=3"
  else if rebase_failure_count >= 2 then Some "rebase_failure_count>=2"
  else if pr_body_artifact_miss_count >= 2 then
    Some "pr_body_artifact_miss_count>=2"
  else if review_unresolved_cycle_count >= 2 then
    Some "review_unresolved_cycle_count>=2"
  else None

let intervention_reason t =
  let ordinary =
    intervention_reason_of_fields ~merged:t.merged ~has_pr:(has_pr t)
      ~session_given_up:(equal_session_fallback (session_fallback t) Given_up)
      ~human_in_queue:
        (List.mem t.queue Operation_kind.Human ~equal:Operation_kind.equal)
      ~ci_failure_count:t.ci_failure_count ~max_ci_failures:t.max_ci_failures
      ~start_attempts_without_pr:t.start_attempts_without_pr
      ~conflict_noop_count:t.conflict_noop_count
      ~no_commits_push_count:t.no_commits_push_count
      ~context_exhaustion_count:t.context_exhaustion_count
      ~push_failure_count:t.push_failure_count
      ~rebase_failure_count:t.rebase_failure_count
      ~pr_body_artifact_miss_count:t.pr_body_artifact_miss_count
      ~review_unresolved_cycle_count:t.review_unresolved_cycle_count
  in
  match ordinary with
  | Some _ as reason -> reason
  | None -> (
      match (t.head_oid, t.review) with
      | Some current, Review_failed { head_oid; _ }
        when String.equal current head_oid ->
          Some "review_request_failed"
      | Some _, Review_not_requested
      | Some _, Review_requested _
      | Some _, Review_failed _
      | None, _ ->
          None)

let needs_intervention t = Option.is_some (intervention_reason t)

let needs_intervention_of_fields ~merged ~has_pr ~session_given_up
    ~human_in_queue ~ci_failure_count ~max_ci_failures
    ~start_attempts_without_pr ~conflict_noop_count ~no_commits_push_count
    ~context_exhaustion_count ~push_failure_count ~rebase_failure_count
    ~pr_body_artifact_miss_count ~review_unresolved_cycle_count =
  Option.is_some
    (intervention_reason_of_fields ~merged ~has_pr ~session_given_up
       ~human_in_queue ~ci_failure_count ~max_ci_failures
       ~start_attempts_without_pr ~conflict_noop_count ~no_commits_push_count
       ~context_exhaustion_count ~push_failure_count ~rebase_failure_count
       ~pr_body_artifact_miss_count ~review_unresolved_cycle_count)

let create ~branch ?(max_ci_failures = default_max_ci_failures) patch_id =
  {
    patch_id;
    branch;
    pr_status = Patch_pr_status.Absent;
    session = Not_started;
    activity = Inactive;
    merged = false;
    queue = [];
    satisfies = false;
    changed = false;
    has_conflict = false;
    base_branch = None;
    notified_base_branch = None;
    ci_failure_count = 0;
    max_ci_failures;
    human_messages = [];
    inflight_human_messages = [];
    ci_checks = [];
    merge_ready = false;
    head_oid = None;
    review_decision = None;
    unresolved_comment_count = 0;
    mergeability_unknown = false;
    merge_queue_required = false;
    merge_queue_entry = None;
    merge_commit_sha = None;
    (* Defaults to [true] ("no known missing sibling"): the poller recomputes
       this every tick before any fan-in start can become eligible, and a fresh
       agent has no merged deps yet. Fail-open at birth, recomputed-to-truth. *)
    base_contains_merged_siblings = true;
    is_draft = false;
    pr_body_delivered = false;
    pr_body_artifact_miss_count = 0;
    review_unresolved_cycle_count = 0;
    start_attempts_without_pr = 0;
    conflict_noop_count = 0;
    no_commits_push_count = 0;
    context_exhaustion_count = 0;
    push_failure_count = 0;
    rebase_failure_count = 0;
    branch_rebased_onto = None;
    branch_rebased_onto_sha = None;
    anchor_history = Anchor_history.empty;
    checks_passing = false;
    generation = 0;
    worktree_path = None;
    branch_blocked = false;
    automerge = Disabled;
    review = Review_not_requested;
    delivered_ci_run_ids = [];
  }

let highest_priority t =
  List.min_elt t.queue ~compare:(fun a b ->
      Int.compare (Priority.priority a) (Priority.priority b))

let enqueue t k =
  if List.mem t.queue k ~equal:Operation_kind.equal then t
  else { t with queue = k :: t.queue }

let mark_merged t = { t with merged = true }

let add_human_message t msg =
  { t with human_messages = msg :: t.human_messages }

let add_human_messages t msgs =
  { t with human_messages = List.append t.human_messages msgs }

let set_session_failed t =
  match t.session with
  | Not_started -> t
  | Started ({ fallback = Fresh_available; _ } as session) ->
      { t with session = Started { session with fallback = Tried_fresh } }
  | Started { fallback = Tried_fresh | Given_up; _ } -> t

let set_tried_fresh t =
  match t.session with
  | Not_started -> t
  | Started ({ fallback = Fresh_available; _ } as session) ->
      { t with session = Started { session with fallback = Tried_fresh } }
  | Started ({ fallback = Tried_fresh; _ } as session) ->
      { t with session = Started { session with fallback = Given_up } }
  | Started { fallback = Given_up; _ } -> t

let clear_session_fallback t =
  match t.session with
  | Not_started -> t
  | Started session ->
      { t with session = Started { session with fallback = Fresh_available } }

(** Handle a Claude session failure. Pure decision logic:
    - Start path (no PR) + fresh failure: reset to Fresh_available for retry
    - Resume failure: escalate to Tried_fresh (will try fresh next)
    - Fresh failure (respond path): escalate one step via set_tried_fresh *)
let on_session_failure t ~is_fresh =
  if (not (has_pr t)) && is_fresh then
    (* Start path fresh failure: full reset for clean retry *)
    {
      t with
      session = Started { resume_id = None; fallback = Fresh_available };
    }
  else if is_fresh then set_tried_fresh t
  else set_session_failed t

let set_has_conflict t = { t with has_conflict = true }
let clear_has_conflict t = { t with has_conflict = false }
let reset_conflict_noop_count t = { t with conflict_noop_count = 0 }

let increment_conflict_noop_count t =
  { t with conflict_noop_count = t.conflict_noop_count + 1 }

let increment_no_commits_push_count t =
  { t with no_commits_push_count = t.no_commits_push_count + 1 }

let reset_no_commits_push_count t = { t with no_commits_push_count = 0 }

(* Context-window exhaustion: bump the counter and drop [llm_session_id] so the
   next session starts fresh — resuming the overflowed thread would re-overflow
   immediately. Deliberately leaves [session_fallback] untouched: exhaustion has
   its own intervention budget (the counter), orthogonal to the resume/fresh
   fallback ladder. *)
let on_context_exhausted t =
  {
    t with
    context_exhaustion_count = t.context_exhaustion_count + 1;
    session =
      (match t.session with
      | Not_started -> Not_started
      | Started session -> Started { session with resume_id = None });
  }

let reset_context_exhaustion_count t = { t with context_exhaustion_count = 0 }

let increment_push_failure_count t =
  { t with push_failure_count = t.push_failure_count + 1 }

let reset_push_failure_count t = { t with push_failure_count = 0 }

let increment_rebase_failure_count t =
  { t with rebase_failure_count = t.rebase_failure_count + 1 }

let reset_rebase_failure_count t = { t with rebase_failure_count = 0 }

let increment_pr_body_artifact_miss_count t =
  { t with pr_body_artifact_miss_count = t.pr_body_artifact_miss_count + 1 }

let reset_pr_body_artifact_miss_count t =
  { t with pr_body_artifact_miss_count = 0 }

let increment_review_unresolved_cycle_count t =
  { t with review_unresolved_cycle_count = t.review_unresolved_cycle_count + 1 }

let reset_review_unresolved_cycle_count t =
  { t with review_unresolved_cycle_count = 0 }

let set_base_branch t branch =
  let notified =
    if has_session t then
      match t.notified_base_branch with None -> Some branch | some -> some
    else t.notified_base_branch
  in
  { t with base_branch = Some branch; notified_base_branch = notified }

let set_notified_base_branch t branch =
  { t with notified_base_branch = Some branch }

let base_branch_changed t =
  match (t.notified_base_branch, t.base_branch) with
  | Some old_base, Some new_base -> not (Branch.equal old_base new_base)
  | _ -> false

let set_merge_ready t v = { t with merge_ready = v }
let set_head_oid t head_oid = { t with head_oid }
let set_review_decision t review_decision = { t with review_decision }

let set_unresolved_comment_count t unresolved_comment_count =
  { t with unresolved_comment_count }

let set_mergeability_unknown t v = { t with mergeability_unknown = v }
let set_merge_queue_required t v = { t with merge_queue_required = v }
let set_merge_queue_entry t merge_queue_entry = { t with merge_queue_entry }
let in_merge_queue t = Option.is_some t.merge_queue_entry
let set_merge_commit_sha t sha = { t with merge_commit_sha = sha }

let set_base_contains_merged_siblings t v =
  { t with base_contains_merged_siblings = v }

let set_is_draft t v = { t with is_draft = v }
let set_pr_body_delivered t v = { t with pr_body_delivered = v }

let increment_start_attempts_without_pr t =
  { t with start_attempts_without_pr = t.start_attempts_without_pr + 1 }

(** Handle a successful Claude run where PR discovery failed by recording a
    durable attempt. The controller derives intervention from this fact. No-op
    when the agent already has a PR — reruns after the first session should not
    accumulate this counter. *)
let on_pr_discovery_failure t =
  if has_pr t then t else increment_start_attempts_without_pr t

let on_pre_session_failure t =
  if has_pr t then t else increment_start_attempts_without_pr t

let set_checks_passing t v = { t with checks_passing = v }
let set_worktree_path t path = { t with worktree_path = Some path }

(* Every approval precondition *except* [merge_ready] (the component-derived
   readiness, [Pr_state.merge_ready_of]). Factored out so [reconcile_automerge]
   can recognize a patch that is fully approval-ready and is only missing
   readiness because GitHub is transiently recomputing mergeability (mergeable
   reads [Unknown], surfaced as [mergeStateStatus = UNKNOWN]) — see
   [Patch_controller.automerge_transient_hold]. *)
let is_approved_modulo_merge_ready t ~main_branch =
  has_pr t
  && (not (is_busy t))
  && (not (needs_intervention t))
  && (not t.is_draft) && (not t.branch_blocked)
  && Option.equal Branch.equal t.base_branch (Some main_branch)

let is_approved t ~main_branch =
  t.merge_ready && is_approved_modulo_merge_ready t ~main_branch

let should_request_review t ~main_branch =
  has_pr t && (not t.has_conflict)
  && (not t.mergeability_unknown)
  && t.checks_passing
  && t.unresolved_comment_count = 0
  && (not t.is_draft)
  && (not (is_busy t))
  && (not (needs_intervention t))
  && Option.equal Branch.equal t.base_branch (Some main_branch)
  && Option.equal String.equal t.review_decision (Some "REVIEW_REQUIRED")
  &&
  match t.head_oid with
  | None -> false
  | Some head_oid -> (
      match t.review with
      | Review_not_requested -> true
      | Review_requested requested -> not (String.equal head_oid requested)
      | Review_failed { head_oid = failed; _ } ->
          not (String.equal head_oid failed))

let increment_ci_failure_count t =
  { t with ci_failure_count = t.ci_failure_count + 1 }

let reset_ci_failure_count t = { t with ci_failure_count = 0 }

(* Config stamp, not a state transition: deliberately does not bump
   [generation], so restoring a snapshot under a different --max-ci-failures
   does not invalidate in-flight outbox messages. *)
let set_max_ci_failures t ~max_ci_failures = { t with max_ci_failures }
let set_ci_checks t checks = { t with ci_checks = checks }

let record_delivered_ci_run_ids t ids =
  (* Maintain sorted + deduplicated list. Small enough (tens of entries per
     patch lifetime) that a list is fine; sorting keeps equality checks stable
     across serialization round-trips. *)
  let combined = List.rev_append ids t.delivered_ci_run_ids in
  let deduped =
    List.dedup_and_sort combined ~compare:(fun a b -> Int.compare a b)
  in
  { t with delivered_ci_run_ids = deduped }

let set_branch_blocked t = { t with branch_blocked = true }
let clear_branch_blocked t = { t with branch_blocked = false }

let set_current_message_id t message_id =
  match t.activity with
  | Inactive ->
      if Option.is_none message_id then t
      else invalid_arg "Patch_agent.set_current_message_id: patch is idle"
  | Interrupted interrupted ->
      { t with activity = Interrupted { interrupted with message_id } }
  | Active active -> { t with activity = Active { active with message_id } }

let bump_generation t = { t with generation = t.generation + 1 }

let set_llm_session_id t resume_id =
  match (t.session, resume_id) with
  | Not_started, None -> t
  | Not_started, Some _ ->
      { t with session = Started { resume_id; fallback = Fresh_available } }
  | Started session, _ ->
      { t with session = Started { session with resume_id } }

let mark_inflight_human_messages_delivered t =
  if
    Option.equal Operation_kind.equal (current_op t) (Some Operation_kind.Human)
  then { t with inflight_human_messages = [] }
  else t

let set_automerge_enabled t v =
  match (t.automerge, v) with
  | Disabled, false | Enabled _, true -> t
  | Disabled, true ->
      { t with automerge = Enabled { deadline = None; failure_count = 0 } }
  | Enabled _, false -> { t with automerge = Disabled }

let set_automerge_deadline t deadline =
  match t.automerge with
  | Disabled -> t
  | Enabled state ->
      { t with automerge = Enabled { state with deadline = Some deadline } }

let clear_automerge_deadline t =
  match t.automerge with
  | Disabled -> t
  | Enabled state ->
      { t with automerge = Enabled { state with deadline = None } }

let mark_review_requested t head_oid =
  { t with review = Review_requested head_oid }

let mark_review_failed t ~head_oid ~error =
  { t with review = Review_failed { head_oid; error } }

let increment_automerge_failure_count t =
  match t.automerge with
  | Disabled -> t
  | Enabled state ->
      {
        t with
        automerge =
          Enabled { state with failure_count = state.failure_count + 1 };
      }

let reset_automerge_failure_count t =
  match t.automerge with
  | Disabled -> t
  | Enabled state ->
      { t with automerge = Enabled { state with failure_count = 0 } }

let resume_current_message t ~op =
  {
    t with
    session = start_session t.session;
    activity = Active { operation = op; phase = Queued; message_id = None };
  }

let mark_running t =
  match t.activity with
  | Inactive | Interrupted _ -> t
  | Active active ->
      { t with activity = Active { active with phase = Running } }

let reset_intervention_state t =
  {
    t with
    session =
      (match t.session with
      | Not_started -> Not_started
      | Started session -> Started { session with fallback = Fresh_available });
    ci_failure_count = 0;
    start_attempts_without_pr = 0;
    conflict_noop_count = 0;
    no_commits_push_count = 0;
    context_exhaustion_count = 0;
    push_failure_count = 0;
    rebase_failure_count = 0;
    pr_body_artifact_miss_count = 0;
    review_unresolved_cycle_count = 0;
    review =
      (match t.review with
      | Review_failed _ -> Review_not_requested
      | Review_not_requested -> Review_not_requested
      | Review_requested oid -> Review_requested oid);
  }

let reset_busy t =
  match t.activity with
  | Inactive | Interrupted _ -> t
  | Active active ->
      {
        t with
        activity =
          Interrupted
            { operation = active.operation; message_id = active.message_id };
      }

let restore ~patch_id ~branch ~pr_status ~session ~activity ~merged ~queue
    ~satisfies ~changed ~has_conflict ~base_branch ~notified_base_branch
    ~ci_failure_count ?(max_ci_failures = default_max_ci_failures)
    ~human_messages ~inflight_human_messages ~ci_checks ~merge_ready
    ?(head_oid = None) ?(review_decision = None) ?(unresolved_comment_count = 0)
    ~mergeability_unknown ~merge_queue_required ~merge_queue_entry
    ~merge_commit_sha ~base_contains_merged_siblings ~is_draft
    ~pr_body_delivered ~pr_body_artifact_miss_count
    ?(review_unresolved_cycle_count = 0) ~start_attempts_without_pr
    ~conflict_noop_count ~no_commits_push_count ~context_exhaustion_count
    ~push_failure_count ~rebase_failure_count ~branch_rebased_onto
    ~branch_rebased_onto_sha ~anchor_history ~checks_passing ~generation
    ~worktree_path ~branch_blocked ~automerge ?(review = Review_not_requested)
    ~delivered_ci_run_ids () =
  {
    patch_id;
    branch;
    pr_status;
    session;
    activity;
    merged;
    queue;
    satisfies;
    changed;
    has_conflict;
    base_branch;
    notified_base_branch;
    ci_failure_count;
    max_ci_failures;
    human_messages;
    inflight_human_messages;
    ci_checks;
    merge_ready;
    head_oid;
    review_decision;
    unresolved_comment_count;
    mergeability_unknown;
    merge_queue_required;
    merge_queue_entry;
    merge_commit_sha;
    base_contains_merged_siblings;
    is_draft;
    pr_body_delivered;
    pr_body_artifact_miss_count;
    review_unresolved_cycle_count;
    start_attempts_without_pr;
    conflict_noop_count;
    no_commits_push_count;
    context_exhaustion_count;
    push_failure_count;
    rebase_failure_count;
    branch_rebased_onto;
    branch_rebased_onto_sha;
    anchor_history;
    checks_passing;
    generation;
    worktree_path;
    branch_blocked;
    automerge;
    review;
    delivered_ci_run_ids;
  }

let set_pr_number t pr_number =
  (* Dispatch on the pure classifier:
     - [Set_present_recover_same] (Missing N → Present N or Present N →
       Present N): preserve all world-state. The body is still delivered, CI
       runs already accounted, notified_base_branch still authoritative.
     - [Set_present_adopt_new] (Absent → Present, Missing M → Present N
       M≠N, Present M → Present N M≠N): reset PR-bootstrap lifecycle fields
       that the OLD setter cleared, plus the PR-keyed CI history that no
       longer corresponds to the new PR's check runs. Does NOT touch
       base_branch / notified_base_branch — those are owned by [start]
       (bootstrap) and the poller (renumbering). *)
  match Patch_pr_status.classify_set_present t.pr_status pr_number with
  | Patch_pr_status.Preserve_existing ->
      { t with pr_status = Patch_pr_status.set_present t.pr_status pr_number }
  | Patch_pr_status.Adopt_new ->
      {
        t with
        pr_status = Patch_pr_status.set_present t.pr_status pr_number;
        is_draft = true;
        merge_ready = false;
        head_oid = None;
        review_decision = None;
        review = Review_not_requested;
        unresolved_comment_count = 0;
        mergeability_unknown = false;
        pr_body_delivered = false;
        checks_passing = false;
        start_attempts_without_pr = 0;
        ci_checks = [];
        ci_failure_count = 0;
        delivered_ci_run_ids = [];
      }

let clear_pr t =
  {
    t with
    pr_status = Patch_pr_status.clear t.pr_status;
    is_draft = false;
    merge_ready = false;
    head_oid = None;
    review_decision = None;
    review = Review_not_requested;
    unresolved_comment_count = 0;
    mergeability_unknown = false;
    checks_passing = false;
    ci_checks = [];
    ci_failure_count = 0;
    base_branch = None;
    notified_base_branch = None;
    delivered_ci_run_ids = [];
  }

let start t ~base_branch =
  if has_pr t then invalid_arg "Patch_agent.start: patch already has a PR";
  if is_busy t then invalid_arg "Patch_agent.start: patch is already busy";
  {
    t with
    session = start_session t.session;
    activity = Active { operation = None; phase = Queued; message_id = None };
    satisfies = true;
    base_branch = Some base_branch;
    notified_base_branch = Some base_branch;
    (* The initial Start plants the branch on [base_branch]; the local branch
       tip is literally this base's HEAD until the agent commits. Record it
       so the drift detector knows the branch is on the right base. *)
    branch_rebased_onto = Some base_branch;
    ci_checks = [];
  }

let set_branch_rebased_onto t branch =
  { t with branch_rebased_onto = Some branch }

let set_branch_rebased_onto_sha t sha =
  match sha with
  | None -> { t with branch_rebased_onto_sha = None }
  | Some s ->
      let s = String.strip s in
      if String.is_empty s then { t with branch_rebased_onto_sha = None }
      else { t with branch_rebased_onto_sha = Some s }

let record_anchor t anchor =
  let anchor_history = Anchor_history.push t.anchor_history anchor in
  {
    t with
    anchor_history;
    branch_rebased_onto = Some (Anchor.base anchor);
    branch_rebased_onto_sha = Some (Anchor.sha anchor);
  }

let anchor_history t = t.anchor_history

let rebase t ~base_branch =
  if not (has_pr t) then invalid_arg "Patch_agent.rebase: patch has no PR";
  if t.merged then invalid_arg "Patch_agent.rebase: patch is merged";

  if is_busy t then invalid_arg "Patch_agent.rebase: patch is busy";
  if not (List.mem t.queue Operation_kind.Rebase ~equal:Operation_kind.equal)
  then invalid_arg "Patch_agent.rebase: Rebase not in queue";
  (match highest_priority t with
  | Some hp when Operation_kind.equal hp Operation_kind.Rebase -> ()
  | _ -> invalid_arg "Patch_agent.rebase: Rebase not highest priority");
  let queue =
    List.filter t.queue ~f:(fun j ->
        not (Operation_kind.equal j Operation_kind.Rebase))
  in
  {
    t with
    session = start_session t.session;
    activity =
      Active { operation = Some Rebase; phase = Queued; message_id = None };
    queue;
    base_branch = Some base_branch;
    notified_base_branch =
      (match t.notified_base_branch with
      | None -> Some base_branch
      | some -> some);
    merge_ready = false;
    mergeability_unknown = false;
    checks_passing = false;
  }

let respond t k =
  if not (has_pr t) then invalid_arg "Patch_agent.respond: patch has no PR";
  if t.merged then invalid_arg "Patch_agent.respond: patch is merged";

  if is_busy t then invalid_arg "Patch_agent.respond: patch is busy";
  if needs_intervention t then
    invalid_arg "Patch_agent.respond: patch needs intervention";
  if Operation_kind.equal k Operation_kind.Rebase then
    invalid_arg "Patch_agent.respond: Rebase is not a feedback operation";
  if not (List.mem t.queue k ~equal:Operation_kind.equal) then
    invalid_arg "Patch_agent.respond: operation not in queue";
  (match highest_priority t with
  | Some hp when Operation_kind.equal hp k -> ()
  | _ -> invalid_arg "Patch_agent.respond: not highest priority");
  let queue =
    List.filter t.queue ~f:(fun j -> not (Operation_kind.equal j k))
  in
  let equal_k = Operation_kind.equal k in
  let is_human = equal_k Human in
  let is_ci = equal_k Ci in
  let is_review = equal_k Review_comments in
  let satisfies = if is_human then false else t.satisfies in
  (* Spec: changed' only when a valid pending comment exists. We set it
     unconditionally here because comment validity is resolved downstream
     by the agent session — a conservative simplification. *)
  let changed = if is_ci || is_review then true else t.changed in
  (* NB: [ci_failure_count] is NOT bumped here. The bump happens in
     [Orchestrator.apply_respond_outcome] on [Respond_ok] for Ci, so the
     counter only reflects CI fix attempts that actually delivered a payload
     (i.e. had at least one failure conclusion) and ran to completion. *)
  {
    t with
    session = start_session t.session;
    activity = Active { operation = Some k; phase = Queued; message_id = None };
    queue;
    satisfies;
    changed;
    human_messages = (if is_human then [] else t.human_messages);
    inflight_human_messages =
      (if is_human then t.human_messages else t.inflight_human_messages);
    notified_base_branch =
      (match t.notified_base_branch with None -> t.base_branch | some -> some);
    merge_ready = false;
    mergeability_unknown = false;
    checks_passing = false;
  }

let complete t =
  if not (is_busy t) then t
  else { t with activity = Inactive; inflight_human_messages = [] }

(* -- Tests for session failure recovery -- *)

let%test
    "on_session_failure: start path fresh resets to Fresh_available and clears \
     session" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t =
    {
      t with
      activity = Active { operation = None; phase = Running; message_id = None };
      session = Started { resume_id = None; fallback = Tried_fresh };
    }
  in
  let t = on_session_failure t ~is_fresh:true in
  equal_session_fallback (session_fallback t) Fresh_available

let%test "on_session_failure: resume failure escalates to Tried_fresh" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t =
    {
      t with
      activity = Active { operation = None; phase = Running; message_id = None };
      session = Started { resume_id = None; fallback = Fresh_available };
    }
  in
  let t = on_session_failure t ~is_fresh:false in
  equal_session_fallback (session_fallback t) Tried_fresh

let%test "on_session_failure: respond path fresh escalates to Tried_fresh" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t = set_pr_number t (Pr_number.of_int 1) in
  let t =
    {
      t with
      activity = Active { operation = None; phase = Running; message_id = None };
      session = Started { resume_id = None; fallback = Fresh_available };
    }
  in
  let t = on_session_failure t ~is_fresh:true in
  equal_session_fallback (session_fallback t) Tried_fresh

let%test
    "on_session_failure: respond path second fresh failure escalates to \
     Given_up" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t = set_pr_number t (Pr_number.of_int 1) in
  let t =
    {
      t with
      activity = Active { operation = None; phase = Running; message_id = None };
      session = Started { resume_id = None; fallback = Tried_fresh };
    }
  in
  let t = on_session_failure t ~is_fresh:true in
  equal_session_fallback (session_fallback t) Given_up

let%test
    "on_session_failure: start fresh failure + complete does not set \
     needs_intervention" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t =
    {
      t with
      activity = Active { operation = None; phase = Running; message_id = None };
      session = Started { resume_id = None; fallback = Tried_fresh };
    }
  in
  let t = on_session_failure t ~is_fresh:true in
  let t = complete t in
  not (needs_intervention t)

let%test "on_pr_discovery_failure increments attempts from zero" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t = on_pr_discovery_failure t in
  t.start_attempts_without_pr = 1

let%test "on_pr_discovery_failure increments attempts again" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t = on_pr_discovery_failure t in
  let t = on_pr_discovery_failure t in
  t.start_attempts_without_pr = 2

let%test "on_pr_discovery_failure is no-op when agent has a PR" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t = set_pr_number t (Pr_number.of_int 42) in
  let t = on_pr_discovery_failure t in
  t.start_attempts_without_pr = 0
