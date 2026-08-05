open Types

(** Shared mutable runtime state, protected by an Eio mutex.

    All fibers (TUI, poller, Claude agent runners) access the application state
    through this module. A single [Eio.Mutex.t] serializes access — coarse but
    sufficient for the expected concurrency level. *)

type snapshot = {
  orchestrator : Orchestrator.t;
  activity_log : Activity_log.t;
  gameplan : Gameplan.t;
  transcripts : (Patch_id.t, string) Base.Hashtbl.t;
}

type t
type durable_store = snapshot -> (unit, string) result

val create :
  gameplan:Gameplan.t ->
  main_branch:Branch.t ->
  ?max_ci_failures:int ->
  ?snapshot:snapshot ->
  durable_store:durable_store ->
  unit ->
  t
(** Build initial runtime state from a gameplan, optionally restoring a previous
    [snapshot]. [max_ci_failures] is the resolved per-project CI-failure cap; it
    is stamped onto the orchestrator (and every agent) in both the fresh and the
    restore path, so config always wins over persisted values. *)

(** {2 Atomic read access} *)

val read : t -> (snapshot -> 'a) -> 'a
(** [read t f] acquires the mutex, passes a consistent snapshot to [f], and
    releases. *)

(** {2 Atomic read-modify-write} *)

val update : t -> (snapshot -> snapshot) -> unit
(** [update t f] acquires the mutex, applies [f] to the current snapshot, stores
    the result, and releases. *)

val update_orchestrator : t -> (Orchestrator.t -> Orchestrator.t) -> unit
(** Convenience: update only the orchestrator. *)

val update_orchestrator_returning :
  t -> (Orchestrator.t -> Orchestrator.t * 'a) -> 'a
(** Like [update_orchestrator] but the callback returns a value alongside the
    new orchestrator state. Useful when you need an atomic check-and-modify that
    also reports what happened. *)

(** {2 Durable state transitions} *)

val commit : t -> (snapshot -> snapshot * 'a) -> ('a, string) result
(** Compute a prospective snapshot while holding the runtime lock, atomically
    write it through the configured durable store, and expose it in memory only
    after the write succeeds. A failed write leaves the old snapshot intact. *)

val commit_orchestrator :
  t -> (Orchestrator.t -> Orchestrator.t) -> (unit, string) result

val commit_orchestrator_returning :
  t -> (Orchestrator.t -> Orchestrator.t * 'a) -> ('a, string) result

val commit_expansion :
  t ->
  policy:Expansion_policy.t option ->
  parent_id:Patch_id.t ->
  Plan_expansion.proposal ->
  (bool, string) result

val update_activity_log : t -> (Activity_log.t -> Activity_log.t) -> unit
(** Convenience: update only the activity log. *)

val snapshot_unsync : t -> snapshot
(** Read the snapshot without acquiring the mutex. Safe only when all fibers
    have terminated (e.g. in a [Fun.protect ~finally] cleanup block after
    [Fiber.all] has returned or raised). *)
