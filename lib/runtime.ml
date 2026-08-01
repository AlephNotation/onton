(* @archlint.module state
   @archlint.domain orchestrator *)

open Types

type snapshot = {
  orchestrator : Orchestrator.t;
  activity_log : Activity_log.t;
  gameplan : Gameplan.t;
  transcripts : (Patch_id.t, string) Base.Hashtbl.t;
}

type durable_store = snapshot -> (unit, string) result

type t = {
  mutex : Eio.Mutex.t;
  durable_store : durable_store;
  mutable snap : snapshot;
}

let create ~gameplan ~(main_branch : Branch.t)
    ?(max_ci_failures = Patch_agent.default_max_ci_failures) ?snapshot
    ~durable_store () =
  let snap =
    match snapshot with
    | Some s ->
        let orchestrator =
          Orchestrator.set_main_branch s.orchestrator main_branch
        in
        { s with orchestrator; gameplan }
    | None ->
        let orchestrator =
          Orchestrator.create ~patches:gameplan.Gameplan.patches ~main_branch
        in
        {
          orchestrator;
          activity_log = Activity_log.empty;
          gameplan;
          transcripts = Base.Hashtbl.create (module Patch_id);
        }
  in
  (* Config wins over whatever the snapshot (or the constructor default)
     carried: the cap is a per-project setting resolved from the CLI flag and
     stored config, re-applied on every startup. *)
  let snap =
    {
      snap with
      orchestrator =
        Orchestrator.set_max_ci_failures snap.orchestrator ~max_ci_failures;
    }
  in
  { mutex = Eio.Mutex.create (); durable_store; snap }

let read t f =
  Eio.Mutex.lock t.mutex;
  match f t.snap with
  | v ->
      Eio.Mutex.unlock t.mutex;
      v
  | exception ex ->
      Eio.Mutex.unlock t.mutex;
      raise ex

let update t f =
  (* Manual lock/unlock instead of [use_rw]: [use_rw] poisons the mutex on
     any exception (including [Cancelled]), which cascades into every other
     fiber and crashes the process. Since [f] is a pure snapshot→snapshot
     transform, the assignment either completes or doesn't — no inconsistent
     state is possible, so poisoning is never warranted. *)
  Eio.Mutex.lock t.mutex;
  match t.snap <- f t.snap with
  | () -> Eio.Mutex.unlock t.mutex
  | exception ex ->
      Eio.Mutex.unlock t.mutex;
      raise ex

let update_orchestrator t f =
  update t (fun s -> { s with orchestrator = f s.orchestrator })

let update_orchestrator_returning t f =
  let result = ref None in
  update t (fun s ->
      let orch', v = f s.orchestrator in
      result := Some v;
      { s with orchestrator = orch' });
  (* [update] guarantees the callback ran exactly once before returning *)
  match !result with
  | Some v -> v
  | None -> assert false

let commit t f =
  Eio.Mutex.lock t.mutex;
  match f t.snap with
  | next, value -> (
      match t.durable_store next with
      | Ok () ->
          t.snap <- next;
          Eio.Mutex.unlock t.mutex;
          Ok value
      | Error message ->
          Eio.Mutex.unlock t.mutex;
          Error message)
  | exception ex ->
      Eio.Mutex.unlock t.mutex;
      raise ex

let commit_orchestrator t f =
  commit t (fun s -> ({ s with orchestrator = f s.orchestrator }, ()))

let commit_orchestrator_returning t f =
  commit t (fun s ->
      let orchestrator, value = f s.orchestrator in
      ({ s with orchestrator }, value))

let update_activity_log t f =
  update t (fun s -> { s with activity_log = f s.activity_log })

let snapshot_unsync t = t.snap
