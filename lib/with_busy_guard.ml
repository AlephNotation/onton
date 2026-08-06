open Base

module type ENV = sig
  val runtime : Runtime.t
  val event_log : Event_log.t
end

module Make (Env : ENV) = struct
  let run ~patch_id f =
    let cancelled = ref false in
    let exception_raised = ref false in
    Stdlib.Fun.protect
      ~finally:(fun () ->
        let reason =
          if !cancelled then Orchestrator.Cancelled
          else if !exception_raised then Orchestrator.Unexpected_exception
          else Orchestrator.Cancelled
        in
        let needs_completion =
          Runtime.read Env.runtime (fun snap ->
              match
                Orchestrator.find_agent snap.Runtime.orchestrator patch_id
              with
              | Some agent -> Patch_agent.is_busy agent
              | None -> false)
        in
        let snapshot =
          if not needs_completion then None
          else
            Runtime.commit_orchestrator_returning_exn Env.runtime (fun orch ->
                match Orchestrator.find_agent orch patch_id with
                | None -> (orch, None)
                | Some before ->
                    if Patch_agent.is_busy before then
                      let orch' =
                        Orchestrator.apply_force_complete orch patch_id reason
                      in
                      let after = Orchestrator.agent orch' patch_id in
                      (orch', Some (before, after))
                    else (orch, None))
        in
        Option.iter snapshot ~f:(fun (before, after) ->
            Event_log.log_force_complete Env.event_log ~patch_id ~reason
              ~agent_before:before ~agent_after:after;
            Runtime_logging.log_event Env.runtime ~patch_id
              (Printf.sprintf
                 "Forced complete (%s) — runner fiber exited with busy=true"
                 (Orchestrator.show_force_complete_reason reason))))
      (fun () ->
        try f () with
        | Eio.Cancel.Cancelled _ as exn ->
            cancelled := true;
            raise exn
        | Runtime.Durable_store_failed _ as exn ->
            exception_raised := true;
            raise exn
        | exn ->
            exception_raised := true;
            Runtime_logging.log_event Env.runtime ~patch_id
              (Printf.sprintf "Unexpected action exception — %s"
                 (Exn.to_string exn)))
end
