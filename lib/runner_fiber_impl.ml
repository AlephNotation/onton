open Onton_core.Types

let log_event runtime ?patch_id msg =
  Runtime_logging.log_event runtime ?patch_id msg

module Runner_env = struct
  module type S = sig
    include Run_env.S

    val owner : string
    val repo : string
    val main_branch : Branch.t
    val max_concurrency : int
    val review_team : string option
    val patch_agent_provider : string option
    val patch_agent_effort : string option
    val findings_registry : Findings_registry.t

    val review_clients :
      (module Review_service_client.S
         with type error = Review_service_client.error)
      list

    val transcripts : (Patch_id.t, string) Stdlib.Hashtbl.t
    val event_log : Event_log.t
    val process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t
    val backend : Backend_registry.kind
    val backend_decision : Backend_routing.decision
    val register_pr : patch_id:Patch_id.t -> pr_number:Pr_number.t -> unit
  end
end

module Make
    (Forge : Forge.S with type error = Github.error)
    (W : Worktree.S)
    (Env : Runner_env.S) =
struct
  exception Durable_commit_failed of string

  module WS_Env : Worktree_setup.ENV = struct
    let runtime = Env.runtime
    let clock = Env.clock
    let fs = Env.fs
    let project_name = Env.project_name
    let user_config = Env.user_config
    let worktree_mutex = Env.worktree_mutex
    let hook_mutex = Env.hook_mutex
    let fetch_mutex = Env.fetch_mutex
  end

  module SD_Env : Session_driver.ENV = struct
    let runtime = Env.runtime
    let clock = Env.clock
    let fs = Env.fs
    let project_name = Env.project_name
    let owner = Env.owner
    let repo = Env.repo
    let transcripts = Env.transcripts
    let user_config = Env.user_config
    let worktree_mutex = Env.worktree_mutex
    let hook_mutex = Env.hook_mutex
    let fetch_mutex = Env.fetch_mutex
    let event_log = Env.event_log
  end

  module WS = Worktree_setup.Make (W) (WS_Env)
  module Session_driver = Session_driver.Make (W) (SD_Env)
  module Long_lived_sessions = Long_lived_sessions.Make (Session_driver)

  module Busy_guard_env = struct
    let runtime = Env.runtime
    let event_log = Env.event_log
  end

  module With_busy_guard = With_busy_guard.Make (Busy_guard_env)
  module Worktree_plan_executor = Worktree_plan_executor.Make (W) (WS_Env)

  let durable_commit runtime f =
    match Runtime.commit_orchestrator_returning runtime f with
    | Ok value -> value
    | Error message -> raise (Durable_commit_failed message)

  let github_command_label (command : Github_effect.t) =
    let pr_number =
      match command.action with
      | Github_effect.Set_pr_draft { pr_number; _ }
      | Github_effect.Set_pr_base { pr_number; _ }
      | Github_effect.Request_review { pr_number; _ }
      | Github_effect.Direct_merge { pr_number }
      | Github_effect.Enqueue { pr_number }
      | Github_effect.Dequeue { pr_number; _ } ->
          pr_number
    in
    Printf.sprintf "%s for PR #%d"
      (match command.action with
      | Github_effect.Set_pr_draft _ -> "set draft state"
      | Github_effect.Set_pr_base _ -> "set base branch"
      | Github_effect.Request_review _ -> "request review"
      | Github_effect.Direct_merge _ -> "merge"
      | Github_effect.Enqueue _ -> "enqueue"
      | Github_effect.Dequeue _ -> "dequeue")
      (Pr_number.to_int pr_number)

  let github_error_is_permanent action = function
    | Github.Http_error { status; _ } -> (
        status >= 400 && status < 500
        &&
        match action with
        | Github_effect.Direct_merge _ | Github_effect.Enqueue _
        | Github_effect.Dequeue _ ->
            status <> 405
        | Github_effect.Set_pr_draft _ | Github_effect.Set_pr_base _
        | Github_effect.Request_review _ ->
            true)
    | Github.Json_parse_error _ -> true
    | Github.Timeout _ | Github.Transport_error _ | Github.Graphql_error _ ->
        false

  let claim_github_command ~runtime ~now effect_id =
    durable_commit runtime (fun orch ->
        Orchestrator.claim_github_effect orch ~now effect_id)

  let finish_github_success ~runtime command success =
    ignore
      (durable_commit runtime (fun orch ->
           let orch =
             Patch_controller.finish_github_success orch
               ~now:(Unix.gettimeofday ()) command success
           in
           (orch, ())))

  let finish_github_failure ~runtime command err =
    let error = Forge.show_error err in
    let permanent =
      github_error_is_permanent command.Github_effect.action err
    in
    ignore
      (durable_commit runtime (fun orch ->
           let orch =
             Patch_controller.finish_github_failure orch
               ~now:(Unix.gettimeofday ()) command ~permanent ~error
           in
           (orch, ())));
    error

  let call_github_adapter f =
    try `Returned (f ()) with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn -> `Crashed exn

  let finish_github_crash ~runtime command exn =
    let error = Printexc.to_string exn in
    ignore
      (durable_commit runtime (fun orch ->
           let orch =
             Patch_controller.finish_github_failure orch
               ~now:(Unix.gettimeofday ()) command ~permanent:false ~error
           in
           (orch, ())));
    error

  let execute_github_command ~runtime (command : Github_effect.t) =
    let label = github_command_label command in
    let patch_id = command.patch_id in
    let log_ok message = log_event runtime ~patch_id message in
    match command.action with
    | Github_effect.Set_pr_draft { pr_number; draft } -> (
        match
          call_github_adapter (fun () -> Forge.set_draft ~pr_number ~draft)
        with
        | `Returned (Ok ()) ->
            finish_github_success ~runtime command
              Patch_controller.Mutation_applied;
            log_ok (Printf.sprintf "%s complete" label)
        | `Returned (Error err) ->
            let error = finish_github_failure ~runtime command err in
            log_ok (Printf.sprintf "%s failed — %s" label error)
        | `Crashed exn ->
            let error = finish_github_crash ~runtime command exn in
            log_ok (Printf.sprintf "%s crashed — %s" label error))
    | Github_effect.Set_pr_base { pr_number; base } -> (
        match
          call_github_adapter (fun () -> Forge.update_pr_base ~pr_number ~base)
        with
        | `Returned (Ok ()) ->
            finish_github_success ~runtime command
              Patch_controller.Mutation_applied;
            log_ok (Printf.sprintf "%s complete" label)
        | `Returned (Error err) ->
            let error = finish_github_failure ~runtime command err in
            log_ok (Printf.sprintf "%s failed — %s" label error)
        | `Crashed exn ->
            let error = finish_github_crash ~runtime command exn in
            log_ok (Printf.sprintf "%s crashed — %s" label error))
    | Github_effect.Request_review { pr_number; team_slug; _ } -> (
        match
          call_github_adapter (fun () ->
              Forge.request_review ~pr_number ~team_slug)
        with
        | `Returned (Ok ()) ->
            finish_github_success ~runtime command
              Patch_controller.Mutation_applied;
            log_ok (Printf.sprintf "%s from team %s complete" label team_slug)
        | `Returned (Error err) ->
            let error = finish_github_failure ~runtime command err in
            log_ok (Printf.sprintf "%s failed — %s" label error)
        | `Crashed exn ->
            let error = finish_github_crash ~runtime command exn in
            log_ok (Printf.sprintf "%s crashed — %s" label error))
    | Github_effect.Direct_merge { pr_number } -> (
        match call_github_adapter (fun () -> Forge.merge_pr ~pr_number) with
        | `Returned (Ok Forge.Merge_succeeded) ->
            finish_github_success ~runtime command
              Patch_controller.Merge_succeeded;
            log_ok (Printf.sprintf "%s complete" label)
        | `Returned (Ok (Forge.Merge_queued _))
        | `Returned (Ok Forge.Merge_unconfirmed) ->
            finish_github_success ~runtime command
              Patch_controller.Merge_pending;
            log_ok
              (Printf.sprintf "%s accepted; awaiting poll confirmation" label)
        | `Returned (Error err) ->
            let error = finish_github_failure ~runtime command err in
            log_ok (Printf.sprintf "%s failed — %s" label error)
        | `Crashed exn ->
            let error = finish_github_crash ~runtime command exn in
            log_ok (Printf.sprintf "%s crashed — %s" label error))
    | Github_effect.Enqueue { pr_number } -> (
        match call_github_adapter (fun () -> Forge.enqueue_pr ~pr_number) with
        | `Returned (Ok (Forge.Enqueued entry))
        | `Returned (Ok (Forge.Already_enqueued entry)) ->
            finish_github_success ~runtime command
              (Patch_controller.Queue_entered entry);
            log_ok (Printf.sprintf "%s complete" label)
        | `Returned (Error err) ->
            let error = finish_github_failure ~runtime command err in
            log_ok (Printf.sprintf "%s failed — %s" label error)
        | `Crashed exn ->
            let error = finish_github_crash ~runtime command exn in
            log_ok (Printf.sprintf "%s crashed — %s" label error))
    | Github_effect.Dequeue { pr_number; _ } -> (
        match call_github_adapter (fun () -> Forge.dequeue_pr ~pr_number) with
        | `Returned (Ok ()) ->
            finish_github_success ~runtime command
              Patch_controller.Mutation_applied;
            log_ok (Printf.sprintf "%s complete" label)
        | `Returned (Error err) ->
            let error = finish_github_failure ~runtime command err in
            log_ok (Printf.sprintf "%s failed — %s" label error)
        | `Crashed exn ->
            let error = finish_github_crash ~runtime command exn in
            log_ok (Printf.sprintf "%s crashed — %s" label error))

  let execute_github_outbox ~runtime =
    let now = Unix.gettimeofday () in
    let runnable =
      Runtime.read runtime (fun snap ->
          Orchestrator.runnable_github_effects snap.Runtime.orchestrator ~now)
    in
    Eio.Fiber.List.iter ~max_fibers:4
      (fun command ->
        match claim_github_command ~runtime ~now command.Github_effect.id with
        | None -> ()
        | Some claimed -> execute_github_command ~runtime claimed)
      runnable

  (** Pluralize a count for inline rendering: [pluralize 1 "comment"] →
      ["1 comment"], [pluralize 2 "comment"] → ["2 comments"]. Pass [~plural]
      when the plural is irregular. *)
  let pluralize ?plural n singular =
    let many = match plural with Some p -> p | None -> singular ^ "s" in
    Printf.sprintf "%d %s" n (if n = 1 then singular else many)

  let command_failure_text (failure : Patch_validator.command_failure) =
    let exit =
      match failure.exit_code with
      | Some code -> Printf.sprintf "exit %d" code
      | None -> "no exit status"
    in
    let output =
      Base.String.concat ~sep:"\n"
        [ Base.String.strip failure.stdout; Base.String.strip failure.stderr ]
      |> Base.String.strip
    in
    Printf.sprintf "%s (%s)\n%s" failure.command exit output

  let prepare_patch_contract ~runtime ~patch_id ~worktree ~base_branch =
    let patch =
      Runtime.read runtime (fun snap ->
          Base.List.find snap.Runtime.gameplan.Gameplan.patches
            ~f:(fun (patch : Patch.t) -> Patch_id.equal patch.Patch.id patch_id))
    in
    match patch with
    | Some patch -> (
        match
          Patch_validator.prepare ~process_mgr:Env.process_mgr ~clock:Env.clock
            ~fs:Env.fs ~cwd:worktree ~base_branch ~project_name:Env.project_name
            ~rebase_in_progress:(W.rebase_in_progress ~path:worktree)
            patch
        with
        | Ok Patch_validator.No_changes ->
            log_event runtime ~patch_id
              "Controller found no worker changes to commit";
            Ok ()
        | Ok Patch_validator.Committed ->
            log_event runtime ~patch_id
              "Controller validated and committed worker changes";
            Ok ()
        | Ok Patch_validator.Rebase_continued ->
            log_event runtime ~patch_id
              "Controller staged conflict resolutions and continued the rebase";
            Ok ()
        | Error (Patch_validator.Validation_failed failure) -> (
            match failure with
            | Patch_validator.Outside_scope paths ->
                log_event runtime ~patch_id
                  (Printf.sprintf
                     "Patch changed files outside its declared scope: %s"
                     (Base.String.concat ~sep:", " paths));
                Error "changed files fall outside the patch scope"
            | Patch_validator.Scope_read_failed failure ->
                log_event runtime ~patch_id
                  ("Could not verify patch scope: "
                  ^ command_failure_text failure);
                Error "could not read the patch diff"
            | Patch_validator.Check_failed (check, failure) ->
                log_event runtime ~patch_id
                  (Printf.sprintf "Required check failed: %s\n%s"
                     check.Check.run
                     (command_failure_text failure));
                Error
                  (Printf.sprintf "required check failed: %s" check.Check.run))
        | Error (Patch_validator.Git_failed failure) ->
            log_event runtime ~patch_id
              ("Controller could not record worker changes: "
              ^ command_failure_text failure);
            Error "controller could not record worker changes")
    | None ->
        log_event runtime ~patch_id
          "Publication rejected: patch has no declared contract";
        Error "patch has no declared contract"

  let read_optional_file path =
    try
      if Stdlib.Sys.file_exists path then
        let ic = Stdlib.In_channel.open_text path in
        Stdlib.Fun.protect
          ~finally:(fun () -> Stdlib.In_channel.close ic)
          (fun () -> Some (Stdlib.In_channel.input_all ic))
      else None
    with _ -> None

  (** Read an artifact file. Returns [Some contents] if the file exists and is
      readable, [None] otherwise. *)
  let read_artifact_file = read_optional_file

  (** Apply the agent-authored notes artifact to the PR. Composes the final body
      as: gameplan description + specs + Implementation Notes (from artifact).
      Returns a tag describing the outcome so the caller can correlate with
      session-level signals (e.g. a Missing artifact alongside a failed Write
      tool call indicates the agent was blocked mid-call, not that it chose to
      skip notes). *)
  let apply_pr_body_artifact ~runtime ~project_name ~patch_id ~pr_number ~patch
      : [ `Ok | `Missing | `Empty | `Patch_failed ] =
    let artifact_path =
      Project_store.pr_body_artifact_path ~project_name ~patch_id
    in
    match read_artifact_file artifact_path with
    | None ->
        log_event runtime ~patch_id
          (Printf.sprintf
             "pr-body: artifact missing at %s; keeping initial PR body"
             artifact_path);
        `Missing
    | Some notes when String.length (String.trim notes) = 0 ->
        log_event runtime ~patch_id
          "pr-body: artifact empty; keeping initial PR body";
        `Empty
    | Some notes -> (
        let description = Prompt.render_pr_description ~project_name patch in
        let check_suffix = Prompt.render_check_suffix patch in
        let body =
          Printf.sprintf "%s%s\n\n## Implementation Notes\n\n%s" description
            check_suffix (String.trim notes)
        in
        match Forge.update_pr_body ~pr_number ~body with
        | Ok () ->
            log_event runtime ~patch_id
              (Printf.sprintf "pr-body: PATCHed PR #%d"
                 (Pr_number.to_int pr_number));
            `Ok
        | Error e ->
            log_event runtime ~patch_id
              (Printf.sprintf "pr-body: PATCH failed — %s" (Forge.show_error e));
            `Patch_failed)

  (** Terminal failure — forces [Given_up] so [complete] raises intervention.
      Used for non-retryable errors (patch not found, PR discovery failed).

      Routes through the pure [Orchestrator.apply_force_complete] decision so
      any inflight human messages are restored to the inbox rather than silently
      dropped, and emits a [log_force_complete] audit event so a dispatched
      [Respond] action always has a matching close in the JSONL log. *)
  let mark_session_failed event_log runtime patch_id =
    let snapshot = ref None in
    Runtime.update_orchestrator runtime (fun orch ->
        match Orchestrator.find_agent orch patch_id with
        | None -> orch
        | Some before ->
            let orch' =
              Orchestrator.apply_force_complete orch patch_id
                Orchestrator.Unexpected_exception
            in
            let after = Orchestrator.agent orch' patch_id in
            snapshot := Some (before, after);
            orch');
    Base.Option.iter !snapshot ~f:(fun (before, after) ->
        (* Only emit a force-complete audit event if the agent was actually
         busy. The "patch not found in gameplan" call site at the Start
         dispatch fires this before [with_busy_guard] runs, so no Respond
         was ever dispatched — logging a force-complete close in that case
         would falsely claim a Respond/close pair. *)
        if Patch_agent.is_busy before then
          Event_log.log_force_complete event_log ~patch_id
            ~reason:Orchestrator.Unexpected_exception ~agent_before:before
            ~agent_after:after)

  (** {1 Fibers} *)

  let poll_review_backends ~runtime ~patch_id ~findings_registry ~review_clients
      ~owner ~repo ~pr_number : Review_service.finding list =
    Base.List.concat_map review_clients
      ~f:(fun
          (module R : Review_service_client.S
            with type error = Review_service_client.error)
        ->
        match R.list_findings ~owner ~repo ~pr_number () with
        | Error err ->
            log_event runtime ~patch_id
              (Printf.sprintf "Poll error — %s" (R.show_error err));
            []
        | Ok response ->
            let parsed_count =
              Base.List.length response.Review_service.findings
            in
            if not (Int.equal parsed_count response.Review_service.count) then
              log_event runtime ~patch_id
                (Printf.sprintf
                   "Review backend %s declared %d finding(s) but parsed %d — \
                    possible review-service schema drift"
                   R.name response.Review_service.count parsed_count);
            Base.List.iter response.Review_service.dropped_findings
              ~f:(fun (e : Review_service.finding_parse_error) ->
                let json =
                  if String.length e.Review_service.json <= 500 then
                    e.Review_service.json
                  else String.sub e.Review_service.json 0 497 ^ "..."
                in
                log_event runtime ~patch_id
                  (Printf.sprintf
                     "Review backend %s dropped finding at index %d while \
                      parsing: %s; json=%s"
                     R.name e.Review_service.index e.Review_service.error json));
            let keyed_findings =
              Base.List.map response.Review_service.findings
                ~f:(fun (f : Review_service.finding) ->
                  let key =
                    Findings_registry.make_key ~backend_name:R.name ~owner ~repo
                      ~pr_number ~finding_id:f.Review_service.id
                  in
                  (key, f))
            in
            Findings_registry.remove_stale_for_scope findings_registry
              ~backend_name:R.name ~owner ~repo ~pr_number
              ~keep_keys:(Base.List.map keyed_findings ~f:fst);
            Base.List.map keyed_findings ~f:(fun (key, f) ->
                Findings_registry.register findings_registry ~key
                  {
                    Findings_registry.backend_name = R.name;
                    owner;
                    repo;
                    pr_number;
                    finding_id = f.Review_service.id;
                  };
                { f with Review_service.id = key }))

  (** Translate a [Github] result into a [Poll_outcome.t]. Pure-modulo-show; the
      boundary lets [Poll_cycle.classify] reason about timeouts the same way it
      reasons about other failures, which is the invariant the interleaving
      property tests exercise. *)

  let run ?status_msg () =
    let runtime = Env.runtime in
    let clock = Env.clock in
    let project_name = Env.project_name in
    let findings_registry = Env.findings_registry in
    let review_clients = Env.review_clients in
    let event_log = Env.event_log in
    let main = Env.main_branch in
    let set_status ~level ~text ?expires_at () =
      match status_msg with
      | Some r -> r := Some { Tui.level; text; expires_at }
      | None -> ()
    in
    let semaphore = Eio.Semaphore.make Env.max_concurrency in
    let with_session_slot f =
      Eio.Semaphore.acquire semaphore;
      Fun.protect ~finally:(fun () -> Eio.Semaphore.release semaphore) f
    in
    (* Serializes [git fetch origin] across worktrees + the poller's
       main-freshness fetch to avoid ref-lock races on the shared
       [refs/remotes/origin/*] store. Shared via [Env.fetch_mutex] so a
       single mutex covers every fiber. See [Worktree.fetch_origin]. *)
    let fetch_mutex = Env.fetch_mutex in
    let long_lived_sessions = Long_lived_sessions.create () in
    let patch_agent_provider =
      Base.Option.value Env.patch_agent_provider ~default:"anthropic"
    in
    let patch_agent_effort =
      Base.Option.value Env.patch_agent_effort ~default:"medium"
    in
    let run_llm_session ~sw ~gameplan_prompt ~patch_prompt ~kind ~patch_id
        ~prompt ~agent ~on_pr_detected =
      let validate_before_push ~worktree ~base_branch =
        prepare_patch_contract ~runtime ~patch_id ~worktree ~base_branch
      in
      let sandbox_for_worktree ~worktree =
        let gameplan =
          Runtime.read runtime (fun snapshot -> snapshot.Runtime.gameplan)
        in
        match
          Base.List.find gameplan.Gameplan.patches ~f:(fun (patch : Patch.t) ->
              Patch_id.equal patch.id patch_id)
        with
        | None -> Error "worker has no declared patch contract"
        | Some patch ->
            let provider =
              match Env.backend with
              | Backend_registry.Ephemeral _ -> (
                  let backend = Env.backend_decision.Backend_routing.backend in
                  match
                    ( Base.String.lowercase backend,
                      Env.backend_decision.Backend_routing.model )
                  with
                  | ("opencode" | "pi"), Some model ->
                      Base.String.lsplit2 (Base.String.lowercase model) ~on:'/'
                      |> Base.Option.map ~f:fst
                      |> Base.Option.value ~default:backend
                  | _ -> backend)
              | Backend_registry.Long_lived _ -> patch_agent_provider
            in
            Worker_sandbox.create
              ~backend:Env.backend_decision.Backend_routing.backend ~provider
              ~project_name ~worktree ~patch ~gameplan ~operation:kind
      in
      match Env.backend with
      | Backend_registry.Ephemeral backend ->
          Session_driver.run ~sandbox_for_worktree ~kind ~patch_id ~prompt
            ~agent ~on_pr_detected ~validate_before_push ~backend
      | Backend_registry.Long_lived backend -> (
          let patch_agent_model_result =
            match Env.backend_decision.Backend_routing.model with
            | Some m when not (Base.String.is_empty (Base.String.strip m)) ->
                Ok (Base.String.strip m)
            | Some _ | None ->
                Error
                  "patch-agent backend requires a concrete non-empty model \
                   after routing"
          in
          match patch_agent_model_result with
          | Error message ->
              log_event runtime ~patch_id message;
              (`Failed, [])
          | Ok patch_agent_model ->
              let session =
                match Long_lived_sessions.find long_lived_sessions patch_id with
                | Some session ->
                    Session_driver.update_long_lived_session_prompts session
                      ~gameplan_prompt ~patch_prompt;
                    session
                | None ->
                    let session =
                      Session_driver.create_long_lived_session ~backend
                        ~provider:patch_agent_provider ~model:patch_agent_model
                        ~effort:patch_agent_effort ~gameplan_prompt
                        ~patch_prompt
                    in
                    Long_lived_sessions.register long_lived_sessions patch_id
                      session;
                    session
              in
              Session_driver.run_long_lived ~sw ~sandbox_for_worktree ~kind
                ~patch_id ~prompt ~agent ~on_pr_detected ~validate_before_push
                ~session)
    in
    let shutdown_finished_long_lived_sessions ~sw () =
      let finished =
        Runtime.read runtime (fun snap ->
            Base.List.fold (Long_lived_sessions.to_alist long_lived_sessions)
              ~init:[] ~f:(fun acc (patch_id, session) ->
                if Session_driver.long_lived_session_failed session then
                  (patch_id, session) :: acc
                else
                  match
                    Orchestrator.find_agent snap.Runtime.orchestrator patch_id
                  with
                  | None -> (patch_id, session) :: acc
                  | Some agent
                    when agent.Patch_agent.merged
                         && not (Patch_agent.is_busy agent) ->
                      (patch_id, session) :: acc
                  | Some _ -> acc))
      in
      Base.List.iter finished ~f:(fun (patch_id, session) ->
          Long_lived_sessions.remove long_lived_sessions patch_id;
          Eio.Fiber.fork_daemon ~sw (fun () ->
              (try Session_driver.shutdown_long_lived_session session with
              | Eio.Cancel.Cancelled _ -> ()
              | exn ->
                  log_event runtime ~patch_id
                    (Printf.sprintf "long-lived backend shutdown error — %s"
                       (Printexc.to_string exn)));
              `Stop_daemon))
    in
    let rec loop sw =
      shutdown_finished_long_lived_sessions ~sw ();
      let gameplan = Runtime.read runtime (fun snap -> snap.Runtime.gameplan) in
      let messages, pre_fire_agents =
        durable_commit runtime (fun orch ->
            let orch, messages =
              Patch_controller.plan_tick_messages orch ~project_name ~gameplan
            in
            let orch, _automerge_commands =
              Patch_controller.reconcile_automerge orch
                ~now:(Unix.gettimeofday ())
            in
            let orch =
              match Env.review_team with
              | None -> orch
              | Some team_slug ->
                  fst
                    (Patch_controller.reconcile_review_requests orch ~team_slug)
            in
            let pre_fire_agents =
              Base.List.filter_map messages
                ~f:(fun (msg : Orchestrator.patch_agent_message) ->
                  match
                    ( Orchestrator.message_status msg,
                      Orchestrator.message_action msg )
                  with
                  | ( Orchestrator.Pending,
                      ( Orchestrator.Respond (pid, _)
                      | Orchestrator.Rebase (pid, _) ) ) ->
                      Some (pid, Orchestrator.agent orch pid)
                  | Orchestrator.Pending, Orchestrator.Start _
                  | Orchestrator.Acked, _
                  | Orchestrator.Completed, _
                  | Orchestrator.Obsolete, _ ->
                      None)
            in
            let orch, dispatched =
              Base.List.fold messages ~init:(orch, [])
                ~f:(fun
                    (acc, dispatched)
                    (msg : Orchestrator.patch_agent_message)
                  ->
                  match Orchestrator.message_status msg with
                  | Orchestrator.Pending ->
                      let acc, action =
                        Orchestrator.accept_message acc
                          (Orchestrator.message_id msg)
                      in
                      let dispatched =
                        match action with
                        | Some _ -> msg :: dispatched
                        | None -> dispatched
                      in
                      (acc, dispatched)
                  | Orchestrator.Acked ->
                      let acc, action =
                        Orchestrator.resume_message acc
                          (Orchestrator.message_id msg)
                      in
                      let dispatched =
                        match action with
                        | Some _ -> msg :: dispatched
                        | None -> dispatched
                      in
                      (acc, dispatched)
                  | Orchestrator.Completed | Orchestrator.Obsolete ->
                      (acc, dispatched))
            in
            (orch, (List.rev dispatched, pre_fire_agents)))
      in
      execute_github_outbox ~runtime;
      (* Log dispatched actions to event log *)
      Base.List.iter messages
        ~f:(fun (msg : Orchestrator.patch_agent_message) ->
          let action = Orchestrator.message_action msg in
          let agent_before =
            match
              Base.List.Assoc.find pre_fire_agents ~equal:Patch_id.equal
                (Orchestrator.message_patch_id msg)
            with
            | Some a -> a
            | None ->
                (* Start actions create the agent during fire, so there is no
                 pre-fire snapshot in [pre_fire_agents]. In that case we log
                 the post-create/default agent state as [agent_before]. *)
                Runtime.read runtime (fun snap ->
                    Orchestrator.agent snap.Runtime.orchestrator
                      (Orchestrator.message_patch_id msg))
          in
          Event_log.log_action event_log ~action ~agent_before);
      (* Spawn all actions concurrently, limited by max_concurrency semaphore *)
      let action_fibers =
        Base.List.filter_map messages
          ~f:(fun (msg : Orchestrator.patch_agent_message) ->
            match Orchestrator.message_action msg with
            | Orchestrator.Start (patch_id, base_branch) -> (
                match
                  Base.List.find gameplan.Gameplan.patches
                    ~f:(fun (p : Patch.t) -> Patch_id.equal p.Patch.id patch_id)
                with
                | None ->
                    log_event runtime ~patch_id
                      "Skipping start — patch not found in gameplan";
                    mark_session_failed event_log runtime patch_id;
                    None
                | Some patch ->
                    Some
                      (fun () ->
                        With_busy_guard.run ~patch_id (fun () ->
                            let result =
                              with_session_slot (fun () ->
                                  let agent =
                                    Runtime.read runtime (fun snap ->
                                        Orchestrator.agent
                                          snap.Runtime.orchestrator patch_id)
                                  in
                                  if
                                    agent.Patch_agent.merged
                                    || Patch_agent.needs_intervention agent
                                    || agent.Patch_agent.branch_blocked
                                    || not (Patch_agent.is_busy agent)
                                  then (
                                    log_event runtime ~patch_id
                                      "Skipping action — became stale during \
                                       semaphore wait";
                                    `Stale)
                                  else (
                                    Runtime.update_orchestrator runtime
                                      (fun orch ->
                                        Orchestrator.mark_running orch patch_id);
                                    match
                                      WS.ensure_worktree ~patch_id ~agent
                                        ~branch:patch.Patch.branch
                                        ~base_ref:(Branch.to_string base_branch)
                                        ()
                                    with
                                    | Worktree_setup.Missing ->
                                        Runtime.update_orchestrator runtime
                                          (fun orch ->
                                            Orchestrator.apply_session_result
                                              orch patch_id
                                              Orchestrator
                                              .Session_worktree_missing);
                                        `Failed
                                    | Worktree_setup.Refused -> `Failed
                                    | Worktree_setup.Path _wt_path ->
                                        (* Capture the initial anchor for this
                                           Start: resolve origin/<base_branch>'s
                                           current tip so the first rebase has
                                           a usable [<upstream>] for [git rebase
                                           --onto]. Closes the production-bug
                                           blind spot where a Start-then-dep-
                                           squash-merge sequence left the agent
                                           with no anchor at first-rebase time
                                           and forced the legacy 2-arg fallback
                                           into a "both added" conflict. *)
                                        let ancestor_ids =
                                          Runtime.read runtime (fun snap ->
                                              Graph.transitive_ancestors
                                                (Orchestrator.graph
                                                   snap.Runtime.orchestrator)
                                                patch_id)
                                        in
                                        let _, _, start_anchor_events =
                                          Worktree_plan_executor.execute
                                            ~patch_id ~agent
                                            ~fetch_lock:fetch_mutex
                                            ~fail_label:"start anchor capture"
                                            ~ancestor_ids
                                            (Worktree_plan.for_start
                                               ~base:base_branch)
                                        in
                                        (match start_anchor_events with
                                        | [] -> ()
                                        | _ ->
                                            Runtime.update_orchestrator runtime
                                              (fun orch ->
                                                Orchestrator.apply_anchor_events
                                                  orch patch_id
                                                  start_anchor_events));
                                        let agents_md =
                                          read_optional_file
                                            (Stdlib.Filename.concat _wt_path
                                               "AGENTS.md")
                                        in
                                        let prompt =
                                          Prompt.render_patch_prompt
                                            ~project_name ?agents_md
                                            ?pr_number:
                                              (Patch_agent.pr_number agent)
                                            patch gameplan
                                            ~base_branch:
                                              (Branch.to_string base_branch)
                                        in
                                        (* PR detection from stream text is a hint
                                     only — always confirmed via the GitHub
                                     REST API after the
                                     backend session finishes *)
                                        let on_pr_detected _pr_number = () in
                                        let gameplan_prompt =
                                          Prompt.render_gameplan_layer
                                            ~project_name gameplan
                                        in
                                        let patch_prompt =
                                          Prompt.render_patch_layer_of_gameplan
                                            ~project_name
                                            ?pr_number:
                                              (Patch_agent.pr_number agent)
                                            patch gameplan
                                            ~base_branch:
                                              (Branch.to_string base_branch)
                                        in
                                        let r, _tool_failures =
                                          run_llm_session ~sw ~gameplan_prompt
                                            ~patch_prompt ~kind:None ~patch_id
                                            ~prompt ~agent ~on_pr_detected
                                        in
                                        (r
                                          :> [ `Failed
                                             | `Ok
                                             | `No_commits
                                             | `Retry_push
                                             | `Stale ])))
                            in
                            let start_outcome =
                              match result with
                              | `Stale -> Orchestrator.Start_stale
                              | `Failed | `Retry_push | `No_commits ->
                                  Orchestrator.Start_failed
                              | `Ok -> Orchestrator.Start_ok
                            in
                            Runtime.update_orchestrator runtime (fun orch ->
                                Orchestrator.apply_start_outcome orch patch_id
                                  start_outcome);
                            match start_outcome with
                            | Orchestrator.Start_failed ->
                                let agent =
                                  Runtime.read runtime (fun snap ->
                                      Orchestrator.agent
                                        snap.Runtime.orchestrator patch_id)
                                in
                                if Patch_agent.needs_intervention agent then
                                  set_status ~level:Tui.Error
                                    ~text:
                                      (Printf.sprintf
                                         "Patch %s: session failed — human \
                                          review needed"
                                         (Patch_id.to_string patch_id))
                                    ()
                            | Orchestrator.Start_ok ->
                                (* Supervisor-owned PR creation: the agent
                                 commits and the supervisor pushed at session
                                 end; now we open the draft PR with a
                                 gameplan-derived title and body.

                                 Re-derive the base branch from current
                                 orchestrator state — dependencies may have
                                 merged while the agent session was running,
                                 making the base captured at dispatch time
                                 stale. *)
                                let fresh_base =
                                  Runtime.read runtime (fun snap ->
                                      let orch = snap.Runtime.orchestrator in
                                      let has_merged pid =
                                        (Orchestrator.agent orch pid)
                                          .Patch_agent.merged
                                      in
                                      let branch_of pid =
                                        match
                                          Base.List.find
                                            gameplan.Gameplan.patches
                                            ~f:(fun (p : Patch.t) ->
                                              Patch_id.equal p.Patch.id pid)
                                        with
                                        | Some p -> p.Patch.branch
                                        | None -> Orchestrator.main_branch orch
                                      in
                                      Graph.initial_base
                                        (Orchestrator.graph orch) patch_id
                                        ~has_merged ~branch_of
                                        ~main:(Orchestrator.main_branch orch))
                                in
                                let pr_title = Patch_validator.pr_title patch in
                                let pr_body =
                                  Prompt.render_pr_description ~project_name
                                    patch
                                  ^ Prompt.render_check_suffix patch
                                in
                                (match
                                   Forge.create_pull_request ~title:pr_title
                                     ~head:patch.Patch.branch ~base:fresh_base
                                     ~body:pr_body ~draft:true
                                 with
                                | Ok pr_number ->
                                    log_event runtime ~patch_id
                                      (Printf.sprintf "PR #%d created"
                                         (Pr_number.to_int pr_number));
                                    Env.register_pr ~patch_id ~pr_number;
                                    Runtime.update_orchestrator runtime
                                      (fun orch ->
                                        Orchestrator.set_pr_number orch patch_id
                                          pr_number)
                                | Error e -> (
                                    match e with
                                    | Github.Http_error
                                        { status = 422; body; _ }
                                      when Github
                                           .response_error_message_contains body
                                             ~substring:
                                               "pull request already exists"
                                      -> (
                                        (* PR already exists — discover it rather
                                         than treating this as failure. We
                                         only fall back on this specific 422;
                                         other 422s (no commits, head missing,
                                         etc.) propagate with the original
                                         error message. *)
                                        match
                                          Forge.list_prs
                                            ~branch:patch.Patch.branch
                                            ~state:`Open ()
                                        with
                                        | Ok ((pr_number, _, _) :: _) ->
                                            log_event runtime ~patch_id
                                              (Printf.sprintf
                                                 "PR #%d already existed, \
                                                  associated"
                                                 (Pr_number.to_int pr_number));
                                            Env.register_pr ~patch_id ~pr_number;
                                            Runtime.update_orchestrator runtime
                                              (fun orch ->
                                                Orchestrator.set_pr_number orch
                                                  patch_id pr_number)
                                        | Ok [] ->
                                            log_event runtime ~patch_id
                                              "PR creation failed (422 \
                                               already-exists) and discovery \
                                               found no open PRs";
                                            Runtime.update_orchestrator runtime
                                              (fun orch ->
                                                Orchestrator
                                                .on_pr_discovery_failure orch
                                                  patch_id)
                                        | Error disc_err ->
                                            log_event runtime ~patch_id
                                              (Printf.sprintf
                                                 "PR creation failed (422 \
                                                  already-exists) and \
                                                  discovery also failed — %s"
                                                 (Forge.show_error disc_err));
                                            Runtime.update_orchestrator runtime
                                              (fun orch ->
                                                Orchestrator
                                                .on_pr_discovery_failure orch
                                                  patch_id))
                                    | Github.Http_error _
                                    | Github.Json_parse_error _
                                    | Github.Graphql_error _ | Github.Timeout _
                                    | Github.Transport_error _ ->
                                        log_event runtime ~patch_id
                                          (Printf.sprintf
                                             "PR creation failed — %s"
                                             (Forge.show_error e));
                                        Runtime.update_orchestrator runtime
                                          (fun orch ->
                                            Orchestrator.on_pr_discovery_failure
                                              orch patch_id)));
                                Runtime.update_orchestrator runtime (fun orch ->
                                    Orchestrator.complete orch patch_id)
                            | Orchestrator.Start_stale -> ())))
            | Orchestrator.Rebase (patch_id, new_base) ->
                Some
                  (fun () ->
                    With_busy_guard.run ~patch_id (fun () ->
                        (* Rebase is orchestrator-executed (no session slot), so
                         work begins immediately under the busy guard. *)
                        Runtime.update_orchestrator runtime (fun orch ->
                            Orchestrator.mark_running orch patch_id);
                        let agent =
                          Runtime.read runtime (fun snap ->
                              Orchestrator.agent snap.Runtime.orchestrator
                                patch_id)
                        in
                        let ancestor_ids =
                          Runtime.read runtime (fun snap ->
                              Graph.transitive_ancestors
                                (Orchestrator.graph snap.Runtime.orchestrator)
                                patch_id)
                        in
                        let rebase_branch_str =
                          Types.Branch.to_string agent.Patch_agent.branch
                        in
                        let predicted_wt_path =
                          WS.resolve_worktree_path ~patch_id ~agent ()
                        in
                        let pre_rebase_head =
                          W.read_branch_sha ~path:predicted_wt_path
                            ~ref_name:("refs/heads/" ^ rebase_branch_str)
                        in
                        let rebase_result, wt_path, anchor_events =
                          Worktree_plan_executor.execute ~patch_id ~agent
                            ~fetch_lock:fetch_mutex ~fail_label:"rebase"
                            ~ancestor_ids
                            (Worktree_plan.for_rebase ~new_base)
                        in
                        let pre_rebase_head =
                          if Base.String.equal predicted_wt_path wt_path then
                            pre_rebase_head
                          else None
                        in
                        let post_rebase_head =
                          W.read_branch_sha ~path:wt_path
                            ~ref_name:("refs/heads/" ^ rebase_branch_str)
                        in
                        let rebase_target_base_sha =
                          W.read_branch_sha ~path:wt_path
                            ~ref_name:("refs/heads/" ^ Branch.to_string new_base)
                        in
                        (match rebase_result with
                        | Worktree.Ok ->
                            log_event runtime ~patch_id
                              (Printf.sprintf "Rebased onto %s"
                                 (Branch.to_string new_base))
                        | Worktree.Noop ->
                            log_event runtime ~patch_id
                              "Rebase noop — already up-to-date"
                        | Worktree.Conflict _ ->
                            log_event runtime ~patch_id
                              "Rebase conflict — enqueued merge-conflict"
                        | Worktree.Error msg ->
                            log_event runtime ~patch_id
                              (Printf.sprintf "Rebase failed — %s" msg));
                        let agent_before, (agent_after, effects) =
                          Runtime.update_orchestrator_returning runtime
                            (fun orch ->
                              let agent_before =
                                Orchestrator.agent orch patch_id
                              in
                              let orch, effects =
                                Orchestrator.apply_rebase_with_anchor orch
                                  patch_id rebase_result new_base anchor_events
                              in
                              let agent_after =
                                Orchestrator.agent orch patch_id
                              in
                              (orch, (agent_before, (agent_after, effects))))
                        in
                        let push_record =
                          Base.List.find_map effects
                            ~f:(fun Orchestrator.Push_branch ->
                              let branch = agent.Patch_agent.branch in
                              let branch_str = Types.Branch.to_string branch in
                              let base_str = Types.Branch.to_string new_base in
                              let local_sha =
                                W.read_branch_sha ~path:wt_path
                                  ~ref_name:("refs/heads/" ^ branch_str)
                              in
                              let remote_tracking_sha =
                                W.read_branch_sha ~path:wt_path
                                  ~ref_name:("refs/remotes/origin/" ^ branch_str)
                              in
                              let base_sha =
                                W.read_branch_sha ~path:wt_path
                                  ~ref_name:("refs/heads/" ^ base_str)
                              in
                              match
                                prepare_patch_contract ~runtime ~patch_id
                                  ~worktree:wt_path ~base_branch:new_base
                              with
                              | Error _ -> Some `Rejected
                              | Ok () ->
                                  let result =
                                    W.force_push_with_lease ~path:wt_path
                                      ~branch ~base:new_base
                                  in
                                  (match result with
                                  | Worktree.Push_ok ->
                                      log_event runtime ~patch_id
                                        "Force-pushed after rebase"
                                  | Worktree.Push_up_to_date ->
                                      log_event runtime ~patch_id
                                        "Push noop after rebase — already \
                                         up-to-date"
                                  | Worktree.Push_no_commits ->
                                      log_event runtime ~patch_id
                                        "Force-push skipped after rebase — \
                                         branch has no commits ahead of base"
                                  | Worktree.Push_rejected reason ->
                                      log_event runtime ~patch_id
                                        (Printf.sprintf
                                           "Force-push rejected — %s"
                                           (Push_reject_classify.short_label
                                              reason))
                                  | Worktree.Push_worktree_missing ->
                                      log_event runtime ~patch_id
                                        (Printf.sprintf
                                           "Worktree disappeared (%s) — rebase \
                                            will reconstruct on retry"
                                           wt_path)
                                  | Worktree.Push_error msg ->
                                      log_event runtime ~patch_id
                                        (Printf.sprintf "Force-push failed — %s"
                                           msg));
                                  Some
                                    (`Pushed
                                       ( result,
                                         local_sha,
                                         remote_tracking_sha,
                                         base_sha )))
                        in
                        let push_outcome =
                          Base.Option.bind push_record ~f:(function
                            | `Rejected -> None
                            | `Pushed (result, _, _, _) -> Some result)
                        in
                        let resolution, push_agent_after =
                          Runtime.update_orchestrator_returning runtime
                            (fun orch ->
                              let orch, resolution =
                                match push_record with
                                | Some `Rejected ->
                                    Orchestrator.reject_rebase_publication orch
                                      patch_id
                                | Some (`Pushed _) | None ->
                                    Orchestrator.apply_rebase_push_result orch
                                      patch_id push_outcome
                              in
                              let push_agent_after =
                                Orchestrator.agent orch patch_id
                              in
                              (orch, (resolution, push_agent_after)))
                        in
                        (match push_record with
                        | Some
                            (`Pushed
                               (result, local_sha, remote_tracking_sha, base_sha))
                          ->
                            Event_log.log_push event_log ~patch_id
                              ~kind:Event_log.Rebase_resolution_push ~result
                              ~local_sha ~remote_tracking_sha ~base_sha
                              ~agent_before:agent_after
                              ~agent_after:push_agent_after
                        | Some `Rejected | None -> ());
                        (match resolution with
                        | Orchestrator.Rebase_push_ok -> ()
                        | Orchestrator.Rebase_push_failed ->
                            log_event runtime ~patch_id
                              "Enqueued merge-conflict after rebase push \
                               rejection"
                        | Orchestrator.Rebase_push_queue_locked ->
                            log_event runtime ~patch_id
                              "Rebase push rejected: PR is queued in the merge \
                               queue; dropping (queue will merge or eject)"
                        | Orchestrator.Rebase_push_error ->
                            log_event runtime ~patch_id
                              "Enqueued rebase retry after push error");
                        Event_log.log_rebase event_log ~patch_id
                          ~result:rebase_result ~pre_rebase_head
                          ~post_rebase_head
                          ~target_base_sha:rebase_target_base_sha ~agent_before
                          ~agent_after))
            | Orchestrator.Respond (patch_id, kind) ->
                (* Use pre-fire agent state for human_messages — fire/respond
                 clears them as a postcondition. *)
                let pre_fire_agent =
                  Base.List.Assoc.find pre_fire_agents patch_id
                    ~equal:Patch_id.equal
                in
                Some
                  (fun () ->
                    (* For Review_comments and Ci, fetch fresh state from
                     GitHub before acquiring a session slot to avoid blocking
                     concurrency on GitHub API I/O. The Ci fetch is the
                     freshness gate against delivering a failure that's
                     already been superseded by a newer run. *)
                    let is_review =
                      Operation_kind.equal kind Operation_kind.Review_comments
                    in
                    let is_ci = Operation_kind.equal kind Operation_kind.Ci in
                    let pr_number =
                      Runtime.read runtime (fun snap ->
                          Patch_agent.pr_number
                            (Orchestrator.agent snap.Runtime.orchestrator
                               patch_id))
                    in
                    let fresh_pr_state =
                      if is_review || is_ci then (
                        match pr_number with
                        | Some pr_num -> (
                            log_event runtime ~patch_id
                              (if is_ci then
                                 "Fetching fresh CI state from GitHub"
                               else "Fetching fresh review comments from GitHub");
                            match Forge.pr_state pr_num with
                            | Ok pr_state -> Some pr_state
                            | Error _err ->
                                log_event runtime ~patch_id
                                  (if is_ci then
                                     "Failed to fetch fresh CI state"
                                   else "Failed to fetch fresh review comments");
                                None)
                        | None ->
                            if is_ci then
                              log_event runtime ~patch_id
                                "No PR number yet — skipping CI state fetch";
                            None)
                      else None
                    in
                    let prefetched_comments =
                      if is_review then
                        match fresh_pr_state with
                        | Some pr_state -> pr_state.Pr_state.comments
                        | None -> []
                      else []
                    in
                    let prefetched_findings =
                      if Operation_kind.equal kind Operation_kind.Findings then
                        match pr_number with
                        | Some pr_num
                          when not (Base.List.is_empty review_clients) ->
                            log_event runtime ~patch_id
                              "Fetching fresh findings from review backends";
                            poll_review_backends ~runtime ~patch_id
                              ~findings_registry ~review_clients
                              ~owner:Env.owner ~repo:Env.repo
                              ~pr_number:(Pr_number.to_int pr_num)
                        | Some _ | None -> []
                      else []
                    in
                    (* Ci freshness gate: if we have no PR number, couldn't
                     fetch, or the fetched state shows no current failures,
                     skip the delivery — don't wake the agent for a failure
                     that's already been superseded. The [set_ci_checks]
                     write is deferred to inside [with_busy_guard] so it
                     can't corrupt state on a stale/cancelled delivery nor
                     race with the poller. *)
                    let ci_skip_reason =
                      if is_ci then
                        match (pr_number, fresh_pr_state) with
                        | None, _ -> Some "no PR number"
                        | Some _, None -> Some "fetch failed"
                        | Some _, Some pr_state ->
                            let agent_has_merge_queue_failure =
                              Runtime.read runtime (fun snap ->
                                  Orchestrator.agent snap.Runtime.orchestrator
                                    patch_id)
                              |> fun agent ->
                              Base.List.exists agent.Patch_agent.ci_checks
                                ~f:Ci_check.is_merge_queue_failure
                            in
                            if
                              Base.List.exists pr_state.Pr_state.ci_checks
                                ~f:Ci_check.is_failure
                              || agent_has_merge_queue_failure
                            then None
                            else (
                              log_event runtime ~patch_id
                                "Fresh CI state shows no failures — skipping \
                                 CI delivery";
                              Some "no current failures")
                      else None
                    in
                    With_busy_guard.run ~patch_id (fun () ->
                        let result =
                          match ci_skip_reason with
                          | Some reason ->
                              (* Fast path: skip decision already made. Don't
                               consume a session slot for a no-op — that
                               slot can go to another agent. *)
                              log_event runtime ~patch_id
                                (Printf.sprintf "Skipped ci delivery — %s"
                                   reason);
                              `Skip_empty
                          | None ->
                              with_session_slot (fun () ->
                                  let ci_merge_queue_removal_checks = ref [] in
                                  Runtime.update_orchestrator runtime
                                    (fun orch ->
                                      Orchestrator.mark_running orch patch_id);
                                  (* Write fresh ci_checks under the busy guard
                                   so the write can't race with the poller or
                                   land after a concurrent complete/merge.
                                   Must happen before the agent re-read so
                                   [agent.ci_checks] reflects the fresh
                                   list. *)
                                  (match (is_ci, fresh_pr_state) with
                                  | true, Some pr_state ->
                                      let synthetic_checks =
                                        Runtime.read runtime (fun snap ->
                                            Orchestrator.agent
                                              snap.Runtime.orchestrator patch_id)
                                        |> fun agent ->
                                        Base.List.filter
                                          agent.Patch_agent.ci_checks
                                          ~f:Ci_check.is_merge_queue_failure
                                      in
                                      (* The synthetic placeholder only tells the
                                         agent "merge queue failed". When it is
                                         present, fetch the real failing checks
                                         from the removal event's [beforeCommit]
                                         (the merge-group commit GitHub ran) and
                                         prefer them; fall back to the
                                         placeholder on empty/error. *)
                                      let merge_queue_checks =
                                        if Base.List.is_empty synthetic_checks
                                        then []
                                        else
                                          match pr_number with
                                          | Some pr_num -> (
                                              match
                                                Forge.merge_queue_removal_checks
                                                  ~pr_number:pr_num
                                              with
                                              | Ok (_ :: _ as real) ->
                                                  ci_merge_queue_removal_checks :=
                                                    real;
                                                  log_event runtime ~patch_id
                                                    (Printf.sprintf
                                                       "Fetched %d failing \
                                                        merge-queue check(s) \
                                                        from removal event"
                                                       (Base.List.length real));
                                                  (* Keep the synthetic marker
                                                     too: downstream freshness
                                                     and retry logic treats
                                                     [Ci_check.is_merge_queue_failure]
                                                     as the durable signal that
                                                     the PR was ejected from the
                                                     queue. *)
                                                  synthetic_checks @ real
                                              | Ok [] ->
                                                  log_event runtime ~patch_id
                                                    "No merge-queue removal \
                                                     checks available — using \
                                                     placeholder";
                                                  synthetic_checks
                                              | Error e ->
                                                  log_event runtime ~patch_id
                                                    (Printf.sprintf
                                                       "Failed to fetch \
                                                        merge-queue removal \
                                                        checks (%s) — using \
                                                        placeholder"
                                                       (Forge.show_error e));
                                                  synthetic_checks)
                                          | None -> synthetic_checks
                                      in
                                      let ci_checks =
                                        pr_state.Pr_state.ci_checks
                                        @ merge_queue_checks
                                      in
                                      Runtime.update_orchestrator runtime
                                        (fun orch ->
                                          Orchestrator.set_ci_checks orch
                                            patch_id ci_checks)
                                  | _ -> ());
                                  let agent =
                                    Runtime.read runtime (fun snap ->
                                        Orchestrator.agent
                                          snap.Runtime.orchestrator patch_id)
                                  in
                                  let delivery =
                                    Patch_decision.respond_delivery ~agent ~kind
                                      ~pre_fire_agent ~prefetched_comments
                                      ~prefetched_findings
                                      ~main_branch:(Branch.to_string main)
                                  in
                                  let render_base_changed_prefix base_change =
                                    match base_change with
                                    | Some bc ->
                                        log_event runtime ~patch_id
                                          (Printf.sprintf
                                             "Base branch changed from %s to \
                                              %s — notifying agent"
                                             bc.Patch_decision.old_base
                                             bc.Patch_decision.new_base);
                                        Prompt.render_base_branch_changed
                                          ~old_base:bc.Patch_decision.old_base
                                          ~new_base:bc.Patch_decision.new_base
                                    | None -> ""
                                  in
                                  match delivery with
                                  | Patch_decision.Respond_stale ->
                                      log_event runtime ~patch_id
                                        "Skipping action — became stale during \
                                         semaphore wait";
                                      `Stale
                                  | Patch_decision.Skip_empty ->
                                      log_event runtime ~patch_id
                                        (if
                                           Operation_kind.equal kind
                                             Operation_kind.Findings
                                         then
                                           "Skipped findings — pre-session \
                                            refresh returned no findings; no \
                                            resolution posted"
                                         else
                                           Printf.sprintf
                                             "Skipped %s — nothing to deliver"
                                             (Operation_kind.to_label kind));
                                      `Skip_empty
                                  | Patch_decision.Deliver
                                      {
                                        payload =
                                          Patch_decision.Merge_conflict_payload;
                                        base_change;
                                      } -> (
                                      let base =
                                        Base.Option.value_map
                                          agent.Patch_agent.base_branch
                                          ~default:(Branch.to_string main)
                                          ~f:Branch.to_string
                                      in
                                      let base_changed_prefix =
                                        render_base_changed_prefix base_change
                                      in
                                      let wt_path_opt =
                                        WS.ensure_worktree ~patch_id ~agent ()
                                      in
                                      let wt_path =
                                        match wt_path_opt with
                                        | Worktree_setup.Path p -> p
                                        | Worktree_setup.Missing
                                        | Worktree_setup.Refused ->
                                            Worktree.worktree_dir ~project_name
                                              ~patch_id
                                      in
                                      (* Helper: capture git context and deliver
                                   an enriched prompt to the agent. *)
                                      let deliver_to_agent ?conflict_info () =
                                        let pr_number =
                                          Patch_agent.pr_number agent
                                        in
                                        let rebase_still_in_progress =
                                          W.rebase_in_progress ~path:wt_path
                                        in
                                        let git_status =
                                          W.git_status ~path:wt_path
                                        in
                                        let git_diff =
                                          W.conflict_diff ~path:wt_path
                                        in
                                        Event_log.log_conflict_delivery
                                          event_log ~patch_id ~path:wt_path
                                          ~rebase_in_progress:
                                            rebase_still_in_progress ~git_status
                                          ~git_diff;
                                        let patch =
                                          Base.List.find
                                            gameplan.Gameplan.patches
                                            ~f:(fun (p : Patch.t) ->
                                              Patch_id.equal p.Patch.id patch_id)
                                        in
                                        let agents_md =
                                          read_optional_file
                                            (Stdlib.Filename.concat wt_path
                                               "AGENTS.md")
                                        in
                                        let prompt =
                                          let raw =
                                            Prompt.render_merge_conflict_prompt
                                              ~project_name ?agents_md
                                              ?pr_number ?patch ~gameplan
                                              ~base_branch:base ~git_status
                                              ~git_diff ?conflict_info ()
                                          in
                                          if String.equal base_changed_prefix ""
                                          then raw
                                          else base_changed_prefix ^ "\n" ^ raw
                                        in
                                        let on_pr_detected _pr_number = () in
                                        let gameplan_prompt =
                                          Prompt.render_gameplan_layer
                                            ~project_name gameplan
                                        in
                                        let patch_prompt =
                                          match patch with
                                          | Some p ->
                                              Prompt
                                              .render_patch_layer_of_gameplan
                                                ~project_name ?pr_number p
                                                gameplan ~base_branch:base
                                          | None -> ""
                                        in
                                        let result, _tool_failures =
                                          run_llm_session ~sw ~gameplan_prompt
                                            ~patch_prompt
                                            ~kind:
                                              (Some
                                                 Operation_kind.Merge_conflict)
                                            ~patch_id ~prompt ~agent
                                            ~on_pr_detected
                                        in
                                        (match result with
                                        | `Ok
                                          when not
                                                 (String.equal
                                                    base_changed_prefix "") ->
                                            Runtime.update_orchestrator runtime
                                              (fun orch ->
                                                Orchestrator
                                                .set_notified_base_branch orch
                                                  patch_id
                                                  (Branch.of_string base))
                                        | _ -> ());
                                        (result
                                          :> [ `Failed
                                             | `Ok
                                             | `No_commits
                                             | `Pr_body_miss
                                             | `Retry_push
                                             | `Review_unresolved
                                             | `Skip_empty
                                             | `Stale ])
                                      in
                                      let ancestor_ids =
                                        Runtime.read runtime (fun snap ->
                                            Graph.transitive_ancestors
                                              (Orchestrator.graph
                                                 snap.Runtime.orchestrator)
                                              patch_id)
                                      in
                                      if W.rebase_in_progress ~path:wt_path then (
                                        log_event runtime ~patch_id
                                          "Delivering merge-conflict — rebase \
                                           already in progress";
                                        (* Match the fresh-rebase path: rebase
                                         target is [origin/<base>], not the
                                         (possibly stale) local tracking ref.
                                         See Worktree_plan.for_merge_conflict. *)
                                        let conflict_info =
                                          W.read_in_progress_conflict_info
                                            ~path:wt_path
                                            ~target:
                                              (Types.Branch.of_string
                                                 ("origin/" ^ base))
                                            ~project_name ~ancestor_ids
                                        in
                                        deliver_to_agent ?conflict_info ())
                                      else
                                        (* Plan-driven: the planner guarantees
                                     Ensure_worktree precedes Fetch_origin
                                     and Rebase_onto. The executor short-
                                     circuits on the first failure. Plans
                                     target origin/<base> so we rebase
                                     against fresh refs, not the stale
                                     local tracking ref. *)
                                        let conflict_branch_str =
                                          Types.Branch.to_string
                                            agent.Patch_agent.branch
                                        in
                                        let predicted_wt_path =
                                          WS.resolve_worktree_path ~patch_id
                                            ~agent ()
                                        in
                                        let cr_pre_rebase_head =
                                          W.read_branch_sha
                                            ~path:predicted_wt_path
                                            ~ref_name:
                                              ("refs/heads/"
                                             ^ conflict_branch_str)
                                        in
                                        let ( rebase_result,
                                              executor_wt_path,
                                              anchor_events ) =
                                          Worktree_plan_executor.execute
                                            ~patch_id ~agent
                                            ~fetch_lock:fetch_mutex
                                            ~fail_label:"merge-conflict rebase"
                                            ~ancestor_ids
                                            (Worktree_plan.for_merge_conflict
                                               ~base:
                                                 (Types.Branch.of_string base))
                                        in
                                        let cr_post_rebase_head =
                                          W.read_branch_sha
                                            ~path:executor_wt_path
                                            ~ref_name:
                                              ("refs/heads/"
                                             ^ conflict_branch_str)
                                        in
                                        let cr_pre_rebase_head =
                                          if
                                            Base.String.equal predicted_wt_path
                                              executor_wt_path
                                          then cr_pre_rebase_head
                                          else None
                                        in
                                        let cr_target_base_sha =
                                          W.read_branch_sha
                                            ~path:executor_wt_path
                                            ~ref_name:
                                              ("refs/remotes/origin/" ^ base)
                                        in
                                        let conflict_info =
                                          match rebase_result with
                                          | Worktree.Conflict ci -> Some ci
                                          | Worktree.Ok | Worktree.Noop
                                          | Worktree.Error _ ->
                                              None
                                        in
                                        (match rebase_result with
                                        | Worktree.Ok ->
                                            log_event runtime ~patch_id
                                              (Printf.sprintf
                                                 "Conflict rebase onto %s \
                                                  succeeded"
                                                 base)
                                        | Worktree.Noop ->
                                            log_event runtime ~patch_id
                                              "Conflict rebase noop — local \
                                               already up-to-date, will push"
                                        | Worktree.Conflict _ ->
                                            log_event runtime ~patch_id
                                              "Conflict rebase hit conflicts — \
                                               delivering to agent"
                                        | Worktree.Error msg ->
                                            log_event runtime ~patch_id
                                              (Printf.sprintf
                                                 "Conflict rebase failed — %s"
                                                 msg));
                                        let ( decision,
                                              agent_before,
                                              agent_after,
                                              effects ) =
                                          Runtime.update_orchestrator_returning
                                            runtime (fun orch ->
                                              let agent_before =
                                                Orchestrator.agent orch patch_id
                                              in
                                              let orch, decision, effects =
                                                Orchestrator
                                                .apply_conflict_rebase_with_anchor
                                                  orch patch_id rebase_result
                                                  (Types.Branch.of_string base)
                                                  anchor_events
                                              in
                                              let agent_after =
                                                Orchestrator.agent orch patch_id
                                              in
                                              ( orch,
                                                ( decision,
                                                  agent_before,
                                                  agent_after,
                                                  effects ) ))
                                        in
                                        Event_log.log_conflict_rebase event_log
                                          ~patch_id ~result:rebase_result
                                          ~decision
                                          ~pre_rebase_head:cr_pre_rebase_head
                                          ~post_rebase_head:cr_post_rebase_head
                                          ~target_base_sha:cr_target_base_sha
                                          ~agent_before ~agent_after;
                                        let push_record =
                                          Base.List.find_map effects
                                            ~f:(fun Orchestrator.Push_branch ->
                                              let branch =
                                                agent.Patch_agent.branch
                                              in
                                              let branch_str =
                                                Types.Branch.to_string branch
                                              in
                                              let local_sha =
                                                W.read_branch_sha ~path:wt_path
                                                  ~ref_name:
                                                    ("refs/heads/" ^ branch_str)
                                              in
                                              let remote_tracking_sha =
                                                W.read_branch_sha ~path:wt_path
                                                  ~ref_name:
                                                    ("refs/remotes/origin/"
                                                   ^ branch_str)
                                              in
                                              let base_sha =
                                                W.read_branch_sha ~path:wt_path
                                                  ~ref_name:
                                                    ("refs/heads/" ^ base)
                                              in
                                              let base_branch =
                                                Types.Branch.of_string base
                                              in
                                              match
                                                prepare_patch_contract ~runtime
                                                  ~patch_id ~worktree:wt_path
                                                  ~base_branch
                                              with
                                              | Error _ -> Some `Rejected
                                              | Ok () ->
                                                  let result =
                                                    W.force_push_with_lease
                                                      ~path:wt_path ~branch
                                                      ~base:base_branch
                                                  in
                                                  (match result with
                                                  | Worktree.Push_ok ->
                                                      log_event runtime
                                                        ~patch_id
                                                        "Force-pushed to \
                                                         resolve conflict"
                                                  | Worktree.Push_up_to_date ->
                                                      log_event runtime
                                                        ~patch_id
                                                        "Conflict push noop — \
                                                         already up-to-date"
                                                  | Worktree.Push_no_commits ->
                                                      log_event runtime
                                                        ~patch_id
                                                        "Conflict force-push \
                                                         skipped — branch has \
                                                         no commits ahead of \
                                                         base"
                                                  | Worktree.Push_rejected
                                                      reason ->
                                                      log_event runtime
                                                        ~patch_id
                                                        (Printf.sprintf
                                                           "Conflict \
                                                            force-push \
                                                            rejected — %s"
                                                           (Push_reject_classify
                                                            .short_label reason))
                                                  | Worktree
                                                    .Push_worktree_missing ->
                                                      log_event runtime
                                                        ~patch_id
                                                        (Printf.sprintf
                                                           "Worktree \
                                                            disappeared (%s) — \
                                                            conflict \
                                                            resolution will \
                                                            reconstruct on \
                                                            retry"
                                                           wt_path)
                                                  | Worktree.Push_error msg ->
                                                      log_event runtime
                                                        ~patch_id
                                                        (Printf.sprintf
                                                           "Conflict \
                                                            force-push failed \
                                                            — %s"
                                                           msg));
                                                  Some
                                                    (`Pushed
                                                       ( result,
                                                         local_sha,
                                                         remote_tracking_sha,
                                                         base_sha )))
                                        in
                                        let push_outcome =
                                          Base.Option.bind push_record
                                            ~f:(function
                                            | `Rejected -> None
                                            | `Pushed (result, _, _, _) ->
                                                Some result)
                                        in
                                        let resolution, push_agent_after =
                                          Runtime.update_orchestrator_returning
                                            runtime (fun orch ->
                                              let orch, resolution =
                                                match push_record with
                                                | Some `Rejected ->
                                                    Orchestrator
                                                    .reject_conflict_publication
                                                      orch patch_id
                                                | Some (`Pushed _) | None ->
                                                    Orchestrator
                                                    .apply_conflict_push_result
                                                      orch patch_id decision
                                                      push_outcome
                                              in
                                              let push_agent_after =
                                                Orchestrator.agent orch patch_id
                                              in
                                              ( orch,
                                                (resolution, push_agent_after)
                                              ))
                                        in
                                        (match push_record with
                                        | Some
                                            (`Pushed
                                               ( result,
                                                 local_sha,
                                                 remote_tracking_sha,
                                                 base_sha )) ->
                                            Event_log.log_push event_log
                                              ~patch_id
                                              ~kind:
                                                Event_log
                                                .Conflict_resolution_push
                                              ~result ~local_sha
                                              ~remote_tracking_sha ~base_sha
                                              ~agent_before:agent_after
                                              ~agent_after:push_agent_after
                                        | Some `Rejected | None -> ());
                                        match resolution with
                                        | Orchestrator.Conflict_done -> `Ok
                                        | Orchestrator.Conflict_retry_push ->
                                            log_event runtime ~patch_id
                                              "Re-enqueued conflict resolution \
                                               after push failure";
                                            `Retry_push
                                        | Orchestrator.Conflict_needs_agent ->
                                            (* [conflict_info] is [None] when
                                             the rebase returned [Ok]/[Noop]
                                             and only the push subsequently
                                             failed; we degrade to a
                                             no-recovery-section prompt
                                             rather than blocking delivery. *)
                                            deliver_to_agent ?conflict_info ()
                                        | Orchestrator.Conflict_give_up ->
                                            `Failed)
                                  | Patch_decision.Deliver
                                      {
                                        payload =
                                          ( Patch_decision.Human_payload _
                                          | Patch_decision.Ci_payload _
                                          | Patch_decision.Review_payload _
                                          | Patch_decision.Findings_payload _
                                          | Patch_decision.Pr_body_payload ) as
                                          payload;
                                        base_change;
                                      } -> (
                                      let pr_number =
                                        Patch_agent.pr_number agent
                                      in
                                      let base_changed_prefix =
                                        render_base_changed_prefix base_change
                                      in
                                      (* Resolve the patch + base branch for
                                       layered prompt prefixes (Ci, Review).
                                       Ad-hoc PRs have no gameplan-defined
                                       patch — [patch_for_layer] is then
                                       [None] and the renderers omit the
                                       gameplan+patch prefix. *)
                                      let patch_for_layer =
                                        Base.List.find gameplan.Gameplan.patches
                                          ~f:(fun (p : Patch.t) ->
                                            Patch_id.equal p.Patch.id patch_id)
                                      in
                                      let base_branch_for_layer =
                                        Base.Option.value_map
                                          agent.Patch_agent.base_branch
                                          ~default:(Branch.to_string main)
                                          ~f:Branch.to_string
                                      in
                                      let wt_path =
                                        WS.resolve_worktree_path ~patch_id
                                          ~agent ()
                                      in
                                      let agents_md =
                                        read_optional_file
                                          (Stdlib.Filename.concat wt_path
                                             "AGENTS.md")
                                      in
                                      let review_artifact_dir =
                                        match payload with
                                        | Patch_decision.Review_payload _ ->
                                            Some
                                              (Project_store
                                               .comment_responses_dir
                                                 ~project_name ~patch_id)
                                        | Patch_decision.Ci_payload _
                                        | Patch_decision.Findings_payload _
                                        | Patch_decision.Human_payload _
                                        | Patch_decision.Pr_body_payload
                                        | Patch_decision.Merge_conflict_payload
                                          ->
                                            None
                                      in
                                      log_event runtime ~patch_id
                                        (match payload with
                                        | Patch_decision.Review_payload
                                            { comments } ->
                                            Printf.sprintf "Delivering %s (%s)"
                                              (Operation_kind.to_label kind)
                                              (pluralize
                                                 (Base.List.length comments)
                                                 "comment")
                                        | Patch_decision.Findings_payload
                                            { findings } ->
                                            Printf.sprintf "Delivering %s (%s)"
                                              (Operation_kind.to_label kind)
                                              (pluralize
                                                 (Base.List.length findings)
                                                 "finding")
                                        | Patch_decision.Human_payload
                                            { messages } ->
                                            Printf.sprintf "Delivering %s (%s)"
                                              (Operation_kind.to_label kind)
                                              (pluralize
                                                 (Base.List.length messages)
                                                 "message")
                                        | Patch_decision.Ci_payload _
                                        | Patch_decision.Pr_body_payload
                                        | Patch_decision.Merge_conflict_payload
                                          ->
                                            Printf.sprintf "Delivering %s"
                                              (Operation_kind.to_label kind));
                                      let exception Skip_delivery in
                                      try
                                        let prompt =
                                          match payload with
                                          | Patch_decision.Ci_payload
                                              { failed_checks } ->
                                              if
                                                Base.List.is_empty failed_checks
                                              then
                                                Prompt
                                                .render_ci_failure_unknown_prompt
                                                  ~project_name ?agents_md
                                                  ?pr_number
                                                  ?patch:patch_for_layer
                                                  ~gameplan
                                                  ~base_branch:
                                                    base_branch_for_layer ()
                                              else
                                                let fetch_cap = 10 in
                                                let checks_to_fetch, extra =
                                                  Base.List.split_n
                                                    failed_checks fetch_cap
                                                in
                                                if
                                                  not (Base.List.is_empty extra)
                                                then
                                                  log_event runtime ~patch_id
                                                    (Printf.sprintf
                                                       "Skipping CI detail \
                                                        fetch for %d check(s) \
                                                        over cap — delivering \
                                                        metadata only"
                                                       (Base.List.length extra));
                                                let current_head_oid =
                                                  Base.Option.bind
                                                    fresh_pr_state ~f:(fun ps ->
                                                      ps.Pr_state.head_oid)
                                                in
                                                let is_merge_queue_removal_check
                                                    check =
                                                  Base.List.mem
                                                    !ci_merge_queue_removal_checks
                                                    check ~equal:Ci_check.equal
                                                in
                                                let head_oid_for_check check =
                                                  match check.Ci_check.id with
                                                  | Some _
                                                    when not
                                                           (is_merge_queue_removal_check
                                                              check) ->
                                                      current_head_oid
                                                  | Some _ | None -> None
                                                in
                                                let detail_for_check check =
                                                  try
                                                    match
                                                      Forge
                                                      .check_failure_details
                                                        ~check
                                                    with
                                                    | Error e ->
                                                        log_event runtime
                                                          ~patch_id
                                                          (Printf.sprintf
                                                             "CI detail fetch \
                                                              failed for %s \
                                                              (%s) — \
                                                              delivering \
                                                              metadata only"
                                                             check.Ci_check.name
                                                             (Forge.show_error e));
                                                        None
                                                    | Ok source -> (
                                                        let enrichment =
                                                          Ci_log_digest.digest
                                                            source
                                                        in
                                                        let log =
                                                          Base.Option.map
                                                            source
                                                              .Ci_log_digest.log
                                                            ~f:
                                                              Ci_log_digest
                                                              .strip_log
                                                        in
                                                        match
                                                          Project_store
                                                          .publish_ci_check_artifact
                                                            ~project_name
                                                            ~patch_id ~check
                                                            ?head_oid:
                                                              (head_oid_for_check
                                                                 check)
                                                            ~summary_md:
                                                              enrichment
                                                                .Ci_log_digest
                                                                 .summary_md
                                                            ?log ()
                                                        with
                                                        | Ok artifact_dir ->
                                                            Some
                                                              Prompt.
                                                                {
                                                                  artifact_dir;
                                                                  enrichment;
                                                                }
                                                        | Error msg ->
                                                            log_event runtime
                                                              ~patch_id
                                                              (Printf.sprintf
                                                                 "CI detail \
                                                                  artifact \
                                                                  publish \
                                                                  failed for \
                                                                  %s (%s) — \
                                                                  delivering \
                                                                  metadata \
                                                                  only"
                                                                 check
                                                                   .Ci_check
                                                                    .name msg);
                                                            None)
                                                  with
                                                  | Eio.Cancel.Cancelled _ as
                                                    exn ->
                                                      raise exn
                                                  | exn ->
                                                      log_event runtime
                                                        ~patch_id
                                                        (Printf.sprintf
                                                           "CI detail \
                                                            processing failed \
                                                            for %s (%s) — \
                                                            delivering \
                                                            metadata only"
                                                           check.Ci_check.name
                                                           (Stdlib.Printexc
                                                            .to_string exn));
                                                      None
                                                in
                                                let detailed_checks =
                                                  Base.List.map checks_to_fetch
                                                    ~f:(fun check ->
                                                      ( check,
                                                        detail_for_check check
                                                      ))
                                                  @ Base.List.map extra
                                                      ~f:(fun check ->
                                                        (check, None))
                                                in
                                                Prompt
                                                .render_ci_failure_prompt_detailed
                                                  ~project_name ?agents_md
                                                  ?pr_number
                                                  ?patch:patch_for_layer
                                                  ~gameplan
                                                  ~base_branch:
                                                    base_branch_for_layer
                                                  detailed_checks
                                          | Patch_decision.Review_payload
                                              { comments } ->
                                              let current_head_sha =
                                                Base.Option.bind fresh_pr_state
                                                  ~f:(fun ps ->
                                                    ps.Pr_state.head_oid)
                                              in
                                              let artifact_dir =
                                                match review_artifact_dir with
                                                | Some artifact_dir ->
                                                    artifact_dir
                                                | None -> assert false
                                              in
                                              (* Clear stale response files
                                             from a prior Review session. The
                                             directory is stable per-patch;
                                             without this, a leftover file
                                             whose reply already posted (but
                                             whose resolve failed) would be
                                             replayed as a duplicate reply. *)
                                              Project_store.reset_artifact_dir
                                                artifact_dir;
                                              Prompt.render_review_prompt
                                                ~project_name ?agents_md
                                                ?pr_number ?current_head_sha
                                                ?viewer_login:
                                                  (Forge.viewer_login ())
                                                ?patch:patch_for_layer ~gameplan
                                                ~base_branch:
                                                  base_branch_for_layer
                                                ~artifact_dir comments
                                          | Patch_decision.Findings_payload
                                              { findings } ->
                                              let current_head_sha =
                                                Base.Option.bind fresh_pr_state
                                                  ~f:(fun ps ->
                                                    ps.Pr_state.head_oid)
                                              in
                                              let artifact_dir =
                                                Project_store
                                                .findings_wontfix_dir
                                                  ~project_name ~patch_id
                                              in
                                              Project_store.reset_artifact_dir
                                                artifact_dir;
                                              Prompt.render_findings_prompt
                                                ~project_name ?agents_md
                                                ?pr_number ?current_head_sha
                                                ?patch:patch_for_layer ~gameplan
                                                ~base_branch:
                                                  base_branch_for_layer
                                                ~artifact_dir findings
                                          | Patch_decision.Human_payload
                                              { messages } ->
                                              Prompt.render_human_message_prompt
                                                ~project_name messages
                                          | Patch_decision.Pr_body_payload ->
                                              let pr, patch =
                                                match
                                                  (pr_number, patch_for_layer)
                                                with
                                                | Some pr, Some patch ->
                                                    (pr, patch)
                                                | None, _ ->
                                                    log_event runtime ~patch_id
                                                      "pr-body: no PR number \
                                                       yet — skipping";
                                                    raise Skip_delivery
                                                | _, None ->
                                                    log_event runtime ~patch_id
                                                      "pr-body: skipping — \
                                                       patch has no gameplan \
                                                       entry (likely ad-hoc)";
                                                    raise Skip_delivery
                                              in
                                              let pr_body =
                                                Prompt.render_pr_description
                                                  ~project_name patch
                                              in
                                              let check_suffix =
                                                Prompt.render_check_suffix patch
                                              in
                                              let artifact_path =
                                                Project_store
                                                .pr_body_artifact_path
                                                  ~project_name ~patch_id
                                              in
                                              Project_store.ensure_dir
                                                (Stdlib.Filename.dirname
                                                   artifact_path);
                                              (* Clear any stale artifact from a
                                             prior Pr_body session. The path
                                             is stable per-patch, so without
                                             this the classifier could read
                                             outdated notes when the current
                                             session doesn't write (blocked,
                                             no-op, or legitimately chose not
                                             to). *)
                                              (try Unix.unlink artifact_path
                                               with
                                               | Unix.Unix_error
                                                   (Unix.ENOENT, _, _)
                                               ->
                                                 ());
                                              Prompt.render_pr_body_prompt
                                                ~project_name ~pr_number:pr
                                                ~pr_body ~check_suffix
                                                ~artifact_path
                                          | Patch_decision
                                            .Merge_conflict_payload ->
                                              (* Invariant: Merge_conflict is handled
                                         in the dedicated match arm above *)
                                              assert false
                                        in
                                        let prompt =
                                          if String.equal base_changed_prefix ""
                                          then prompt
                                          else
                                            base_changed_prefix ^ "\n" ^ prompt
                                        in
                                        let on_pr_detected _pr_number = () in
                                        let base =
                                          Base.Option.value_map
                                            agent.Patch_agent.base_branch
                                            ~default:(Branch.to_string main)
                                            ~f:Branch.to_string
                                        in
                                        (* Lock in CI run dedup before firing the
                                       session. Recording pre-flight (rather
                                       than post-) means a session that starts
                                       but later fails still counts as
                                       "delivered", so we don't re-nag the
                                       agent with the same failing run on the
                                       next tick. *)
                                        (match payload with
                                        | Patch_decision.Ci_payload
                                            { failed_checks } ->
                                            let ids =
                                              Base.List.filter_map failed_checks
                                                ~f:(fun (c : Ci_check.t) ->
                                                  c.Ci_check.id)
                                            in
                                            if not (Base.List.is_empty ids) then
                                              Runtime.update_orchestrator
                                                runtime (fun orch ->
                                                  Orchestrator
                                                  .record_delivered_ci_run_ids
                                                    orch patch_id ids)
                                        | Patch_decision.Human_payload _
                                        | Patch_decision.Review_payload _
                                        | Patch_decision.Findings_payload _
                                        | Patch_decision.Pr_body_payload
                                        | Patch_decision.Merge_conflict_payload
                                          ->
                                            ());
                                        (* Snapshot the pr-body artifact before
                                       the session so the post-session sync
                                       step (after Session_driver.run) can
                                       detect content changes from any kind
                                       of session, not only Pr_body. For
                                       Pr_body the artifact was just unlinked
                                       above, so the snapshot is None. *)
                                        let pr_body_pre_snapshot =
                                          read_artifact_file
                                            (Project_store.pr_body_artifact_path
                                               ~project_name ~patch_id)
                                        in
                                        let gameplan_prompt =
                                          Prompt.render_gameplan_layer
                                            ~project_name gameplan
                                        in
                                        let patch_prompt =
                                          match patch_for_layer with
                                          | Some p ->
                                              Prompt
                                              .render_patch_layer_of_gameplan
                                                ~project_name ?pr_number p
                                                gameplan
                                                ~base_branch:
                                                  base_branch_for_layer
                                          | None -> ""
                                        in
                                        let result, tool_failures =
                                          run_llm_session ~sw ~gameplan_prompt
                                            ~patch_prompt ~kind:(Some kind)
                                            ~patch_id ~prompt ~agent
                                            ~on_pr_detected
                                        in
                                        let result =
                                          (result
                                            :> [ `Failed
                                               | `Ok
                                               | `No_commits
                                               | `Pr_body_miss
                                               | `Retry_push
                                               | `Review_unresolved ])
                                        in
                                        (match result with
                                        | `Ok
                                          when Base.Option.is_some base_change
                                          ->
                                            Runtime.update_orchestrator runtime
                                              (fun orch ->
                                                Orchestrator
                                                .set_notified_base_branch orch
                                                  patch_id
                                                  (Branch.of_string base))
                                        | _ -> ());
                                        (* Artifact-driven phase (Pr_body): read
                                   the agent's artifact and PATCH the PR body.
                                   Success for this phase is artifact delivery,
                                   never commit presence — a healthy Pr_body
                                   session authors the notes file outside the
                                   worktree and makes no commits, so it arrives
                                   here as [`No_commits]. Both [`Ok] and
                                   [`No_commits] sessions run the apply
                                   (pr_body_respond_plan), and the classify
                                   verdict owns the arm's result: [`Ok]
                                   upgrades a no-commit session so Respond_ok
                                   flips pr_body_delivered and resets the
                                   no-commit noop counter that
                                   apply_session_result bumped;
                                   [`Pr_body_miss] (blocked Write or failed
                                   PATCH) lets the reconciler re-enqueue
                                   Pr_body before escalating to
                                   needs_intervention. A missing/empty
                                   artifact with no Write failure is the
                                   agent's deliberate choice not to add notes
                                   and classifies [`Ok]. *)
                                        let session_ok =
                                          match result with
                                          | `Ok -> true
                                          | _ -> false
                                        in
                                        let session_no_commits =
                                          match result with
                                          | `No_commits -> true
                                          | _ -> false
                                        in
                                        let result =
                                          match payload with
                                          | Patch_decision.Pr_body_payload -> (
                                              match
                                                Patch_decision
                                                .pr_body_respond_plan
                                                  ~session_ok
                                                  ~session_no_commits
                                              with
                                              | Patch_decision
                                                .Pr_body_pass_through ->
                                                  result
                                              | Patch_decision.Pr_body_apply
                                                -> (
                                                  match
                                                    ( pr_number,
                                                      Base.List.find
                                                        gameplan
                                                          .Gameplan.patches
                                                        ~f:(fun (p : Patch.t) ->
                                                          Patch_id.equal
                                                            p.Patch.id patch_id)
                                                    )
                                                  with
                                                  | Some pr, Some patch ->
                                                      let artifact_outcome =
                                                        apply_pr_body_artifact
                                                          ~runtime ~project_name
                                                          ~patch_id
                                                          ~pr_number:pr ~patch
                                                      in
                                                      (Patch_decision
                                                       .classify_pr_body_respond
                                                         ~artifact_outcome
                                                         ~tool_failures
                                                        :> [ `Failed
                                                           | `Ok
                                                           | `No_commits
                                                           | `Pr_body_miss
                                                           | `Retry_push
                                                           | `Review_unresolved
                                                           ])
                                                  | None, _ ->
                                                      log_event runtime
                                                        ~patch_id
                                                        "pr-body: no PR number \
                                                         yet — skipping \
                                                         artifact apply";
                                                      result
                                                  | _, None ->
                                                      log_event runtime
                                                        ~patch_id
                                                        "pr-body: skipping \
                                                         artifact apply — \
                                                         patch has no gameplan \
                                                         entry (likely ad-hoc)";
                                                      result))
                                          | Patch_decision.Findings_payload
                                              { findings } ->
                                              if session_ok then (
                                                let artifact_dir =
                                                  Project_store
                                                  .findings_wontfix_dir
                                                    ~project_name ~patch_id
                                                in
                                                Findings_resolver
                                                .resolve_after_session
                                                  ~review_clients
                                                  ~log:(fun msg ->
                                                    log_event runtime ~patch_id
                                                      msg)
                                                  ~findings_registry
                                                  ~artifact_dir
                                                  ~delivered:findings
                                                  ~actor:
                                                    (Printf.sprintf "lo:%s"
                                                       (Patch_id.to_string
                                                          patch_id))
                                                  ();
                                                result)
                                              else
                                                let ids =
                                                  Base.List.map findings
                                                    ~f:(fun
                                                        (f :
                                                          Review_service.finding)
                                                      -> f.Review_service.id)
                                                in
                                                log_event runtime ~patch_id
                                                  (Printf.sprintf
                                                     "Session failed before \
                                                      resolving findings; \
                                                      forgetting delivered \
                                                      finding registry \
                                                      entries: %s"
                                                     (String.concat ", " ids));
                                                Base.List.iter findings
                                                  ~f:(fun
                                                      (f :
                                                        Review_service.finding)
                                                    ->
                                                    Findings_registry.forget
                                                      findings_registry
                                                      ~key:f.Review_service.id);
                                                result
                                          | Patch_decision.Review_payload
                                              { comments } ->
                                              (* Supervisor-owned comment
                                             response/resolution. [`Ok] from
                                             the session driver implies the
                                             post-session push shipped (push
                                             failure surfaces as
                                             [`Retry_push]). [`No_commits] is
                                             also eligible because a
                                             comment-only answer has no code
                                             fix to push. On a failed session
                                             the response files are left in
                                             place but not posted — the poller
                                             re-detects the unresolved threads
                                             and the next Review session
                                             re-delivers (its setup clears the
                                             stale files). *)
                                              if
                                                session_ok || session_no_commits
                                              then
                                                match
                                                  Comment_responder
                                                  .respond_after_session
                                                    ~reply:(fun
                                                        ~comment_id ~body ->
                                                      match pr_number with
                                                      | None ->
                                                          Error
                                                            "no PR number \
                                                             recorded for this \
                                                             patch"
                                                      | Some pr ->
                                                          Base.Result.map_error
                                                            (Forge
                                                             .reply_to_review_comment
                                                               ~pr_number:pr
                                                               ~comment_id ~body)
                                                            ~f:Forge.show_error)
                                                    ~resolve:(fun ~thread_id ->
                                                      Base.Result.map_error
                                                        (Forge
                                                         .resolve_review_thread
                                                           ~thread_id)
                                                        ~f:Forge.show_error)
                                                    ~log:(fun msg ->
                                                      log_event runtime
                                                        ~patch_id msg)
                                                    ~viewer_login:
                                                      (Forge.viewer_login ())
                                                    ~artifact_dir:
                                                      (match
                                                         review_artifact_dir
                                                       with
                                                      | Some artifact_dir ->
                                                          artifact_dir
                                                      | None -> assert false)
                                                    ~delivered:comments ()
                                                with
                                                | `Converged ->
                                                    if session_no_commits then
                                                      `Ok
                                                    else result
                                                | `Unresolved _ ->
                                                    `Review_unresolved
                                              else (
                                                log_event runtime ~patch_id
                                                  "comment-responses: session \
                                                   did not complete cleanly — \
                                                   skipping reply/resolve; \
                                                   unresolved comments will \
                                                   re-deliver";
                                                result)
                                          | Patch_decision.Ci_payload
                                              { failed_checks } ->
                                              if session_no_commits then
                                                let rerun_results =
                                                  Base.List.map failed_checks
                                                    ~f:(fun check ->
                                                      match
                                                        Forge
                                                        .rerun_failed_jobs_for_check
                                                          ~check
                                                      with
                                                      | Ok () ->
                                                          log_event runtime
                                                            ~patch_id
                                                            (Printf.sprintf
                                                               "ci-rerun: \
                                                                requested \
                                                                failed-job \
                                                                rerun for %s"
                                                               check
                                                                 .Ci_check.name);
                                                          true
                                                      | Error err ->
                                                          log_event runtime
                                                            ~patch_id
                                                            (Printf.sprintf
                                                               "ci-rerun: \
                                                                failed to \
                                                                request \
                                                                failed-job \
                                                                rerun for %s — \
                                                                %s"
                                                               check
                                                                 .Ci_check.name
                                                               (Forge.show_error
                                                                  err));
                                                          false)
                                                in
                                                if
                                                  Base.List.exists rerun_results
                                                    ~f:(fun x -> x)
                                                then `Ok
                                                else `Retry_push
                                              else result
                                          | Patch_decision.Human_payload _
                                          | Patch_decision
                                            .Merge_conflict_payload ->
                                              result
                                        in
                                        (* Opportunistic pr-body sync. If the
                                       agent updated the artifact during a
                                       non-Pr_body session, PATCH the PR.
                                       Pure planner returns Sync_skip for
                                       kind=Pr_body (its delivery path is
                                       owned by classify_pr_body_respond),
                                       so this block is a no-op for that
                                       kind and never double-PATCHes. *)
                                        (* Re-derive session_ok from the (possibly
                                       rebound) result so the planner sees the
                                       final outcome — the Pr_body_payload arm
                                       above can flip result to Pr_body_miss. *)
                                        let session_ok =
                                          match result with
                                          | `Ok -> true
                                          | _ -> false
                                        in
                                        let pr_body_post_snapshot =
                                          read_artifact_file
                                            (Project_store.pr_body_artifact_path
                                               ~project_name ~patch_id)
                                        in
                                        let plan =
                                          Patch_decision.plan_artifact_sync
                                            ~kind ~session_ok
                                            ~pre:pr_body_pre_snapshot
                                            ~post:pr_body_post_snapshot
                                        in
                                        let patch_result =
                                          match plan with
                                          | Patch_decision.Sync_skip -> None
                                          | Patch_decision.Sync_attempt_pr_body
                                            -> (
                                              match pr_number with
                                              | None ->
                                                  log_event runtime ~patch_id
                                                    "pr-body: skipping \
                                                     opportunistic sync — no \
                                                     PR number yet";
                                                  None
                                              | Some pr -> (
                                                  (* Ad-hoc agents (added via
                                             Orchestrator.add_agent) have a
                                             pr_number but no gameplan entry,
                                             so a missing patch here is not a
                                             bug — skip the opportunistic
                                             sync and let
                                             classify_artifact_sync_outcome
                                             map patch_result=None to
                                             Sync_no_op. *)
                                                  match
                                                    Base.List.find
                                                      gameplan.Gameplan.patches
                                                      ~f:(fun (p : Patch.t) ->
                                                        Patch_id.equal
                                                          p.Patch.id patch_id)
                                                  with
                                                  | Some patch ->
                                                      Some
                                                        (apply_pr_body_artifact
                                                           ~runtime
                                                           ~project_name
                                                           ~patch_id
                                                           ~pr_number:pr ~patch)
                                                  | None ->
                                                      log_event runtime
                                                        ~patch_id
                                                        "pr-body: skipping \
                                                         opportunistic sync — \
                                                         patch has no gameplan \
                                                         entry (likely ad-hoc)";
                                                      None))
                                        in
                                        (match
                                           Patch_decision
                                           .classify_artifact_sync_outcome ~plan
                                             ~patch_result
                                         with
                                        | Patch_decision.Sync_no_op -> ()
                                        | Patch_decision.Sync_delivered ->
                                            log_event runtime ~patch_id
                                              (Printf.sprintf
                                                 "pr-body: synced \
                                                  opportunistically from %s \
                                                  session"
                                                 (Operation_kind.to_label kind));
                                            Runtime.update_orchestrator runtime
                                              (fun orch ->
                                                let orch =
                                                  Orchestrator
                                                  .set_pr_body_delivered orch
                                                    patch_id true
                                                in
                                                Orchestrator
                                                .reset_pr_body_artifact_miss_count
                                                  orch patch_id)
                                        | Patch_decision.Sync_patch_failed ->
                                            log_event runtime ~patch_id
                                              "pr-body: opportunistic sync \
                                               PATCH failed; leaving state — \
                                               next Pr_body cycle will retry");
                                        (result
                                          :> [ `Failed
                                             | `Ok
                                             | `No_commits
                                             | `Pr_body_miss
                                             | `Review_unresolved
                                             | `Retry_push
                                             | `Skip_empty
                                             | `Stale ])
                                      with Skip_delivery -> `Skip_empty))
                        in
                        let respond_outcome =
                          match result with
                          | `Stale -> Orchestrator.Respond_stale
                          | `Skip_empty -> Orchestrator.Respond_skip_empty
                          | `Failed -> Orchestrator.Respond_failed
                          | `No_commits -> Orchestrator.Respond_no_commits
                          | `Retry_push -> Orchestrator.Respond_retry_push
                          | `Pr_body_miss -> Orchestrator.Respond_pr_body_miss
                          | `Review_unresolved ->
                              Orchestrator.Respond_review_unresolved
                          | `Ok -> Orchestrator.Respond_ok
                        in
                        Runtime.update_orchestrator runtime (fun orch ->
                            Orchestrator.apply_respond_outcome orch patch_id
                              kind respond_outcome);
                        match respond_outcome with
                        | Orchestrator.Respond_failed ->
                            let agent =
                              Runtime.read runtime (fun snap ->
                                  Orchestrator.agent snap.Runtime.orchestrator
                                    patch_id)
                            in
                            if Patch_agent.needs_intervention agent then
                              set_status ~level:Tui.Error
                                ~text:
                                  (Printf.sprintf
                                     "Patch %s: session failed — human review \
                                      needed"
                                     (Patch_id.to_string patch_id))
                                ()
                        | Orchestrator.Respond_pr_body_miss ->
                            (* Triggered by classify_pr_body_respond for either
                             [`Patch_failed] (notes written, GitHub PATCH call
                             failed) or [`Missing | `Empty] + observed Write
                             tool failure (blocked mid-Write). The specific
                             cause is already logged by apply_pr_body_artifact;
                             keep this line cause-agnostic so it doesn't
                             mislabel a [`Patch_failed] miss as a Write
                             failure. The reconciler re-enqueues Pr_body; at
                             cap (>=2) needs_intervention fires. *)
                            let agent =
                              Runtime.read runtime (fun snap ->
                                  Orchestrator.agent snap.Runtime.orchestrator
                                    patch_id)
                            in
                            log_event runtime ~patch_id
                              (Printf.sprintf
                                 "pr-body: miss recorded (miss count: %d)%s"
                                 agent.Patch_agent.pr_body_artifact_miss_count
                                 (if Patch_agent.needs_intervention agent then
                                    "; escalating to human review"
                                  else "; will re-enqueue"));
                            if Patch_agent.needs_intervention agent then
                              set_status ~level:Tui.Error
                                ~text:
                                  (Printf.sprintf
                                     "Patch %s: pr-body artifact repeatedly \
                                      missing — human review needed"
                                     (Patch_id.to_string patch_id))
                                ()
                        | Orchestrator.Respond_review_unresolved ->
                            (* Per-comment causes are already logged by
                             Comment_responder; this line records the counter
                             march so an operator can see how close the loop
                             is to the cap. *)
                            let agent =
                              Runtime.read runtime (fun snap ->
                                  Orchestrator.agent snap.Runtime.orchestrator
                                    patch_id)
                            in
                            log_event runtime ~patch_id
                              (Printf.sprintf
                                 "comment-responses: non-converged review \
                                  cycle recorded (cycle count: %d)%s"
                                 agent.Patch_agent.review_unresolved_cycle_count
                                 (if Patch_agent.needs_intervention agent then
                                    "; escalating to human review"
                                  else "; unresolved comments will re-deliver"));
                            if Patch_agent.needs_intervention agent then
                              set_status ~level:Tui.Error
                                ~text:
                                  (Printf.sprintf
                                     "Patch %s: review comments repeatedly \
                                      left unresolved — human review needed"
                                     (Patch_id.to_string patch_id))
                                ()
                        | Orchestrator.Respond_ok ->
                            if
                              Operation_kind.equal kind
                                Operation_kind.Merge_conflict
                            then
                              set_status ~level:Tui.Info
                                ~text:
                                  (Printf.sprintf
                                     "Patch %s: conflict resolved, rebasing…"
                                     (Patch_id.to_string patch_id))
                                ~expires_at:(Unix.gettimeofday () +. 10.0)
                                ()
                        | Orchestrator.Respond_stale
                        | Orchestrator.Respond_no_commits
                        | Orchestrator.Respond_retry_push
                        | Orchestrator.Respond_skip_empty ->
                            ())))
      in
      (* Spawn action fibers without waiting for completion. Each fiber is
       guarded by with_busy_guard (ensures complete on exit) and
       with_session_slot (semaphore backpressure). The runner loop continues
       immediately to pick up newly-queued actions from the poller. *)
      Base.List.iter action_fibers ~f:(fun f ->
          Eio.Fiber.fork_daemon ~sw (fun () ->
              f ();
              `Stop_daemon));
      Eio.Time.sleep clock 1.0;
      loop sw
    in
    Eio.Switch.run @@ fun sw -> loop sw
end
