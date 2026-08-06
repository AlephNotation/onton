open Base
open Onton
open Onton_core
open Onton_core.Types

let worktree_path =
  let path = Stdlib.Filename.temp_file "onton-validation-repair-" "" in
  Stdlib.Sys.remove path;
  Unix.mkdir path 0o700;
  path

let data_path = Stdlib.Filename.temp_dir "onton-session-contract-" ""

let rec remove_tree path =
  match Unix.lstat path with
  | stat ->
      if Poly.equal stat.Unix.st_kind Unix.S_DIR then (
        Stdlib.Sys.readdir path
        |> Array.iter ~f:(fun name ->
            remove_tree (Stdlib.Filename.concat path name));
        Unix.rmdir path)
      else Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

module Fake_worktree : Worktree.S = struct
  let resolve_main_root () = worktree_path
  let is_checked_out_in_repo_root _ = false
  let remote_branch_exists _ = false
  let create ~project_name:_ ~patch_id:_ ~branch:_ ~base_ref:_ = assert false
  let remove _ = assert false
  let detect_branch ~path:_ = assert false
  let list_with_branches () = []
  let find_for_branch _ = None
  let prune_admin () = ()
  let run_hook ~clock:_ ~script:_ ~cwd:_ ~env:_ () = assert false
  let fetch_origin ~fetch_lock:_ ~path:_ = assert false

  let fetch_origin_branch ~fetch_lock:_ ~branch:_ : Worktree.fetch_branch_result
      =
    assert false

  let git_status ~path:_ = assert false
  let conflict_diff ~path:_ = assert false

  let rebase_onto ~path:_ ~target:_ ~upstream:_ ~project_name:_ ~ancestor_ids:_
      () =
    assert false

  let read_branch_sha ~path:_ ~ref_name:_ = Some "sha"
  let is_ancestor ~path:_ ~ancestor:_ ~descendant:_ = false

  let read_in_progress_conflict_info ~path:_ ~target:_ ~project_name:_
      ~ancestor_ids:_ =
    assert false

  let force_push_with_lease ~path:_ ~branch:_ ~base:_ = Worktree.Push_ok
  let rebase_in_progress ~path:_ = false
end

let patch =
  {
    Patch.id = Patch_id.of_string "repair";
    branch = Branch.of_string "onton/repair";
    goal = "repair validation";
    dependencies = [];
    files = [ "lib/session_driver.ml" ];
    checks = [ { Check.run = "dune build"; proves = "builds" } ];
    agent = None;
  }

let gameplan =
  {
    Gameplan.project_name = "repair";
    repo_owner = "owner";
    repo_name = "repo";
    patches = [ patch ];
  }

let () =
  Eio_main.run @@ fun env ->
  let old_data_dir = Stdlib.Sys.getenv_opt "ONTON_DATA_DIR" in
  Unix.putenv "ONTON_DATA_DIR" data_path;
  Stdlib.Fun.protect
    ~finally:(fun () ->
      (match old_data_dir with
      | Some value -> Unix.putenv "ONTON_DATA_DIR" value
      | None -> Unix.putenv "ONTON_DATA_DIR" "");
      remove_tree data_path;
      if Stdlib.Sys.file_exists worktree_path then Unix.rmdir worktree_path)
    (fun () ->
      let patch_id = patch.Patch.id in
      let runtime =
        Runtime.create ~gameplan ~main_branch:(Branch.of_string "main")
          ~durable_store:(fun _ -> Ok ())
          ()
      in
      let orchestrator =
        Runtime.read runtime (fun snapshot -> snapshot.Runtime.orchestrator)
        |> fun orch ->
        Orchestrator.fire orch
          (Orchestrator.Start (patch_id, Branch.of_string "main"))
        |> fun orch ->
        Orchestrator.set_pr_number orch patch_id (Pr_number.of_int 7)
        |> fun orch ->
        Orchestrator.complete orch patch_id |> fun orch ->
        Orchestrator.set_worktree_path orch patch_id worktree_path
        |> fun orch ->
        Orchestrator.send_human_message orch patch_id
          "original accepted feedback"
      in
      Runtime.update_orchestrator runtime (fun _ -> orchestrator);
      let orchestrator, messages =
        Patch_controller.plan_tick_messages orchestrator ~project_name:"repair"
          ~gameplan
      in
      let message =
        match messages with
        | [ message ] -> message
        | _ -> failwith "expected one Human response"
      in
      let orchestrator, accepted =
        Orchestrator.accept_message orchestrator
          (Orchestrator.message_id message)
      in
      assert (Option.is_some accepted);
      Runtime.update_orchestrator runtime (fun _ -> orchestrator);
      let module Env : Session_driver.ENV = struct
        let runtime = runtime
        let clock = Eio.Stdenv.clock env
        let fs = Eio.Stdenv.fs env
        let project_name = "repair"
        let owner = "owner"
        let repo = "repo"
        let transcripts = Stdlib.Hashtbl.create 1
        let user_config = { User_config.on_worktree_create = None }
        let worktree_mutex = Eio.Mutex.create ()
        let hook_mutex = Eio.Mutex.create ()
        let fetch_mutex = Eio.Mutex.create ()
        let event_log = Event_log.create ~path:"/dev/null"
      end in
      let module Driver = Session_driver.Make (Fake_worktree) (Env) in
      let first_delivered_prompt = ref None in
      let first_resume_session = ref None in
      let backend =
        {
          Llm_backend.name = "fake";
          run_streaming =
            (fun ~sandbox:_
              ~project_name:_
              ~cwd:_
              ~patch_id:_
              ~prompt
              ~resume_session
              ~session_uuid:_
              ~on_event
            ->
              first_delivered_prompt := Some prompt;
              first_resume_session := resume_session;
              on_event
                (Types.Stream_event.Session_init
                   {
                     session_id = "preserved-session";
                     api_key_source = None;
                     model = None;
                     claude_code_version = None;
                     permission_mode = None;
                   });
              on_event Types.Stream_event.Turn_started;
              on_event
                (Types.Stream_event.Final_result
                   { text = "done"; stop_reason = Types.Stop_reason.End_turn });
              {
                Llm_backend.exit_code = 0;
                stdout = "";
                stderr = "";
                got_events = true;
                saw_final_result = true;
                timed_out = false;
              });
        }
      in
      let agent =
        Runtime.read runtime (fun snapshot ->
            Orchestrator.agent snapshot.Runtime.orchestrator patch_id)
      in
      let validation_detail =
        "Controller validation rejected publication. Command: dune build\n\
         Proof: scope ok\n\
         Failure output: unbound module Removed"
      in
      let result, _ =
        Driver.run
          ~sandbox_for_worktree:(fun ~worktree:_ -> Ok (Stdlib.Obj.magic ()))
          ~kind:(Some Operation_kind.Human) ~patch_id ~prompt:"full reply"
          ~resume_prompt:"delta reply" ~agent
          ~on_pr_detected:(fun _ -> ())
          ~validate_before_push:(fun ~worktree:_ ~base_branch:_ ->
            Error
              (Orchestrator.Repair_required
                 {
                   target =
                     Validation_repair.Check
                       { Check.run = "dune build"; proves = "scope ok" };
                   detail = validation_detail;
                 }))
          ~backend
      in
      assert (
        match result with
        | `Failed -> true
        | `Ok | `Retry_push | `No_commits -> false);
      assert (
        Option.equal String.equal !first_delivered_prompt (Some "full reply"));
      assert (Option.is_none !first_resume_session);
      let rejected =
        Runtime.read runtime (fun snapshot -> snapshot.Runtime.orchestrator)
      in
      let repaired = Orchestrator.agent rejected patch_id in
      assert (not (Patch_agent.is_busy repaired));
      assert (Patch_agent.validation_failure_count repaired = 1);
      assert (
        Option.equal String.equal
          (Patch_agent.llm_session_id repaired)
          (Some "preserved-session"));
      assert (
        List.equal String.equal repaired.Patch_agent.human_messages
          [ validation_detail ]);
      (match Driver.session_mode repaired with
      | `Resume session_id ->
          assert (String.equal session_id "preserved-session")
      | `Fresh | `Give_up -> assert false);
      let snapshot =
        Runtime.read runtime (fun snapshot -> snapshot)
        |> Persistence.snapshot_to_yojson |> Persistence.snapshot_of_yojson
      in
      let restored =
        match snapshot with
        | Ok snapshot -> snapshot
        | Error message -> failwith message
      in
      let repaired =
        Orchestrator.agent restored.Runtime.orchestrator patch_id
      in
      assert (
        List.equal String.equal repaired.Patch_agent.human_messages
          [ validation_detail ]);
      let repair_orchestrator, next =
        Patch_controller.plan_tick_messages restored.Runtime.orchestrator
          ~project_name:"repair" ~gameplan
      in
      let repair_message =
        match next with
        | [ message ] ->
            assert (
              List.equal String.equal
                (Orchestrator.message_payload message)
                [ validation_detail ]);
            message
        | _ -> failwith "expected only the controller validation diagnostic"
      in
      let repair_orchestrator, accepted =
        Orchestrator.accept_message repair_orchestrator
          (Orchestrator.message_id repair_message)
      in
      assert (Option.is_some accepted);
      let repair_orchestrator =
        Orchestrator.set_head_oid repair_orchestrator patch_id (Some "sha")
      in
      Runtime.update_orchestrator runtime (fun _ -> repair_orchestrator);
      let completion_path =
        Project_store.completion_claim_path ~project_name:"repair" ~patch_id
      in
      let repair_delivered_prompt = ref None in
      let repair_resume_session = ref None in
      let successful_backend : Llm_backend.t =
        {
          Llm_backend.name = backend.Llm_backend.name;
          run_streaming =
            (fun ~sandbox:_
              ~project_name:_
              ~cwd:_
              ~patch_id:_
              ~prompt
              ~resume_session
              ~session_uuid:_
              ~on_event
            ->
              repair_delivered_prompt := Some prompt;
              repair_resume_session := resume_session;
              Project_store.ensure_dir (Stdlib.Filename.dirname completion_path);
              let output = Stdlib.open_out_bin completion_path in
              Stdlib.Fun.protect
                ~finally:(fun () -> Stdlib.close_out_noerr output)
                (fun () ->
                  Stdlib.output_string output "{\"status\":\"complete\"}");
              on_event Types.Stream_event.Turn_started;
              on_event
                (Types.Stream_event.Final_result
                   { text = "done"; stop_reason = Types.Stop_reason.End_turn });
              {
                Llm_backend.exit_code = 0;
                stdout = "";
                stderr = "";
                got_events = true;
                saw_final_result = true;
                timed_out = false;
              });
        }
      in
      let agent =
        Runtime.read runtime (fun snapshot ->
            Orchestrator.agent snapshot.Runtime.orchestrator patch_id)
      in
      let completed_event_agent = ref None in
      let interested_in = function
        | Telemetry.Event.Complete _ -> true
        | Telemetry.Event.Poll _ | Telemetry.Event.Action _
        | Telemetry.Event.Stream _ | Telemetry.Event.Spawn_started _
        | Telemetry.Event.Spawn_finalized _ | Telemetry.Event.Free_form _ ->
            false
      in
      let consume = function
        | Telemetry.Event.Complete { payload = `Assoc fields; _ } ->
            completed_event_agent :=
              List.Assoc.find fields ~equal:String.equal "agent_after"
        | Telemetry.Event.Complete _ | Telemetry.Event.Poll _
        | Telemetry.Event.Action _ | Telemetry.Event.Stream _
        | Telemetry.Event.Spawn_started _ | Telemetry.Event.Spawn_finalized _
        | Telemetry.Event.Free_form _ ->
            ()
      in
      let completion_sink : Telemetry.Sink.t =
        {
          Telemetry.Sink.name = "session-validation-repair-completion";
          interested_in;
          consume;
        }
      in
      let result, _ =
        Telemetry_dispatch.with_sink ~sink:completion_sink (fun () ->
            Driver.run
              ~sandbox_for_worktree:(fun ~worktree:_ ->
                Ok (Stdlib.Obj.magic ()))
              ~kind:(Some Operation_kind.Human) ~patch_id ~prompt:"full repair"
              ~resume_prompt:"delta repair" ~agent
              ~on_pr_detected:(fun _ -> ())
              ~validate_before_push:(fun ~worktree:_ ~base_branch:_ -> Ok ())
              ~backend:successful_backend)
      in
      assert (
        match result with
        | `Ok -> true
        | `Failed | `Retry_push | `No_commits -> false);
      assert (
        Option.equal String.equal !repair_delivered_prompt (Some "delta repair"));
      assert (
        Option.equal String.equal !repair_resume_session
          (Some "preserved-session"));
      let completed =
        Runtime.read runtime (fun snapshot ->
            Orchestrator.agent snapshot.Runtime.orchestrator patch_id)
      in
      assert (Patch_agent.validation_failure_count completed = 0);
      assert (Patch_agent.completion_matches_head completed);
      assert (
        match !completed_event_agent with
        | Some json -> (
            match Persistence.patch_agent_of_yojson json with
            | Ok agent -> Patch_agent.completion_matches_head agent
            | Error _ -> false)
        | None -> false);
      Stdlib.print_endline
        "session validation repair and completion attestation passed")
