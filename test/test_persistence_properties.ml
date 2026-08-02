(* @archlint.module test
   @archlint.domain orchestrator *)

open Base
open Onton_core.Types
open Onton_test_support.Test_generators

let messages_equal left right =
  List.equal Onton.Orchestrator.equal_patch_agent_message
    (Onton.Orchestrator.all_messages left)
    (Onton.Orchestrator.all_messages right)

let github_commands_equal left right =
  List.equal Onton_core.Github_effect.equal
    (Onton.Orchestrator.all_github_effects left)
    (Onton.Orchestrator.all_github_effects right)

let snapshots_equal (left : Onton.Runtime.snapshot)
    (right : Onton.Runtime.snapshot) =
  let agents orchestrator =
    Onton.Orchestrator.agents_map orchestrator |> Map.to_alist
  in
  List.equal
    (fun (left_id, left_agent) (right_id, right_agent) ->
      Patch_id.equal left_id right_id
      && Onton_core.Patch_agent.equal left_agent right_agent)
    (agents left.orchestrator)
    (agents right.orchestrator)
  && Branch.equal
       (Onton.Orchestrator.main_branch left.orchestrator)
       (Onton.Orchestrator.main_branch right.orchestrator)
  && messages_equal left.orchestrator right.orchestrator
  && github_commands_equal left.orchestrator right.orchestrator
  && Gameplan.equal left.gameplan right.gameplan
  && Onton_core.Activity_log.equal left.activity_log right.activity_log

let gen_snapshot =
  QCheck2.Gen.(
    map3
      (fun gameplan main_branch activity_log ->
        {
          Onton.Runtime.orchestrator =
            Onton.Orchestrator.create ~patches:gameplan.Gameplan.patches
              ~main_branch;
          activity_log;
          gameplan;
          transcripts = Hashtbl.create (module Patch_id);
        })
      gen_gameplan gen_branch gen_activity_log)

let remove_field name = function
  | `Assoc fields ->
      `Assoc
        (List.filter fields ~f:(fun (field, _) -> not (String.equal field name)))
  | json -> json

let replace_field name value = function
  | `Assoc fields ->
      `Assoc
        ((name, value)
        :: List.filter fields ~f:(fun (field, _) ->
            not (String.equal field name)))
  | json -> json

let rec remove_outbox_message_field name = function
  | `Assoc fields ->
      `Assoc
        (List.map fields ~f:(fun (field, value) ->
             if String.equal field "outbox" then
               let outbox =
                 match value with
                 | `Assoc messages ->
                     `Assoc
                       (List.map messages ~f:(fun (id, message) ->
                            (id, remove_field name message)))
                 | other -> other
               in
               (field, outbox)
             else if String.equal field "orchestrator" then
               (field, remove_outbox_message_field name value)
             else (field, value)))
  | json -> json

let snapshot_roundtrip =
  QCheck2.Test.make ~name:"snapshot JSON round-trip is identity" ~count:300
    gen_snapshot (fun snapshot ->
      match
        snapshot |> Onton.Persistence.snapshot_to_yojson
        |> Onton.Persistence.snapshot_of_yojson
      with
      | Ok restored -> snapshots_equal snapshot restored
      | Error message ->
          Stdlib.prerr_endline message;
          false)

let file_roundtrip =
  QCheck2.Test.make ~name:"atomic snapshot file round-trip" ~count:50
    gen_snapshot (fun snapshot ->
      let path = Stdlib.Filename.temp_file "onton-persistence-" ".json" in
      Stdlib.Fun.protect
        ~finally:(fun () ->
          if Stdlib.Sys.file_exists path then Stdlib.Sys.remove path)
        (fun () ->
          match Onton.Persistence.save ~path snapshot with
          | Error _ -> false
          | Ok () -> (
              match Onton.Persistence.load ~path with
              | Ok restored -> snapshots_equal snapshot restored
              | Error message ->
                  Stdlib.prerr_endline message;
                  false)))

let agent_roundtrip =
  QCheck2.Test.make ~name:"fully populated agent round-trip is identity"
    ~count:300 gen_patch_agent_fully_populated (fun agent ->
      match
        agent |> Onton.Persistence.patch_agent_to_yojson
        |> Onton.Persistence.patch_agent_of_yojson
      with
      | Ok restored ->
          if Onton_core.Patch_agent.equal agent restored then true
          else (
            Stdlib.prerr_endline
              ("original: " ^ Onton_core.Patch_agent.show agent);
            Stdlib.prerr_endline
              ("restored: " ^ Onton_core.Patch_agent.show restored);
            false)
      | Error message ->
          Stdlib.prerr_endline message;
          false)

let pr_status_roundtrip =
  QCheck2.Test.make ~name:"strict PR status variants round-trip" ~count:300
    QCheck2.Gen.(
      oneof
        [
          return Onton_core.Patch_pr_status.Absent;
          map
            (fun number ->
              Onton_core.Patch_pr_status.Present (Pr_number.of_int number))
            (int_range 1 100_000);
        ])
    (fun status ->
      match
        status |> Onton_core.Patch_pr_status.yojson_of_t
        |> Onton_core.Patch_pr_status.t_of_yojson
      with
      | Ok restored -> Onton_core.Patch_pr_status.equal status restored
      | Error _ -> false)

let strict_pr_status_rejects_legacy_shapes =
  QCheck2.Test.make ~name:"PR status rejects untagged legacy shapes"
    QCheck2.Gen.unit (fun () ->
      List.for_all
        [ `Null; `Int 42; `Assoc [ ("kind", `String "missing") ] ]
        ~f:(fun json ->
          Result.is_error (Onton_core.Patch_pr_status.t_of_yojson json)))

let strict_agent_requires_identity_fields =
  QCheck2.Test.make ~name:"agent decoder requires branch and PR status"
    ~count:100 gen_patch_agent_fully_populated (fun agent ->
      let json = Onton.Persistence.patch_agent_to_yojson agent in
      Result.is_error
        (Onton.Persistence.patch_agent_of_yojson (remove_field "branch" json))
      && Result.is_error
           (Onton.Persistence.patch_agent_of_yojson
              (remove_field "pr_status" json)))

let current_version_is_required =
  QCheck2.Test.make ~name:"snapshot schema is explicitly version 7" ~count:100
    gen_snapshot (fun snapshot ->
      let json = Onton.Persistence.snapshot_to_yojson snapshot in
      let has_version =
        match json with
        | `Assoc fields ->
            List.Assoc.find fields ~equal:String.equal "version"
            |> Option.value_map ~default:false ~f:(Yojson.Safe.equal (`Int 7))
        | _ -> false
      in
      has_version
      && Result.is_error
           (Onton.Persistence.snapshot_of_yojson
              (replace_field "version" (`Int 1) json)))

let outbox_survives_restart =
  QCheck2.Test.make ~name:"pending outbox work survives snapshot restart"
    QCheck2.Gen.unit (fun () ->
      let patch =
        {
          Patch.id = Patch_id.of_string "durable";
          branch = Branch.of_string "onton/durable";
          goal = "prove durable controller work";
          dependencies = [];
          files = [ "lib/durable.ml" ];
          checks =
            [ { Check.run = "dune build"; proves = "the project builds" } ];
        }
      in
      let gameplan =
        {
          Gameplan.project_name = "durable";
          repo_owner = "owner";
          repo_name = "repo";
          patches = [ patch ];
        }
      in
      let orchestrator =
        Onton.Orchestrator.create ~patches:[ patch ]
          ~main_branch:(Branch.of_string "main")
      in
      let direct_message = "Keep the public API stable." in
      let orchestrator =
        Onton.Orchestrator.send_human_message orchestrator patch.id
          direct_message
      in
      let orchestrator =
        Onton.Patch_controller.plan_tick_messages orchestrator
          ~project_name:gameplan.project_name ~gameplan
        |> fst
      in
      let snapshot =
        {
          Onton.Runtime.orchestrator;
          activity_log = Onton_core.Activity_log.empty;
          gameplan;
          transcripts = Hashtbl.create (module Patch_id);
        }
      in
      match
        snapshot |> Onton.Persistence.snapshot_to_yojson
        |> Onton.Persistence.snapshot_of_yojson
      with
      | Error _ -> false
      | Ok restored -> (
          let runnable =
            Onton.Orchestrator.runnable_messages restored.orchestrator
          in
          List.equal Onton.Orchestrator.equal_patch_agent_message
            (Onton.Orchestrator.runnable_messages orchestrator)
            runnable
          &&
          match runnable with
          | [ message ] ->
              List.equal String.equal
                (Onton.Orchestrator.message_payload message)
                [ direct_message ]
          | _ -> false))

let validation_counter_roundtrip_and_legacy_default =
  QCheck2.Test.make
    ~name:"validation failure count persists and defaults to zero for v7 state"
    QCheck2.Gen.unit (fun () ->
      let agent =
        Onton_core.Patch_agent.create
          ~branch:(Branch.of_string "onton/durable")
          (Patch_id.of_string "durable")
        |> Onton_core.Patch_agent.increment_validation_failure_count
        |> Onton_core.Patch_agent.increment_validation_failure_count
      in
      let json = Onton.Persistence.patch_agent_to_yojson agent in
      let persisted =
        match Onton.Persistence.patch_agent_of_yojson json with
        | Ok restored -> restored.validation_failure_count = 2
        | Error _ -> false
      in
      let legacy =
        match
          Onton.Persistence.patch_agent_of_yojson
            (remove_field "validation_failure_count" json)
        with
        | Ok restored -> restored.validation_failure_count = 0
        | Error _ -> false
      in
      persisted && legacy)

let legacy_outbox_payload_defaults_empty =
  QCheck2.Test.make ~name:"v7 outbox messages without payload restore empty"
    QCheck2.Gen.unit (fun () ->
      let patch =
        {
          Patch.id = Patch_id.of_string "legacy-payload";
          branch = Branch.of_string "onton/legacy-payload";
          goal = "restore old outbox";
          dependencies = [];
          files = [];
          checks = [];
        }
      in
      let gameplan =
        {
          Gameplan.project_name = "legacy";
          repo_owner = "owner";
          repo_name = "repo";
          patches = [ patch ];
        }
      in
      let orchestrator =
        Onton.Orchestrator.create ~patches:[ patch ]
          ~main_branch:(Branch.of_string "main")
        |> fun orch ->
        Onton.Orchestrator.send_human_message orch patch.id "new payload"
        |> fun orch ->
        Onton.Patch_controller.plan_tick_messages orch
          ~project_name:gameplan.project_name ~gameplan
        |> fst
      in
      let snapshot =
        {
          Onton.Runtime.orchestrator;
          activity_log = Onton_core.Activity_log.empty;
          gameplan;
          transcripts = Hashtbl.create (module Patch_id);
        }
      in
      let legacy_json =
        Onton.Persistence.snapshot_to_yojson snapshot
        |> remove_outbox_message_field "payload"
      in
      match Onton.Persistence.snapshot_of_yojson legacy_json with
      | Error _ -> false
      | Ok restored -> (
          match Onton.Orchestrator.all_messages restored.orchestrator with
          | [ message ] ->
              List.is_empty (Onton.Orchestrator.message_payload message)
          | _ -> false))

let () =
  let exit_code =
    QCheck_base_runner.run_tests ~verbose:true
      [
        snapshot_roundtrip;
        file_roundtrip;
        agent_roundtrip;
        pr_status_roundtrip;
        strict_pr_status_rejects_legacy_shapes;
        strict_agent_requires_identity_fields;
        current_version_is_required;
        outbox_survives_restart;
        validation_counter_roundtrip_and_legacy_default;
        legacy_outbox_payload_defaults_empty;
      ]
  in
  if exit_code <> 0 then Stdlib.exit exit_code
