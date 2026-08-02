let () =
  let patches =
    [
      Onton_core.Types.Patch.
        {
          id = Onton_core.Types.Patch_id.of_string "1";
          goal = "Test patch completes";
          branch = Onton_core.Types.Branch.of_string "test-1";
          dependencies = [];
          files = [];
          checks = [];
          agent = None;
        };
    ]
  in
  let main_branch = Onton_core.Types.Branch.of_string "main" in
  let orch = Onton.Orchestrator.create ~patches ~main_branch in
  let gameplan =
    Onton_core.Types.Gameplan.
      {
        project_name = "test-project";
        repo_owner = "";
        repo_name = "";
        patches;
      }
  in
  let _orch, actions =
    Onton.Patch_controller.tick orch ~project_name:"test-project" ~gameplan
  in
  (match actions with
  | [ Onton.Orchestrator.Start (pid, base) ] ->
      assert (
        Onton_core.Types.Patch_id.equal pid
          (Onton_core.Types.Patch_id.of_string "1"));
      assert (Onton_core.Types.Branch.equal base main_branch)
  | [] | _ :: _ :: _
  | [ Onton.Orchestrator.Respond _ ]
  | [ Onton.Orchestrator.Rebase _ ] ->
      failwith "expected exactly one Start action for patch \"1\"");
  print_endline "all tests passed"
