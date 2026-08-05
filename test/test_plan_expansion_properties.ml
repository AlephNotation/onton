open Base
open Onton_core

let seed =
  {
    Types.Gameplan.project_name = "grow";
    repo_owner = "owner";
    repo_name = "repo";
    patches =
      [
        {
          Types.Patch.id = Types.Patch_id.of_string "seed";
          goal = "seed";
          branch = Types.Branch.of_string "grow/patch-seed";
          dependencies = [];
          files = [ "lib/a.ml" ];
          checks = [ { Types.Check.run = "dune build"; proves = "build" } ];
          agent = Some { Types.Patch.Agent.backend = "codex"; model = "m" };
        };
      ];
  }

let policy =
  Some
    {
      Types.Expansion_policy.max_patches = 3;
      files = [ "lib/b.ml" ];
      checks = [ { Types.Check.run = "dune build"; proves = "build" } ];
    }

let proposal =
  {|{"patches":[{"id":"child","goal":"child","dependsOn":[],"files":["lib/b.ml"],"checks":[{"run":"dune build","proves":"build"}]}]}|}

let parsed_proposal () =
  match Plan_expansion.parse_json_string proposal with
  | Ok value -> value
  | Error message -> failwith message

let () =
  let parent_id = Types.Patch_id.of_string "seed" in
  let first =
    Plan_expansion.materialize ~gameplan:seed ~policy ~parent_id
      (parsed_proposal ())
  in
  match first with
  | Error message -> failwith message
  | Ok result -> (
      assert result.Plan_expansion.changed;
      assert (List.length result.Plan_expansion.patches = 2);
      let replay =
        Plan_expansion.materialize
          ~gameplan:
            { seed with Types.Gameplan.patches = result.Plan_expansion.patches }
          ~policy ~parent_id (parsed_proposal ())
      in
      match replay with
      | Ok { Plan_expansion.changed = false; _ } -> ()
      | Ok _ | Error _ -> failwith "identical replay was not a no-op")

let () =
  let forbidden =
    {|{"patches":[{"id":"child","goal":"child","dependsOn":[],"files":["lib/no.ml"],"checks":[{"run":"dune build","proves":"build"}]}]}|}
  in
  match Plan_expansion.parse_json_string forbidden with
  | Error _ -> failwith "proposal syntax unexpectedly failed"
  | Ok value ->
      assert (
        Result.is_error
          (Plan_expansion.materialize ~gameplan:seed ~policy
             ~parent_id:(Types.Patch_id.of_string "seed")
             value))
