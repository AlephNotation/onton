open Onton_core

let () =
  Eio_main.run @@ fun _env ->
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
            agent = None;
          };
        ];
    }
  in
  let policy =
    Some
      {
        Types.Expansion_policy.max_patches = 2;
        files = [ "lib/b.ml" ];
        checks = [ { Types.Check.run = "dune build"; proves = "build" } ];
      }
  in
  let proposal =
    match
      Plan_expansion.parse_json_string
        {|{"patches":[{"id":"child","goal":"child","dependsOn":[],"files":["lib/b.ml"],"checks":[{"run":"dune build","proves":"build"}]}]}|}
    with
    | Ok proposal -> proposal
    | Error message -> failwith message
  in
  let main = Types.Branch.of_string "main" in
  let runtime =
    Onton.Runtime.create ~gameplan:seed ~main_branch:main
      ~durable_store:(fun _ -> Ok ())
      ()
  in
  (match
     Onton.Runtime.commit_expansion runtime ~policy
       ~parent_id:(Types.Patch_id.of_string "seed")
       proposal
   with
  | Ok true -> ()
  | Ok false | Error _ -> failwith "expansion was not atomically accepted");
  assert (
    Onton.Runtime.read runtime (fun snapshot ->
        List.length snapshot.Onton.Runtime.gameplan.Types.Gameplan.patches = 2));
  let failing =
    Onton.Runtime.create ~gameplan:seed ~main_branch:main
      ~durable_store:(fun _ -> Error "disk")
      ()
  in
  assert (
    Result.is_error
      (Onton.Runtime.commit_expansion failing ~policy
         ~parent_id:(Types.Patch_id.of_string "seed")
         proposal));
  assert (
    Onton.Runtime.read failing (fun snapshot ->
        List.length snapshot.Onton.Runtime.gameplan.Types.Gameplan.patches = 1))
