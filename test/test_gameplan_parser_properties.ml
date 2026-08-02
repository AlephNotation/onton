open Base
open Onton_core

let parses_generated_valid_plan =
  QCheck2.Test.make ~name:"valid generated single-patch plans parse" ~count:300
    QCheck2.Gen.(pair nat_small nat_small)
    (fun (project_suffix, patch_suffix) ->
      let project = Printf.sprintf "project-%d" project_suffix in
      let patch = Printf.sprintf "patch-%d" patch_suffix in
      let json =
        Printf.sprintf
          {|{"project":%S,"repository":"owner/repo","patches":[{"id":%S,"goal":"work","dependsOn":[],"files":["lib/work.ml"],"checks":[{"run":"dune build","proves":"build passes"}]}]}|}
          project patch
      in
      match Gameplan_parser.parse_json_string json with
      | Error _ -> false
      | Ok parsed ->
          let gameplan = parsed.Gameplan_parser.gameplan in
          String.equal gameplan.Types.Gameplan.project_name project
          && List.length gameplan.Types.Gameplan.patches = 1)

let () = QCheck2.Test.check_exn parses_generated_valid_plan
