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
let parse text = Gameplan_parser.parse_json_string text

let () =
  let valid =
    {|{"project":"p","repository":"owner/repo","patches":[{"id":"a","goal":"g","dependsOn":[],"files":["a"],"checks":[{"run":"x","proves":"y"}],"agent":{"backend":"codex","model":"  gpt-5.6-sol  "}}]}|}
  in
  match parse valid with
  | Ok parsed -> (
      match parsed.Gameplan_parser.gameplan.Types.Gameplan.patches with
      | [ (patch : Types.Patch.t) ] -> (
          let expected : Types.Patch.Agent.t =
            { Types.Patch.Agent.backend = "codex"; model = "gpt-5.6-sol" }
          in
          match patch.Types.Patch.agent with
          | Some actual when Types.Patch.Agent.equal actual expected -> ()
          | Some _ | None -> failwith "agent override did not decode strictly")
      | _ -> failwith "agent override did not decode strictly")
  | Error _ -> failwith "agent override did not decode strictly"

let () =
  let invalid_agents =
    [
      {|{"backend":"codex"}|};
      {|{"model":"gpt-5"}|};
      {|{"backend":" ","model":"gpt-5"}|};
      {|{"backend":"codex","model":" "}|};
      {|{"backend":"unknown","model":"gpt-5"}|};
      {|{"backend":"codex","model":"gpt-5","extra":true}|};
    ]
  in
  List.iter invalid_agents ~f:(fun agent ->
      let json =
        Printf.sprintf
          {|{"project":"p","repository":"owner/repo","patches":[{"id":"a","goal":"g","dependsOn":[],"files":["a"],"checks":[{"run":"x","proves":"y"}],"agent":%s}]}|}
          agent
      in
      match parse json with
      | Error _ -> ()
      | Ok _ -> failwith "invalid patch agent override was accepted")
