open Base

type t = {
  gameplan : Types.Gameplan.t;
  dependency_graph : Types.Patch_id.t list Map.M(Types.Patch_id).t;
}

exception Parse_error of string

let fail where message = raise (Parse_error (where ^ ": " ^ message))

let object_fields ~where = function
  | `Assoc fields -> fields
  | _ -> fail where "must be an object"

let reject_unknown ~where ~allowed fields =
  match
    List.find fields ~f:(fun (key, _) ->
        not (List.mem allowed key ~equal:String.equal))
  with
  | None -> ()
  | Some (key, _) -> fail where (Printf.sprintf "unknown field %S" key)

let field ~where fields key =
  match List.Assoc.find fields key ~equal:String.equal with
  | Some value -> value
  | None -> fail where (Printf.sprintf "missing required field %S" key)

let nonempty_string ~where = function
  | `String value ->
      let value = String.strip value in
      if String.is_empty value then fail where "must not be empty" else value
  | _ -> fail where "must be a string"

let array ~where = function
  | `List values -> values
  | _ -> fail where "must be an array"

let is_valid_patch_id value =
  (not (String.is_empty value))
  && String.for_all value ~f:(fun c ->
      Char.is_alphanum c || Char.equal c '-' || Char.equal c '_')

let patch_id ~where json =
  let value = nonempty_string ~where json in
  if is_valid_patch_id value then Types.Patch_id.of_string value
  else fail where "must contain only letters, digits, '-' or '_'"

let unique_strings ~where values =
  let rec loop seen acc index = function
    | [] -> List.rev acc
    | json :: rest ->
        let item =
          nonempty_string ~where:(Printf.sprintf "%s[%d]" where index) json
        in
        if Set.mem seen item then
          fail where (Printf.sprintf "contains duplicate value %S" item)
        else loop (Set.add seen item) (item :: acc) (index + 1) rest
  in
  loop (Set.empty (module String)) [] 0 values

let validate_repo_path ~where path =
  if Stdlib.Filename.is_relative path |> not then
    fail where "must be repository-relative";
  if
    String.split path ~on:'/'
    |> List.exists ~f:(fun segment ->
        String.is_empty segment || String.equal segment "..")
  then fail where "must not contain empty or '..' path segments"

let parse_check ~where json =
  let fields = object_fields ~where json in
  reject_unknown ~where ~allowed:[ "run"; "proves" ] fields;
  let run =
    nonempty_string ~where:(where ^ ".run") (field ~where fields "run")
  in
  let proves =
    nonempty_string ~where:(where ^ ".proves") (field ~where fields "proves")
  in
  { Types.Check.run; proves }

let parse_checks ~where json =
  let values = array ~where json in
  if List.is_empty values then fail where "must contain at least one check";
  List.mapi values ~f:(fun index value ->
      parse_check ~where:(Printf.sprintf "%s[%d]" where index) value)

let parse_repository json =
  let repository = nonempty_string ~where:"repository" json in
  match String.split repository ~on:'/' with
  | [ owner; repo ]
    when (not (String.is_empty (String.strip owner)))
         && not (String.is_empty (String.strip repo)) ->
      (String.strip owner, String.strip repo)
  | _ -> fail "repository" "must have the form owner/repo"

type parsed_patch = {
  id : Types.Patch_id.t;
  goal : string;
  dependencies : Types.Patch_id.t list;
  files : string list;
  checks : Types.Check.t list;
}

let parse_patch ~index json =
  let where = Printf.sprintf "patches[%d]" index in
  let fields = object_fields ~where json in
  reject_unknown ~where
    ~allowed:[ "id"; "goal"; "dependsOn"; "files"; "checks" ]
    fields;
  let id = patch_id ~where:(where ^ ".id") (field ~where fields "id") in
  let goal =
    nonempty_string ~where:(where ^ ".goal") (field ~where fields "goal")
  in
  let dependencies =
    array ~where:(where ^ ".dependsOn") (field ~where fields "dependsOn")
    |> List.mapi ~f:(fun dependency_index value ->
        patch_id
          ~where:(Printf.sprintf "%s.dependsOn[%d]" where dependency_index)
          value)
  in
  let dependencies =
    let seen = Hash_set.create (module Types.Patch_id) in
    List.map dependencies ~f:(fun dependency ->
        if Hash_set.mem seen dependency then
          fail (where ^ ".dependsOn")
            (Printf.sprintf "contains duplicate patch %s"
               (Types.Patch_id.to_string dependency));
        Hash_set.add seen dependency;
        dependency)
  in
  let files =
    array ~where:(where ^ ".files") (field ~where fields "files")
    |> unique_strings ~where:(where ^ ".files")
  in
  if List.is_empty files then fail (where ^ ".files") "must not be empty";
  List.iteri files ~f:(fun file_index path ->
      validate_repo_path
        ~where:(Printf.sprintf "%s.files[%d]" where file_index)
        path);
  let checks =
    parse_checks ~where:(where ^ ".checks") (field ~where fields "checks")
  in
  { id; goal; dependencies; files; checks }

let detect_cycle dependency_graph =
  let visited = Hash_set.create (module Types.Patch_id) in
  let active = Hash_set.create (module Types.Patch_id) in
  let rec visit path id =
    if Hash_set.mem active id then
      fail "patches"
        (Printf.sprintf "dependency cycle: %s"
           (List.rev (id :: path)
           |> List.map ~f:Types.Patch_id.to_string
           |> String.concat ~sep:" -> "));
    if not (Hash_set.mem visited id) then (
      Hash_set.add active id;
      Map.find dependency_graph id
      |> Option.value ~default:[]
      |> List.iter ~f:(visit (id :: path));
      Hash_set.remove active id;
      Hash_set.add visited id)
  in
  Map.iter_keys dependency_graph ~f:(visit [])

let validate_graph patches =
  let ids = Hash_set.create (module Types.Patch_id) in
  List.iter patches ~f:(fun patch ->
      if Hash_set.mem ids patch.id then
        fail "patches"
          (Printf.sprintf "duplicate patch id %s"
             (Types.Patch_id.to_string patch.id));
      Hash_set.add ids patch.id);
  List.iter patches ~f:(fun patch ->
      List.iter patch.dependencies ~f:(fun dependency ->
          if Types.Patch_id.equal patch.id dependency then
            fail "patches"
              (Printf.sprintf "patch %s depends on itself"
                 (Types.Patch_id.to_string patch.id));
          if not (Hash_set.mem ids dependency) then
            fail "patches"
              (Printf.sprintf "patch %s depends on unknown patch %s"
                 (Types.Patch_id.to_string patch.id)
                 (Types.Patch_id.to_string dependency))));
  let graph =
    List.fold patches
      ~init:(Map.empty (module Types.Patch_id))
      ~f:(fun graph patch ->
        Map.set graph ~key:patch.id ~data:patch.dependencies)
  in
  detect_cycle graph;
  graph

let parse_json json =
  let where = "plan" in
  let fields = object_fields ~where json in
  reject_unknown ~where ~allowed:[ "project"; "repository"; "patches" ] fields;
  let project_name =
    nonempty_string ~where:"project" (field ~where fields "project")
  in
  if String.is_empty (Types.Gameplan.slugify project_name) then
    fail "project" "must contain a letter or digit";
  let repo_owner, repo_name =
    parse_repository (field ~where fields "repository")
  in
  let parsed_patches =
    array ~where:"patches" (field ~where fields "patches")
    |> List.mapi ~f:(fun index value -> parse_patch ~index value)
  in
  if List.is_empty parsed_patches then fail "patches" "must not be empty";
  let dependency_graph = validate_graph parsed_patches in
  let slug = Types.Gameplan.slugify project_name in
  let patches =
    List.map parsed_patches ~f:(fun patch ->
        {
          Types.Patch.id = patch.id;
          goal = patch.goal;
          branch =
            Types.Branch.of_string
              (slug ^ "/patch-" ^ Types.Patch_id.to_string patch.id);
          dependencies = patch.dependencies;
          files = patch.files;
          checks = patch.checks;
        })
  in
  {
    gameplan = { Types.Gameplan.project_name; repo_owner; repo_name; patches };
    dependency_graph;
  }

let parse_json_string input =
  match Yojson.Safe.from_string input with
  | exception Yojson.Json_error message -> Error ("JSON parse error: " ^ message)
  | json -> (
      try Ok (parse_json json) with Parse_error message -> Error message)

let valid_example =
  {|
  {
    "project": "minimal-plan",
    "repository": "flowglad/onton",
    "patches": [
      {
        "id": "core",
        "goal": "The executor consumes a minimal plan",
        "dependsOn": [],
        "files": ["lib_core/gameplan_parser.ml"],
        "checks": [
          { "run": "dune runtest", "proves": "the minimal plan parses" }
        ]
      }
    ]
  }
  |}

let%test "minimal plan parses" = Result.is_ok (parse_json_string valid_example)

let%test "legacy proof-shaped plan is rejected" =
  String.is_substring
    (Result.error (parse_json_string {|{"projectName":"legacy"}|})
    |> Option.value ~default:"")
    ~substring:"unknown field"

let%test "plan requires executable checks" =
  String.is_substring
    (Result.error
       (parse_json_string
          {|
          {
            "project": "bad",
            "repository": "flowglad/onton",
            "patches": [
              {
                "id": "unchecked",
                "goal": "an unchecked patch is rejected",
                "dependsOn": [],
                "files": ["lib/unchecked.ml"],
                "checks": []
              }
            ]
          }
          |})
    |> Option.value ~default:"")
    ~substring:"at least one check"
