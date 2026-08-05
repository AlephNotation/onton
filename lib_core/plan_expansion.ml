open Base
open Types

type proposal_patch = {
  id : string;
  goal : string;
  dependencies : string list;
  files : string list;
  checks : Check.t list;
}
[@@deriving show, eq, compare, sexp_of]

type proposal = proposal_patch list [@@deriving show, eq, compare, sexp_of]

type materialization = { patches : Patch.t list; changed : bool }
[@@deriving show, eq, compare, sexp_of]

exception Invalid of string

let fail s = raise (Invalid s)

let fields ~where = function
  | `Assoc xs -> xs
  | _ -> fail (where ^ " must be an object")

let get ~where xs key =
  match List.Assoc.find xs key ~equal:String.equal with
  | Some v -> v
  | None -> fail (where ^ ": missing " ^ key)

let unknown ~where ~allowed xs =
  List.iter xs ~f:(fun (k, _) ->
      if not (List.mem allowed k ~equal:String.equal) then
        fail (where ^ ": unknown field " ^ k))

let string ~where = function
  | `String s when not (String.is_empty (String.strip s)) -> String.strip s
  | _ -> fail (where ^ " must be a nonempty string")

let array ~where = function
  | `List xs -> xs
  | _ -> fail (where ^ " must be an array")

let valid_id s =
  (not (String.is_empty s))
  && String.for_all s ~f:(fun c ->
      Char.is_alphanum c || Char.equal c '-' || Char.equal c '_')

let path ~where p =
  if
    (not (Stdlib.Filename.is_relative p))
    || String.equal p ".."
    || List.exists (String.split p ~on:'/') ~f:(fun x ->
        String.is_empty x || String.equal x ".." || String.equal x ".")
  then fail (where ^ " must be a repository-relative path")

let unique ~where xs =
  let seen = Hash_set.create (module String) in
  List.iter xs ~f:(fun x ->
      if Hash_set.mem seen x then fail (where ^ " contains duplicates")
      else Hash_set.add seen x)

let check ~where j =
  let xs = fields ~where j in
  unknown ~where ~allowed:[ "run"; "proves" ] xs;
  {
    Check.run = string ~where:(where ^ ".run") (get ~where xs "run");
    proves = string ~where:(where ^ ".proves") (get ~where xs "proves");
  }

let checks ~where j =
  let xs =
    array ~where j
    |> List.mapi ~f:(fun i x ->
        check ~where:(Printf.sprintf "%s[%d]" where i) x)
  in
  if List.is_empty xs then fail (where ^ " must not be empty");
  List.iteri xs ~f:(fun index check ->
      if List.exists (List.take xs index) ~f:(Check.equal check) then
        fail (where ^ " contains duplicate checks"));
  xs

let proposal_patch ~index j =
  let where = Printf.sprintf "patches[%d]" index in
  let xs = fields ~where j in
  unknown ~where ~allowed:[ "id"; "goal"; "dependsOn"; "files"; "checks" ] xs;
  let id = string ~where:(where ^ ".id") (get ~where xs "id") in
  if not (valid_id id) then fail (where ^ ".id is malformed");
  let dependencies =
    array ~where:(where ^ ".dependsOn") (get ~where xs "dependsOn")
    |> List.mapi ~f:(fun i x ->
        string ~where:(Printf.sprintf "%s.dependsOn[%d]" where i) x)
  in
  unique ~where:(where ^ ".dependsOn") dependencies;
  let files =
    array ~where:(where ^ ".files") (get ~where xs "files")
    |> List.mapi ~f:(fun i x ->
        string ~where:(Printf.sprintf "%s.files[%d]" where i) x)
  in
  unique ~where:(where ^ ".files") files;
  if List.is_empty files then fail (where ^ ".files must not be empty");
  List.iter files ~f:(path ~where:(where ^ ".files"));
  {
    id;
    goal = string ~where:(where ^ ".goal") (get ~where xs "goal");
    dependencies;
    files;
    checks = checks ~where:(where ^ ".checks") (get ~where xs "checks");
  }

let parse_json_string input =
  try
    let root = Yojson.Safe.from_string input in
    let xs = fields ~where:"proposal" root in
    unknown ~where:"proposal" ~allowed:[ "patches" ] xs;
    let ps =
      array ~where:"proposal.patches" (get ~where:"proposal" xs "patches")
      |> List.mapi ~f:(fun index json -> proposal_patch ~index json)
    in
    if List.is_empty ps then fail "proposal.patches must not be empty";
    unique ~where:"proposal.patches ids" (List.map ps ~f:(fun p -> p.id));
    Ok ps
  with
  | Yojson.Json_error e -> Error ("JSON parse error: " ^ e)
  | Invalid e -> Error e

let reaches deps from target =
  let rec visit seen node =
    if String.equal node target then true
    else if Set.mem seen node then false
    else
      List.exists
        (Map.find deps node |> Option.value ~default:[])
        ~f:(visit (Set.add seen node))
  in
  visit (Set.empty (module String)) from

let materialize ~(gameplan : Gameplan.t) ~policy ~parent_id proposal =
  match policy with
  | None -> Error "plan expansion is not authorized"
  | Some (policy : Expansion_policy.t) -> (
      try
        let parent =
          match
            List.find gameplan.patches ~f:(fun p ->
                Patch_id.equal p.id parent_id)
          with
          | Some p -> p
          | None -> fail "unknown emitting parent"
        in
        let locals = List.map proposal ~f:(fun p -> p.id) in
        List.iter proposal ~f:(fun p ->
            List.iter p.dependencies ~f:(fun d ->
                if not (List.mem locals d ~equal:String.equal) then
                  fail "sibling dependency is unknown"));
        let deps =
          List.fold proposal
            ~init:(Map.empty (module String))
            ~f:(fun m p -> Map.set m ~key:p.id ~data:p.dependencies)
        in
        List.iter locals ~f:(fun id ->
            if
              List.exists
                (Map.find deps id |> Option.value ~default:[])
                ~f:(fun dependency -> reaches deps dependency id)
            then fail "proposal dependency cycle");
        List.iter proposal ~f:(fun p ->
            List.iter p.files ~f:(fun f ->
                if not (List.mem policy.files f ~equal:String.equal) then
                  fail "file is outside expansion authority");
            List.iter p.checks ~f:(fun c ->
                if not (List.mem policy.checks c ~equal:Check.equal) then
                  fail "check is outside expansion authority"));
        List.iter proposal ~f:(fun a ->
            List.iter proposal ~f:(fun b ->
                if
                  (not (String.equal a.id b.id))
                  && List.exists a.files ~f:(fun f ->
                      List.mem b.files f ~equal:String.equal)
                  && not (reaches deps a.id b.id || reaches deps b.id a.id)
                then fail "overlapping sibling scopes are unordered"));
        let derived (p : proposal_patch) =
          {
            Patch.id =
              Patch_id.of_string (Patch_id.to_string parent_id ^ "--" ^ p.id);
            goal = p.goal;
            branch =
              Branch.of_string
                (Gameplan.slugify gameplan.project_name
                ^ "/patch-"
                ^ Patch_id.to_string parent_id
                ^ "--" ^ p.id);
            dependencies =
              (if List.is_empty p.dependencies then [ parent_id ]
               else
                 List.map p.dependencies ~f:(fun d ->
                     Patch_id.of_string (Patch_id.to_string parent_id ^ "--" ^ d)));
            files = p.files;
            checks = p.checks;
            agent = parent.agent;
          }
        in
        let candidates = List.map proposal ~f:derived in
        List.iter candidates ~f:(fun p ->
            match
              List.find gameplan.patches ~f:(fun old ->
                  Patch_id.equal old.id p.id)
            with
            | None -> ()
            | Some old ->
                if not (Patch.equal old p) then
                  fail "derived patch id conflicts with different content");
        let fresh =
          List.filter candidates ~f:(fun p ->
              not
                (List.exists gameplan.patches ~f:(fun old ->
                     Patch_id.equal old.id p.id)))
        in
        if List.length gameplan.patches + List.length fresh > policy.max_patches
        then fail "expansion exceeds maxPatches";
        Ok
          {
            patches = gameplan.patches @ fresh;
            changed = not (List.is_empty fresh);
          }
      with
      | Invalid e -> Error e
      | _ -> Error "invalid expansion materialization")
