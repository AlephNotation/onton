open Base

type t = {
  default_backend : string option;
  default_model : string option;
  review_team : string option;
  review_backends : Review_backend.t list;
}

let empty =
  {
    default_backend = None;
    default_model = None;
    review_team = None;
    review_backends = [];
  }

let config_path ~config_dir = Stdlib.Filename.concat config_dir "config.json"

let optional_string json field =
  match Json.field field json with
  | None -> Ok None
  | Some (`String value) ->
      let value = String.strip value in
      Ok (if String.is_empty value then None else Some value)
  | Some _ -> Error (field ^ " must be a string")

let parse_default ~known_backends json =
  match json with
  | `Null -> Ok (None, None)
  | `Assoc fields ->
      let unknown =
        List.filter_map fields ~f:(fun (name, _) ->
            if String.equal name "backend" || String.equal name "model" then
              None
            else Some name)
      in
      if not (List.is_empty unknown) then
        Error
          (Printf.sprintf "default contains unknown field(s): %s"
             (String.concat ~sep:", " unknown))
      else
        Result.bind (optional_string json "backend") ~f:(fun backend ->
            match backend with
            | Some name
              when not (List.mem known_backends name ~equal:String.equal) ->
                Error
                  (Printf.sprintf
                     "default.backend = %S is not a known backend (expected \
                      one of: %s)"
                     name
                     (String.concat ~sep:", " known_backends))
            | Some _ | None ->
                Result.map (optional_string json "model") ~f:(fun model ->
                    (backend, model)))
  | _ -> Error "default must be an object {backend, model}"

let parse_string ~known_backends ?(known_review_kinds = [ "review-service" ])
    raw =
  match Yojson.Safe.from_string raw with
  | exception Yojson.Json_error message ->
      Error (Printf.sprintf "config.json: %s" message)
  | `Assoc fields as json ->
      let allowed = [ "default"; "review_team"; "reviewBackends" ] in
      let unknown =
        List.filter_map fields ~f:(fun (name, _) ->
            if List.mem allowed name ~equal:String.equal then None
            else Some name)
      in
      if not (List.is_empty unknown) then
        Error
          (Printf.sprintf "config.json contains unknown field(s): %s"
             (String.concat ~sep:", " unknown))
      else
        let default_json =
          Option.value (Json.field "default" json) ~default:`Null
        in
        let review_backends_json =
          Option.value (Json.field "reviewBackends" json) ~default:`Null
        in
        Result.bind (parse_default ~known_backends default_json)
          ~f:(fun (default_backend, default_model) ->
            Result.bind (optional_string json "review_team")
              ~f:(fun review_team ->
                Result.map
                  (Review_backend.parse_array ~known_kinds:known_review_kinds
                     review_backends_json) ~f:(fun review_backends ->
                    {
                      default_backend;
                      default_model;
                      review_team;
                      review_backends;
                    })))
  | _ -> Error "config.json: top-level value must be an object"

let load ~config_dir ~known_backends ?known_review_kinds () =
  let path = config_path ~config_dir in
  if not (Stdlib.Sys.file_exists path) then Ok empty
  else
    try
      let input = Stdlib.In_channel.open_text path in
      let raw =
        Exn.protect
          ~finally:(fun () -> Stdlib.In_channel.close input)
          ~f:(fun () -> Stdlib.In_channel.input_all input)
      in
      parse_string ~known_backends ?known_review_kinds raw
    with Sys_error message ->
      Error (Printf.sprintf "config.json: cannot read: %s" message)

let%test "empty object yields empty config" =
  match parse_string ~known_backends:[ "claude" ] "{}" with
  | Ok config -> Poly.equal config empty
  | Error _ -> false

let%test "unknown top-level fields are rejected" =
  Result.is_error (parse_string ~known_backends:[ "claude" ] {|{"routing":{}}|})

let%test "default backend and model parse" =
  match
    parse_string ~known_backends:[ "claude" ]
      {|{"default":{"backend":"claude","model":"sonnet"}}|}
  with
  | Ok config ->
      Option.equal String.equal config.default_backend (Some "claude")
      && Option.equal String.equal config.default_model (Some "sonnet")
  | Error _ -> false
