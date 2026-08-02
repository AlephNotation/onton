open Base

type decision = { backend : string; model : string option }

let supported_backends =
  [ "claude"; "codex"; "opencode"; "pi"; "gemini"; "patch-agent" ]

let is_supported_backend backend =
  List.mem supported_backends backend ~equal:String.equal

let decide ~backend ~model = { backend; model }

let for_patch ~default (patch : Types.Patch.t) =
  match patch.agent with
  | None -> default
  | Some { Types.Patch.Agent.backend; model } -> { backend; model = Some model }

let distinct_effective_backends ~default patches =
  List.fold patches
    ~init:(Set.singleton (module String) default.backend)
    ~f:(fun backends patch ->
      Set.add backends (for_patch ~default patch).backend)
  |> Set.to_list

let first_nonempty (values : string list) ~default =
  match
    List.find_map values ~f:(fun value ->
        let value = String.strip value in
        if String.is_empty value then None else Some value)
  with
  | Some value -> value
  | None -> default

let resolve_pair ~cli_backend ~cli_model ~stored_backend ~stored_model
    ~(repo_config : Repo_config.t) ~built_in_backend =
  let configured_backend =
    Option.value repo_config.Repo_config.default_backend ~default:""
  in
  let configured_model =
    Option.value repo_config.Repo_config.default_model ~default:""
  in
  let backend =
    first_nonempty
      [ cli_backend; stored_backend; configured_backend ]
      ~default:built_in_backend
  in
  let model =
    first_nonempty [ cli_model; stored_model; configured_model ] ~default:""
  in
  (backend, model)

let empty_config = Repo_config.empty

let config_with ?(default_backend = None) ?(default_model = None) () =
  { Repo_config.empty with default_backend; default_model }

let%test "resolve_pair: CLI wins" =
  let backend, model =
    resolve_pair ~cli_backend:"codex" ~cli_model:"gpt-5"
      ~stored_backend:"claude" ~stored_model:"sonnet"
      ~repo_config:
        (config_with ~default_backend:(Some "gemini")
           ~default_model:(Some "gemini-2.5-pro") ())
      ~built_in_backend:"claude"
  in
  String.equal backend "codex" && String.equal model "gpt-5"

let%test "resolve_pair: stored values beat repository defaults" =
  let backend, model =
    resolve_pair ~cli_backend:"" ~cli_model:"" ~stored_backend:"claude"
      ~stored_model:"sonnet"
      ~repo_config:
        (config_with ~default_backend:(Some "codex")
           ~default_model:(Some "gpt-5") ())
      ~built_in_backend:"claude"
  in
  String.equal backend "claude" && String.equal model "sonnet"

let%test "resolve_pair: repository defaults beat built-in defaults" =
  let backend, model =
    resolve_pair ~cli_backend:"" ~cli_model:"" ~stored_backend:""
      ~stored_model:""
      ~repo_config:
        (config_with ~default_backend:(Some "codex")
           ~default_model:(Some "gpt-5") ())
      ~built_in_backend:"claude"
  in
  String.equal backend "codex" && String.equal model "gpt-5"

let%test "resolve_pair: model may remain unset" =
  let backend, model =
    resolve_pair ~cli_backend:"" ~cli_model:"" ~stored_backend:""
      ~stored_model:"" ~repo_config:empty_config ~built_in_backend:"claude"
  in
  String.equal backend "claude" && String.is_empty model

let%test "resolve_pair: fields resolve independently and trim whitespace" =
  let backend, model =
    resolve_pair ~cli_backend:"  codex  " ~cli_model:"" ~stored_backend:"claude"
      ~stored_model:""
      ~repo_config:(config_with ~default_model:(Some "  gpt-5  ") ())
      ~built_in_backend:"claude"
  in
  String.equal backend "codex" && String.equal model "gpt-5"
