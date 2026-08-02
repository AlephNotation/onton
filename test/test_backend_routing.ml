open Base
open Onton_core

let known_backends = Backend_routing.supported_backends

let optional_model =
  QCheck2.Gen.oneof
    [
      QCheck2.Gen.return None;
      QCheck2.Gen.map
        (fun model -> Some model)
        (QCheck2.Gen.oneof_list [ "sonnet"; "opus"; "gpt-5"; "auto" ]);
    ]

let () =
  let open QCheck2 in
  let decision_preserves_pair =
    Test.make ~name:"decision preserves the selected backend/model" ~count:500
      (Gen.pair (Gen.oneof_list known_backends) optional_model)
      (fun (backend, model) ->
        let decision = Backend_routing.decide ~backend ~model in
        String.equal decision.backend backend
        && Option.equal String.equal decision.model model)
  in
  let cli_backend_wins =
    Test.make ~name:"CLI backend wins over stored and configured values"
      ~count:200
      (Gen.tup3
         (Gen.oneof_list known_backends)
         (Gen.oneof_list known_backends)
         (Gen.oneof_list known_backends))
      (fun (cli_backend, stored_backend, configured_backend) ->
        let backend, _model =
          Backend_routing.resolve_pair ~cli_backend ~cli_model:""
            ~stored_backend ~stored_model:""
            ~repo_config:
              {
                Repo_config.empty with
                default_backend = Some configured_backend;
              }
            ~built_in_backend:"claude"
        in
        String.equal backend cli_backend)
  in
  let built_in_backend_is_total_fallback =
    Test.make ~name:"built-in backend is the total fallback" ~count:200
      (Gen.oneof_list known_backends) (fun built_in_backend ->
        let backend, model =
          Backend_routing.resolve_pair ~cli_backend:" " ~cli_model:""
            ~stored_backend:"" ~stored_model:"" ~repo_config:Repo_config.empty
            ~built_in_backend
        in
        String.equal backend built_in_backend && String.is_empty model)
  in
  let fields_resolve_independently =
    Test.make ~name:"backend and model resolve independently" ~count:200
      (Gen.pair
         (Gen.oneof_list known_backends)
         (Gen.oneof_list [ "sonnet"; "opus"; "gpt-5" ]))
      (fun (stored_backend, configured_model) ->
        let backend, model =
          Backend_routing.resolve_pair ~cli_backend:"" ~cli_model:""
            ~stored_backend ~stored_model:""
            ~repo_config:
              { Repo_config.empty with default_model = Some configured_model }
            ~built_in_backend:"claude"
        in
        String.equal backend stored_backend
        && String.equal model configured_model)
  in
  let patch_override_is_atomic =
    Test.make ~name:"patch agent override wins as one complete pair" ~count:200
      (Gen.pair
         (Gen.oneof_list known_backends)
         (Gen.oneof_list [ "sol"; "luna" ]))
      (fun (backend, model) ->
        let patch : Types.Patch.t =
          {
            id = Types.Patch_id.of_string "p";
            goal = "p";
            branch = Types.Branch.of_string "p";
            dependencies = [];
            files = [];
            checks = [];
            agent = Some { Types.Patch.Agent.backend; model };
          }
        in
        let selected =
          Backend_routing.for_patch
            ~default:
              (Backend_routing.decide ~backend:"claude" ~model:(Some "default"))
            patch
        in
        String.equal selected.backend backend
        && Option.equal String.equal selected.model (Some model))
  in
  let exit_code =
    QCheck_base_runner.run_tests ~verbose:true
      [
        decision_preserves_pair;
        cli_backend_wins;
        built_in_backend_is_total_fallback;
        fields_resolve_independently;
        patch_override_is_atomic;
      ]
  in
  if exit_code <> 0 then Stdlib.exit exit_code
