open Base
open Onton_core
module Gen = QCheck2.Gen
module Test = QCheck2.Test

let create_is_total =
  Test.make ~name:"worker sandbox policy construction is total" ~count:500
    Gen.(tup4 string_small string_small string_small string_small)
    (fun (worktree, context, writable, state) ->
      try
        ignore
          (Worker_sandbox_policy.create ~worktree ~read_only_paths:[ context ]
             ~read_only_dirs:[] ~writable_files:[ writable ] ~writable_dirs:[]
             ~creatable_dirs:[] ~runtime_files:[] ~runtime_roots:[]
             ~state_dir:state ~network:Worker_sandbox_policy.Denied);
        true
      with _ -> false)

let environment_is_total =
  let entry = Gen.pair Gen.string_small Gen.string_small in
  Test.make ~name:"worker environment construction is total" ~count:500
    Gen.(pair (list entry) (list entry))
    (fun (base, overrides) ->
      try
        ignore
          (Worker_sandbox_policy.environment
             ~allowed_provider_names:[ "OPENAI_API_KEY" ]
             ~base:
               (base
               |> List.map ~f:(fun (name, value) -> name ^ "=" ^ value)
               |> Array.of_list)
             ~overrides);
        true
      with _ -> false)

let profile_is_deterministic =
  Test.make ~name:"worker sandbox profile rendering is deterministic" ~count:300
    Gen.(list nat_small)
    (fun suffixes ->
      let paths =
        List.map suffixes ~f:(fun suffix -> Printf.sprintf "/context/%d" suffix)
      in
      match
        Worker_sandbox_policy.create ~worktree:"/worktree"
          ~read_only_paths:paths ~read_only_dirs:[] ~writable_files:[]
          ~writable_dirs:[] ~creatable_dirs:[] ~runtime_files:[]
          ~runtime_roots:[ "/runtime" ] ~state_dir:"/state"
          ~network:Worker_sandbox_policy.Https_only
      with
      | Error _ -> false
      | Ok policy ->
          String.equal
            (Worker_sandbox_policy.macos_profile policy)
            (Worker_sandbox_policy.macos_profile policy))

let empty_create_capability_is_fail_closed =
  Test.make ~name:"empty directory-create capability emits no allow rule"
    ~count:100 Gen.unit (fun () ->
      match
        Worker_sandbox_policy.create ~worktree:"/worktree" ~read_only_paths:[]
          ~read_only_dirs:[] ~writable_files:[] ~writable_dirs:[]
          ~creatable_dirs:[] ~runtime_files:[] ~runtime_roots:[]
          ~state_dir:"/state" ~network:Worker_sandbox_policy.Denied
      with
      | Error _ -> false
      | Ok policy ->
          not
            (String.is_substring
               (Worker_sandbox_policy.macos_profile policy)
               ~substring:"(allow file-write-create"))

let denied_credentials_never_survive =
  Test.make ~name:"unselected credentials never survive environment scrubbing"
    ~count:300 Gen.string_small (fun value ->
      match
        Worker_sandbox_policy.environment
          ~allowed_provider_names:[ "OPENAI_API_KEY" ]
          ~base:
            [|
              "OPENAI_API_KEY=selected";
              "ANTHROPIC_API_KEY=" ^ value;
              "GITHUB_TOKEN=" ^ value;
              "SSH_AUTH_SOCK=" ^ value;
            |]
          ~overrides:[]
      with
      | Error _ -> false
      | Ok environment ->
          Array.for_all environment ~f:(fun entry ->
              not
                (String.is_prefix entry ~prefix:"ANTHROPIC_API_KEY="
                || String.is_prefix entry ~prefix:"GITHUB_TOKEN="
                || String.is_prefix entry ~prefix:"SSH_AUTH_SOCK=")))

let runtime_files_are_added_exactly =
  Test.make ~name:"runtime files extend the exact-file capability" ~count:300
    Gen.(list nat_small)
    (fun suffixes ->
      let files =
        List.map suffixes ~f:(fun suffix ->
            Printf.sprintf "/runtime/bin-%d" suffix)
      in
      match
        Worker_sandbox_policy.create ~worktree:"/worktree" ~read_only_paths:[]
          ~read_only_dirs:[] ~writable_files:[] ~writable_dirs:[]
          ~creatable_dirs:[] ~runtime_files:[] ~runtime_roots:[]
          ~state_dir:"/state" ~network:Worker_sandbox_policy.Denied
      with
      | Error _ -> false
      | Ok policy -> (
          match Worker_sandbox_policy.add_runtime_files policy files with
          | Error _ -> false
          | Ok policy ->
              List.equal String.equal policy.runtime_files
                (List.dedup_and_sort files ~compare:String.compare)))

let environment_allowlist_is_explicit =
  Test.make ~name:"provider environment allowlist is explicit" ~count:300
    Gen.bool (fun allow_openai ->
      let providers = if allow_openai then [ "OPENAI_API_KEY" ] else [] in
      Bool.equal
        (Worker_sandbox_policy.allowed_environment_name
           ~allowed_provider_names:providers "OPENAI_API_KEY")
        allow_openai
      && not
           (Worker_sandbox_policy.allowed_environment_name
              ~allowed_provider_names:providers "GITHUB_TOKEN"))

let () =
  List.iter
    [
      create_is_total;
      environment_is_total;
      profile_is_deterministic;
      empty_create_capability_is_fail_closed;
      denied_credentials_never_survive;
      runtime_files_are_added_exactly;
      environment_allowlist_is_explicit;
    ] ~f:(fun test -> Test.check_exn test)
