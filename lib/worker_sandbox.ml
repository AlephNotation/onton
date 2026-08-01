(* @archlint.module shell
   @archlint.domain worker-sandbox *)

open Base

type t = {
  policy : Worker_sandbox_policy.t;
  provider_environment_names : string list;
  home_dir : string;
  temp_dir : string;
  xdg_config_dir : string;
}

type spawn = {
  argv : string list;
  environment : string array;
  process_group : bool;
}

let sandbox_exec = "/usr/bin/sandbox-exec"
let system_profile = "/System/Library/Sandbox/Profiles/system.sb"

let preflight () =
  if not (Stdlib.Sys.file_exists sandbox_exec) then
    Error
      "worker isolation is unavailable: /usr/bin/sandbox-exec is missing; \
       refusing to launch an unsandboxed worker"
  else if not (Stdlib.Sys.file_exists system_profile) then
    Error
      "worker isolation is unavailable: the macOS system sandbox profile is \
       missing; refusing to launch an unsandboxed worker"
  else Ok ()

let normalize_absolute path =
  let path =
    if Stdlib.Filename.is_relative path then
      Stdlib.Filename.concat (Stdlib.Sys.getcwd ()) path
    else path
  in
  let rec strip path =
    if String.length path > 1 && Char.equal path.[String.length path - 1] '/'
    then strip (String.drop_suffix path 1)
    else path
  in
  strip path

let is_within ~root path =
  String.equal path root
  || String.is_prefix path ~prefix:(root ^ Stdlib.Filename.dir_sep)

let rec nearest_existing path =
  if Stdlib.Sys.file_exists path then Some path
  else
    let parent = Stdlib.Filename.dirname path in
    if String.equal parent path then None else nearest_existing parent

let validate_worktree path =
  let lexical = normalize_absolute path in
  try
    let canonical = Unix.realpath lexical |> normalize_absolute in
    if not (Stdlib.Sys.is_directory canonical) then
      Error (Printf.sprintf "worker worktree is not a directory: %s" lexical)
    else Ok canonical
  with exn ->
    Error
      (Printf.sprintf "cannot resolve worker worktree %s: %s" lexical
         (Exn.to_string exn))

let validate_declared_file ~worktree relative =
  if Stdlib.Filename.is_relative relative |> not then
    Error (Printf.sprintf "declared worker path must be relative: %S" relative)
  else
    let lexical =
      Stdlib.Filename.concat worktree relative |> normalize_absolute
    in
    if not (is_within ~root:worktree lexical) then
      Error
        (Printf.sprintf "declared worker path escapes the worktree: %S" relative)
    else
      match nearest_existing lexical with
      | None ->
          Error (Printf.sprintf "cannot resolve declared path: %S" relative)
      | Some existing -> (
          try
            let canonical_existing =
              Unix.realpath existing |> normalize_absolute
            in
            if not (is_within ~root:worktree canonical_existing) then
              Error
                (Printf.sprintf
                   "declared worker path traverses a symlink outside the \
                    worktree: %S"
                   relative)
            else if Stdlib.Sys.file_exists lexical then
              let canonical = Unix.realpath lexical |> normalize_absolute in
              if String.equal canonical lexical then Ok lexical
              else
                Error
                  (Printf.sprintf
                     "declared worker file must not be a symlink: %S" relative)
            else
              let suffix =
                String.drop_prefix lexical (String.length existing)
              in
              let canonical = canonical_existing ^ suffix in
              if String.equal canonical lexical then Ok lexical
              else
                Error
                  (Printf.sprintf
                     "declared worker path contains a symlinked parent: %S"
                     relative)
          with exn ->
            Error
              (Printf.sprintf "cannot validate declared worker path %S: %s"
                 relative (Exn.to_string exn)))

let path_entries () =
  match Stdlib.Sys.getenv_opt "PATH" with
  | None -> []
  | Some path ->
      String.split path ~on:':'
      |> List.filter_map ~f:(fun entry ->
          let entry = String.strip entry in
          if String.is_empty entry || Stdlib.Filename.is_relative entry then
            None
          else Some (normalize_absolute entry))

let executable_for_backend = function
  | "claude" -> Some "claude"
  | "codex" -> Some "codex"
  | "opencode" -> Some "opencode"
  | "pi" -> Some "pi"
  | "gemini" -> Some "gemini"
  | "patch-agent" -> Some "patch-agent"
  | _ -> None

let provider_environment_names provider =
  match String.lowercase (String.strip provider) with
  | "anthropic" | "claude" -> [ "ANTHROPIC_API_KEY"; "CLAUDE_CODE_OAUTH_TOKEN" ]
  | "openai" | "codex" -> [ "OPENAI_API_KEY" ]
  | "google" | "gemini" -> [ "GEMINI_API_KEY"; "GOOGLE_API_KEY" ]
  | "openrouter" -> [ "OPENROUTER_API_KEY" ]
  | "xai" -> [ "XAI_API_KEY" ]
  | "groq" -> [ "GROQ_API_KEY" ]
  | "mistral" -> [ "MISTRAL_API_KEY" ]
  | "cohere" -> [ "COHERE_API_KEY" ]
  | _ -> []

let resolve_from_path command =
  path_entries ()
  |> List.find_map ~f:(fun dir ->
      let candidate = Stdlib.Filename.concat dir command in
      if Stdlib.Sys.file_exists candidate then Some candidate else None)

let prefix_before_bin path =
  match String.substr_index path ~pattern:"/bin/" with
  | Some index when index > 0 -> Some (String.prefix path index)
  | Some _ | None -> None

let runtime_roots ~backend =
  let fixed =
    [
      "/opt/homebrew";
      "/usr/local";
      Stdlib.Filename.dirname Stdlib.Sys.executable_name;
    ]
    |> List.filter ~f:Stdlib.Sys.file_exists
  in
  let backend_roots =
    match Option.bind (executable_for_backend backend) ~f:resolve_from_path with
    | None -> []
    | Some executable ->
        let executable = normalize_absolute executable in
        let resolved =
          try Some (Unix.realpath executable |> normalize_absolute)
          with _ -> None
        in
        let roots = [ Some (Stdlib.Filename.dirname executable) ] in
        let roots = prefix_before_bin executable :: roots in
        let roots =
          match resolved with
          | None -> roots
          | Some resolved ->
              Some (Stdlib.Filename.dirname resolved)
              :: prefix_before_bin resolved :: roots
        in
        List.filter_opt roots
  in
  List.dedup_and_sort
    (fixed @ path_entries () @ backend_roots)
    ~compare:String.compare

let ancestor_notes ~project_name (patch : Types.Patch.t)
    (gameplan : Types.Gameplan.t) =
  let graph = Graph.of_patches gameplan.patches in
  Graph.transitive_ancestors graph patch.id
  |> List.map ~f:(fun patch_id ->
      Project_store.pr_body_artifact_path ~project_name ~patch_id)

let writable_outputs ~project_name ~patch_id = function
  | Some Types.Operation_kind.Pr_body ->
      ([ Project_store.pr_body_artifact_path ~project_name ~patch_id ], [])
  | Some Types.Operation_kind.Review_comments ->
      ([], [ Project_store.comment_responses_dir ~project_name ~patch_id ])
  | Some Types.Operation_kind.Findings ->
      ([], [ Project_store.findings_wontfix_dir ~project_name ~patch_id ])
  | Some (Types.Operation_kind.Rebase | Human | Merge_conflict | Ci) | None ->
      ([], [])

let create ~backend ~provider ~project_name ~worktree ~(patch : Types.Patch.t)
    ~gameplan ~operation =
  Result.bind (preflight ()) ~f:(fun () ->
      Result.bind (validate_worktree worktree) ~f:(fun worktree ->
          Result.bind
            (patch.files
            |> List.map ~f:(validate_declared_file ~worktree)
            |> Result.all)
            ~f:(fun writable_files ->
              let patch_root =
                Stdlib.Filename.concat
                  (Stdlib.Filename.concat
                     (Project_store.project_dir project_name)
                     "spawn-envs")
                  (Types.Patch_id.to_string patch.id)
              in
              let state_dir = Stdlib.Filename.concat patch_root "sandbox" in
              let home_dir = Stdlib.Filename.concat state_dir "home" in
              let temp_dir = Stdlib.Filename.concat state_dir "tmp" in
              let xdg_config_dir = Stdlib.Filename.concat state_dir "config" in
              List.iter
                [ state_dir; home_dir; temp_dir; xdg_config_dir ]
                ~f:Project_store.ensure_dir;
              let output_files, output_dirs =
                writable_outputs ~project_name ~patch_id:patch.id operation
              in
              List.iter output_dirs ~f:Project_store.ensure_dir;
              List.iter output_files ~f:(fun path ->
                  Project_store.ensure_dir (Stdlib.Filename.dirname path));
              let read_only_dirs =
                [
                  Project_store.ci_artifact_dir ~project_name ~patch_id:patch.id;
                ]
                |> List.filter ~f:Stdlib.Sys.file_exists
              in
              let read_only_paths =
                Project_store.plan_artifact_path project_name
                :: ancestor_notes ~project_name patch gameplan
              in
              Result.map
                (Worker_sandbox_policy.create ~worktree ~read_only_paths
                   ~read_only_dirs
                   ~writable_files:(writable_files @ output_files)
                   ~writable_dirs:output_dirs
                   ~runtime_roots:(runtime_roots ~backend) ~state_dir
                   ~network:Worker_sandbox_policy.Https_only)
                ~f:(fun policy ->
                  {
                    policy;
                    provider_environment_names =
                      provider_environment_names provider;
                    home_dir;
                    temp_dir;
                    xdg_config_dir;
                  }))))

let environment t ~overrides =
  Worker_sandbox_policy.environment
    ~allowed_provider_names:t.provider_environment_names
    ~base:(Unix.environment ())
    ~overrides:
      (overrides
      @ [
          ("PATH", String.concat ~sep:":" (path_entries ()));
          ("HOME", t.home_dir);
          ("TMPDIR", t.temp_dir);
          ("XDG_CONFIG_HOME", t.xdg_config_dir);
        ])

let profile t = Worker_sandbox_policy.macos_profile t.policy
let state_dir t = t.policy.state_dir

let wrap_argv t ~setsid_exec args =
  let command =
    match setsid_exec with Some path -> path :: args | None -> args
  in
  sandbox_exec :: "-p" :: profile t :: command

let prepare_spawn t ~overrides ~setsid_exec args =
  match setsid_exec with
  | None ->
      Error
        "worker process-group isolation is unavailable: onton-setsid-exec is \
         missing; refusing to launch a worker whose descendants cannot be \
         reaped"
  | Some path when Stdlib.Filename.is_relative path ->
      Error "worker process-group shim must be an absolute path"
  | Some path when not (Stdlib.Sys.file_exists path) ->
      Error ("worker process-group shim does not exist: " ^ path)
  | Some _ ->
      Result.map (environment t ~overrides) ~f:(fun environment ->
          {
            argv = wrap_argv t ~setsid_exec args;
            environment;
            process_group = true;
          })
