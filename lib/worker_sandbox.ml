open Base

type t = {
  policy : Worker_sandbox_policy.t;
  provider_environment_names : string list;
  backend_command : string;
  launch_prefix : string list;
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

type declared_path = { file : string; creatable_dirs : string list }

let directories_between ~ancestor descendant =
  let rec collect current acc =
    if String.equal current ancestor then Ok acc
    else
      let parent = Stdlib.Filename.dirname current in
      if String.equal parent current || not (is_within ~root:ancestor current)
      then Error "declared worker path has no in-worktree directory ancestry"
      else collect parent (current :: acc)
  in
  collect descendant []

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
              if not (String.equal canonical lexical) then
                Error
                  (Printf.sprintf
                     "declared worker file must not be a symlink: %S" relative)
              else if Stdlib.Sys.is_directory canonical then
                Error
                  (Printf.sprintf "declared worker file is a directory: %S"
                     relative)
              else Ok { file = lexical; creatable_dirs = [] }
            else
              let suffix =
                String.drop_prefix lexical (String.length existing)
              in
              let canonical = canonical_existing ^ suffix in
              if not (String.equal canonical lexical) then
                Error
                  (Printf.sprintf
                     "declared worker path contains a symlinked parent: %S"
                     relative)
              else if not (Stdlib.Sys.is_directory canonical_existing) then
                Error
                  (Printf.sprintf
                     "declared worker path descends through a file: %S" relative)
              else
                Result.map
                  (directories_between ~ancestor:canonical_existing
                     (Stdlib.Filename.dirname lexical))
                  ~f:(fun creatable_dirs -> { file = lexical; creatable_dirs })
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
  | "codex" -> [ "CODEX_API_KEY" ]
  | "openai" -> [ "OPENAI_API_KEY" ]
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
      if Stdlib.Sys.file_exists candidate then
        match Unix.access candidate [ Unix.X_OK ] with
        | () -> Some (normalize_absolute candidate)
        | exception Unix.Unix_error _ -> None
      else None)

let resolve_setsid_exec ~executable_name ~override =
  let executable_path =
    if
      Stdlib.Filename.is_relative executable_name
      && not (String.is_substring executable_name ~substring:"/")
    then
      Option.value
        (resolve_from_path executable_name)
        ~default:(normalize_absolute executable_name)
    else normalize_absolute executable_name
  in
  let executable_dir = Stdlib.Filename.dirname executable_path in
  let candidate =
    match override with
    | Some "" ->
        Error "worker isolation unavailable: ONTON_SETSID_EXEC disabled"
    | Some path -> Ok path
    | None ->
        let installed =
          Stdlib.Filename.concat executable_dir "onton-setsid-exec"
        in
        if Stdlib.Sys.file_exists installed then Ok installed
        else Ok (Stdlib.Filename.concat executable_dir "setsid_exec/main.exe")
  in
  Result.bind candidate ~f:(fun path ->
      if Stdlib.Sys.file_exists path then
        try Ok (Unix.realpath path)
        with exn ->
          Error
            (Printf.sprintf "cannot resolve onton-setsid-exec at %s: %s" path
               (Exn.to_string exn))
      else
        Error
          (Printf.sprintf
             "worker isolation unavailable: onton-setsid-exec not found at %s"
             path))

let read_first_line path =
  try
    let channel = Stdlib.open_in_bin path in
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_in_noerr channel)
      (fun () -> Some (Stdlib.input_line channel))
  with _ -> None

let split_words line =
  String.split_on_chars line ~on:[ ' '; '\t' ]
  |> List.filter ~f:(fun word -> not (String.is_empty word))

let node_package_root script =
  let marker = "/node_modules/" in
  match
    String.substr_index_all script ~pattern:marker ~may_overlap:false
    |> List.last
  with
  | None -> None
  | Some marker_index ->
      let package_start = marker_index + String.length marker in
      let remainder = String.drop_prefix script package_start in
      let segments = String.split remainder ~on:'/' in
      let package_segments =
        match segments with
        | scope :: package :: _ when String.is_prefix scope ~prefix:"@" ->
            [ scope; package ]
        | package :: _ -> [ package ]
        | [] -> []
      in
      if List.is_empty package_segments then None
      else
        let relative = String.concat ~sep:"/" package_segments in
        Some (String.prefix script (package_start + String.length relative))

let canonical_file path =
  try
    let canonical = Unix.realpath path |> normalize_absolute in
    if Stdlib.Sys.is_directory canonical then
      Error (Printf.sprintf "worker runtime path is a directory: %s" path)
    else Ok canonical
  with exn ->
    Error
      (Printf.sprintf "cannot resolve worker runtime file %s: %s" path
         (Exn.to_string exn))

let system_runtime_file path =
  List.exists [ "/System/"; "/usr/lib/" ] ~f:(fun prefix ->
      String.is_prefix path ~prefix)

let otool_dependencies path =
  if not (Stdlib.Sys.file_exists "/usr/bin/otool") then Ok []
  else
    try
      let channel =
        Unix.open_process_args_in "/usr/bin/otool" [| "otool"; "-L"; path |]
      in
      let lines = Stdlib.In_channel.input_lines channel in
      match Unix.close_process_in channel with
      | Unix.WEXITED 0 ->
          List.drop lines 1
          |> List.filter_map ~f:(fun line ->
              match split_words (String.strip line) with
              | dependency :: _ -> Some dependency
              | _ -> None)
          |> Result.return
      | Unix.WEXITED code ->
          Error (Printf.sprintf "otool -L failed for %s with exit %d" path code)
      | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
          Error
            (Printf.sprintf "otool -L failed for %s with signal %d" path signal)
    with exn ->
      Error
        (Printf.sprintf "cannot inspect worker runtime %s: %s" path
           (Exn.to_string exn))

let resolve_linked_dependency ~executable ~loader dependency =
  let relative_candidate prefix base =
    Option.map (String.chop_prefix dependency ~prefix) ~f:(fun suffix ->
        Stdlib.Filename.concat base suffix)
  in
  let candidates =
    if Stdlib.Filename.is_relative dependency |> not then [ dependency ]
    else
      [
        relative_candidate "@loader_path/" (Stdlib.Filename.dirname loader);
        relative_candidate "@executable_path/"
          (Stdlib.Filename.dirname executable);
        relative_candidate "@rpath/" (Stdlib.Filename.dirname loader);
        relative_candidate "@rpath/" (Stdlib.Filename.dirname executable);
      ]
      |> List.filter_opt
  in
  match List.find candidates ~f:Stdlib.Sys.file_exists with
  | Some path -> Ok path
  | None ->
      Error
        (Printf.sprintf "cannot resolve linked runtime dependency %s from %s"
           dependency loader)

let linked_runtime_files executable =
  let rec visit seen pending files =
    match pending with
    | [] -> Ok files
    | path :: rest when Set.mem seen path -> visit seen rest files
    | path :: rest ->
        let seen = Set.add seen path in
        Result.bind (otool_dependencies path) ~f:(fun dependencies ->
            Result.bind
              (dependencies
              |> List.map ~f:(fun dependency ->
                  if
                    Stdlib.Filename.is_relative dependency |> not
                    && system_runtime_file dependency
                  then Ok None
                  else
                    Result.bind
                      (resolve_linked_dependency ~executable ~loader:path
                         dependency) ~f:(fun resolved ->
                        if system_runtime_file resolved then Ok None
                        else
                          Result.map (canonical_file resolved)
                            ~f:(fun canonical -> Some (resolved, canonical))))
              |> Result.all)
              ~f:(fun resolved ->
                let resolved = List.filter_opt resolved in
                let next = List.map resolved ~f:snd @ rest in
                let files =
                  List.fold resolved ~init:files
                    ~f:(fun files (original, canonical) ->
                      original :: canonical :: files)
                in
                visit seen next files))
  in
  visit (Set.empty (module String)) [ executable ] []

type runtime = {
  command : string;
  launch_prefix : string list;
  files : string list;
  roots : string list;
}

let interpreter_of_shebang line =
  match String.chop_prefix line ~prefix:"#!" with
  | None -> Ok None
  | Some body -> (
      match split_words (String.strip body) with
      | [] -> Error "worker backend has an empty shebang"
      | "/usr/bin/env" :: program :: arguments ->
          Result.map
            (resolve_from_path program
            |> Result.of_option
                 ~error:("worker backend interpreter is not on PATH: " ^ program)
            )
            ~f:(fun interpreter -> Some (interpreter, arguments))
      | interpreter :: arguments
        when Stdlib.Filename.is_relative interpreter |> not ->
          Ok (Some (interpreter, arguments))
      | interpreter :: _ ->
          Error ("worker backend interpreter must be absolute: " ^ interpreter))

let resolve_runtime backend =
  Result.bind
    (executable_for_backend backend
    |> Result.of_option ~error:("unsupported worker backend: " ^ backend))
    ~f:(fun command ->
      Result.bind
        (resolve_from_path command
        |> Result.of_option
             ~error:("worker backend executable is not on PATH: " ^ command))
        ~f:(fun original ->
          Result.bind (canonical_file original) ~f:(fun executable ->
              let first_line = read_first_line executable in
              Result.bind
                (match first_line with
                | Some line -> interpreter_of_shebang line
                | None -> Ok None)
                ~f:(function
                  | None ->
                      Result.map (linked_runtime_files executable)
                        ~f:(fun linked ->
                          {
                            command;
                            launch_prefix = [ executable ];
                            files = original :: executable :: linked;
                            roots = [];
                          })
                  | Some (interpreter_original, interpreter_arguments) ->
                      Result.bind (canonical_file interpreter_original)
                        ~f:(fun interpreter ->
                          let package_roots =
                            if
                              String.equal
                                (Stdlib.Filename.basename interpreter)
                                "node"
                            then
                              Result.map
                                (node_package_root executable
                                |> Result.of_option
                                     ~error:
                                       ("node worker backend is not installed "
                                      ^ "inside a package-scoped node_modules "
                                      ^ "directory: " ^ executable))
                                ~f:List.return
                            else Ok []
                          in
                          Result.bind package_roots ~f:(fun roots ->
                              Result.map (linked_runtime_files interpreter)
                                ~f:(fun linked ->
                                  {
                                    command;
                                    launch_prefix =
                                      (interpreter :: interpreter_arguments)
                                      @ [ executable ];
                                    files =
                                      [
                                        original;
                                        executable;
                                        interpreter_original;
                                        interpreter;
                                      ]
                                      @ linked;
                                    roots;
                                  })))))))

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

let capability_segment label value =
  let value = String.lowercase (String.strip value) in
  if String.is_empty value then Error (label ^ " must not be empty")
  else if
    String.exists value ~f:(fun char ->
        not
          (Char.is_alphanum char || Char.equal char '-' || Char.equal char '_'
         || Char.equal char '.'))
  then Error (Printf.sprintf "%s is not a safe path component: %S" label value)
  else Ok value

let canonical_existing_dir path =
  try Ok (Unix.realpath path |> normalize_absolute)
  with exn ->
    Error
      (Printf.sprintf "cannot resolve worker directory %s: %s" path
         (Exn.to_string exn))

let canonical_output_file path =
  let parent = Stdlib.Filename.dirname path in
  Result.map (canonical_existing_dir parent) ~f:(fun parent ->
      Stdlib.Filename.concat parent (Stdlib.Filename.basename path))

let create ~backend ~provider ~project_name ~worktree ~(patch : Types.Patch.t)
    ~gameplan ~operation =
  let ( let* ) result f = Result.bind result ~f in
  let* () = preflight () in
  let* worktree = validate_worktree worktree in
  let* declared_paths =
    patch.files |> List.map ~f:(validate_declared_file ~worktree) |> Result.all
  in
  let* runtime = resolve_runtime backend in
  let* backend_segment = capability_segment "worker backend" backend in
  let* provider_segment = capability_segment "worker provider" provider in
  let patch_root =
    Stdlib.Filename.concat
      (Stdlib.Filename.concat
         (Project_store.project_dir project_name)
         "spawn-envs")
      (Types.Patch_id.to_string patch.id)
  in
  let state_dir =
    Stdlib.Filename.concat
      (Stdlib.Filename.concat
         (Stdlib.Filename.concat patch_root "sandbox")
         backend_segment)
      provider_segment
  in
  let home_dir = Stdlib.Filename.concat state_dir "home" in
  let temp_dir = Stdlib.Filename.concat state_dir "tmp" in
  let xdg_config_dir = Stdlib.Filename.concat state_dir "config" in
  List.iter
    [ state_dir; home_dir; temp_dir; xdg_config_dir ]
    ~f:Project_store.ensure_dir;
  let* state_dir = canonical_existing_dir state_dir in
  let home_dir = Stdlib.Filename.concat state_dir "home" in
  let temp_dir = Stdlib.Filename.concat state_dir "tmp" in
  let xdg_config_dir = Stdlib.Filename.concat state_dir "config" in
  let output_files, output_dirs =
    writable_outputs ~project_name ~patch_id:patch.id operation
  in
  List.iter output_dirs ~f:Project_store.ensure_dir;
  List.iter output_files ~f:(fun path ->
      Project_store.ensure_dir (Stdlib.Filename.dirname path));
  let* output_dirs =
    output_dirs |> List.map ~f:canonical_existing_dir |> Result.all
  in
  let* output_files =
    output_files |> List.map ~f:canonical_output_file |> Result.all
  in
  let read_only_dirs =
    [ Project_store.ci_artifact_dir ~project_name ~patch_id:patch.id ]
    |> List.filter ~f:Stdlib.Sys.file_exists
  in
  let read_only_paths =
    Project_store.plan_artifact_path project_name
    :: ancestor_notes ~project_name patch gameplan
  in
  let writable_files =
    List.map declared_paths ~f:(fun path -> path.file) @ output_files
  in
  let creatable_dirs =
    List.concat_map declared_paths ~f:(fun path -> path.creatable_dirs)
  in
  let* policy =
    Worker_sandbox_policy.create ~worktree ~read_only_paths ~read_only_dirs
      ~writable_files ~writable_dirs:output_dirs ~creatable_dirs
      ~runtime_files:runtime.files ~runtime_roots:runtime.roots ~state_dir
      ~network:Worker_sandbox_policy.Https_only
  in
  Ok
    {
      policy;
      provider_environment_names = provider_environment_names provider;
      backend_command = runtime.command;
      launch_prefix = runtime.launch_prefix;
      home_dir;
      temp_dir;
      xdg_config_dir;
    }

let environment t ~overrides =
  Worker_sandbox_policy.environment
    ~allowed_provider_names:t.provider_environment_names
    ~base:(Unix.environment ())
    ~overrides:
      (overrides
      @ [
          ("PATH", "/usr/bin:/bin:/usr/sbin:/sbin");
          ("HOME", t.home_dir);
          ("TMPDIR", t.temp_dir);
          ("XDG_CONFIG_HOME", t.xdg_config_dir);
          ("OPENSSL_CONF", "/dev/null");
          ("SSL_CERT_FILE", "/etc/ssl/cert.pem");
        ])

let profile t = Worker_sandbox_policy.macos_profile t.policy
let state_dir t = t.policy.state_dir

let backend_arguments t args =
  match args with
  | [] -> Error "worker backend command is empty"
  | command :: arguments ->
      let basename = Stdlib.Filename.basename command in
      if String.equal basename t.backend_command then
        Ok (t.launch_prefix @ arguments)
      else
        Error
          (Printf.sprintf
             "worker launch command %S does not match sandbox backend %S"
             command t.backend_command)

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
  | Some path ->
      Result.bind (canonical_file path) ~f:(fun canonical_setsid ->
          Result.bind
            (Worker_sandbox_policy.add_runtime_files t.policy
               [ path; canonical_setsid ])
            ~f:(fun policy ->
              Result.bind (backend_arguments t args) ~f:(fun backend_args ->
                  Result.map (environment t ~overrides) ~f:(fun environment ->
                      {
                        argv =
                          sandbox_exec :: "-p"
                          :: Worker_sandbox_policy.macos_profile policy
                          :: canonical_setsid :: backend_args;
                        environment;
                        process_group = true;
                      }))))
