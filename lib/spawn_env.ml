open Base

let patch_root ~project_dir ~patch_id =
  Stdlib.Filename.concat project_dir
    (Stdlib.Filename.concat "spawn-envs" (Types.Patch_id.to_string patch_id))

let sandbox_root ~project_dir ~patch_id =
  Stdlib.Filename.concat (patch_root ~project_dir ~patch_id) "sandbox"

(* On macOS, Claude Code stores its OAuth token in the macOS Keychain rather
   than in [.credentials.json]. The Keychain lookup is scoped such that
   pointing Claude at a per-patch [CLAUDE_CONFIG_DIR] makes it report
   "Not logged in" even when the user has a valid Keychain entry. The
   documented escape hatch is [CLAUDE_CODE_OAUTH_TOKEN] (precedence #5 in
   docs), generated via [claude setup-token]. We read it from the shell env
   (preferred) or fall back to [$XDG_CONFIG_HOME/onton/claude-oauth-token]
   (or [~/.config/onton/claude-oauth-token]). *)
let resolve_claude_oauth_token_with ~getenv_opt ~read_token_file =
  match getenv_opt "CLAUDE_CODE_OAUTH_TOKEN" with
  | Some t when not (String.is_empty (String.strip t)) -> Some (String.strip t)
  | _ ->
      let xdg_dir =
        match getenv_opt "XDG_CONFIG_HOME" with
        | Some d when not (String.is_empty d) -> Some d
        | _ -> (
            match getenv_opt "HOME" with
            | Some home when not (String.is_empty home) ->
                Some (Stdlib.Filename.concat home ".config")
            | _ -> None)
      in
      Option.bind xdg_dir ~f:(fun dir ->
          let path = Stdlib.Filename.concat dir "onton/claude-oauth-token" in
          match read_token_file path with
          | None -> None
          | Some s ->
              let t = String.strip s in
              if String.is_empty t then None else Some t)

let read_token_file_opt path =
  try
    let ic = Stdlib.open_in path in
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_in_noerr ic)
      (fun () ->
        let len = Stdlib.in_channel_length ic in
        let buf = Stdlib.Bytes.create len in
        Stdlib.really_input ic buf 0 len;
        Some (Stdlib.Bytes.to_string buf))
  with _ -> None

let resolve_claude_oauth_token () =
  resolve_claude_oauth_token_with ~getenv_opt:Stdlib.Sys.getenv_opt
    ~read_token_file:read_token_file_opt

let claude_oauth_token_overrides () =
  match resolve_claude_oauth_token () with
  | None -> []
  | Some token -> [ ("CLAUDE_CODE_OAUTH_TOKEN", token) ]

let per_patch_env_in_state_dir ~backend ~state_dir =
  let config_dir = Stdlib.Filename.concat state_dir "config" in
  Project_store.ensure_dir config_dir;
  match String.lowercase (String.strip backend) with
  | "anthropic" | "claude" ->
      ("CLAUDE_CONFIG_DIR", config_dir) :: claude_oauth_token_overrides ()
  | "openai" | "codex" -> [ ("CODEX_HOME", config_dir) ]
  | "opencode" -> [ ("OPENCODE_CONFIG_DIR", config_dir) ]
  | _ -> []

let per_patch_env ~backend ~state_dir =
  per_patch_env_in_state_dir ~backend ~state_dir

(* Claude Code stores each conversation at
   [<CLAUDE_CONFIG_DIR>/projects/<cwd-key>/<session-id>.jsonl], where
   [cwd-key] is the absolute cwd path with every ["/"] replaced by ["-"].
   When [claude --resume <id>] hits a stub (a session-init header with no
   conversation turns, left behind by a session that failed before producing
   content), the file remains on disk forever; this helper lets the caller
   delete it after a [Session_no_resume] classification. *)
let claude_project_dir_key ~worktree_path =
  String.substr_replace_all worktree_path ~pattern:"/" ~with_:"-"

let claude_session_jsonl_path_in_project_dir ~project_dir ~patch_id
    ~worktree_path ~session_id =
  let root = sandbox_root ~project_dir ~patch_id in
  let claude_dir =
    Stdlib.Filename.concat
      (Stdlib.Filename.concat (Stdlib.Filename.concat root "claude") "claude")
      "config"
  in
  let key = claude_project_dir_key ~worktree_path in
  Stdlib.Filename.concat
    (Stdlib.Filename.concat (Stdlib.Filename.concat claude_dir "projects") key)
    (session_id ^ ".jsonl")

let claude_session_jsonl_path ~project_name ~patch_id ~worktree_path ~session_id
    =
  claude_session_jsonl_path_in_project_dir
    ~project_dir:(Project_store.project_dir project_name)
    ~patch_id ~worktree_path ~session_id

let split_env_entry entry =
  match String.lsplit2 entry ~on:'=' with
  | Some (key, value) -> (key, value)
  | None -> (entry, "")

let merge_env ~base_env ~overrides =
  let merged = Hashtbl.create (module String) in
  Array.iter base_env ~f:(fun entry ->
      let key, value = split_env_entry entry in
      Hashtbl.set merged ~key ~data:value);
  List.iter overrides ~f:(fun (key, value) ->
      Hashtbl.set merged ~key ~data:value);
  Hashtbl.to_alist merged
  |> List.sort ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2)
  |> List.map ~f:(fun (key, value) -> key ^ "=" ^ value)
  |> Array.of_list

let rec remove_tree path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stat -> (
      match stat.Unix.st_kind with
      | Unix.S_DIR ->
          Stdlib.Sys.readdir path
          |> Array.iter ~f:(fun child ->
              remove_tree (Stdlib.Filename.concat path child));
          Unix.rmdir path
      | Unix.S_REG | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
      | Unix.S_SOCK ->
          Unix.unlink path)

let with_temp_project_dir f =
  let dir = Stdlib.Filename.temp_dir "onton-spawn-env-" "" in
  Stdlib.Fun.protect ~finally:(fun () -> remove_tree dir) (fun () -> f dir)

let%test "distinct patch_ids yield distinct config dirs" =
  with_temp_project_dir @@ fun project_dir ->
  let patch_a = Types.Patch_id.of_string "patch-1" in
  let patch_b = Types.Patch_id.of_string "patch-2" in
  let state_dir patch_id =
    Stdlib.Filename.concat
      (Stdlib.Filename.concat (sandbox_root ~project_dir ~patch_id) "claude")
      "claude"
  in
  let env_a =
    per_patch_env_in_state_dir ~backend:"claude" ~state_dir:(state_dir patch_a)
  in
  let env_b =
    per_patch_env_in_state_dir ~backend:"claude" ~state_dir:(state_dir patch_b)
  in
  let find key env = List.Assoc.find env ~equal:String.equal key in
  not
    (String.equal
       (Option.value_exn (find "CLAUDE_CONFIG_DIR" env_a))
       (Option.value_exn (find "CLAUDE_CONFIG_DIR" env_b)))

let%test "merged env contains per-patch overrides" =
  with_temp_project_dir @@ fun project_dir ->
  let patch_a = Types.Patch_id.of_string "patch-1" in
  let patch_b = Types.Patch_id.of_string "patch-2" in
  let base_env =
    [|
      "PATH=/usr/bin";
      "CLAUDE_CONFIG_DIR=/shared/claude";
      "CODEX_HOME=/shared/codex";
    |]
  in
  let env_a =
    merge_env ~base_env
      ~overrides:
        (per_patch_env_in_state_dir ~backend:"opencode"
           ~state_dir:
             (Stdlib.Filename.concat
                (Stdlib.Filename.concat
                   (sandbox_root ~project_dir ~patch_id:patch_a)
                   "opencode")
                "openai"))
  in
  let env_b =
    merge_env ~base_env
      ~overrides:
        (per_patch_env_in_state_dir ~backend:"opencode"
           ~state_dir:
             (Stdlib.Filename.concat
                (Stdlib.Filename.concat
                   (sandbox_root ~project_dir ~patch_id:patch_b)
                   "opencode")
                "openai"))
  in
  let find key env =
    Array.find_map env ~f:(fun entry ->
        match String.lsplit2 entry ~on:'=' with
        | Some (k, v) when String.equal k key -> Some v
        | _ -> None)
  in
  Option.is_some (find "PATH" env_a)
  && Option.is_some (find "OPENCODE_CONFIG_DIR" env_a)
  &&
  let opencode_a = Option.value_exn (find "OPENCODE_CONFIG_DIR" env_a) in
  let opencode_b = Option.value_exn (find "OPENCODE_CONFIG_DIR" env_b) in
  String.is_substring opencode_a
    ~substring:"spawn-envs/patch-1/sandbox/opencode/openai/config"
  && Option.equal String.equal
       (find "CLAUDE_CONFIG_DIR" env_a)
       (Some "/shared/claude")
  && Option.equal String.equal (find "CODEX_HOME" env_a) (Some "/shared/codex")
  && not (String.equal opencode_a opencode_b)

let%test
    "claude_session_jsonl_path: composes per-patch claude dir + cwd key + \
     session id" =
  let path =
    claude_session_jsonl_path_in_project_dir ~project_dir:"/proj"
      ~patch_id:(Types.Patch_id.of_string "42")
      ~worktree_path:"/Users/x/worktrees/foo/patch-42"
      ~session_id:"c9f311bc-7e1a-4b36-bc1f-940513fb75f9"
  in
  String.equal path
    "/proj/spawn-envs/42/sandbox/claude/claude/config/projects/-Users-x-worktrees-foo-patch-42/c9f311bc-7e1a-4b36-bc1f-940513fb75f9.jsonl"

let%test "claude_project_dir_key: replaces every slash with a dash" =
  String.equal (claude_project_dir_key ~worktree_path:"/a/b/c") "-a-b-c"
  && String.equal (claude_project_dir_key ~worktree_path:"/") "-"
  && String.equal (claude_project_dir_key ~worktree_path:"") ""

let%test "resolve_claude_oauth_token: env var present wins" =
  let getenv_opt = function
    | "CLAUDE_CODE_OAUTH_TOKEN" -> Some "shell-tok"
    | _ -> None
  in
  let read_token_file _ = Some "file-tok" in
  match resolve_claude_oauth_token_with ~getenv_opt ~read_token_file with
  | Some "shell-tok" -> true
  | _ -> false

let%test "resolve_claude_oauth_token: empty env var falls back to file" =
  let getenv_opt = function
    | "CLAUDE_CODE_OAUTH_TOKEN" -> Some "   "
    | "HOME" -> Some "/h"
    | _ -> None
  in
  let read_token_file = function
    | "/h/.config/onton/claude-oauth-token" -> Some "file-tok"
    | _ -> None
  in
  match resolve_claude_oauth_token_with ~getenv_opt ~read_token_file with
  | Some "file-tok" -> true
  | _ -> false

let%test "resolve_claude_oauth_token: HOME-based fallback strips whitespace" =
  let getenv_opt = function "HOME" -> Some "/h" | _ -> None in
  let read_token_file = function
    | "/h/.config/onton/claude-oauth-token" -> Some "  tok\n"
    | _ -> None
  in
  match resolve_claude_oauth_token_with ~getenv_opt ~read_token_file with
  | Some "tok" -> true
  | _ -> false

let%test "resolve_claude_oauth_token: XDG_CONFIG_HOME wins over HOME" =
  let getenv_opt = function
    | "XDG_CONFIG_HOME" -> Some "/x"
    | "HOME" -> Some "/h"
    | _ -> None
  in
  let read_token_file = function
    | "/x/onton/claude-oauth-token" -> Some "xdg-tok"
    | _ -> None
  in
  match resolve_claude_oauth_token_with ~getenv_opt ~read_token_file with
  | Some "xdg-tok" -> true
  | _ -> false

let%test "resolve_claude_oauth_token: missing file → None" =
  let getenv_opt = function "HOME" -> Some "/h" | _ -> None in
  let read_token_file _ = None in
  Option.is_none (resolve_claude_oauth_token_with ~getenv_opt ~read_token_file)

let%test "resolve_claude_oauth_token: empty file → None" =
  let getenv_opt = function "HOME" -> Some "/h" | _ -> None in
  let read_token_file _ = Some "   \n" in
  Option.is_none (resolve_claude_oauth_token_with ~getenv_opt ~read_token_file)

let%test "resolve_claude_oauth_token: no HOME, no XDG → None" =
  let getenv_opt _ = None in
  let read_token_file _ = Some "tok" in
  Option.is_none (resolve_claude_oauth_token_with ~getenv_opt ~read_token_file)

let%test "resolve_claude_oauth_token: empty HOME is missing" =
  let getenv_opt = function "HOME" -> Some "" | _ -> None in
  let read_token_file _ = Some "tok" in
  Option.is_none (resolve_claude_oauth_token_with ~getenv_opt ~read_token_file)
