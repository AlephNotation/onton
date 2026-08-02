val per_patch_env : backend:string -> state_dir:string -> (string * string) list
(** Backend config-dir overrides for a resolved worker sandbox. The backing
    directory is [<state_dir>/config], inside the selected
    patch/backend/provider capability root. This function never copies or links
    ambient CLI credential stores into the worker boundary.

    On macOS, scoping [CLAUDE_CONFIG_DIR] makes Claude unable to find the
    Keychain-stored OAuth credential, so [per_patch_env] also injects
    [CLAUDE_CODE_OAUTH_TOKEN] when one is available. The token is sourced from
    the parent process env if already set; otherwise read from
    [$XDG_CONFIG_HOME/onton/claude-oauth-token] (or
    [~/.config/onton/claude-oauth-token]). Generate the token once via
    [claude setup-token] and write it to that file (mode 0600). *)

val merge_env :
  base_env:string array -> overrides:(string * string) list -> string array
(** Merge [overrides] into [base_env], replacing any existing entries with the
    same variable name. *)

val claude_session_jsonl_path :
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  worktree_path:string ->
  session_id:string ->
  string
(** Absolute path to a claude conversation file:
    [<per-patch-CLAUDE_CONFIG_DIR>/projects/<cwd-key>/<session-id>.jsonl], where
    [cwd-key] is [worktree_path] with every ["/"] replaced by ["-"] (claude's
    internal projects-dir keying). Used to clean up stub files left behind when
    [claude --resume] fails with "No conversation found". *)
