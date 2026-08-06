(** Project-level data directory management.

    Persists project configuration and plan sources to
    [~/.local/share/onton/<project-slug>/] (or [$ONTON_DATA_DIR/<slug>/]).
    Enables resuming a project by name without the original plan file. *)

val slugify : string -> string
(** Convert a project name to a filesystem-safe slug. *)

val project_dir : string -> string
(** Absolute path to a project's data directory. *)

val snapshot_path : string -> string
(** Path to the project's persisted snapshot JSON. *)

val managed_repo_dir : string -> string
(** Path to the project's onton-managed git checkout ([<project-dir>/repo]). The
    plan supplies [owner]/[repo]; onton clones the repo into this directory (or
    fetches it on resume) and uses it as the source for all worktrees. *)

val event_log_path : string -> string
(** Path to the project's append-only event log (JSONL). *)

val config_path : string -> string
(** Path to the project's persisted config JSON. *)

val ensure_dir : string -> unit
(** Create a directory and parents if needed. *)

val reset_artifact_dir : string -> unit
(** Create the directory if needed and delete every file directly inside it.
    Used at session setup on per-item artifact directories
    ({!comment_responses_dir}, {!findings_wontfix_dir}) so stale files from a
    prior session can never be replayed as fresh agent output. *)

type stored_config = {
  schema_version : int;
  project_name : string;
  github_owner : string;
  github_repo : string;
  backend : string;
  model : string;
  main_branch : string;
  poll_interval : float;
  repo_root : string;
  max_concurrency : int;
  max_ci_failures : int;
      (** Per-project cap on consecutive CI-failure responses. *)
  url_scheme : string option;
      (** Persisted transport for the managed [origin]. *)
}
[@@deriving yojson]

val save_config :
  project_name:string ->
  github_owner:string ->
  github_repo:string ->
  backend:string ->
  model:string ->
  main_branch:string ->
  poll_interval:float ->
  repo_root:string ->
  max_concurrency:int ->
  max_ci_failures:int ->
  ?url_scheme:string option ->
  unit ->
  unit
(** Persist project config to the data directory. Creates the directory if
    needed. *)

val load_config : project_name:string -> (stored_config, string) result
(** Load a strict versioned project config. *)

val save_plan_source : project_name:string -> source_path:string -> unit
(** Copy the validated JSON plan into the project data directory. *)

val plan_path : string -> string
(** Path to the stored plan ([plan.json]). *)

val sessions_dir : string -> string
(** Path to the per-session artifact directory root ([sessions/]). *)

val plan_artifact_path : string -> string
(** Absolute path of the agent-readable plan copy ([artifacts/plan.json]). Lives
    in the agent-facing [artifacts/] subtree (alongside the per-patch artifact
    dirs) so patch agents can consult the full plan on demand without being
    pointed at the project-dir root, which holds [config.json] and its token. A
    pure function of the project name — prompt layers embed it without touching
    the filesystem. *)

val publish_plan_artifact : project_name:string -> unit
(** Copy the stored plan ({!plan_path}) to {!plan_artifact_path} for patch
    agents to read. Called once at startup after save or resume validation. *)

val pr_body_artifact_path :
  project_name:string -> patch_id:Types.Patch_id.t -> string
(** Absolute path the agent writes the LLM-authored PR body to. Lives under the
    project's data directory at [artifacts/<patch_id>/pr-body.md] — outside the
    worktree so it can never be accidentally committed. *)

val completion_claim_path :
  project_name:string -> patch_id:Types.Patch_id.t -> string
(** Exact per-patch artifact path for a code turn's completion claim. *)

val clear_completion_claim :
  project_name:string -> patch_id:Types.Patch_id.t -> (unit, string) result
(** Remove the prior turn's claim. Missing is success; any other failure is
    returned so worker launch can fail closed instead of accepting stale
    evidence. *)

val read_completion_claim :
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  (Completion_claim.t, string) result
(** Read and strictly decode a bounded regular-file claim. Symlinks, oversized
    files, malformed JSON, and missing artifacts are errors. *)

val findings_wontfix_dir :
  project_name:string -> patch_id:Types.Patch_id.t -> string
(** Absolute path of the [findings_wontfix/] artifact directory for a Findings
    session. For any finding it has decided not to fix, the agent writes the
    reason to the per-finding file named on the finding's prompt block
    ([Onton_core.Review_service.wontfix_filename_of_id]); the supervisor
    consumes the directory post-session and POSTs the corresponding resolve
    verbs to the review backend. Findings without a file default to [addressed];
    a present-but-blank or unreadable file fails closed — that finding's resolve
    is skipped rather than guessed. *)

val comment_responses_dir :
  project_name:string -> patch_id:Types.Patch_id.t -> string
(** Absolute path of the [comment_responses/] artifact directory for a
    Review_comments session. The agent writes one [<comment_id>.md] file per
    comment, containing just the response text; after a successful post-session
    push the supervisor replies to and resolves every delivered comment with a
    response file. Comments without one stay unresolved and re-deliver on the
    next poll. *)

val ci_artifact_dir : project_name:string -> patch_id:Types.Patch_id.t -> string
(** Absolute directory for per-check CI diagnostics under
    [artifacts/<patch_id>/ci]. Pure path helper for prompt rendering; does not
    touch the filesystem. *)

val ci_check_key : Types.Ci_check.t -> string
(** Stable artifact key for a CI check. CheckRuns use [run-<databaseId>];
    id-less StatusContexts and synthesized checks use [status-<slugified name>].
*)

val ci_check_artifact_dir :
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  check:Types.Ci_check.t ->
  string
(** Absolute directory for one check's CI diagnostic artifacts under
    {!ci_artifact_dir}. Pure path helper for prompt rendering; does not touch
    the filesystem. *)

val publish_ci_check_artifact :
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  check:Types.Ci_check.t ->
  ?head_oid:string ->
  summary_md:string ->
  ?log:string ->
  unit ->
  (string, string) result
(** Publish one check's CI diagnostic artifacts. Writes [check.json] and
    [summary.md], plus [log.txt] exactly when [log] is supplied. Returns the
    check artifact directory, or the first write error. *)

val project_exists : string -> bool
(** Whether a project data directory with config exists. *)

val list_projects : unit -> string list
(** List all project slugs in the data directory. *)
