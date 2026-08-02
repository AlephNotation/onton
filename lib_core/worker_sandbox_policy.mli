open Base

type network = Denied | Https_only [@@deriving show, eq, sexp_of, compare]

type t = private {
  worktree : string;
  read_only_paths : string list;
  read_only_dirs : string list;
  writable_files : string list;
  writable_dirs : string list;
  creatable_dirs : string list;
  runtime_files : string list;
  runtime_roots : string list;
  state_dir : string;
  network : network;
}
[@@deriving show, eq, sexp_of, compare]

val create :
  worktree:string ->
  read_only_paths:string list ->
  read_only_dirs:string list ->
  writable_files:string list ->
  writable_dirs:string list ->
  creatable_dirs:string list ->
  runtime_files:string list ->
  runtime_roots:string list ->
  state_dir:string ->
  network:network ->
  (t, string) Result.t
(** Construct a closed worker capability set. Every path must be absolute and
    free of control characters and lexical parent traversal. *)

val add_runtime_files : t -> string list -> (t, string) Result.t
(** Add exact runtime files after validating them. Used at the final launch
    boundary for the process-group shim, whose path is not known when the patch
    capability set is constructed. *)

val macos_profile : t -> string
(** Render the deny-by-default Seatbelt profile used by [sandbox-exec]. The
    profile grants no host process signalling, inbound sockets, or ambient
    filesystem access. *)

val environment :
  allowed_provider_names:string list ->
  base:string array ->
  overrides:(string * string) list ->
  (string array, string) Result.t
(** Build the worker environment from an explicit allowlist. Unknown override
    names are rejected instead of silently widening the launch contract. *)

val allowed_environment_name :
  allowed_provider_names:string list -> string -> bool
