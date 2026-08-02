(** Selection of the single backend/model pair used by a run. *)

type decision = { backend : string; model : string option }

val supported_backends : string list
val is_supported_backend : string -> bool

val decide : backend:string -> model:string option -> decision
(** Construct the resolved pair. [None] means the provider chooses its default
    model; model names have no Onton-specific sentinel values. *)

val for_patch : default:decision -> Types.Patch.t -> decision
(** An override wins atomically; absent overrides preserve the run default. *)

val distinct_effective_backends :
  default:decision -> Types.Patch.t list -> string list

val resolve_pair :
  cli_backend:string ->
  cli_model:string ->
  stored_backend:string ->
  stored_model:string ->
  repo_config:Repo_config.t ->
  built_in_backend:string ->
  string * string
(** Resolve each field independently using
    [CLI > stored project > repository config > built-in]. Empty and
    whitespace-only values are absent. The backend always has a built-in
    fallback; an empty model means no model argument should be passed. *)
