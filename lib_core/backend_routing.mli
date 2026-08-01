(* @archlint.module interface
   @archlint.domain backend-routing *)

(** Selection of the single backend/model pair used by a run. *)

type decision = { backend : string; model : string option }

val decide : backend:string -> model:string option -> decision
(** Construct the resolved pair. [None] means the provider chooses its default
    model; model names have no Onton-specific sentinel values. *)

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
