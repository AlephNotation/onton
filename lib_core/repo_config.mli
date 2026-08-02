(** Small per-repository operator configuration. *)

type t = {
  default_backend : string option;
  default_model : string option;
  review_team : string option;
  review_backends : Review_backend.t list;
}

val empty : t

val load :
  config_dir:string ->
  known_backends:string list ->
  ?known_review_kinds:string list ->
  unit ->
  (t, string) Stdlib.Result.t
(** Load [config.json]. The accepted top-level fields are [default],
    [review_team], and [reviewBackends]; unknown fields are rejected. *)
