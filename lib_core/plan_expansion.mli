open Base

type proposal_patch = {
  id : string;
  goal : string;
  dependencies : string list;
  files : string list;
  checks : Types.Check.t list;
}
[@@deriving show, eq, compare, sexp_of]

type proposal = proposal_patch list [@@deriving show, eq, compare, sexp_of]

type materialization = { patches : Types.Patch.t list; changed : bool }
[@@deriving show, eq, compare, sexp_of]

val parse_json_string : string -> (proposal, string) Result.t

val materialize :
  gameplan:Types.Gameplan.t ->
  policy:Types.Expansion_policy.t option ->
  parent_id:Types.Patch_id.t ->
  proposal ->
  (materialization, string) Result.t
