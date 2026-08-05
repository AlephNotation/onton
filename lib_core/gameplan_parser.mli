open Base

type t = {
  gameplan : Types.Gameplan.t;
  dependency_graph : Types.Patch_id.t list Map.M(Types.Patch_id).t;
  expansion : Types.Expansion_policy.t option;
}

val parse_json_string : string -> (t, string) Result.t
