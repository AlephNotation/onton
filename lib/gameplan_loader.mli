(* @archlint.module interface
   @archlint.domain plan-parser *)

val parse_file : string -> (Gameplan_parser.t, string) result
(** Read and parse a JSON gameplan file. *)
