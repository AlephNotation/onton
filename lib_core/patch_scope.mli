(* @archlint.module interface
   @archlint.domain patch-validator *)

val outside_scope : allowed:string list -> changed:string list -> string list
(** Return changed paths not declared by the patch, sorted and deduplicated. *)
