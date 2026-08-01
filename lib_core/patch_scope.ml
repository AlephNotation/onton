(* @archlint.module core
   @archlint.domain patch-validator *)

open Base

let outside_scope ~allowed ~changed =
  let allowed = Set.of_list (module String) allowed in
  List.filter changed ~f:(fun path -> not (Set.mem allowed path))
  |> List.dedup_and_sort ~compare:String.compare
