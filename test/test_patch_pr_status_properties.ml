(* @archlint.module test
   @archlint.domain patch-pr-status *)

open Base
open Onton_core
open Types

let lifecycle_and_codec_agree =
  QCheck2.Test.make ~name:"PR status lifecycle and codec agree" ~count:500
    QCheck2.Gen.(int_range 1 100000)
    (fun value ->
      let number = Pr_number.of_int value in
      let present = Patch_pr_status.set_present Patch_pr_status.Absent number in
      let decision = Patch_pr_status.classify_set_present present number in
      match
        Patch_pr_status.t_of_yojson (Patch_pr_status.yojson_of_t present)
      with
      | Error _ -> false
      | Ok decoded ->
          Patch_pr_status.has_pr present
          && Option.equal Pr_number.equal
               (Patch_pr_status.pr_number present)
               (Some number)
          && Patch_pr_status.equal_set_present_decision decision
               Patch_pr_status.Preserve_existing
          && Patch_pr_status.equal decoded present
          && Patch_pr_status.equal
               (Patch_pr_status.clear present)
               Patch_pr_status.Absent)

let () = QCheck2.Test.check_exn lifecycle_and_codec_agree
