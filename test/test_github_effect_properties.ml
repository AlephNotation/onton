(* @archlint.module test
   @archlint.domain github-effect *)

open Base
open Onton_core
open Types

let lifecycle_is_deterministic =
  QCheck2.Test.make ~name:"GitHub command lifecycle is deterministic" ~count:500
    QCheck2.Gen.(pair (int_range 1 10000) int_small)
    (fun (number, now_value) ->
      let now = Float.of_int now_value in
      let patch_id = Patch_id.of_string (Printf.sprintf "patch-%d" number) in
      let action =
        Github_effect.Direct_merge { pr_number = Pr_number.of_int number }
      in
      let command = Github_effect.create ~patch_id action in
      let restored =
        Github_effect.restore ~id:command.Github_effect.id ~patch_id ~action
          ~status:Github_effect.Running ~attempts:(-number) ~last_error:None
      in
      let claimed = Github_effect.claim ~now restored in
      let retried =
        Github_effect.retry ~now ~delay:1.0 ~error:"retry" command
      in
      let failed = Github_effect.fail ~error:"failed" command in
      Effect_id.equal command.Github_effect.id
        (Github_effect.identity ~patch_id action)
      && Github_effect.runnable ~now restored
      && Option.is_some claimed
      && (not (Github_effect.runnable ~now retried))
      && Github_effect.runnable ~now:(now +. 1.0) retried
      && (not (Github_effect.runnable ~now failed))
      && restored.Github_effect.attempts = 0)

let () = QCheck2.Test.check_exn lifecycle_is_deterministic
