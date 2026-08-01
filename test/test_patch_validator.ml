(* @archlint.module test
   @archlint.domain patch-validator *)

open Base
open Onton
open Onton_core.Types

let check run proves = Check.{ run; proves }

let () =
  let outside =
    Patch_validator.outside_scope ~allowed:[ "lib/a.ml" ]
      ~changed:[ "test/b.ml"; "lib/a.ml"; "test/b.ml" ]
  in
  assert (List.equal String.equal outside [ "test/b.ml" ]);
  Eio_main.run @@ fun env ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let fs = Eio.Stdenv.fs env in
  let cwd = Stdlib.Filename.temp_dir "onton-patch-validator-" "" in
  let marker name = Stdlib.Filename.concat cwd name in
  let cleanup () =
    List.iter [ "passed"; "should-not-run" ] ~f:(fun name ->
        let path = marker name in
        if Stdlib.Sys.file_exists path then Stdlib.Sys.remove path);
    Unix.rmdir cwd
  in
  Exn.protect ~finally:cleanup ~f:(fun () ->
      let run_checks checks =
        Patch_validator.run_checks ~process_mgr ~clock ~fs ~cwd checks
      in
      assert (
        Result.is_ok
          (run_checks
             [
               check "printf passed > passed" "the command runs in the worktree";
             ]));
      assert (Stdlib.Sys.file_exists (marker "passed"));
      let failure =
        run_checks
          [
            check "printf evidence; printf diagnostic >&2; exit 7"
              "failure output is captured";
            check "touch should-not-run" "execution stops after failure";
          ]
      in
      match failure with
      | Ok () -> assert false
      | Error (Patch_validator.Check_failed (_, result)) ->
          assert (
            Option.equal Int.equal result.Patch_validator.exit_code (Some 7));
          assert (String.equal result.Patch_validator.stdout "evidence");
          assert (String.equal result.Patch_validator.stderr "diagnostic");
          assert (not (Stdlib.Sys.file_exists (marker "should-not-run")))
      | Error
          (Patch_validator.Scope_read_failed _ | Patch_validator.Outside_scope _)
        ->
          assert false)
