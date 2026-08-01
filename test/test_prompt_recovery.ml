(* @archlint.module test
   @archlint.domain prompt-recovery *)

open Base
open Onton

(** Unit tests for the merge-conflict prompt's controller boundary.

    Pin the verbatim text the patch agent receives so an accidental change to
    the prompt is caught at test time rather than granting a worker Git
    authority during conflict recovery. *)

let assert_contains label haystack ~substring =
  if not (String.is_substring haystack ~substring) then (
    Stdlib.print_endline ("FAIL: " ^ label);
    Stdlib.print_endline ("  expected substring: " ^ substring);
    Stdlib.print_endline "  ----- prompt -----";
    Stdlib.print_endline haystack;
    Stdlib.print_endline "  ----- end prompt -----";
    Stdlib.exit 1)

let assert_not_contains label haystack ~substring =
  if String.is_substring haystack ~substring then (
    Stdlib.print_endline ("FAIL: " ^ label);
    Stdlib.print_endline ("  unexpected substring: " ^ substring);
    Stdlib.exit 1)

(* Property #11: legacy byte-equal regression. With no conflict_info, the
   output must match what callers got before the recovery section existed. *)
let () =
  let with_ci_none =
    Prompt.render_merge_conflict_prompt ~project_name:"" ~base_branch:"main" ()
  in
  let with_ci_explicit_none =
    Prompt.render_merge_conflict_prompt ~project_name:"" ~base_branch:"main"
      ?conflict_info:None ()
  in
  if not (String.equal with_ci_none with_ci_explicit_none) then (
    Stdlib.print_endline
      "FAIL: omitting conflict_info != passing ?conflict_info:None";
    Stdlib.exit 1);
  assert_not_contains "no conflict_info -> no recovery context" with_ci_none
    ~substring:"## Controller recovery context"

(* Conflict metadata is diagnostic context only. It must never turn into Git
   commands that a sandboxed worker is asked to execute. *)
let () =
  let ci : Worktree.conflict_info =
    Worktree.
      {
        target = "origin/main";
        old_base = "deadbeefcafef00d";
        unique_commits =
          [
            { sha = "newest1abc"; subject = "[proj] Patch 7: head" };
            { sha = "middle2def"; subject = "[proj] Patch 7: middle" };
            { sha = "oldest3ghi"; subject = "[proj] Patch 7: tail" };
          ];
        strategy = Onto;
        orig_head = "abcdef0123456789abcdef0123456789abcdef01";
      }
  in
  let prompt =
    Prompt.render_merge_conflict_prompt ~project_name:"" ~base_branch:"main"
      ~conflict_info:ci ()
  in
  assert_contains "Onto: contains controller recovery header" prompt
    ~substring:"## Controller recovery context";
  assert_contains "Onto: identifies the controller-owned target" prompt
    ~substring:"target `origin/main`";
  assert_contains "Onto: retains the original HEAD for controller context"
    prompt ~substring:"abcdef0123456789abcdef0123456789abcdef01";
  assert_contains "Onto: tells the worker not to run Git" prompt
    ~substring:"Do not run Git commands";
  List.iter [ "git fetch"; "git rebase"; "git reset"; "git add"; "git push" ]
    ~f:(fun command ->
      assert_not_contains
        ("Onto: excludes " ^ command)
        prompt ~substring:command)

(* The strategy does not widen the worker boundary. *)
let () =
  let ci : Worktree.conflict_info =
    Worktree.
      {
        target = "origin/release";
        old_base = "";
        unique_commits = [];
        strategy = Plain;
        orig_head = "";
      }
  in
  let prompt =
    Prompt.render_merge_conflict_prompt ~project_name:"" ~base_branch:"release"
      ~conflict_info:ci ()
  in
  let recovery_section =
    match
      String.substr_index prompt ~pattern:"## Controller recovery context"
    with
    | Some i -> String.subo prompt ~pos:i
    | None -> ""
  in
  assert_contains "Plain: contains controller recovery header" recovery_section
    ~substring:"## Controller recovery context";
  assert_contains "Plain: names the controller-owned target" recovery_section
    ~substring:"target `origin/release`";
  assert_not_contains "Plain: contains no worker Git command" recovery_section
    ~substring:"git "

let () = Stdlib.print_endline "All prompt-recovery tests passed."
