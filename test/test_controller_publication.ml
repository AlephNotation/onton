open Base
open Onton
open Onton_core.Types

let remove_tree path =
  let rec remove path =
    match Unix.lstat path with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | stat -> (
        match stat.Unix.st_kind with
        | Unix.S_DIR ->
            Stdlib.Sys.readdir path
            |> Array.iter ~f:(fun child ->
                remove (Stdlib.Filename.concat path child));
            Unix.rmdir path
        | Unix.S_REG | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
        | Unix.S_SOCK ->
            Unix.unlink path)
  in
  remove path

let write path contents =
  let channel = Stdlib.Out_channel.open_text path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.Out_channel.close channel)
    (fun () -> Stdlib.Out_channel.output_string channel contents)

let git_exit cwd args =
  let command =
    String.concat ~sep:" "
      ([ "git"; "-C"; Stdlib.Filename.quote cwd ]
      @ List.map args ~f:Stdlib.Filename.quote)
  in
  Stdlib.Sys.command command

let git cwd args =
  let command = String.concat ~sep:" " args in
  match git_exit cwd args with
  | 0 -> ()
  | code -> failwith (Printf.sprintf "git setup failed (%d): %s" code command)

let read_command command =
  let channel = Unix.open_process_in command in
  Stdlib.Fun.protect
    ~finally:(fun () -> ignore (Unix.close_process_in channel))
    (fun () -> Stdlib.In_channel.input_all channel |> String.strip)

let () =
  Eio_main.run @@ fun eio ->
  let process_mgr = Eio.Stdenv.process_mgr eio in
  let clock = Eio.Stdenv.clock eio in
  let fs = Eio.Stdenv.fs eio in
  let repo = Stdlib.Filename.temp_dir "onton-controller-publication-" "" in
  Stdlib.Fun.protect ~finally:(fun () -> remove_tree repo) @@ fun () ->
  git repo [ "init"; "-b"; "main" ];
  git repo [ "config"; "user.name"; "Onton Test" ];
  git repo [ "config"; "user.email"; "onton@example.invalid" ];
  let owned = Stdlib.Filename.concat repo "owned.txt" in
  let removed = Stdlib.Filename.concat repo "removed.txt" in
  let check_count = Stdlib.Filename.concat repo ".git/check-count" in
  write owned "base\n";
  write removed "delete me\n";
  git repo [ "add"; "owned.txt"; "removed.txt" ];
  git repo [ "commit"; "-m"; "base" ];
  git repo [ "checkout"; "-b"; "patch/test" ];
  let patch : Patch.t =
    {
      Patch.id = Patch_id.of_string "7";
      goal = "record accepted worker output";
      branch = Branch.of_string "patch/test";
      dependencies = [];
      files = [ "owned.txt"; "removed.txt" ];
      checks =
        [
          {
            Check.run = "printf x >> .git/check-count; test -s owned.txt";
            proves = "file exists";
          };
        ];
      agent = None;
    }
  in
  write owned "worker change\n";
  Unix.unlink removed;
  let prepared =
    Patch_validator.prepare ~process_mgr ~clock ~fs ~cwd:repo
      ~base_branch:(Branch.of_string "main") ~project_name:"test-project"
      ~rebase_in_progress:false patch
  in
  (match prepared with
  | Ok Patch_validator.Committed -> ()
  | Ok (Patch_validator.No_changes | Patch_validator.Rebase_continued)
  | Error
      ( Patch_validator.Validation_failed _ | Patch_validator.Git_failed _
      | Patch_validator.Rebase_conflict_remaining _ ) ->
      assert false);
  assert (String.equal (read_command ("cat " ^ check_count)) "x");
  let subject =
    read_command
      (Printf.sprintf "git -C %s log -1 --format=%%s"
         (Stdlib.Filename.quote repo))
  in
  assert (String.equal subject "[test-project] Patch 7");
  (match
     Patch_validator.prepare ~process_mgr ~clock ~fs ~cwd:repo
       ~base_branch:(Branch.of_string "main") ~project_name:"test-project"
       ~rebase_in_progress:false patch
   with
  | Ok Patch_validator.No_changes -> ()
  | Ok (Patch_validator.Committed | Patch_validator.Rebase_continued)
  | Error
      ( Patch_validator.Validation_failed _ | Patch_validator.Git_failed _
      | Patch_validator.Rebase_conflict_remaining _ ) ->
      assert false);
  assert (String.equal (read_command ("cat " ^ check_count)) "xx");
  assert (
    String.is_empty
      (read_command
         (Printf.sprintf "git -C %s status --porcelain"
            (Stdlib.Filename.quote repo))));
  write owned "intended worker change\n";
  let mutating_patch =
    {
      patch with
      Patch.checks =
        [
          {
            Check.run = "printf mutation > check-created.txt";
            proves = "a deliberately invalid mutating check";
          };
        ];
    }
  in
  (match
     Patch_validator.prepare ~process_mgr ~clock ~fs ~cwd:repo
       ~base_branch:(Branch.of_string "main") ~project_name:"test-project"
       ~rebase_in_progress:false mutating_patch
   with
  | Error
      (Patch_validator.Validation_failed
         (Patch_validator.Check_modified_repository (_, [ "check-created.txt" ])))
    ->
      ()
  | Ok
      ( Patch_validator.No_changes | Patch_validator.Committed
      | Patch_validator.Rebase_continued )
  | Error (Patch_validator.Git_failed _)
  | Error (Patch_validator.Rebase_conflict_remaining _)
  | Error
      (Patch_validator.Validation_failed
         ( Patch_validator.Scope_read_failed _ | Patch_validator.Check_failed _
         | Patch_validator.Check_modified_repository _
         | Patch_validator.Outside_scope _ )) ->
      assert false);
  assert (
    not
      (Stdlib.Sys.file_exists (Stdlib.Filename.concat repo "check-created.txt")));
  assert (String.equal (read_command ("cat " ^ owned)) "intended worker change");
  assert (
    String.equal
      (read_command
         (Printf.sprintf "git -C %s diff --cached --name-only"
            (Stdlib.Filename.quote repo)))
      "owned.txt");
  let failing_mutating_patch =
    {
      patch with
      Patch.checks =
        [
          {
            Check.run = "printf mutation > check-created.txt; exit 1";
            proves = "a failing check still cannot mutate the repository";
          };
        ];
    }
  in
  (match
     Patch_validator.prepare ~process_mgr ~clock ~fs ~cwd:repo
       ~base_branch:(Branch.of_string "main") ~project_name:"test-project"
       ~rebase_in_progress:false failing_mutating_patch
   with
  | Error
      (Patch_validator.Validation_failed
         (Patch_validator.Check_modified_repository (_, [ "check-created.txt" ])))
    ->
      ()
  | Ok
      ( Patch_validator.No_changes | Patch_validator.Committed
      | Patch_validator.Rebase_continued )
  | Error (Patch_validator.Git_failed _)
  | Error (Patch_validator.Rebase_conflict_remaining _)
  | Error
      (Patch_validator.Validation_failed
         ( Patch_validator.Scope_read_failed _ | Patch_validator.Outside_scope _
         | Patch_validator.Check_failed _
         | Patch_validator.Check_modified_repository _ )) ->
      assert false);
  assert (
    not
      (Stdlib.Sys.file_exists (Stdlib.Filename.concat repo "check-created.txt")));
  git repo [ "reset"; "--hard"; "HEAD" ];
  write (Stdlib.Filename.concat repo "outside.txt") "escape\n";
  (match
     Patch_validator.prepare ~process_mgr ~clock ~fs ~cwd:repo
       ~base_branch:(Branch.of_string "main") ~project_name:"test-project"
       ~rebase_in_progress:false patch
   with
  | Error
      (Patch_validator.Validation_failed
         (Patch_validator.Outside_scope [ "outside.txt" ])) ->
      ()
  | Ok
      ( Patch_validator.No_changes | Patch_validator.Committed
      | Patch_validator.Rebase_continued )
  | Error (Patch_validator.Git_failed _)
  | Error (Patch_validator.Rebase_conflict_remaining _)
  | Error
      (Patch_validator.Validation_failed
         ( Patch_validator.Scope_read_failed _ | Patch_validator.Check_failed _
         | Patch_validator.Check_modified_repository _
         | Patch_validator.Outside_scope _ )) ->
      assert false);
  let rebase_repo = Stdlib.Filename.temp_dir "onton-multi-conflict-" "" in
  Stdlib.Fun.protect ~finally:(fun () -> remove_tree rebase_repo) @@ fun () ->
  git rebase_repo [ "init"; "-b"; "main" ];
  git rebase_repo [ "config"; "user.name"; "Onton Test" ];
  git rebase_repo [ "config"; "user.email"; "onton@example.invalid" ];
  write (Stdlib.Filename.concat rebase_repo "a.txt") "base-a\n";
  write (Stdlib.Filename.concat rebase_repo "b.txt") "base-b\n";
  git rebase_repo [ "add"; "a.txt"; "b.txt" ];
  git rebase_repo [ "commit"; "-m"; "base" ];
  git rebase_repo [ "checkout"; "-b"; "patch/multi" ];
  write (Stdlib.Filename.concat rebase_repo "a.txt") "patch-a\n";
  git rebase_repo [ "commit"; "-am"; "patch a" ];
  write (Stdlib.Filename.concat rebase_repo "b.txt") "patch-b\n";
  git rebase_repo [ "commit"; "-am"; "patch b" ];
  git rebase_repo [ "checkout"; "main" ];
  write (Stdlib.Filename.concat rebase_repo "a.txt") "main-a\n";
  write (Stdlib.Filename.concat rebase_repo "b.txt") "main-b\n";
  git rebase_repo [ "commit"; "-am"; "main changes" ];
  git rebase_repo [ "checkout"; "patch/multi" ];
  assert (git_exit rebase_repo [ "rebase"; "main" ] <> 0);
  write (Stdlib.Filename.concat rebase_repo "a.txt") "resolved-a\n";
  git rebase_repo [ "add"; "a.txt" ];
  let multi_patch =
    {
      patch with
      Patch.branch = Branch.of_string "patch/multi";
      files = [ "a.txt"; "b.txt" ];
      checks = [];
    }
  in
  match
    Patch_validator.prepare ~process_mgr ~clock ~fs ~cwd:rebase_repo
      ~base_branch:(Branch.of_string "main") ~project_name:"test-project"
      ~rebase_in_progress:true multi_patch
  with
  | Error (Patch_validator.Rebase_conflict_remaining ([ "b.txt" ], _failure)) ->
      ()
  | Ok
      ( Patch_validator.No_changes | Patch_validator.Committed
      | Patch_validator.Rebase_continued )
  | Error (Patch_validator.Git_failed _)
  | Error (Patch_validator.Validation_failed _)
  | Error (Patch_validator.Rebase_conflict_remaining _) ->
      assert false
