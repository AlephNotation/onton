(* @archlint.module test
   @archlint.domain worker-sandbox *)

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

let git cwd args =
  let command =
    String.concat ~sep:" "
      ([ "git"; "-C"; Stdlib.Filename.quote cwd ]
      @ List.map args ~f:Stdlib.Filename.quote)
  in
  match Stdlib.Sys.command command with
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
  write owned "base\n";
  git repo [ "add"; "owned.txt" ];
  git repo [ "commit"; "-m"; "base" ];
  git repo [ "checkout"; "-b"; "patch/test" ];
  let patch : Patch.t =
    {
      Patch.id = Patch_id.of_string "7";
      goal = "record accepted worker output";
      branch = Branch.of_string "patch/test";
      dependencies = [];
      files = [ "owned.txt" ];
      checks = [ { Check.run = "test -s owned.txt"; proves = "file exists" } ];
    }
  in
  write owned "worker change\n";
  let prepared =
    Patch_validator.prepare ~process_mgr ~clock ~fs ~cwd:repo
      ~base_branch:(Branch.of_string "main") ~project_name:"test-project"
      ~rebase_in_progress:false patch
  in
  (match prepared with
  | Ok Patch_validator.Committed -> ()
  | Ok (Patch_validator.No_changes | Patch_validator.Rebase_continued)
  | Error (Patch_validator.Validation_failed _ | Patch_validator.Git_failed _)
    ->
      assert false);
  let subject =
    read_command
      (Printf.sprintf "git -C %s log -1 --format=%%s"
         (Stdlib.Filename.quote repo))
  in
  assert (String.equal subject "[test-project] Patch 7");
  assert (
    String.is_empty
      (read_command
         (Printf.sprintf "git -C %s status --porcelain"
            (Stdlib.Filename.quote repo))));
  write (Stdlib.Filename.concat repo "outside.txt") "escape\n";
  match
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
  | Error
      (Patch_validator.Validation_failed
         ( Patch_validator.Scope_read_failed _ | Patch_validator.Check_failed _
         | Patch_validator.Outside_scope _ )) ->
      assert false
