(* @archlint.module shell
   @archlint.domain patch-validator *)

open Base
open Onton_core.Types

type command_failure = {
  command : string;
  exit_code : int option;
  stdout : string;
  stderr : string;
}
[@@deriving show, eq]

type failure =
  | Scope_read_failed of command_failure
  | Outside_scope of string list
  | Check_failed of Check.t * command_failure
[@@deriving show, eq]

type preparation = No_changes | Committed | Rebase_continued
[@@deriving show, eq]

type prepare_failure =
  | Validation_failed of failure
  | Git_failed of command_failure
[@@deriving show, eq]

let timeout_seconds = 600.0

let run_command ~process_mgr ~clock ~fs ~cwd args =
  let stdout = Buffer.create 4096 in
  let stderr = Buffer.create 4096 in
  let worktree = Eio.Path.(fs / cwd) in
  let execute () =
    Eio.Switch.run @@ fun sw ->
    let child =
      Eio.Process.spawn ~sw process_mgr ~cwd:worktree
        ~env:(Git_env.clean_env ())
        ~stdout:(Eio.Flow.buffer_sink stdout)
        ~stderr:(Eio.Flow.buffer_sink stderr)
        args
    in
    Eio.Process.await child
  in
  let command = String.concat ~sep:" " args in
  let failure exit_code message =
    Error
      { command; exit_code; stdout = Buffer.contents stdout; stderr = message }
  in
  match
    Eio.Time.with_timeout clock timeout_seconds (fun () -> Ok (execute ()))
  with
  | Ok (`Exited 0) -> Ok (Buffer.contents stdout)
  | Ok (`Exited exit_code) -> failure (Some exit_code) (Buffer.contents stderr)
  | Ok (`Signaled signal) ->
      failure (Some (128 + signal)) (Buffer.contents stderr)
  | Error `Timeout -> failure None "command timed out after 600 seconds"
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn -> failure None (Exn.to_string exn)

let changed_files ~process_mgr ~clock ~fs ~cwd ~base_branch =
  let range = Branch.to_string base_branch ^ "...HEAD" in
  let commands =
    [
      [ "git"; "diff"; "--name-only"; "--no-renames"; range ];
      [ "git"; "diff"; "--name-only"; "--no-renames" ];
      [ "git"; "diff"; "--cached"; "--name-only"; "--no-renames" ];
      [ "git"; "ls-files"; "--others"; "--exclude-standard" ];
    ]
  in
  List.fold_result commands ~init:[] ~f:(fun changed command ->
      match run_command ~process_mgr ~clock ~fs ~cwd command with
      | Error failure -> Error (Scope_read_failed failure)
      | Ok output ->
          Ok
            (String.split_lines output
            |> List.filter ~f:(fun path -> not (String.is_empty path))
            |> List.append changed))
  |> Result.map ~f:(List.dedup_and_sort ~compare:String.compare)

let run_checks ~process_mgr ~clock ~fs ~cwd checks =
  List.fold_result checks ~init:() ~f:(fun () check ->
      match
        run_command ~process_mgr ~clock ~fs ~cwd
          [ "/bin/sh"; "-lc"; check.Check.run ]
      with
      | Ok _ -> Ok ()
      | Error failure -> Error (Check_failed (check, failure)))

let run ~process_mgr ~clock ~fs ~cwd ~base_branch patch =
  match changed_files ~process_mgr ~clock ~fs ~cwd ~base_branch with
  | Error _ as error -> error
  | Ok changed -> (
      match Patch_scope.outside_scope ~allowed:patch.Patch.files ~changed with
      | _ :: _ as paths -> Error (Outside_scope paths)
      | [] -> run_checks ~process_mgr ~clock ~fs ~cwd patch.Patch.checks)

let commit_subject ~project_name (patch : Patch.t) =
  let goal =
    patch.goal |> String.split_lines |> List.hd
    |> Option.value ~default:"work"
    |> String.strip
  in
  Printf.sprintf "[%s] Patch %s: %s" project_name
    (Patch_id.to_string patch.id)
    (if String.is_empty goal then "work" else goal)

let prepare ~process_mgr ~clock ~fs ~cwd ~base_branch ~project_name
    ~rebase_in_progress patch =
  let validate () =
    Result.map_error (run ~process_mgr ~clock ~fs ~cwd ~base_branch patch)
      ~f:(fun failure -> Validation_failed failure)
  in
  let git command =
    Result.map_error (run_command ~process_mgr ~clock ~fs ~cwd command)
      ~f:(fun failure -> Git_failed failure)
  in
  Result.bind (validate ()) ~f:(fun () ->
      Result.bind
        (match patch.Patch.files with
        | [] -> Ok ""
        | files -> git ([ "git"; "add"; "--" ] @ files))
        ~f:(fun _ ->
          Result.bind (validate ()) ~f:(fun () ->
              if rebase_in_progress then
                Result.map
                  (git
                     [ "git"; "-c"; "core.editor=true"; "rebase"; "--continue" ])
                  ~f:(fun _ -> Rebase_continued)
              else
                Result.bind
                  (git [ "git"; "diff"; "--cached"; "--name-only" ])
                  ~f:(fun staged ->
                    if String.is_empty (String.strip staged) then Ok No_changes
                    else
                      Result.map
                        (git
                           [
                             "git";
                             "commit";
                             "--no-verify";
                             "-m";
                             commit_subject ~project_name patch;
                           ])
                        ~f:(fun _ -> Committed)))))
