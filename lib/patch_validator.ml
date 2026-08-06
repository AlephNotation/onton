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
  | Check_modified_repository of Check.t * string list
[@@deriving show, eq]

type preparation = No_changes | Committed | Rebase_continued
[@@deriving show, eq]

type prepare_failure =
  | Validation_failed of failure
  | Git_failed of command_failure
  | Rebase_conflict_remaining of string list * command_failure
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

let read_changed_files ~process_mgr ~clock ~fs ~cwd commands =
  List.fold_result commands ~init:[] ~f:(fun changed command ->
      match run_command ~process_mgr ~clock ~fs ~cwd command with
      | Error failure -> Error failure
      | Ok output ->
          Ok
            (String.split_lines output
            |> List.filter ~f:(fun path -> not (String.is_empty path))
            |> List.append changed))
  |> Result.map ~f:(List.dedup_and_sort ~compare:String.compare)

let pending_files ~process_mgr ~clock ~fs ~cwd =
  read_changed_files ~process_mgr ~clock ~fs ~cwd
    [
      [ "git"; "diff"; "--name-only"; "--no-renames" ];
      [ "git"; "diff"; "--cached"; "--name-only"; "--no-renames" ];
      [ "git"; "ls-files"; "--others"; "--exclude-standard" ];
    ]

let changed_files ~process_mgr ~clock ~fs ~cwd ~base_branch =
  let range = Branch.to_string base_branch ^ "...HEAD" in
  read_changed_files ~process_mgr ~clock ~fs ~cwd
    [
      [ "git"; "diff"; "--name-only"; "--no-renames"; range ];
      [ "git"; "diff"; "--name-only"; "--no-renames" ];
      [ "git"; "diff"; "--cached"; "--name-only"; "--no-renames" ];
      [ "git"; "ls-files"; "--others"; "--exclude-standard" ];
    ]
  |> Result.map_error ~f:(fun failure -> Scope_read_failed failure)

let run_checks ~process_mgr ~clock ~fs ~cwd checks =
  List.fold_result checks ~init:() ~f:(fun () check ->
      match
        run_command ~process_mgr ~clock ~fs ~cwd
          [ "/bin/sh"; "-lc"; check.Check.run ]
      with
      | Ok _ -> Ok ()
      | Error failure -> Error (Check_failed (check, failure)))

type repository_state = { head : string; index_tree : string } [@@deriving eq]

let repository_state ~process_mgr ~clock ~fs ~cwd =
  Result.bind
    (run_command ~process_mgr ~clock ~fs ~cwd [ "git"; "rev-parse"; "HEAD" ])
    ~f:(fun head ->
      Result.map
        (run_command ~process_mgr ~clock ~fs ~cwd [ "git"; "write-tree" ])
        ~f:(fun index_tree ->
          { head = String.strip head; index_tree = String.strip index_tree }))

let unstaged_files ~process_mgr ~clock ~fs ~cwd =
  read_changed_files ~process_mgr ~clock ~fs ~cwd
    [
      [ "git"; "diff"; "--name-only"; "--no-renames" ];
      [ "git"; "ls-files"; "--others"; "--exclude-standard" ];
    ]

let restore_repository_state ~process_mgr ~clock ~fs ~cwd before =
  let git command = run_command ~process_mgr ~clock ~fs ~cwd command in
  let ( let* ) result f = Result.bind result ~f in
  let* _ = git [ "git"; "reset"; "--soft"; before.head ] in
  let* _ = git [ "git"; "read-tree"; "--reset"; "-u"; before.index_tree ] in
  let* remaining = unstaged_files ~process_mgr ~clock ~fs ~cwd in
  let* _ =
    match remaining with
    | [] -> Ok ""
    | paths -> git ([ "git"; "clean"; "-fd"; "--" ] @ paths)
  in
  let* restored = repository_state ~process_mgr ~clock ~fs ~cwd in
  let* remaining = unstaged_files ~process_mgr ~clock ~fs ~cwd in
  if equal_repository_state restored before && List.is_empty remaining then
    Ok ()
  else
    Error
      {
        command = "restore repository after mutating check";
        exit_code = None;
        stdout = "";
        stderr =
          "controller could not restore the exact pre-check HEAD, index, and \
           worktree";
      }

let unresolved_files ~process_mgr ~clock ~fs ~cwd =
  read_changed_files ~process_mgr ~clock ~fs ~cwd
    [ [ "git"; "diff"; "--name-only"; "--no-renames"; "--diff-filter=U" ] ]

let run_checks_unchanged ~process_mgr ~clock ~fs ~cwd checks =
  let check_one check =
    match repository_state ~process_mgr ~clock ~fs ~cwd with
    | Error failure -> Error (`Git failure)
    | Ok before -> begin
        let check_result =
          run_command ~process_mgr ~clock ~fs ~cwd
            [ "/bin/sh"; "-lc"; check.Check.run ]
        in
        match repository_state ~process_mgr ~clock ~fs ~cwd with
        | Error failure -> Error (`Git failure)
        | Ok after -> begin
            match unstaged_files ~process_mgr ~clock ~fs ~cwd with
            | Error failure -> Error (`Git failure)
            | Ok unstaged -> (
                let metadata_changed =
                  not (equal_repository_state before after)
                in
                if metadata_changed || not (List.is_empty unstaged) then
                  let changed =
                    (if metadata_changed then [ "<Git HEAD or index changed>" ]
                     else [])
                    @ unstaged
                  in
                  match
                    restore_repository_state ~process_mgr ~clock ~fs ~cwd before
                  with
                  | Ok () ->
                      Error
                        (`Validation
                           (Check_modified_repository (check, changed)))
                  | Error failure -> Error (`Git failure)
                else
                  match check_result with
                  | Ok _ -> Ok ()
                  | Error failure ->
                      Error (`Validation (Check_failed (check, failure))))
          end
      end
  in
  List.fold_result checks ~init:() ~f:(fun () check -> check_one check)

let run ~process_mgr ~clock ~fs ~cwd ~base_branch patch =
  match changed_files ~process_mgr ~clock ~fs ~cwd ~base_branch with
  | Error _ as error -> error
  | Ok changed -> (
      match Patch_scope.outside_scope ~allowed:patch.Patch.files ~changed with
      | _ :: _ as paths -> Error (Outside_scope paths)
      | [] -> run_checks ~process_mgr ~clock ~fs ~cwd patch.Patch.checks)

let commit_subject ~project_name (patch : Patch.t) =
  Printf.sprintf "[%s] Patch %s" project_name (Patch_id.to_string patch.id)

let pr_title (patch : Patch.t) =
  Printf.sprintf "Patch %s" (Patch_id.to_string patch.id)

let prepare ~process_mgr ~clock ~fs ~cwd ~base_branch ~project_name
    ~rebase_in_progress patch =
  let validate_scope () =
    match changed_files ~process_mgr ~clock ~fs ~cwd ~base_branch with
    | Error failure -> Error (Validation_failed failure)
    | Ok changed -> (
        match Patch_scope.outside_scope ~allowed:patch.Patch.files ~changed with
        | _ :: _ as paths -> Error (Validation_failed (Outside_scope paths))
        | [] -> Ok ())
  in
  let git command =
    Result.map_error (run_command ~process_mgr ~clock ~fs ~cwd command)
      ~f:(fun failure -> Git_failed failure)
  in
  let validate_checks () =
    match
      run_checks_unchanged ~process_mgr ~clock ~fs ~cwd patch.Patch.checks
    with
    | Ok () -> Ok ()
    | Error (`Validation failure) -> Error (Validation_failed failure)
    | Error (`Git failure) -> Error (Git_failed failure)
  in
  let ( let* ) result f = Result.bind result ~f in
  let* () = validate_scope () in
  let* pending =
    pending_files ~process_mgr ~clock ~fs ~cwd
    |> Result.map_error ~f:(fun failure -> Git_failed failure)
  in
  let* _ =
    match pending with
    | [] -> Ok ""
    | files -> git ([ "git"; "add"; "-A"; "--" ] @ files)
  in
  let* continued =
    if rebase_in_progress then
      match
        run_command ~process_mgr ~clock ~fs ~cwd
          [ "git"; "-c"; "core.editor=true"; "rebase"; "--continue" ]
      with
      | Ok _ -> Ok true
      | Error failure -> (
          match unresolved_files ~process_mgr ~clock ~fs ~cwd with
          | Ok (_ :: _ as paths) ->
              Error (Rebase_conflict_remaining (paths, failure))
          | Ok [] | Error _ -> Error (Git_failed failure))
    else Ok false
  in
  let* () = validate_scope () in
  let* () = validate_checks () in
  if continued then Ok Rebase_continued
  else
    let* staged = git [ "git"; "diff"; "--cached"; "--name-only" ] in
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
        ~f:(fun _ -> Committed)
