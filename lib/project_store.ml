open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let data_dir () =
  match Stdlib.Sys.getenv_opt "ONTON_DATA_DIR" with
  | Some d when not (String.is_empty d) -> d
  | Some _ | None -> (
      match Stdlib.Sys.getenv_opt "XDG_DATA_HOME" with
      | Some xdg -> Stdlib.Filename.concat xdg "onton"
      | None ->
          Stdlib.Filename.concat
            (Stdlib.Filename.concat (Stdlib.Sys.getenv "HOME") ".local/share")
            "onton")

let slugify name =
  String.concat_map name ~f:(fun c ->
      if Char.is_alphanum c || Char.equal c '-' then String.of_char c
      else if Char.equal c ' ' || Char.equal c '_' then "-"
      else "")
  |> String.lowercase

let project_dir project_name =
  Stdlib.Filename.concat (data_dir ()) (slugify project_name)

let snapshot_path project_name =
  Stdlib.Filename.concat (project_dir project_name) "snapshot.json"

let managed_repo_dir project_name =
  Stdlib.Filename.concat (project_dir project_name) "repo"

let event_log_path project_name =
  Stdlib.Filename.concat (project_dir project_name) "events.jsonl"

let sessions_dir project_name =
  Stdlib.Filename.concat (project_dir project_name) "sessions"

let config_path project_name =
  Stdlib.Filename.concat (project_dir project_name) "config.json"

let plan_path project_name =
  Stdlib.Filename.concat (project_dir project_name) "plan.json"

let artifacts_root project_name =
  Stdlib.Filename.concat (project_dir project_name) "artifacts"

let artifact_dir ~project_name ~patch_id =
  Stdlib.Filename.concat
    (artifacts_root project_name)
    (Types.Patch_id.to_string patch_id)

let plan_artifact_path project_name =
  Stdlib.Filename.concat (artifacts_root project_name) "plan.json"

let pr_body_artifact_path ~project_name ~patch_id =
  Stdlib.Filename.concat (artifact_dir ~project_name ~patch_id) "pr-body.md"

let completion_claim_path ~project_name ~patch_id =
  Stdlib.Filename.concat
    (artifact_dir ~project_name ~patch_id)
    "completion.json"

let clear_completion_claim ~project_name ~patch_id =
  let path = completion_claim_path ~project_name ~patch_id in
  try
    Unix.unlink path;
    Ok ()
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
  | exn ->
      Error
        (Printf.sprintf "cannot clear stale completion claim %s: %s" path
           (Exn.to_string exn))

let read_completion_claim ~project_name ~patch_id =
  let path = completion_claim_path ~project_name ~patch_id in
  try
    let path_stat = Unix.lstat path in
    if Poly.equal path_stat.Unix.st_kind Unix.S_LNK then
      Error "completion claim must not be a symlink"
    else if not (Poly.equal path_stat.Unix.st_kind Unix.S_REG) then
      Error "completion claim must be a regular file"
    else if path_stat.Unix.st_size > 4096 then
      Error "completion claim exceeds 4096 bytes"
    else
      let fd = Unix.openfile path [ Unix.O_RDONLY; Unix.O_NONBLOCK ] 0 in
      let channel = Unix.in_channel_of_descr fd in
      let result =
        Stdlib.Fun.protect
          ~finally:(fun () -> Stdlib.close_in_noerr channel)
          (fun () ->
            let opened_stat = Unix.fstat fd in
            if
              (not (Poly.equal opened_stat.Unix.st_kind Unix.S_REG))
              || opened_stat.Unix.st_dev <> path_stat.Unix.st_dev
              || opened_stat.Unix.st_ino <> path_stat.Unix.st_ino
            then Error "completion claim changed while it was opened"
            else
              let buffer = Bytes.create 4097 in
              let rec read offset =
                if offset = Bytes.length buffer then offset
                else
                  match
                    Stdlib.input channel buffer offset
                      (Bytes.length buffer - offset)
                  with
                  | 0 -> offset
                  | count -> read (offset + count)
              in
              let length = read 0 in
              let final_stat = Unix.fstat fd in
              if length > 4096 then Error "completion claim exceeds 4096 bytes"
              else if
                final_stat.Unix.st_size <> length
                || final_stat.Unix.st_size <> opened_stat.Unix.st_size
                || (not
                      (Float.equal final_stat.Unix.st_mtime opened_stat.st_mtime))
                || not
                     (Float.equal final_stat.Unix.st_ctime opened_stat.st_ctime)
              then Error "completion claim changed while it was read"
              else Ok (Stdlib.Bytes.sub_string buffer 0 length))
      in
      Result.bind result ~f:(fun content ->
          match Yojson.Safe.from_string content with
          | json -> Completion_claim.of_yojson json
          | exception exn ->
              Error ("completion claim is not valid JSON: " ^ Exn.to_string exn))
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      Error "worker did not write a completion claim"
  | exn -> Error ("cannot read completion claim: " ^ Exn.to_string exn)

(** Absolute directory the agent writes per-finding wontfix files to during a
    Findings session ([<slugged_finding_id>.md], reason text only; see
    [Review_service.wontfix_filename_of_id]). Lives alongside [pr-body.md] under
    [artifacts/<patch_id>/]. The supervisor reads it after the session to decide
    which findings to POST as ["wontfix"]; findings without a file are POSTed as
    ["addressed"]. *)
let findings_wontfix_dir ~project_name ~patch_id =
  Stdlib.Filename.concat
    (artifact_dir ~project_name ~patch_id)
    "findings_wontfix"

(** Absolute directory the agent writes per-comment response files to during a
    Review_comments session ([<comment_id>.md], response text only). Lives
    alongside [pr-body.md] under [artifacts/<patch_id>/]. After a successful
    post-session push the supervisor reads the files and, for every delivered
    comment with a response, replies to the thread and resolves it. *)
let comment_responses_dir ~project_name ~patch_id =
  Stdlib.Filename.concat
    (artifact_dir ~project_name ~patch_id)
    "comment_responses"

let ci_artifact_dir ~project_name ~patch_id =
  Stdlib.Filename.concat (artifact_dir ~project_name ~patch_id) "ci"

let ci_check_key (c : Types.Ci_check.t) =
  match c.id with
  | Some id -> "run-" ^ Int.to_string id
  | None ->
      let slug = slugify c.name in
      let slug =
        if String.is_empty slug then
          "name-"
          ^ String.prefix
              (Stdlib.Digest.to_hex (Stdlib.Digest.string c.name))
              12
        else slug
      in
      "status-" ^ slug

let ci_check_artifact_dir ~project_name ~patch_id ~check =
  Stdlib.Filename.concat
    (ci_artifact_dir ~project_name ~patch_id)
    (ci_check_key check)

let ensure_dir path =
  let rec mkdir_p dir =
    if not (Stdlib.Sys.file_exists dir) then (
      mkdir_p (Stdlib.Filename.dirname dir);
      Stdlib.Sys.mkdir dir 0o755)
  in
  mkdir_p path

let reset_artifact_dir path =
  ensure_dir path;
  match Stdlib.Sys.readdir path with
  | exception Sys_error _ -> ()
  | names ->
      Array.iter names ~f:(fun name ->
          let entry_path = Stdlib.Filename.concat path name in
          let is_dir =
            try Stdlib.Sys.is_directory entry_path with Sys_error _ -> true
          in
          if not is_dir then
            try Unix.unlink entry_path with
            | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
            | Unix.Unix_error (Unix.EISDIR, _, _) -> ())

type stored_config = {
  schema_version : int;
  project_name : string;
  github_owner : string;
  github_repo : string;
  backend : string;
  model : string;
  main_branch : string;
  poll_interval : float;
  repo_root : string;
  max_concurrency : int;
  max_ci_failures : int;
  url_scheme : string option;
}
[@@deriving yojson]

let save_config ~project_name ~github_owner ~github_repo ~backend ~model
    ~main_branch ~poll_interval ~repo_root ~max_concurrency ~max_ci_failures
    ?(url_scheme : string option = None) () =
  let dir = project_dir project_name in
  ensure_dir dir;
  let config =
    {
      schema_version = 1;
      project_name;
      github_owner;
      github_repo;
      backend;
      model;
      main_branch;
      poll_interval;
      repo_root;
      max_concurrency;
      max_ci_failures;
      url_scheme;
    }
  in
  let json = yojson_of_stored_config config in
  let oc = Stdlib.open_out_bin (config_path project_name) in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () ->
      Stdlib.output_string oc (Yojson.Safe.pretty_to_string json);
      Stdlib.flush oc)

let load_config ~project_name =
  let path = config_path project_name in
  try
    let ic = Stdlib.open_in path in
    let content =
      Stdlib.Fun.protect
        ~finally:(fun () -> Stdlib.close_in_noerr ic)
        (fun () -> Stdlib.In_channel.input_all ic)
    in
    let json = Yojson.Safe.from_string content in
    let config = stored_config_of_yojson json in
    if config.schema_version = 1 then Ok config
    else
      Error
        (Printf.sprintf "unsupported project config version: %d"
           config.schema_version)
  with exn -> Error (Stdlib.Printexc.to_string exn)

let save_plan_source ~project_name ~source_path =
  let dir = project_dir project_name in
  ensure_dir dir;
  let ic = Stdlib.open_in source_path in
  let content =
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_in_noerr ic)
      (fun () -> Stdlib.In_channel.input_all ic)
  in
  let oc = Stdlib.open_out_bin (plan_path project_name) in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () ->
      Stdlib.output_string oc content;
      Stdlib.flush oc)

let publish_plan_artifact ~project_name =
  let source = plan_path project_name in
  if Stdlib.Sys.file_exists source then (
    let ic = Stdlib.open_in_bin source in
    let content =
      Stdlib.Fun.protect
        ~finally:(fun () -> Stdlib.close_in_noerr ic)
        (fun () -> Stdlib.In_channel.input_all ic)
    in
    let dest = plan_artifact_path project_name in
    ensure_dir (Stdlib.Filename.dirname dest);
    let oc = Stdlib.open_out_bin dest in
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_out_noerr oc)
      (fun () ->
        Stdlib.output_string oc content;
        Stdlib.flush oc))

(* Cannot use Persistence.write_file_atomically here: Persistence depends on
   Project_store.ensure_dir, so calling it from Project_store creates a module
   cycle. Keep the same temp-file + rename behavior locally. *)
let write_file_atomically ~path ~content =
  let dir = Stdlib.Filename.dirname path in
  let base = Stdlib.Filename.basename path in
  let tmp_path = Stdlib.Filename.temp_file ~temp_dir:dir (base ^ ".") ".tmp" in
  try
    let oc = Stdlib.open_out_bin tmp_path in
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_out oc)
      (fun () ->
        Stdlib.output_string oc content;
        Stdlib.flush oc);
    Stdlib.Sys.rename tmp_path path;
    Ok ()
  with exn ->
    (try Stdlib.Sys.remove tmp_path with _ -> ());
    Error (Stdlib.Printexc.to_string exn)

let remove_if_exists path =
  try
    Unix.unlink path;
    Ok ()
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
  | exn -> Error (Stdlib.Printexc.to_string exn)

let ci_check_json ~(check : Types.Ci_check.t) ~head_oid =
  `Assoc
    [
      ("name", `String check.name);
      ("conclusion", `String check.conclusion);
      ( "details_url",
        Option.value_map check.details_url ~default:`Null ~f:(fun s ->
            `String s) );
      ( "started_at",
        Option.value_map check.started_at ~default:`Null ~f:(fun s -> `String s)
      );
      ( "check_run_id",
        Option.value_map check.id ~default:`Null ~f:(fun id -> `Int id) );
      ( "head_oid",
        Option.value_map head_oid ~default:`Null ~f:(fun s -> `String s) );
    ]

let publish_ci_check_artifact ~project_name ~patch_id ~check ?head_oid
    ~summary_md ?log () =
  try
    let dir = ci_check_artifact_dir ~project_name ~patch_id ~check in
    ensure_dir dir;
    let check_json_path = Stdlib.Filename.concat dir "check.json" in
    let summary_path = Stdlib.Filename.concat dir "summary.md" in
    let log_path = Stdlib.Filename.concat dir "log.txt" in
    match
      write_file_atomically ~path:check_json_path
        ~content:(Yojson.Safe.pretty_to_string (ci_check_json ~check ~head_oid))
    with
    | Error msg -> Error msg
    | Ok () -> (
        match write_file_atomically ~path:summary_path ~content:summary_md with
        | Error msg -> Error msg
        | Ok () -> (
            match log with
            | None -> Result.map (remove_if_exists log_path) ~f:(fun () -> dir)
            | Some content ->
                Result.map (write_file_atomically ~path:log_path ~content)
                  ~f:(fun () -> dir)))
  with exn -> Error (Stdlib.Printexc.to_string exn)

let project_exists project_name =
  Stdlib.Sys.file_exists (config_path project_name)

let list_projects () =
  let dir = data_dir () in
  if Stdlib.Sys.file_exists dir then
    Stdlib.Sys.readdir dir |> Array.to_list
    |> List.filter ~f:(fun name ->
        Stdlib.Sys.is_directory (Stdlib.Filename.concat dir name)
        && Stdlib.Sys.file_exists
             (Stdlib.Filename.concat
                (Stdlib.Filename.concat dir name)
                "config.json"))
  else []

(* === Inline tests === *)

(* Mirrors [Session_artifacts]'s test helper: point ONTON_DATA_DIR at a
   fresh temp dir for the duration of [f], then restore. *)
let with_temp_data_dir f =
  let old = Stdlib.Sys.getenv_opt "ONTON_DATA_DIR" in
  let dir = Stdlib.Filename.temp_dir "onton-project-store-" "" in
  Unix.putenv "ONTON_DATA_DIR" dir;
  Stdlib.Fun.protect
    ~finally:(fun () ->
      (match old with
      | Some value -> Unix.putenv "ONTON_DATA_DIR" value
      | None -> Unix.putenv "ONTON_DATA_DIR" "");
      try
        ignore
          (Stdlib.Sys.command
             (Printf.sprintf "rm -rf %s" (Stdlib.Filename.quote dir)))
      with _ -> ())
    (fun () -> f ())

let read_file_for_test path =
  let ic = Stdlib.open_in_bin path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_in_noerr ic)
    (fun () -> Stdlib.In_channel.input_all ic)

let ci_check_for_test ?id ?details_url ?description ?started_at
    ?(name = "Build and Test") ?(conclusion = "failure") () : Types.Ci_check.t =
  { name; conclusion; details_url; description; started_at; id }

let dir_entries_for_test path =
  Stdlib.Sys.readdir path |> Array.to_list |> List.sort ~compare:String.compare

let json_field_for_test name = function
  | `Assoc fields -> List.Assoc.find fields name ~equal:String.equal
  | _ -> None

let%test "completion claims are fresh, bounded, and strictly decoded" =
  with_temp_data_dir (fun () ->
      let project_name = "Completion Project" in
      let patch_id = Types.Patch_id.of_string "patch-claim" in
      let path = completion_claim_path ~project_name ~patch_id in
      ensure_dir (Stdlib.Filename.dirname path);
      match
        write_file_atomically ~path ~content:"{\"status\":\"complete\"}"
      with
      | Error _ -> false
      | Ok () ->
          (match read_completion_claim ~project_name ~patch_id with
            | Ok Completion_claim.Complete -> true
            | Ok (Completion_claim.Blocked _) | Error _ -> false)
          && Result.is_ok (clear_completion_claim ~project_name ~patch_id)
          && (not (Stdlib.Sys.file_exists path))
          && Result.is_error (read_completion_claim ~project_name ~patch_id))

let%test "completion claim byte limit and symlink checks fail closed" =
  with_temp_data_dir (fun () ->
      let project_name = "Completion Boundary" in
      let patch_id = Types.Patch_id.of_string "bounded" in
      let path = completion_claim_path ~project_name ~patch_id in
      ensure_dir (Stdlib.Filename.dirname path);
      match write_file_atomically ~path ~content:(String.make 4097 'x') with
      | Error _ -> false
      | Ok () -> (
          let oversized = read_completion_claim ~project_name ~patch_id in
          Unix.unlink path;
          let target = path ^ ".target" in
          match
            write_file_atomically ~path:target
              ~content:"{\"status\":\"complete\"}"
          with
          | Error _ -> false
          | Ok () ->
              Unix.symlink target path;
              Result.is_error oversized
              && Result.is_error (read_completion_claim ~project_name ~patch_id)
          ))

let%test "ci_check_key uses run id for CheckRuns and slug for id-less checks" =
  let run_check = ci_check_for_test ~id:123 ~name:"CI / Test Suite" () in
  let status_check =
    ci_check_for_test ~name:"all_jobs_succeed / Required!" ()
  in
  let empty_slug_check = ci_check_for_test ~name:"???" () in
  String.equal (ci_check_key run_check) "run-123"
  && String.equal
       (ci_check_key status_check)
       "status-all-jobs-succeed--required"
  && String.equal
       (ci_check_key empty_slug_check)
       ("status-name-"
       ^ String.prefix (Stdlib.Digest.to_hex (Stdlib.Digest.string "???")) 12)

let%test "publish_ci_check_artifact writes metadata and summary without log" =
  with_temp_data_dir (fun () ->
      let project_name = "CI Artifact Project" in
      let patch_id = Types.Patch_id.of_string "patch-2" in
      let check =
        ci_check_for_test ~id:42 ~name:"Unit Tests" ~conclusion:"failure"
          ~details_url:"https://github.example/checks/42"
          ~started_at:"2026-07-01T12:00:00Z" ()
      in
      match
        publish_ci_check_artifact ~project_name ~patch_id ~check
          ~head_oid:"abc123" ~summary_md:"summary\n" ()
      with
      | Error _ -> false
      | Ok dir ->
          List.equal String.equal (dir_entries_for_test dir)
            [ "check.json"; "summary.md" ]
          && String.equal
               (read_file_for_test (Stdlib.Filename.concat dir "summary.md"))
               "summary\n"
          &&
          let json =
            Yojson.Safe.from_string
              (read_file_for_test (Stdlib.Filename.concat dir "check.json"))
          in
          [%equal: Yojson.Safe.t option]
            (json_field_for_test "name" json)
            (Some (`String "Unit Tests"))
          && [%equal: Yojson.Safe.t option]
               (json_field_for_test "conclusion" json)
               (Some (`String "failure"))
          && [%equal: Yojson.Safe.t option]
               (json_field_for_test "details_url" json)
               (Some (`String "https://github.example/checks/42"))
          && [%equal: Yojson.Safe.t option]
               (json_field_for_test "started_at" json)
               (Some (`String "2026-07-01T12:00:00Z"))
          && [%equal: Yojson.Safe.t option]
               (json_field_for_test "check_run_id" json)
               (Some (`Int 42))
          && [%equal: Yojson.Safe.t option]
               (json_field_for_test "head_oid" json)
               (Some (`String "abc123")))

let%test "publish_ci_check_artifact writes log only when supplied" =
  with_temp_data_dir (fun () ->
      let project_name = "CI Artifact Project With Log" in
      let patch_id = Types.Patch_id.of_string "patch-2" in
      let check = ci_check_for_test ~name:"lint/status" () in
      match
        publish_ci_check_artifact ~project_name ~patch_id ~check
          ~summary_md:"summary\n" ~log:"full log\n" ()
      with
      | Error _ -> false
      | Ok dir ->
          String.equal
            (ci_check_artifact_dir ~project_name ~patch_id ~check)
            dir
          && List.equal String.equal (dir_entries_for_test dir)
               [ "check.json"; "log.txt"; "summary.md" ]
          && String.equal
               (read_file_for_test (Stdlib.Filename.concat dir "log.txt"))
               "full log\n"
          &&
          let json =
            Yojson.Safe.from_string
              (read_file_for_test (Stdlib.Filename.concat dir "check.json"))
          in
          [%equal: Yojson.Safe.t option]
            (json_field_for_test "check_run_id" json)
            (Some `Null)
          && [%equal: Yojson.Safe.t option]
               (json_field_for_test "head_oid" json)
               (Some `Null))

let%test "publish_ci_check_artifact overwrites the same key cleanly" =
  with_temp_data_dir (fun () ->
      let project_name = "CI Artifact Republish" in
      let patch_id = Types.Patch_id.of_string "patch-2" in
      let check = ci_check_for_test ~id:7 ~name:"same run" () in
      match
        publish_ci_check_artifact ~project_name ~patch_id ~check
          ~summary_md:"old\n" ~log:"stale log\n" ()
      with
      | Error _ -> false
      | Ok dir -> (
          match
            publish_ci_check_artifact ~project_name ~patch_id ~check
              ~summary_md:"new\n" ()
          with
          | Error _ -> false
          | Ok dir' ->
              String.equal dir dir'
              && List.equal String.equal
                   (dir_entries_for_test dir')
                   [ "check.json"; "summary.md" ]
              && String.equal
                   (read_file_for_test
                      (Stdlib.Filename.concat dir' "summary.md"))
                   "new\n"))

let%test "publish_plan_artifact copies the stored plan for agents" =
  with_temp_data_dir (fun () ->
      let project_name = "publish-test" in
      ensure_dir (project_dir project_name);
      let content = "{\"project_name\": \"publish-test\", \"patches\": []}" in
      let oc = Stdlib.open_out_bin (plan_path project_name) in
      Stdlib.output_string oc content;
      Stdlib.close_out oc;
      publish_plan_artifact ~project_name;
      let dest = plan_artifact_path project_name in
      Stdlib.Sys.file_exists dest
      && String.equal (read_file_for_test dest) content)

let%test "publish_plan_artifact refreshes a stale copy" =
  with_temp_data_dir (fun () ->
      let project_name = "publish-test-refresh" in
      ensure_dir (project_dir project_name);
      let write path content =
        let oc = Stdlib.open_out_bin path in
        Stdlib.output_string oc content;
        Stdlib.close_out oc
      in
      write (plan_path project_name) "{\"v\": 1}";
      publish_plan_artifact ~project_name;
      write (plan_path project_name) "{\"v\": 2}";
      publish_plan_artifact ~project_name;
      String.equal
        (read_file_for_test (plan_artifact_path project_name))
        "{\"v\": 2}")

let%test "publish_plan_artifact is a no-op without a stored plan" =
  with_temp_data_dir (fun () ->
      let project_name = "publish-test-empty" in
      publish_plan_artifact ~project_name;
      not (Stdlib.Sys.file_exists (plan_artifact_path project_name)))

(* The token is no longer persisted: [save_config] must not write a
   [github_token] key, and the config it writes must round-trip through
   [load_config]. *)
let%test "save_config persists no github_token and round-trips" =
  with_temp_data_dir (fun () ->
      let project_name = "no-token" in
      save_config ~project_name ~github_owner:"o" ~github_repo:"r"
        ~backend:"claude" ~model:"sonnet" ~main_branch:"main" ~poll_interval:5.0
        ~repo_root:"/tmp/r" ~max_concurrency:4
        ~max_ci_failures:Patch_agent.default_max_ci_failures ();
      let raw = read_file_for_test (config_path project_name) in
      (not (String.is_substring raw ~substring:"github_token"))
      &&
      match load_config ~project_name with
      | Ok cfg ->
          String.equal cfg.project_name project_name
          && String.equal cfg.github_owner "o"
          && Int.equal cfg.max_ci_failures Patch_agent.default_max_ci_failures
      | Error _ -> false)
