(* @archlint.module shell
   @archlint.domain priority *)

open Base

let command_for_backend = function
  | "claude" -> Some "claude"
  | "codex" -> Some "codex"
  | "opencode" -> Some "opencode"
  | "pi" -> Some "pi"
  | "gemini" -> Some "gemini"
  | "patch-agent" -> Some "patch-agent"
  | _ -> None

let path_dirs getenv_opt =
  match getenv_opt "PATH" with
  | None | Some "" -> []
  | Some path ->
      String.split path ~on:':'
      |> List.map ~f:(fun dir -> if String.is_empty dir then "." else dir)

let is_executable_file path =
  try
    match (Unix.stat path).Unix.st_kind with
    | Unix.S_REG ->
        Unix.access path [ Unix.X_OK ];
        true
    | Unix.S_DIR | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
    | Unix.S_SOCK ->
        false
  with Unix.Unix_error _ -> false

let find_executable ?(getenv_opt = Stdlib.Sys.getenv_opt)
    ?(is_executable = is_executable_file) command =
  if String.contains command '/' then
    if is_executable command then Some command else None
  else
    path_dirs getenv_opt
    |> List.find_map ~f:(fun dir ->
        let path = Stdlib.Filename.concat dir command in
        if is_executable path then Some path else None)

let check_backend ?getenv_opt ?is_executable backend =
  match command_for_backend backend with
  | None ->
      Error
        (Printf.sprintf "unknown backend %S; cannot run backend preflight"
           backend)
  | Some command -> (
      match find_executable ?getenv_opt ?is_executable command with
      | Some _ -> Ok ()
      | None ->
          Error
            (Printf.sprintf
               "backend %S requires executable %S on PATH, but it was not \
                found or is not executable. Install that backend CLI, fix \
                PATH, or choose another backend with --backend."
               backend command))

let validate ?getenv_opt ?is_executable ~backend () =
  check_backend ?getenv_opt ?is_executable backend

let%test "validate accepts an installed backend" =
  Result.is_ok
    (validate
       ~getenv_opt:(fun _ -> Some "/bin")
       ~is_executable:(fun path -> String.equal path "/bin/codex")
       ~backend:"codex" ())

let%test "validate reports a missing backend executable" =
  match
    validate
      ~getenv_opt:(fun _ -> Some "/bin")
      ~is_executable:(fun _ -> false)
      ~backend:"codex" ()
  with
  | Error message ->
      String.is_substring message ~substring:"codex"
      && String.is_substring message ~substring:"not found or is not executable"
  | Ok () -> false
