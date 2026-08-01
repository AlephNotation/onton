(* @archlint.module core
   @archlint.domain worker-sandbox *)

open Base

type network = Denied | Https_only [@@deriving show, eq, sexp_of, compare]

type t = {
  worktree : string;
  read_only_paths : string list;
  read_only_dirs : string list;
  writable_files : string list;
  writable_dirs : string list;
  creatable_dirs : string list;
  runtime_files : string list;
  runtime_roots : string list;
  state_dir : string;
  network : network;
}
[@@deriving show, eq, sexp_of, compare]

let contains_control value =
  String.exists value ~f:(fun char -> Char.to_int char < 0x20)

let has_parent_segment path =
  String.split path ~on:'/'
  |> List.exists ~f:(fun segment -> String.equal segment "..")

let validate_path label path =
  if String.is_empty path then Error (label ^ " must not be empty")
  else if Stdlib.Filename.is_relative path then
    Error (Printf.sprintf "%s must be absolute: %S" label path)
  else if contains_control path then
    Error (Printf.sprintf "%s contains a control character" label)
  else if has_parent_segment path then
    Error (Printf.sprintf "%s contains parent traversal: %S" label path)
  else Ok path

let validate_paths label paths =
  List.mapi paths ~f:(fun index path ->
      validate_path (Printf.sprintf "%s[%d]" label index) path)
  |> Result.all

let sorted_unique paths = List.dedup_and_sort paths ~compare:String.compare

let create ~worktree ~read_only_paths ~read_only_dirs ~writable_files
    ~writable_dirs ~creatable_dirs ~runtime_files ~runtime_roots ~state_dir
    ~network =
  Result.bind (validate_path "worktree" worktree) ~f:(fun worktree ->
      Result.bind (validate_paths "read_only_paths" read_only_paths)
        ~f:(fun read_only_paths ->
          Result.bind (validate_paths "read_only_dirs" read_only_dirs)
            ~f:(fun read_only_dirs ->
              Result.bind (validate_paths "writable_files" writable_files)
                ~f:(fun writable_files ->
                  Result.bind (validate_paths "writable_dirs" writable_dirs)
                    ~f:(fun writable_dirs ->
                      Result.bind
                        (validate_paths "creatable_dirs" creatable_dirs)
                        ~f:(fun creatable_dirs ->
                          Result.bind
                            (validate_paths "runtime_files" runtime_files)
                            ~f:(fun runtime_files ->
                              Result.bind
                                (validate_paths "runtime_roots" runtime_roots)
                                ~f:(fun runtime_roots ->
                                  Result.map
                                    (validate_path "state_dir" state_dir)
                                    ~f:(fun state_dir ->
                                      {
                                        worktree;
                                        read_only_paths =
                                          sorted_unique read_only_paths;
                                        read_only_dirs =
                                          sorted_unique read_only_dirs;
                                        writable_files =
                                          sorted_unique writable_files;
                                        writable_dirs =
                                          sorted_unique writable_dirs;
                                        creatable_dirs =
                                          sorted_unique creatable_dirs;
                                        runtime_files =
                                          sorted_unique runtime_files;
                                        runtime_roots =
                                          sorted_unique runtime_roots;
                                        state_dir;
                                        network;
                                      })))))))))

let add_runtime_files t runtime_files =
  Result.map (validate_paths "runtime_files" runtime_files)
    ~f:(fun runtime_files ->
      { t with runtime_files = sorted_unique (t.runtime_files @ runtime_files) })

let sbpl_string value =
  let buffer = Buffer.create (String.length value + 2) in
  Buffer.add_char buffer '"';
  String.iter value ~f:(function
    | '"' -> Buffer.add_string buffer "\\\""
    | '\\' -> Buffer.add_string buffer "\\\\"
    | char -> Buffer.add_char buffer char);
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let rule filter path = Printf.sprintf "(%s %s)" filter (sbpl_string path)

let rules filter paths =
  paths |> List.map ~f:(rule filter) |> String.concat ~sep:"\n    "

let system_read_roots =
  [
    "/System";
    "/usr/bin";
    "/usr/lib";
    "/usr/libexec";
    "/usr/sbin";
    "/usr/share";
    "/bin";
    "/sbin";
    "/Library/Apple";
    "/private/var/db/timezone";
    "/dev/fd";
  ]

let system_read_files =
  [
    "/private/etc/hosts";
    "/private/etc/resolv.conf";
    "/private/etc/ssl/cert.pem";
    "/private/etc/ssl/openssl.cnf";
    "/etc/hosts";
    "/etc/resolv.conf";
    "/etc/ssl/cert.pem";
    "/etc/ssl/openssl.cnf";
  ]

let macos_profile t =
  let readable_dirs =
    sorted_unique
      (system_read_roots
      @ [ t.worktree; t.state_dir ]
      @ t.read_only_dirs @ t.runtime_roots @ t.writable_dirs)
  in
  let readable_files =
    sorted_unique
      (system_read_files @ t.read_only_paths @ t.writable_files
     @ t.runtime_files)
  in
  let network_rules =
    match t.network with
    | Denied -> ""
    | Https_only ->
        {|
  (allow network-outbound
    (remote tcp "*:443")
    (remote udp "*:443")
    (literal "/private/var/run/mDNSResponder"))
  (deny network-outbound
    (remote ip "localhost:*"))|}
  in
  let creation_rules =
    match t.creatable_dirs with
    | [] -> ""
    | directories ->
        Printf.sprintf
          {|
  (allow file-write-create
    %s)|}
          (rules "literal" directories)
  in
  Printf.sprintf
    {|(version 1)
  (deny default)
  (import "system.sb")

  ;; Workers may create descendants, but cannot inspect or signal host
  ;; processes. Every descendant inherits this profile.
  (allow process-exec process-fork)

  ;; Path traversal needs metadata, but file contents remain deny-by-default.
  (allow file-read-metadata file-test-existence)
  (allow file-read*
    %s
    %s)

  (allow file-write*
    (literal "/dev/null")
    (literal "/dev/zero")
    %s
    %s)
%s
%s
|}
    (rules "subpath" readable_dirs)
    (rules "literal" readable_files)
    (rules "subpath" (sorted_unique (t.state_dir :: t.writable_dirs)))
    (rules "literal" t.writable_files)
    creation_rules network_rules

let inherited_environment_names =
  Set.of_list
    (module String)
    [
      "LANG";
      "LC_ALL";
      "LC_CTYPE";
      "TERM";
      "COLORTERM";
      "USER";
      "LOGNAME";
      "SHELL";
    ]

let override_environment_names =
  Set.of_list
    (module String)
    [
      "PATH";
      "HOME";
      "TMPDIR";
      "CLAUDE_CONFIG_DIR";
      "CODEX_HOME";
      "OPENCODE_CONFIG_DIR";
      "XDG_CONFIG_HOME";
      "OPENSSL_CONF";
    ]

let allowed_environment_name ~allowed_provider_names name =
  Set.mem inherited_environment_names name
  || Set.mem override_environment_names name
  || List.mem allowed_provider_names name ~equal:String.equal
  || String.is_prefix name ~prefix:"LC_"

let inherited_environment_name ~allowed_provider_names name =
  Set.mem inherited_environment_names name
  || List.mem allowed_provider_names name ~equal:String.equal
  || String.is_prefix name ~prefix:"LC_"

let split_environment_entry entry =
  match String.lsplit2 entry ~on:'=' with
  | Some pair -> pair
  | None -> (entry, "")

let environment ~allowed_provider_names ~base ~overrides =
  match
    List.find overrides ~f:(fun (name, _) ->
        not (allowed_environment_name ~allowed_provider_names name))
  with
  | Some (name, _) ->
      Error
        (Printf.sprintf "worker environment override is not allowed: %s" name)
  | None ->
      let values = Hashtbl.create (module String) in
      Array.iter base ~f:(fun entry ->
          let name, value = split_environment_entry entry in
          if inherited_environment_name ~allowed_provider_names name then
            Hashtbl.set values ~key:name ~data:value);
      List.iter overrides ~f:(fun (name, value) ->
          Hashtbl.set values ~key:name ~data:value);
      Hashtbl.to_alist values
      |> List.sort ~compare:(fun (left, _) (right, _) ->
          String.compare left right)
      |> List.map ~f:(fun (name, value) -> name ^ "=" ^ value)
      |> Array.of_list |> Result.return
