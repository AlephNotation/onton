open Base

let read_file path =
  try
    let channel = Stdlib.In_channel.open_text path in
    let contents =
      Exn.protect
        ~finally:(fun () -> Stdlib.In_channel.close channel)
        ~f:(fun () -> Stdlib.In_channel.input_all channel)
    in
    Ok contents
  with Sys_error message ->
    Error (Printf.sprintf "Cannot read %s: %s" path message)

let parse_file path =
  Result.bind (read_file path) ~f:Gameplan_parser.parse_json_string
