open Base

type t = Complete | Blocked of string [@@deriving show, eq, sexp_of]

let of_yojson = function
  | `Assoc fields -> (
      let names = List.map fields ~f:fst in
      let unique_names = List.dedup_and_sort names ~compare:String.compare in
      if List.length names <> List.length unique_names then
        Error "completion claim contains duplicate fields"
      else
        match List.Assoc.find fields ~equal:String.equal "status" with
        | Some (`String "complete") ->
            if List.equal String.equal unique_names [ "status" ] then
              Ok Complete
            else Error "complete claim accepts only the status field"
        | Some (`String "blocked") -> (
            if not (List.equal String.equal unique_names [ "reason"; "status" ])
            then Error "blocked claim requires exactly status and reason"
            else
              match List.Assoc.find fields ~equal:String.equal "reason" with
              | Some (`String reason) ->
                  let reason = String.strip reason in
                  if String.is_empty reason then
                    Error "blocked claim reason must not be empty"
                  else Ok (Blocked reason)
              | Some _ | None -> Error "blocked claim reason must be a string")
        | Some (`String status) ->
            Error (Printf.sprintf "unknown completion status %S" status)
        | Some _ -> Error "completion claim status must be a string"
        | None -> Error "completion claim is missing status")
  | _ -> Error "completion claim must be a JSON object"
