open Base
open Types

type t = Absent | Present of Pr_number.t
[@@deriving show, eq, sexp_of, compare]

let has_pr = function Absent -> false | Present _ -> true
let pr_number = function Absent -> None | Present number -> Some number

type set_present_decision = Preserve_existing | Adopt_new
[@@deriving show, eq]

let classify_set_present status number =
  match status with
  | Present existing when Pr_number.equal existing number -> Preserve_existing
  | Absent | Present _ -> Adopt_new

let set_present _status number = Present number

let clear = function
  | Present _ -> Absent
  | Absent -> invalid_arg "Patch_pr_status.clear: already absent"

let yojson_of_t = function
  | Absent -> `Assoc [ ("kind", `String "absent") ]
  | Present number ->
      `Assoc
        [
          ("kind", `String "present");
          ("pr_number", Pr_number.yojson_of_t number);
        ]

let t_of_yojson json =
  let decode_number value =
    Result.map_error
      (Json.try_of_yojson Pr_number.t_of_yojson value)
      ~f:(Printf.sprintf "malformed pr_number: %s")
  in
  match json with
  | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "kind" with
      | Some (`String "absent") -> Ok Absent
      | Some (`String "present") -> (
          match List.Assoc.find fields ~equal:String.equal "pr_number" with
          | Some value ->
              Result.map (decode_number value) ~f:(fun n -> Present n)
          | None -> Error "Patch_pr_status: present without pr_number")
      | Some (`String kind) ->
          Error (Printf.sprintf "Patch_pr_status: unknown kind %S" kind)
      | Some _ -> Error "Patch_pr_status: kind must be a string"
      | None -> Error "Patch_pr_status: missing kind")
  | _ -> Error "Patch_pr_status: expected an object"

let%test "absent has no PR" =
  (not (has_pr Absent)) && Option.is_none (pr_number Absent)

let%test "present exposes its PR number" =
  let number = Pr_number.of_int 7 in
  has_pr (Present number)
  && Option.equal Pr_number.equal (pr_number (Present number)) (Some number)

let%test "same number preserves existing lifecycle" =
  let number = Pr_number.of_int 7 in
  equal_set_present_decision
    (classify_set_present (Present number) number)
    Preserve_existing

let%test "a different number is a new PR lifecycle" =
  equal_set_present_decision
    (classify_set_present (Present (Pr_number.of_int 7)) (Pr_number.of_int 8))
    Adopt_new
