open Base
open Onton_core

let gen_init_info =
  let open QCheck2.Gen in
  let gen_opt_string = option (string_small_of printable) in
  let* api_key_source = gen_opt_string in
  let* model = gen_opt_string in
  let* claude_code_version = gen_opt_string in
  return Failure_subkind.{ api_key_source; model; claude_code_version }

let gen_run_classification =
  let open QCheck2.Gen in
  oneof
    [
      map
        (fun msg -> Run_classification.Process_error msg)
        (string_small_of printable);
      pure Run_classification.No_session_to_resume;
      pure Run_classification.Timed_out;
      map
        (fun stream_errors -> Run_classification.Success { stream_errors })
        (string_small_of printable);
      map2
        (fun exit_code detail ->
          Run_classification.Session_failed { exit_code; detail })
        int_small
        (string_small_of printable);
    ]

let gen_subkind =
  let open QCheck2.Gen in
  oneof
    [
      pure Failure_subkind.Ok;
      pure Failure_subkind.Auth_unavailable;
      map
        (fun status -> Failure_subkind.Api_error { status })
        (option int_small);
      pure Failure_subkind.Network_error;
      pure Failure_subkind.Timed_out;
      pure Failure_subkind.Context_exhausted;
      pure Failure_subkind.No_session_to_resume;
      pure Failure_subkind.Empty_response;
      pure Failure_subkind.Process_error;
      map
        (fun detail -> Failure_subkind.Other detail)
        (string_small_of printable);
    ]

let prop_classify_total =
  QCheck2.Test.make ~name:"Failure_subkind.classify is total" ~count:500
    QCheck2.Gen.(
      quad gen_run_classification gen_init_info
        (string_small_of printable)
        (string_small_of printable))
    (fun (classification, init, text_tail, stderr_tail) ->
      try
        let _ =
          Failure_subkind.classify ~classification ~init ~text_tail ~stderr_tail
        in
        true
      with _ -> false)

let prop_subkind_codec_roundtrips =
  QCheck2.Test.make ~name:"Failure_subkind JSON codec roundtrips" ~count:500
    gen_subkind (fun subkind ->
      Failure_subkind.equal subkind
        (Failure_subkind.t_of_yojson (Failure_subkind.yojson_of_t subkind)))

let prop_init_info_codec_roundtrips =
  QCheck2.Test.make ~name:"Failure_subkind init metadata roundtrips" ~count:500
    gen_init_info (fun init ->
      Failure_subkind.equal_init_info init
        (Failure_subkind.init_info_of_yojson
           (Failure_subkind.yojson_of_init_info init)))

let prop_to_string_is_nonempty =
  QCheck2.Test.make ~name:"Failure_subkind labels are nonempty" ~count:500
    gen_subkind (fun subkind ->
      not (String.is_empty (Failure_subkind.to_string subkind)))

let () =
  let errcode =
    QCheck_base_runner.run_tests ~verbose:true
      [
        prop_classify_total;
        prop_subkind_codec_roundtrips;
        prop_init_info_codec_roundtrips;
        prop_to_string_is_nonempty;
      ]
  in
  if errcode <> 0 then Stdlib.exit errcode
