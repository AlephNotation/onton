open Base
open Onton_core

let route_preserves_selected_sink_order =
  QCheck2.Test.make ~name:"telemetry routing preserves selected sink order"
    ~count:500
    QCheck2.Gen.(list bool)
    (fun selected ->
      let sinks =
        List.mapi selected ~f:(fun index interested ->
            Telemetry.Sink.
              {
                name = Int.to_string index;
                interested_in = (fun _ -> interested);
                consume = ignore;
              })
      in
      let event =
        Telemetry.Event.Free_form
          {
            patch_id = None;
            level = Telemetry.Event.Info;
            message = "generated";
          }
      in
      let expected =
        List.filter_mapi selected ~f:(fun index interested ->
            if interested then Some (Int.to_string index) else None)
      in
      let actual =
        List.map (Telemetry.route ~sinks event) ~f:(fun sink ->
            sink.Telemetry.Sink.name)
      in
      List.equal String.equal actual expected)

let () = QCheck2.Test.check_exn route_preserves_selected_sink_order
