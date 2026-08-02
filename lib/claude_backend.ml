let create ~name ~model ~process_mgr ~clock ~timeout ~setsid_exec :
    Llm_backend.t =
  {
    name;
    run_streaming =
      (fun ~sandbox
        ~project_name
        ~cwd
        ~patch_id
        ~prompt
        ~resume_session
        ~session_uuid
        ~on_event
      ->
        Claude_runner.run_streaming ~model ~process_mgr ~clock ~timeout
          ~setsid_exec ~sandbox ~project_name ~cwd ~patch_id ~prompt
          ~resume_session ~session_uuid ~on_event);
  }
