open Core
open Support

let%expect_test
    "administrative paths resolve without opening deployment or credential files"
  =
  with_workspace (fun env root _ ->
    let source_file = Eio.Path.native_exn Eio.Path.(root / "server.sexp") in
    let text project_sources =
      sprintf
        {|(version 1)
(server ((data_dir "./data") (unix_socket "./agent.sock") %s))
(workspaces ()) (prompts ()) (permission_profiles ())|}
        project_sources
    in
    let validate value =
      let raw = Agent_server.Config_parser.parse_string ~source_file (text value) |> ok in
      Agent_server.Config_validator.validate ~env raw
    in
    let config =
      validate
        {|(project_sources ((external_roots (./shared)) (git ((helper_directory ./helpers) (ca_bundle ./ca.pem) (memory_bytes_per_process 8589934592) (repositories ((https ((repository https://example.invalid/repo.git) (authorization_header_file ./auth))) (local ./repo.git)))))))|}
      |> ok
    in
    let configuration = Option.value_exn config.server.project_sources in
    let relative path = Filename.basename path in
    let git = Option.value_exn configuration.git in
    print_s
      [%sexp
        (List.map configuration.external_roots ~f:relative : string list)
      , (relative git.helper_directory : string)
      , (Option.map git.ca_bundle ~f:relative : string option)
      , (git.memory_bytes_per_process : int)];
    assert (List.for_all configuration.external_roots ~f:Filename.is_absolute);
    assert (Filename.is_absolute git.helper_directory);
    assert (Option.for_all git.ca_bundle ~f:Filename.is_absolute);
    assert (Option.is_none (validate "" |> ok).server.project_sources);
    assert (Option.is_none (validate "(project_sources ())" |> ok).server.project_sources);
    let invalid fields =
      assert (Result.is_error (validate ("(project_sources " ^ fields ^ ")")))
    in
    List.iter
      [ "((git ((helper_directory ./helpers))))"
      ; "((git ((helper_directory ./helpers) (memory_bytes_per_process 0))))"
      ; "((external_roots ()) (external_roots ()))"
      ; "((unknown true))"
      ; "((git ((helper_directory ./helpers) (memory_bytes_per_process 1) (repositories \
         ((https ((repository http://example.invalid/repo.git))))))))"
      ]
      ~f:invalid;
    let disabled = validate "(project_sources ((external_roots (./shared))))" |> ok in
    assert (not (Agent_server.Config.Server.equal config.server disabled.server)));
  [%expect {| ((shared) helpers (ca.pem) 8589934592) |}]
;;
