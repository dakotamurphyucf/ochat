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

let%expect_test
    "configured authoring grants bind exact roots, actor kinds and Project requests"
  =
  with_workspace (fun env root _ ->
    let source_file = Eio.Path.native_exn Eio.Path.(root / "server.sexp") in
    let text access =
      sprintf
        {|(version 1)
(server ((data_dir "./data") (unix_socket "./agent.sock")
         (project_sources ((external_roots (./shared))))
         (authoring_project_access %s)))
(workspaces ()) (prompts ()) (permission_profiles ())|}
        access
    in
    let validate access =
      let raw =
        Agent_server.Config_parser.parse_string ~source_file (text access) |> ok
      in
      Agent_server.Config_validator.validate ~env raw
    in
    let grant root kind =
      sprintf
        "((root %s) (principal_id pri_authoring_config) (authentication_kind %s))"
        root
        kind
    in
    let selected root =
      sprintf
        "((root %s) (project chatml.project.json) (request (entry ((name main) \
         (environment prod)))) (target one_off_v1))"
        root
    in
    let access grants selections =
      sprintf "((read_grants (%s)) (selections (%s)))" grants selections
    in
    let valid = access (grant "./shared" "unix.peer") (selected "./shared") in
    let admitted = validate valid |> ok in
    let config = Option.value_exn admitted.server.authoring_project_access in
    let without_access =
      { admitted with server = { admitted.server with authoring_project_access = None } }
    in
    assert
      (Agent_server.Config_diff.between ~previous:admitted ~current:without_access)
        .server_changed;
    let admitted_grant = List.hd_exn config.read_grants in
    let admitted_selection = List.hd_exn config.selections in
    print_s
      [%sexp
        (Filename.basename admitted_grant.root : string)
      , (admitted_grant.authentication_kind : string)
      , (Filename.basename admitted_selection.root : string)
      , (admitted_selection.project : string)
      , (List.length config.selections : int)];
    let invalid grants selections =
      assert (Result.is_error (validate (access grants selections)))
    in
    invalid (grant "./other" "unix.peer") (selected "./other");
    invalid (grant "./shared" "unix.peer") (selected "./other");
    invalid (grant "./shared" "INVALID") (selected "./shared");
    invalid
      (String.concat
         ~sep:" "
         [ grant "./shared" "unix.peer"; grant "./shared" "unix.peer" ])
      (selected "./shared");
    invalid
      (grant "./shared" "unix.peer")
      (String.concat ~sep:" " [ selected "./shared"; selected "./shared" ]));
  [%expect {| (shared unix.peer shared chatml.project.json 1) |}]
;;
