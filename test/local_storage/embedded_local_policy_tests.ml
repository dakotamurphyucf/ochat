let%test_module "bounded embedded local Developer policy" =
  (module struct
    open! Core
    module P = Agent_protocol
    module V = Agent_server.Config_validator.Embedded_local

    let ok = function
      | Ok value -> value
      | Error diagnostics ->
        raise_s
          [%sexp
            "local policy diagnostics"
          , (diagnostics : Agent_server.Config.Diagnostic.t list)]
    ;;

    let policy
          ?(principal = "Embedded_operator")
          ?(authentication_kind = "embedded.local")
          ?(scopes = "developer.check")
          ()
      =
      let capacity =
        Agent_server.Developer_startup_configuration.Capacity.standard_v1
        |> Agent_server.Developer_startup_configuration.Capacity.sexp_of_t
        |> Sexp.to_string
      in
      sprintf
        {|(developer
           ((selections
             (((source (Document_source (origin Supplied_document) (script_id main)))
               (target One_off_v1)
               (principals
                 (((principal_id %s) (authentication_kind %s)))))))
            (capacity %s)
            (model Disabled)
            (approvals Disabled)
            (repl Disabled)
            (portable true)))
          (developer_scopes (%s))|}
        principal
        authentication_kind
        capacity
        scopes
    ;;

    let with_policy_file_at_root make_text f =
      Eio_main.run (fun env ->
        Mirage_crypto_rng_unix.use_default ();
        let root = Agent_server_test_support.temporary_root env in
        let path = Filename.concat root "local-policy.sexp" in
        Exn.protect
          ~f:(fun () ->
            Eio.Path.save
              ~create:(`Exclusive 0o600)
              Eio.Path.(Eio.Stdenv.fs env / path)
              (make_text root);
            f env path)
          ~finally:(fun () ->
            Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)))
    ;;

    let with_policy_file text f = with_policy_file_at_root (fun _ -> text) f

    let%test_unit "exact minted selector and scope survive ordinary validation" =
      with_policy_file (policy ()) (fun env path ->
        let loaded = V.load ~env ~path |> ok in
        let operator_id = P.Id.Principal.create () in
        let bound = V.bind loaded ~env ~operator_id |> ok in
        assert (P.Id.Principal.equal (V.operator_id bound) operator_id);
        assert (Set.mem (V.developer_scopes bound) P.Scope.Developer_check);
        assert (Set.length (V.developer_scopes bound) = 1))
    ;;

    let%test_unit "project, upload and manifest selectors bind through actual startup" =
      with_policy_file_at_root
        (fun root ->
           let root_atom = Sexp.to_string_mach (Sexp.Atom root) in
           sprintf
             {|%s
              (project_sources ((external_roots (%s))))
              (authoring_project_access
               ((read_grants (((root %s) (principal_id Embedded_operator)
                               (authentication_kind embedded.local))))
                (selections (((root %s) (project check.chatml)
                              (request (entry ((name main)))) (target one_off_v1))))))
              (authoring_upload_access
               ((mode augment_core)
                (grants (((principal_id Embedded_operator)
                          (authentication_kind embedded.local)
                          (operation upload_write))))
                (selections (((principal_id Embedded_operator)
                              (authentication_kind embedded.local)
                              (request (entry ((name main))))
                              (selected_environment default) (target one_off_v1))))))
              (manifest_grants
               (((id exact) (prompt embedded.prompt)
                 (workspaces (embedded.workspace))
                 (manifest_sha256 %s) (source_sha256 %s)
                 (principals (Embedded_operator)))))|}
             (policy ())
             root_atom
             root_atom
             root_atom
             (String.make 64 '0')
             (String.make 64 '0'))
        (fun env path ->
           let local_policy = V.load ~env ~path |> ok in
           let operator_id = P.Id.Principal.create () in
           let bound = V.bind local_policy ~env ~operator_id |> ok in
           assert (P.Id.Principal.equal operator_id (V.operator_id bound));
           let root = Filename.dirname path in
           let workspace = Filename.concat root "workspace" in
           Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
           let prompt = Filename.concat root "agent.chatmd" in
           Eio.Path.save
             ~create:(`Exclusive 0o600)
             Eio.Path.(Eio.Stdenv.fs env / prompt)
             "<developer>Complete local policy fixture.</developer>";
           let options : Agent_server.Embedded.options =
             { prompt_file = prompt
             ; workspace
             ; tool_dir = root
             ; home = Some root
             ; storage = Agent_server.Local_storage.Transient
             ; start_immediately = false
             ; permission_profile = Agent_server.Embedded.default_permission_profile
             ; attachment_mode = Read_write
             ; event_capacity = 32
             }
           in
           Eio.Switch.run (fun sw ->
             match
               Agent_server.Embedded.open_local_host ~sw ~env ~local_policy options
             with
             | Ok host -> Agent_server.Embedded.close_host host
             | Error error ->
               raise_s [%sexp "complete local policy failed", (error : P.Error.t)]))
    ;;

    let%test_unit "all local principal coordinates require the exact selector" =
      let project principal kind =
        sprintf
          {|(authoring_project_access
             ((read_grants (((root /tmp) (principal_id %s)
                             (authentication_kind %s))))
              (selections ())))|}
          principal
          kind
      in
      let upload grant_principal selection_principal =
        sprintf
          {|(authoring_upload_access
             ((grants (((principal_id %s) (authentication_kind embedded.local)
                        (operation upload_write))))
              (selections (((principal_id %s) (authentication_kind embedded.local)
                            (request (entry ((name main))))
                            (selected_environment default) (target one_off_v1))))))|}
          grant_principal
          selection_principal
      in
      let manifest principals =
        sprintf
          {|(manifest_grants
             (((id exact) (prompt embedded.prompt) (workspaces (embedded.workspace))
               (manifest_sha256 %s) (source_sha256 %s) (principals %s))))|}
          (String.make 64 '0')
          (String.make 64 '0')
          principals
      in
      let cases =
        [ "developer literal", policy ~principal:"pri_foreign" ()
        ; "developer wrong auth", policy ~authentication_kind:"http.static_bearer" ()
        ; "project literal", policy () ^ project "pri_foreign" "embedded.local"
        ; "project wrong auth", policy () ^ project "Embedded_operator" "unix.peer"
        ; "upload grant literal", policy () ^ upload "pri_foreign" "Embedded_operator"
        ; "upload selection literal", policy () ^ upload "Embedded_operator" "pri_foreign"
        ; "manifest wildcard", policy () ^ manifest "()"
        ; "manifest literal", policy () ^ manifest "(pri_foreign)"
        ]
      in
      List.iter cases ~f:(fun (label, source) ->
        with_policy_file source (fun env path ->
          let loaded = V.load ~env ~path |> ok in
          match V.bind loaded ~env ~operator_id:(P.Id.Principal.create ()) with
          | Error [ diagnostic ] ->
            [%test_eq: string] diagnostic.code "config.embedded_local_selector"
          | Error diagnostics ->
            raise_s
              [%sexp
                "unexpected selector diagnostics"
              , (label : string)
              , (diagnostics : Agent_server.Config.Diagnostic.t list)]
          | Ok _ -> raise_s [%sexp "selector was accepted", (label : string)]))
    ;;

    let%test_unit "local policy bytes, syntax and sections are bounded" =
      let invalid_sources =
        [ "unsupported scope", policy ~scopes:"developer.check diagnostics.read" ()
        ; "unknown section", policy () ^ "(unknown_section ())"
        ; "duplicate section", policy () ^ "(developer ())"
        ; "excess depth", String.make 65 '(' ^ String.make 65 ')'
        ; ( "excess nodes"
          , "(developer ()) (developer_scopes ()) "
            ^ String.concat ~sep:" " (List.init 65_537 ~f:(fun _ -> "x")) )
        ; "unsupported sexp comment", policy () ^ "#; (ignored ())"
        ; "excess bytes", policy () ^ String.make (9 * 1024 * 1024) ' '
        ]
      in
      List.iter invalid_sources ~f:(fun (label, source) ->
        with_policy_file source (fun env path ->
          if String.equal label "unsupported scope"
          then (
            let loaded = V.load ~env ~path |> ok in
            assert (
              Result.is_error (V.bind loaded ~env ~operator_id:(P.Id.Principal.create ()))))
          else if Result.is_ok (V.load ~env ~path)
          then raise_s [%sexp "invalid local policy loaded", (label : string)]));
      with_policy_file
        (policy () ^ "; line comment with (()\n(project_sources \"(quoted parentheses)\")")
        (fun env path -> ignore (V.load ~env ~path |> ok : V.t))
    ;;

    let%test_unit "embedded host binds the Store identity and only requested method scope"
      =
      with_policy_file (policy ()) (fun env path ->
        let local_policy = V.load ~env ~path |> ok in
        let root = Filename.dirname path in
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "agent.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Local policy fixture.</developer>";
        let options : Agent_server.Embedded.options =
          { prompt_file = prompt
          ; workspace
          ; tool_dir = root
          ; home = Some root
          ; storage = Agent_server.Local_storage.Transient
          ; start_immediately = false
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 32
          }
        in
        Eio.Switch.run (fun sw ->
          let host =
            match
              Agent_server.Embedded.open_local_host ~sw ~env ~local_policy options
            with
            | Ok host -> host
            | Error error -> raise_s [%sexp "local startup failed", (error : P.Error.t)]
          in
          let principal = Agent_server.Embedded.host_principal host in
          assert (P.Principal.has_scope principal P.Scope.Developer_check);
          assert (not (P.Principal.has_scope principal P.Scope.Developer_fixture_run));
          assert (not (P.Principal.has_scope principal P.Scope.Developer_live_run));
          assert (not (P.Principal.has_scope principal P.Scope.Developer_repl));
          assert (
            Result.is_error
              (Agent_client.Developer_repl_connection.create
                 (Agent_server.Embedded.host_connection host)));
          Agent_server.Embedded.close_host host))
    ;;

    let%test_unit "embedded default host does not install Developer or REPL" =
      with_policy_file (policy ()) (fun env path ->
        let root = Filename.dirname path in
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "agent.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Unconfigured local fixture.</developer>";
        let options : Agent_server.Embedded.options =
          { prompt_file = prompt
          ; workspace
          ; tool_dir = root
          ; home = Some root
          ; storage = Agent_server.Local_storage.Transient
          ; start_immediately = false
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 32
          }
        in
        Eio.Switch.run (fun sw ->
          let host =
            Agent_server.Embedded.open_local_host ~sw ~env options
            |> Result.map_error ~f:(fun error ->
              Sexp.to_string_hum ([%sexp_of: P.Error.t] error))
            |> Result.ok_or_failwith
          in
          Exn.protect
            ~f:(fun () ->
              let connection = Agent_server.Embedded.host_connection host in
              let initialized =
                Agent_client.Connection.initialization connection |> Option.value_exn
              in
              assert (
                not
                  (List.mem
                     initialized.enabled_features
                     P.Developer_common.Features.run
                     ~equal:String.equal));
              assert (
                not
                  (List.mem
                     initialized.enabled_features
                     P.Developer_repl_wire.Features.repl
                     ~equal:String.equal));
              assert (
                Result.is_error (Agent_client.Developer_connection.create connection));
              assert (
                Result.is_error (Agent_client.Developer_repl_connection.create connection)))
            ~finally:(fun () -> Agent_server.Embedded.close_host host)))
    ;;

    let%test_unit
        "embedded connections select installed Developer and REPL per Initialize"
      =
      let module C = P.Developer_common in
      let module R = P.Developer_repl_wire in
      let module W = P.Developer_wire in
      let protocol_ok = function
        | Ok value -> value
        | Error error -> raise_s [%sexp "embedded protocol failed", (error : P.Error.t)]
      in
      let key name = P.Idempotency_key.of_string name |> protocol_ok in
      let policy root =
        let root_atom = Sexp.to_string_mach (Sexp.Atom root) in
        let capacity =
          Agent_server.Developer_startup_configuration.Capacity.standard_v1
          |> Agent_server.Developer_startup_configuration.Capacity.sexp_of_t
          |> Sexp.to_string_mach
        in
        sprintf
          {|(project_sources ((external_roots (%s))))
            (authoring_project_access
             ((read_grants (((root %s) (principal_id Embedded_operator)
                             (authentication_kind embedded.local))))
              (selections (((root %s) (project check.chatml)
                            (request (entry ((name main)))) (target one_off_v1))))))
            (developer
             ((selections
               (((source (Local_source (root %s) (member check.chatml)))
                 (target One_off_v1)
                 (principals (((principal_id Embedded_operator)
                               (authentication_kind embedded.local)))))
                ((source (Local_source (root %s) (member fixture.chatml)))
                 (target
                   (Tool_v1 (tool_name fixture) (input_schema_json "{}")
                            (output_schema_json "{}")))
                 (principals (((principal_id Embedded_operator)
                               (authentication_kind embedded.local)))))))
              (capacity %s) (model Disabled) (approvals Disabled)
              (repl Standard_v1) (portable true)))
            (developer_scopes
              (developer.check developer.fixture.run developer.live.run developer.repl))|}
          root_atom
          root_atom
          root_atom
          root_atom
          root_atom
          capacity
      in
      with_policy_file_at_root policy (fun env path ->
        let root = Filename.dirname path in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / Filename.concat root "check.chatml")
          "let main input = Task.pure(input)";
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / Filename.concat root "fixture.chatml")
          {|let run ctx input =
  let* result = Tool.call("fixture", input) in
  match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(message) -> Task.fail(message)|};
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "agent.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Embedded typed REPL fixture.</developer>";
        let options : Agent_server.Embedded.options =
          { prompt_file = prompt
          ; workspace
          ; tool_dir = root
          ; home = Some root
          ; storage = Agent_server.Local_storage.Transient
          ; start_immediately = false
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 32
          }
        in
        let local_policy = V.load ~env ~path |> ok in
        Eio.Switch.run (fun sw ->
          let host =
            Agent_server.Embedded.open_local_host ~sw ~env ~local_policy options
            |> protocol_ok
          in
          Exn.protect
            ~f:(fun () ->
              let connection = Agent_server.Embedded.host_connection host in
              let initialized =
                Agent_client.Connection.initialization connection |> Option.value_exn
              in
              assert (
                List.mem initialized.enabled_features C.Features.run ~equal:String.equal);
              assert (
                List.mem initialized.enabled_features R.Features.repl ~equal:String.equal);
              let developer =
                Agent_client.Developer_connection.create connection |> protocol_ok
              in
              let repl =
                Agent_client.Developer_repl_connection.create connection |> protocol_ok
              in
              let limits =
                Agent_client.Developer_connection.capabilities developer
                |> C.Capabilities.limits
              in
              let member =
                Source.Portable_path.create "check.chatml" |> Or_error.ok_exn
              in
              let route =
                C.Route.local_source
                  ~limits:(C.Limits.request limits)
                  ~root_index:0
                  ~member
                |> protocol_ok
              in
              let run_id = P.Id.Developer_run.create () in
              let check_key = key "embedded-typed-check" in
              let check =
                C.Submit.create
                  ~limits
                  ~run_id
                  ~key:check_key
                  ~kind:Check
                  ~route
                  ~target:One_off_v1
                  ~prompt:None
                  ~workspace:None
                  ~entrypoint:"main"
                  ~input:`Null
                  ~fixtures:None
                  ~requested_execution:None
                  ~requested_max_output_bytes:None
                |> protocol_ok
              in
              (match
                 Agent_client.Developer_connection.send developer (W.Request.Submit check)
                 |> protocol_ok
               with
               | W.Response.Submit _ -> ()
               | _ -> failwith "embedded Check response changed method");
              (match
                 Agent_client.Developer_connection.send
                   developer
                   (W.Request.Await_terminal (C.Lookup.create ~run_id ~key:check_key))
                 |> protocol_ok
               with
               | W.Response.Await_terminal view ->
                 (match C.View.phase view with
                  | Terminal { receipt; execution_started = false } ->
                    (match P.Developer_receipt.value receipt with
                     | Check Ready -> ()
                     | _ -> failwith "embedded Check was not ready")
                  | _ -> failwith "embedded Check did not settle before execution")
               | _ -> failwith "embedded Check await changed method");
              let live_key = key "embedded-typed-live" in
              let live =
                C.Submit.create
                  ~limits
                  ~run_id
                  ~key:live_key
                  ~kind:Live_run
                  ~route
                  ~target:One_off_v1
                  ~prompt:
                    (Some
                       (Agent_server.Catalog_identity.prompt_definition "embedded.prompt"))
                  ~workspace:
                    (Some
                       (Agent_server.Catalog_identity.workspace_definition
                          "embedded.workspace"))
                  ~entrypoint:"main"
                  ~input:(`String "live")
                  ~fixtures:None
                  ~requested_execution:None
                  ~requested_max_output_bytes:None
                |> protocol_ok
              in
              (match
                 Agent_client.Developer_connection.send developer (W.Request.Submit live)
                 |> protocol_ok
               with
               | W.Response.Submit _ -> ()
               | _ -> failwith "embedded Live response changed method");
              (match
                 Agent_client.Developer_connection.send
                   developer
                   (W.Request.Await_terminal (C.Lookup.create ~run_id ~key:live_key))
                 |> protocol_ok
               with
               | W.Response.Await_terminal view ->
                 (match C.View.phase view with
                  | Terminal { receipt; execution_started = true } ->
                    (match P.Developer_receipt.value receipt with
                     | Live (Succeeded (`String "live")) -> ()
                     | _ -> failwith "embedded Live did not return its input")
                  | _ -> failwith "embedded Live did not execute")
               | _ -> failwith "embedded Live await changed method");
              let fixture_route =
                C.Route.local_source
                  ~limits:(C.Limits.request limits)
                  ~root_index:0
                  ~member:(Source.Portable_path.create "fixture.chatml" |> Or_error.ok_exn)
                |> protocol_ok
              in
              let step =
                C.Fixture.Step.create
                  ~limits:(C.Limits.request limits)
                  ~tool_name:"fixture"
                  ~input_schema:(`Object [])
                  ~output_schema:(`Object [])
                  ~expected_input:`Null
                  ~response:(Return (`String "fixture-result"))
                |> protocol_ok
              in
              let fixtures =
                C.Fixture.create
                  ~limits:(C.Limits.request limits)
                  ~max_steps:(C.Limits.max_fixture_steps limits)
                  ~max_bytes:(C.Limits.max_fixture_bytes limits)
                  ~steps:[ step ]
                |> protocol_ok
              in
              let fixture_run_id = P.Id.Developer_run.create () in
              let fixture_key = key "embedded-typed-fixture" in
              let fixture =
                C.Submit.create
                  ~limits
                  ~run_id:fixture_run_id
                  ~key:fixture_key
                  ~kind:Fixture_run
                  ~route:fixture_route
                  ~target:Tool_v1
                  ~prompt:None
                  ~workspace:None
                  ~entrypoint:"run"
                  ~input:`Null
                  ~fixtures:(Some fixtures)
                  ~requested_execution:None
                  ~requested_max_output_bytes:None
                |> protocol_ok
              in
              (match
                 Agent_client.Developer_connection.send
                   developer
                   (W.Request.Submit fixture)
                 |> protocol_ok
               with
               | W.Response.Submit _ -> ()
               | _ -> failwith "embedded Fixture response changed method");
              (match
                 Agent_client.Developer_connection.send
                   developer
                   (W.Request.Await_terminal
                      (C.Lookup.create ~run_id:fixture_run_id ~key:fixture_key))
                 |> protocol_ok
               with
               | W.Response.Await_terminal view ->
                 (match C.View.phase view with
                  | Terminal { receipt; execution_started = _ } ->
                    (match P.Developer_receipt.value receipt with
                     | Fixture
                         (Completed
                            { outcome = Succeeded (`String "fixture-result")
                            ; matched_steps = 1
                            ; _
                            }) -> ()
                     | _ ->
                       raise_s
                         [%sexp
                           "embedded Fixture did not match its one step"
                         , (P.Developer_receipt.to_json receipt : Jsonaf.t)])
                  | Reserved | Waiting_permission | Execution_started ->
                    failwith "embedded Fixture did not settle")
               | _ -> failwith "embedded Fixture await changed method");
              let repl_limits =
                Agent_client.Developer_repl_connection.capabilities repl
                |> R.Capabilities.limits
              in
              let repl_route =
                C.Route.local_source
                  ~limits:(R.Limits.request repl_limits)
                  ~root_index:0
                  ~member
                |> protocol_ok
              in
              let created =
                R.Create.create
                  ~limits:repl_limits
                  ~run_id:(P.Id.Developer_run.create ())
                  ~key:(key "embedded-repl-create")
                  ~route:repl_route
                  ~target:One_off_v1
                  ~prompt:
                    (Agent_server.Catalog_identity.prompt_definition "embedded.prompt")
                  ~workspace:
                    (Agent_server.Catalog_identity.workspace_definition
                       "embedded.workspace")
                  ~requested_execution:None
                  ~requested_max_output_bytes:None
                |> protocol_ok
                |> Agent_client.Developer_repl_connection.create_instance repl
                |> protocol_ok
              in
              let submitted =
                R.Submit.create
                  ~limits:repl_limits
                  ~instance:(R.View.instance created)
                  ~epoch:(R.View.epoch created)
                  ~expected_revision:(R.View.revision created)
                  ~key:(key "embedded-repl-submit")
                  ~source:"Task.pure(8)"
                |> protocol_ok
                |> Agent_client.Developer_repl_connection.submit repl
                |> protocol_ok
              in
              assert (Int.equal (List.length (R.Submission.task_references submitted)) 1);
              let separate = Agent_server.Embedded.connect_host host |> protocol_ok in
              Exn.protect
                ~f:(fun () ->
                  assert (
                    Result.is_error
                      (Agent_client.Developer_repl_connection.create separate));
                  let implementation =
                    P.Initialize.Implementation.create
                      ~name:"embedded-selected-check-only"
                      ~version:"1"
                    |> protocol_ok
                  in
                  let request =
                    P.Initialize.Request.create
                      ~implementation
                      ~protocol_min:P.Version.current
                      ~protocol_max:P.Version.current
                      ~features:[ C.Features.run ]
                      ~event_encodings:[ Json ]
                      ~max_inbound_event_bytes:(2 * 1024 * 1024)
                      ()
                    |> protocol_ok
                  in
                  (match
                     Agent_client.Connection.request_without_history
                       separate
                       (Protocol_initialize request)
                     |> protocol_ok
                   with
                   | Protocol_initialize response ->
                     assert (
                       not
                         (List.mem
                            response.enabled_features
                            R.Features.repl
                            ~equal:String.equal))
                   | _ -> failwith "separate Initialize changed method");
                  assert (
                    Result.is_error
                      (Agent_client.Developer_repl_connection.create separate)))
                ~finally:(fun () -> Agent_client.Connection.close separate))
            ~finally:(fun () -> Agent_server.Embedded.close_host host)))
    ;;

    let%test_unit
        "failed post-Store bind releases the durable root and preserves identity"
      =
      with_policy_file (policy ~principal:"pri_foreign" ()) (fun env bad_path ->
        let bad = V.load ~env ~path:bad_path |> ok in
        let root = Filename.dirname bad_path in
        let good_path = Filename.concat root "valid-policy.sexp" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / good_path)
          (policy ());
        let good = V.load ~env ~path:good_path |> ok in
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "agent.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Durable local policy fixture.</developer>";
        let durable =
          match
            Agent_server.Local_storage.Root.create
              ~path:(Filename.concat root "durable-store")
              ()
          with
          | Ok root -> root
          | Error error -> raise_s [%sexp "durable root failed", (error : P.Error.t)]
        in
        let options : Agent_server.Embedded.options =
          { prompt_file = prompt
          ; workspace
          ; tool_dir = root
          ; home = Some root
          ; storage = Durable durable
          ; start_immediately = false
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 32
          }
        in
        Eio.Switch.run (fun sw ->
          match
            Agent_server.Embedded.open_local_host ~sw ~env ~local_policy:bad options
          with
          | Error { code = Invalid_request; _ } -> ()
          | Error error -> raise_s [%sexp "unexpected bind failure", (error : P.Error.t)]
          | Ok host ->
            Agent_server.Embedded.close_host host;
            failwith "literal selector admitted after Store open");
        let open_valid () =
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
            Eio.Switch.run (fun sw ->
              let host =
                match
                  Agent_server.Embedded.open_local_host
                    ~sw
                    ~env
                    ~local_policy:good
                    options
                with
                | Ok host -> host
                | Error error ->
                  raise_s [%sexp "valid local reopen failed", (error : P.Error.t)]
              in
              let id = (Agent_server.Embedded.host_principal host).id in
              Agent_server.Embedded.close_host host;
              id))
        in
        let first = open_valid () in
        let reopened = open_valid () in
        assert (P.Id.Principal.equal first reopened))
    ;;
  end)
;;
