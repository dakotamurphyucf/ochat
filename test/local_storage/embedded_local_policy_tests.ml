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
          Agent_server.Embedded.close_host host))
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
