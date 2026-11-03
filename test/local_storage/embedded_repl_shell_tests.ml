let%test_module "embedded local REPL native Shell grant" =
  (module struct
    open! Core
    module P = Agent_protocol
    module R = P.Developer_repl_wire
    module C = P.Developer_common
    module E = Agent_server.Embedded
    module V = Agent_server.Config_validator.Embedded_local

    let protocol = function
      | Ok value -> value
      | Error error -> raise_s [%sexp "embedded Shell protocol", (error : P.Error.t)]
    ;;

    let key name = P.Idempotency_key.of_string name |> protocol

    let prompt =
      {|<developer>Run only explicitly selected local source.</developer>
<shell_access id="fixture" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false" child_processes="true"
      arbitrary_code="true" privilege_change="false">
    <read path="${tool_dir}"/><read path="${workspace}"/><write path="${workspace}"/>
  </capabilities>
  <backends merge="replace"><direct when="macos"/><direct when="linux"/></backends>
  <policy default="allow"/><audit format="none"/>
</shell_access>
<tool name="count" type="shell" mode="script" runtime="fixture" result="stdout"
    script="${tool_dir}/count.sh" interpreter="/bin/sh" executable="false"/>|}
    ;;

    let policy root ?grant () =
      let root_atom = Sexp.to_string_mach (Sexp.Atom root) in
      let capacity =
        Agent_server.Developer_startup_configuration.Capacity.standard_v1
        |> Agent_server.Developer_startup_configuration.Capacity.sexp_of_t
        |> Sexp.to_string_mach
      in
      let grant =
        Option.value_map grant ~default:"" ~f:(fun (source, manifest) ->
          sprintf
            {|(manifest_grants
               (((id embedded-native-exact) (prompt embedded.prompt)
                 (workspaces (embedded.workspace)) (manifest_sha256 %s)
                 (source_sha256 %s) (principals (Embedded_operator)))))|}
            manifest
            source)
      in
      sprintf
        {|(project_sources ((external_roots (%s))))
          (authoring_project_access
           ((read_grants (((root %s) (principal_id Embedded_operator)
                           (authentication_kind embedded.local))))
            (selections (((root %s) (project count.chatml)
                          (request (entry ((name main)))) (target one_off_v1))))))
          (developer
           ((selections
             (((source (Local_source (root %s) (member count.chatml)))
               (target One_off_v1)
               (principals (((principal_id Embedded_operator)
                             (authentication_kind embedded.local)))))))
            (capacity %s) (model Disabled) (approvals Disabled)
            (repl Standard_v1) (portable true)))
          (developer_scopes (developer.live.run developer.repl))
          %s|}
        root_atom
        root_atom
        root_atom
        root_atom
        capacity
        grant
    ;;

    let shell_hashes env prompt_file =
      let prompt_id = Agent_server.Catalog_identity.prompt_definition "embedded.prompt" in
      let workspace_id =
        Agent_server.Catalog_identity.workspace_definition "embedded.workspace"
      in
      let definition =
        Agent_session.Prompt_definition.create
          ~id:prompt_id
          ~config_name:"embedded.prompt"
          ~root_file:prompt_file
          ~root_invocation:Ui
          ~allowed_workspaces:[ workspace_id ]
          ~permission_profile:E.default_permission_profile.id
          ~runtime_policy:None
          ~enabled:true
          ~description:None
        |> Result.map_error ~f:(fun error ->
          Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
        |> Result.ok_or_failwith
      in
      let capture =
        Agent_session.Prompt_revision_builder.capture
          ~env
          ~preprocess:Fn.id
          ~limits:(Agent_session.Prompt_revision_builder.capture_limits ())
          ~checkpoint:ignore
          definition
        |> Result.map_error ~f:(fun diagnostics ->
          Sexp.to_string_hum
            ([%sexp_of: Agent_session.Prompt_revision_builder.Diagnostic.t list]
               diagnostics))
        |> Result.ok_or_failwith
      in
      ( Chatmd_shell_spec.Source_ref.digest prompt
      , Agent_session.Prompt_revision_builder.Capture.shell_manifest_sha256 capture )
    ;;

    let%test_unit "exact manifest grant executes one native Task and survives reopen" =
      Eio_main.run (fun env ->
        Mirage_crypto_rng_unix.use_default ();
        let root = Agent_server_test_support.temporary_root env in
        Exn.protect
          ~f:(fun () ->
            let workspace = Filename.concat root "workspace" in
            Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
            let prompt_file = Filename.concat root "agent.chatmd" in
            Eio.Path.save
              ~create:(`Exclusive 0o600)
              Eio.Path.(Eio.Stdenv.fs env / prompt_file)
              prompt;
            let script_file = Filename.concat root "count.sh" in
            Eio.Path.save
              ~create:(`Exclusive 0o700)
              Eio.Path.(Eio.Stdenv.fs env / script_file)
              "#!/bin/sh\nset -eu\nprintf 'ran\\n' >> count.effects\nprintf 'counted'\n";
            Eio.Path.save
              ~create:(`Exclusive 0o600)
              Eio.Path.(Eio.Stdenv.fs env / Filename.concat root "count.chatml")
              {|let main input = Tool.call("count", `Object([]))|};
            let policy_file = Filename.concat root "local-policy.sexp" in
            let save_policy ?grant () =
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / policy_file)
                (policy root ?grant ())
            in
            let load_policy () =
              V.load ~env ~path:policy_file
              |> Result.map_error ~f:(fun diagnostics ->
                Sexp.to_string_hum
                  ([%sexp_of: Agent_server.Config.Diagnostic.t list] diagnostics))
              |> Result.ok_or_failwith
            in
            let durable =
              Agent_server.Local_storage.Root.create
                ~path:(Filename.concat root "durable-store")
                ()
              |> protocol
            in
            let options : E.options =
              { prompt_file
              ; workspace
              ; tool_dir = root
              ; home = Some root
              ; storage = Durable durable
              ; start_immediately = false
              ; permission_profile = E.default_permission_profile
              ; attachment_mode = Read_write
              ; event_capacity = 32
              }
            in
            let with_host local_policy f =
              Eio.Switch.run (fun sw ->
                let host = E.open_local_host ~sw ~env ~local_policy options |> protocol in
                Exn.protect ~f:(fun () -> f host) ~finally:(fun () -> E.close_host host))
            in
            let effect_path = Filename.concat workspace "count.effects" in
            save_policy ();
            with_host (load_policy ()) (fun host ->
              let connection = E.host_connection host in
              let repl =
                Agent_client.Developer_repl_connection.create connection |> protocol
              in
              let limits =
                Agent_client.Developer_repl_connection.capabilities repl
                |> R.Capabilities.limits
              in
              let route =
                C.Route.local_source
                  ~limits:(R.Limits.request limits)
                  ~root_index:0
                  ~member:(Source.Portable_path.create "count.chatml" |> Or_error.ok_exn)
                |> protocol
              in
              let request =
                R.Create.create
                  ~limits
                  ~run_id:(P.Id.Developer_run.create ())
                  ~key:(key "embedded-native-denied")
                  ~route
                  ~target:One_off_v1
                  ~prompt:
                    (Agent_server.Catalog_identity.prompt_definition "embedded.prompt")
                  ~workspace:
                    (Agent_server.Catalog_identity.workspace_definition
                       "embedded.workspace")
                  ~requested_execution:None
                  ~requested_max_output_bytes:None
                |> protocol
              in
              let created =
                Agent_client.Developer_repl_connection.create_instance repl request
                |> protocol
              in
              let completion_request =
                R.Completion_request.create
                  ~limits
                  ~instance:(R.View.instance created)
                  ~epoch:(R.View.epoch created)
                  ~expected_revision:(R.View.revision created)
                  ~path:[ "Task" ]
                  ~prefix:"p"
                  ~limit:1
                |> protocol
              in
              let completed =
                Agent_client.Developer_repl_connection.complete repl completion_request
                |> protocol
              in
              if
                not
                  (List.equal String.equal (R.Completion_page.names completed) [ "pure" ])
              then failwith "live REPL startup completion did not read selected contract";
              let submitted =
                R.Submit.create
                  ~limits
                  ~instance:(R.View.instance created)
                  ~epoch:(R.View.epoch created)
                  ~expected_revision:(R.View.revision created)
                  ~key:(key "embedded-native-denied-expression")
                  ~source:{|Tool.call("count", `Object([]))|}
                |> protocol
                |> Agent_client.Developer_repl_connection.submit repl
                |> protocol
              in
              if
                not
                  (Result.is_error
                     (Agent_client.Developer_repl_connection.complete
                        repl
                        completion_request))
              then failwith "live REPL completion accepted a stale pre-submit revision";
              let task_key = key "embedded-native-denied-task" in
              let task_request =
                R.Task_submit.create
                  ~limits
                  ~key:task_key
                  ~reference:(R.Submission.task_references submitted |> List.hd_exn)
                  ~requested_execution:None
                  ~requested_max_output_bytes:None
                  ~fresh_epoch:(Some (R.View.instance created, R.View.epoch created))
                |> protocol
              in
              match
                Agent_client.Developer_repl_connection.task_submit
                  repl
                  ~view:created
                  task_request
              with
              | Error { code = Permission_denied; _ } -> ()
              | Error error ->
                raise_s
                  [%sexp
                    "ungranted Shell rejected for another reason", (error : P.Error.t)]
              | Ok _ ->
                let terminal =
                  Agent_client.Developer_repl_connection.task_await_terminal
                    repl
                    (C.Lookup.create ~run_id:(R.View.run_id created) ~key:task_key)
                  |> protocol
                in
                (match R.Task_view.phase terminal with
                 | Terminal
                     { outcome = Failed { error = { code = Invalid_state; _ }; _ }; _ } ->
                   ()
                 | _ ->
                   raise_s
                     [%sexp
                       "ungranted Shell Task was not denied"
                     , (R.Task_view.to_json terminal : Jsonaf.t)]));
            assert (not (Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / effect_path)));
            let source, manifest = shell_hashes env prompt_file in
            save_policy ~grant:(source, manifest) ();
            let original_task = ref None in
            with_host (load_policy ()) (fun host ->
              let connection = E.host_connection host in
              let repl =
                Agent_client.Developer_repl_connection.create connection |> protocol
              in
              let limits =
                Agent_client.Developer_repl_connection.capabilities repl
                |> R.Capabilities.limits
              in
              let route =
                C.Route.local_source
                  ~limits:(R.Limits.request limits)
                  ~root_index:0
                  ~member:(Source.Portable_path.create "count.chatml" |> Or_error.ok_exn)
                |> protocol
              in
              let run_id = P.Id.Developer_run.create () in
              let created =
                R.Create.create
                  ~limits
                  ~run_id
                  ~key:(key "embedded-native-create")
                  ~route
                  ~target:One_off_v1
                  ~prompt:
                    (Agent_server.Catalog_identity.prompt_definition "embedded.prompt")
                  ~workspace:
                    (Agent_server.Catalog_identity.workspace_definition
                       "embedded.workspace")
                  ~requested_execution:None
                  ~requested_max_output_bytes:None
                |> protocol
                |> Agent_client.Developer_repl_connection.create_instance repl
                |> protocol
              in
              let submitted =
                R.Submit.create
                  ~limits
                  ~instance:(R.View.instance created)
                  ~epoch:(R.View.epoch created)
                  ~expected_revision:(R.View.revision created)
                  ~key:(key "embedded-native-expression")
                  ~source:{|Tool.call("count", `Object([]))|}
                |> protocol
                |> Agent_client.Developer_repl_connection.submit repl
                |> protocol
              in
              let reference = R.Submission.task_references submitted |> List.hd_exn in
              let task_key = key "embedded-native-task" in
              let task_request =
                R.Task_submit.create
                  ~limits
                  ~key:task_key
                  ~reference
                  ~requested_execution:None
                  ~requested_max_output_bytes:None
                  ~fresh_epoch:(Some (R.View.instance created, R.View.epoch created))
                |> protocol
              in
              let started =
                Agent_client.Developer_repl_connection.task_submit
                  repl
                  ~view:created
                  task_request
                |> protocol
              in
              let lookup = C.Lookup.create ~run_id ~key:task_key in
              let terminal =
                Agent_client.Developer_repl_connection.task_await_terminal repl lookup
                |> protocol
              in
              assert (
                P.Developer_command_id.equal
                  (R.Task_view.command_id started)
                  (R.Task_view.command_id terminal));
              (match R.Task_view.phase terminal with
               | Terminal { execution_started = true; outcome = Succeeded _ } -> ()
               | _ -> failwith "native Shell Task did not succeed");
              original_task := Some (lookup, R.Task_view.command_id terminal);
              let reset =
                Agent_client.Developer_repl_connection.reset_and_wait
                  repl
                  (R.Current_epoch.create
                     ~instance:(R.View.instance created)
                     ~epoch:(R.View.epoch created)
                     ~key:(key "embedded-native-reset"))
                |> protocol
              in
              assert (
                not (R.Wire_id.Epoch.equal (R.View.epoch created) (R.View.epoch reset)));
              (match
                 Agent_client.Connection.request_repl
                   connection
                   ~limits
                   (R.Request.Task_submit task_request)
                 |> protocol
               with
               | R.Response.Task_submit replay ->
                 assert (
                   P.Developer_command_id.equal
                     (R.Task_view.command_id replay)
                     (R.Task_view.command_id terminal))
               | _ -> failwith "historical native Task replay changed method");
              let after_reset =
                R.Submit.create
                  ~limits
                  ~instance:(R.View.instance reset)
                  ~epoch:(R.View.epoch reset)
                  ~expected_revision:(R.View.revision reset)
                  ~key:(key "embedded-native-new-epoch-expression")
                  ~source:{|Tool.call("count", `Object([]))|}
                |> protocol
                |> Agent_client.Developer_repl_connection.submit repl
                |> protocol
              in
              let fresh_key = key "embedded-native-new-epoch-task" in
              let fresh_request =
                R.Task_submit.create
                  ~limits
                  ~key:fresh_key
                  ~reference:(R.Submission.task_references after_reset |> List.hd_exn)
                  ~requested_execution:None
                  ~requested_max_output_bytes:None
                  ~fresh_epoch:(Some (R.View.instance reset, R.View.epoch reset))
                |> protocol
              in
              let fresh =
                Agent_client.Developer_repl_connection.task_submit
                  repl
                  ~view:reset
                  fresh_request
                |> protocol
              in
              let fresh_terminal =
                Agent_client.Developer_repl_connection.task_await_terminal
                  repl
                  (C.Lookup.create ~run_id ~key:fresh_key)
                |> protocol
              in
              assert (
                not
                  (P.Developer_command_id.equal
                     (R.Task_view.command_id terminal)
                     (R.Task_view.command_id fresh)));
              match R.Task_view.phase fresh_terminal with
              | Terminal { execution_started = true; outcome = Succeeded _ } -> ()
              | _ -> failwith "fresh epoch native Shell Task did not succeed");
            let effects =
              Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / effect_path)
              |> String.split_lines
            in
            [%test_eq: string list] effects [ "ran"; "ran" ];
            with_host (load_policy ()) (fun host ->
              let repl =
                Agent_client.Developer_repl_connection.create (E.host_connection host)
                |> protocol
              in
              let lookup, expected_id = Option.value_exn !original_task in
              let observed =
                Agent_client.Developer_repl_connection.task_observe repl lookup
                |> protocol
                |> Option.value_exn
              in
              assert (
                P.Developer_command_id.equal expected_id (R.Task_view.command_id observed)));
            let effects =
              Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / effect_path)
              |> String.split_lines
            in
            [%test_eq: string list] effects [ "ran"; "ran" ])
          ~finally:(fun () ->
            Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)))
    ;;

    let%test_unit
        "live completion isolates actor, connection lease, instance and reset epoch"
      =
      Eio_main.run (fun env ->
        Mirage_crypto_rng_unix.use_default ();
        let root = Agent_server_test_support.temporary_root env in
        Exn.protect
          ~finally:(fun () ->
            Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
          ~f:(fun () ->
            let workspace = Filename.concat root "workspace" in
            Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
            let prompt_file = Filename.concat root "agent.chatmd" in
            Eio.Path.save
              ~create:(`Exclusive 0o600)
              Eio.Path.(Eio.Stdenv.fs env / prompt_file)
              "<developer>Completion isolation fixture.</developer>";
            Eio.Path.save
              ~create:(`Exclusive 0o600)
              Eio.Path.(Eio.Stdenv.fs env / Filename.concat root "count.chatml")
              "let main input = Task.pure(input)";
            let policy_file = Filename.concat root "local-policy.sexp" in
            Eio.Path.save
              ~create:(`Exclusive 0o600)
              Eio.Path.(Eio.Stdenv.fs env / policy_file)
              (policy root ());
            let local_policy =
              V.load ~env ~path:policy_file
              |> Result.map_error ~f:(fun diagnostics ->
                Sexp.to_string_hum
                  ([%sexp_of: Agent_server.Config.Diagnostic.t list] diagnostics))
              |> Result.ok_or_failwith
            in
            let options : E.options =
              { prompt_file
              ; workspace
              ; tool_dir = root
              ; home = Some root
              ; storage = Transient
              ; start_immediately = false
              ; permission_profile = E.default_permission_profile
              ; attachment_mode = Read_write
              ; event_capacity = 32
              }
            in
            Eio.Switch.run (fun sw ->
              let host = E.open_local_host ~sw ~env ~local_policy options |> protocol in
              Exn.protect
                ~finally:(fun () -> E.close_host host)
                ~f:(fun () ->
                  let repl =
                    Agent_client.Developer_repl_connection.create (E.host_connection host)
                    |> protocol
                  in
                  let limits =
                    Agent_client.Developer_repl_connection.capabilities repl
                    |> R.Capabilities.limits
                  in
                  let route =
                    C.Route.local_source
                      ~limits:(R.Limits.request limits)
                      ~root_index:0
                      ~member:
                        (Source.Portable_path.create "count.chatml" |> Or_error.ok_exn)
                    |> protocol
                  in
                  let create name =
                    R.Create.create
                      ~limits
                      ~run_id:(P.Id.Developer_run.create ())
                      ~key:(key name)
                      ~route
                      ~target:One_off_v1
                      ~prompt:
                        (Agent_server.Catalog_identity.prompt_definition
                           "embedded.prompt")
                      ~workspace:
                        (Agent_server.Catalog_identity.workspace_definition
                           "embedded.workspace")
                      ~requested_execution:None
                      ~requested_max_output_bytes:None
                    |> protocol
                    |> Agent_client.Developer_repl_connection.create_instance repl
                    |> protocol
                  in
                  let first = create "completion-isolation-first" in
                  let request ~instance ~epoch ~revision =
                    R.Completion_request.create
                      ~limits
                      ~instance
                      ~epoch
                      ~expected_revision:revision
                      ~path:[ "Task" ]
                      ~prefix:"p"
                      ~limit:1
                    |> protocol
                  in
                  let for_view view =
                    request
                      ~instance:(R.View.instance view)
                      ~epoch:(R.View.epoch view)
                      ~revision:(R.View.revision view)
                  in
                  let first_request = for_view first in
                  let assert_page result =
                    let page = result |> protocol in
                    if
                      not
                        (List.equal
                           String.equal
                           (R.Completion_page.names page)
                           [ "pure" ])
                    then failwith "completion isolation fixture lacked a valid baseline"
                  in
                  assert_page
                    (Agent_client.Developer_repl_connection.complete repl first_request);
                  let dispatcher = E.host_dispatcher host in
                  let reject_after_guard_revocation ~name ~revoke =
                    let current = ref true in
                    let armed = ref false in
                    let watching = ref false in
                    let guard_checks = ref 0 in
                    let revoked = ref false in
                    let revoke_now = ref (fun () -> ()) in
                    let actor =
                      Operator_authorization.guarded
                        ~principal:(E.host_principal host)
                        ~is_current:(fun () ->
                          if !watching then Int.incr guard_checks;
                          let checked_current = !current in
                          if !armed
                          then (
                            armed := false;
                            revoked := true;
                            !revoke_now ());
                          checked_current)
                    in
                    let context =
                      Agent_server.Connection_context.create_authenticated
                        ~actor
                        ~connection_id:name
                        ~transport:In_memory
                        ~publish_notification:ignore
                        ~max_attachments:8
                    in
                    Exn.protect
                      ~finally:(fun () -> E.host_close_connection host context)
                      ~f:(fun () ->
                        let initialize =
                          P.Initialize.Request.create
                            ~implementation:
                              (P.Initialize.Implementation.create ~name ~version:"1"
                               |> protocol)
                            ~protocol_min:P.Version.current
                            ~protocol_max:P.Version.current
                            ~features:[ R.Features.repl ]
                            ~event_encodings:[ Json ]
                            ~max_inbound_event_bytes:(2 * 1024 * 1024)
                            ()
                          |> protocol
                        in
                        ignore
                          (Agent_server.Dispatcher.dispatch_command
                             dispatcher
                             ~context
                             (Protocol_initialize initialize)
                           |> protocol
                           : P.Public.Result.t);
                        let dispatch () =
                          Agent_server.Dispatcher.dispatch_developer_repl
                            dispatcher
                            ~context
                            (R.Request.Complete first_request)
                        in
                        (match dispatch () |> protocol with
                         | R.Response.Complete page -> assert_page (Ok page)
                         | _ -> failwith "completion baseline changed response method");
                        (* The first armed guard runs after the endpoint captured
                           and compared its lease. Returning its just-checked true
                           result models revocation immediately after that check;
                           later host/request checks must fail closed. This private
                           context's lease mutex is uncontended, so revocation is
                           nonyielding and needs no scheduler/timing assumption. *)
                        (revoke_now := fun () -> revoke current context);
                        watching := true;
                        armed := true;
                        let result = dispatch () in
                        if (not !revoked) || !guard_checks < 2
                        then failwith "completion omitted the post-revocation guard";
                        match result with
                        | Error { code = Permission_denied | Lease_stale; _ } -> ()
                        | Error error ->
                          raise_s
                            [%sexp
                              "completion revocation rejected for another reason"
                            , (error : P.Error.t)]
                        | Ok _ ->
                          failwith
                            "completion published after authority or lease revocation")
                  in
                  reject_after_guard_revocation
                    ~name:"completion-actor-revocation"
                    ~revoke:(fun current _ -> current := false);
                  reject_after_guard_revocation
                    ~name:"completion-lease-revocation"
                    ~revoke:(fun _ context ->
                      Agent_server.Connection_context.close_authoring context);
                  let second = create "completion-isolation-second" in
                  assert_page
                    (Agent_client.Developer_repl_connection.complete
                       repl
                       (for_view second));
                  let mixed =
                    request
                      ~instance:(R.View.instance second)
                      ~epoch:(R.View.epoch first)
                      ~revision:(R.View.revision first)
                  in
                  let assert_stale request =
                    match
                      Agent_client.Developer_repl_connection.complete repl request
                    with
                    | Error { code = Lease_stale; _ } -> ()
                    | Error error ->
                      raise_s
                        [%sexp
                          "completion identity rejected for another reason"
                        , (error : P.Error.t)]
                    | Ok _ ->
                      failwith "completion accepted another instance or reset epoch"
                  in
                  assert_stale mixed;
                  let reset =
                    Agent_client.Developer_repl_connection.reset_and_wait
                      repl
                      (R.Current_epoch.create
                         ~instance:(R.View.instance first)
                         ~epoch:(R.View.epoch first)
                         ~key:(key "completion-isolation-reset"))
                    |> protocol
                  in
                  assert_stale first_request;
                  assert_stale
                    (request
                       ~instance:(R.View.instance reset)
                       ~epoch:(R.View.epoch first)
                       ~revision:(R.View.revision reset));
                  assert_page
                    (Agent_client.Developer_repl_connection.complete
                       repl
                       (for_view reset))))))
    ;;
  end)
;;
