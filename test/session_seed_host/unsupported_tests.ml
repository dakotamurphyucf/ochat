open! Core
open Agent_server_test_support
open Support

let%expect_test "custom host without seed Factory omits discovery and denies seed" =
  with_root (fun env ~root ~prompt_file:_ config ->
    with_host env ~root config (fun sw daemon client ->
      let implementation =
        P.Initialize.Implementation.create ~name:"seed-custom-fixture" ~version:"1"
        |> protocol_ok
      in
      let init =
        P.Initialize.Request.create
          ~implementation
          ~protocol_min:P.Version.current
          ~protocol_max:P.Version.current
          ~features:[ "sessions.seed" ]
          ~event_encodings:[ Json ]
          ~max_inbound_event_bytes:(16 * 1024 * 1024)
          ()
        |> protocol_ok
      in
      let response =
        C.Connection.request_without_history client (Protocol_initialize init)
        |> protocol_ok
        |> function
        | P.Method_result.Protocol_initialize response -> response
        | _ -> failwith "initialize"
      in
      assert (List.mem response.enabled_features "sessions.seed" ~equal:String.equal);
      let info =
        C.Connection.request_without_history client Server_info
        |> protocol_ok
        |> function
        | P.Method_result.Server_info info -> info
        | _ -> failwith "server info"
      in
      let store = S.Daemon.store daemon in
      let registry = S.Daemon.registry daemon in
      let factory = S.Daemon.factory daemon in
      let idempotency_store =
        Agent_store.Idempotency_store.open_or_create
          ~env
          ~path:(Filename.concat root "custom-receipts.json")
        |> store_ok
      in
      let audit_store =
        Agent_store.Audit_store.open_or_create
          ~env
          ~directory:(Filename.concat root "custom-audit")
          ~max_payload_length:S.Daemon.default_options.factory_limits.max_journal_payload
        |> store_ok
      in
      let now () = response.server_time in
      let lifecycle_service =
        S.Session_lifecycle_service.create
          ~store
          ~registry
          ~idempotency:idempotency_store
          ~now
          ~read_owned_session:(S.Session_factory.read_owned_session factory)
          ~validate_removal:(S.Session_factory.validate_session_removal factory)
      in
      let handler =
        S.Command_handler.create
          ~sw
          ~env
          ~registry
          ~prompts:(S.Daemon.prompts daemon)
          ~workspaces:(S.Daemon.workspaces daemon)
          ~start_queue:(A.Start_queue.create ())
          ~idempotency_store
          ~audit_store
          ~blob_store:(S.Daemon.blob_store daemon)
          ~session_store:store
          ~lifecycle_service
          ~retained:
            (S.Retained_session.create
               ~store
               ~registry
               ~read_owned:(S.Session_factory.read_owned_session factory))
          ~archive_payload_limit:
            S.Daemon.default_options.factory_limits.snapshot_payload_limit
          ~initialize:(fun ~principal:_ _ -> Ok response)
          ~ping:(fun _ -> failwith "unexpected ping")
          ~server_info:(fun () -> info)
          ~server_health:(fun _ -> failwith "unexpected health")
          ~cancel_job:(fun _ -> failwith "unexpected cancellation")
          ~create_session:(S.Session_factory.create_session factory)
          ~seed_factory:None
          ~seed_recovery_limits:S.Session_factory.Seed_recovery_limits.default
          ~prepare_session_start:(S.Session_factory.prepare_session_start factory)
          ~workspace_retained:(S.Session_factory.workspace_retained factory)
          ~provider_operator:None
          ~dependency_administration:None
          ~prepare_administration:(S.Session_factory.prepare_administration factory)
      in
      let notifications = Eio.Stream.create 16 in
      let actor =
        Operator_authorization.guarded ~principal:(principal ()) ~is_current:(fun () ->
          true)
      in
      let context =
        S.Connection_context.create_authenticated
          ~actor
          ~connection_id:"seed-custom-host"
          ~transport:In_memory
          ~publish_notification:(Eio.Stream.add notifications)
          ~max_attachments:16
      in
      let dispatcher = S.Dispatcher.create handler in
      let custom =
        C.In_memory.create
          ~notifications
          ~request:(S.Dispatcher.dispatch_command dispatcher ~context)
          ~close:(fun () -> S.Command_handler.close_connection handler context)
      in
      Exn.protect
        ~finally:(fun () -> C.Connection.close custom)
        ~f:(fun () ->
          let projected =
            C.Connection.request_without_history custom (Protocol_initialize init)
            |> protocol_ok
            |> function
            | P.Method_result.Protocol_initialize response -> response
            | _ -> failwith "custom initialize"
          in
          assert (
            not (List.mem projected.enabled_features "sessions.seed" ~equal:String.equal));
          (match
             C.Connection.request_without_history custom Server_info |> protocol_ok
           with
           | Server_info projected ->
             assert (not (List.mem projected.features "sessions.seed" ~equal:String.equal))
           | _ -> failwith "custom server info");
          let before =
            Agent_store.Session_index.list_checked
              (Agent_store.Session_store.session_index store)
            |> store_ok
          in
          (match
             C.Session_handle.seed
               custom
               (request
                  ~code:(catalog_code client)
                  ~key:"unsupported-seed"
                  (transcript ()))
           with
           | Error { code = Method_not_found; _ } -> ()
           | Error _ | Ok _ -> failwith "unsupported seed was admitted");
          let after =
            Agent_store.Session_index.list_checked
              (Agent_store.Session_store.session_index store)
            |> store_ok
          in
          assert (Int.equal (List.length before) (List.length after));
          print_endline
            "actual custom Handler: capability omitted twice; unsupported seed leaves \
             index unchanged")));
  [%expect
    {| actual custom Handler: capability omitted twice; unsupported seed leaves index unchanged |}]
;;
