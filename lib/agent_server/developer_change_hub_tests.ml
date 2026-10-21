let%test_module "bounded Developer change ownership" =
  (module struct
    open! Core
    module P = Agent_protocol
    module D = Document_schema
    module Hub = Developer_change_hub
    module Change = P.Developer_wire.Change

    let () = Mirage_crypto_rng_unix.use_default ()

    let must = function
      | Ok value -> value
      | Error _ -> failwith "expected valid Developer watch fixture"
    ;;

    let wire_limits () =
      let bound max_bytes =
        must (D.Limits.create ~max_bytes ~max_depth:32 ~max_fields:512 ~max_nodes:1024)
      in
      must
        (P.Developer_common.Limits.create
           ~request:(bound 4096)
           ~receipt:(bound 4096)
           ~response:(bound 4096)
           ~approval_page:(bound 4096)
           ~max_output_bytes:1024
           ~max_fixture_steps:4
           ~max_fixture_bytes:1024
           ~max_approval_display_bytes:256
           ~max_approval_decision_reason_bytes:256
           ~max_approval_page:4
           ~max_event_bytes:2048)
    ;;

    let%test_unit
        "unwatch joins publication; other keys resync and reinitialize stays live"
      =
      Eio_main.run (fun env ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 8. (fun () ->
          Eio.Switch.run (fun sw ->
            let wire_limits = wire_limits () in
            let limits =
              must
                (Hub.Limits.create
                   ~max_connections:1
                   ~max_watches_per_connection:2
                   ~max_pending_hints_per_connection:1)
            in
            let hub = must (Hub.create ~sw ~limits ~wire_limits ~repl_wire_limits:None) in
            let principal =
              must
                (P.Principal.create
                   ~id:(must (P.Id.Principal.of_string "pri_watcher"))
                   ~authentication_kind:"test"
                   ~scopes:(P.Scope.Set.of_list [ P.Scope.Developer_check ])
                   ~attributes:[])
            in
            let actor = Operator_authorization.trusted_local principal in
            let key_a = must (P.Idempotency_key.of_string "watch-a") in
            let key_b = must (P.Idempotency_key.of_string "watch-b") in
            let run_id = must (P.Id.Developer_run.of_string "dvr_watch_case") in
            let entered_a, entered_a_u = Eio.Promise.create () in
            let release_a, release_a_u = Eio.Promise.create () in
            let entered_b, entered_b_u = Eio.Promise.create () in
            let release_b, release_b_u = Eio.Promise.create () in
            let hold_b = ref false in
            let notifications = ref [] in
            let connection =
              Connection_context.create_authenticated
                ~actor
                ~connection_id:"watch-connection"
                ~transport:In_memory
                ~publish_notification:(fun envelope ->
                  match envelope with
                  | P.Envelope.Notification { params; _ } ->
                    let change = must (Change.of_json ~limits:wire_limits params) in
                    if P.Idempotency_key.equal change.key key_a
                    then (
                      if not (Eio.Promise.is_resolved entered_a)
                      then Eio.Promise.resolve entered_a_u ();
                      Eio.Cancel.protect (fun () -> Eio.Promise.await release_a));
                    if P.Idempotency_key.equal change.key key_b && !hold_b
                    then (
                      if not (Eio.Promise.is_resolved entered_b)
                      then Eio.Promise.resolve entered_b_u ();
                      Eio.Cancel.protect (fun () -> Eio.Promise.await release_b));
                    notifications := change :: !notifications
                  | Request _ | Response _ -> failwith "expected change notification")
                ~max_attachments:1
            in
            Connection_context.mark_initialized
              ~enabled_features:[ "developer.events" ]
              connection;
            let watch key =
              Hub.watch
                hub
                ~connection
                ~actor
                ~run_id
                ~key
                ~authorize_key:(fun () -> Ok ())
                ~authorize_change:(fun _ -> Ok ())
              |> must
            in
            watch key_a;
            watch key_b;
            Hub.enqueue hub ~run_id ~key:key_a ~kind:Change.Command_changed;
            Eio.Promise.await entered_a;
            let first_done, first_done_u = Eio.Promise.create () in
            Eio.Fiber.fork ~sw (fun () ->
              Eio.Promise.resolve
                first_done_u
                (Hub.unwatch hub ~connection ~run_id ~key:key_a));
            let second_done, second_done_u = Eio.Promise.create () in
            Eio.Fiber.fork ~sw (fun () ->
              Eio.Promise.resolve
                second_done_u
                (Hub.unwatch hub ~connection ~run_id ~key:key_a));
            Hub.enqueue hub ~run_id ~key:key_b ~kind:Change.Approval_changed;
            assert (not (Eio.Promise.is_resolved first_done));
            assert (not (Eio.Promise.is_resolved second_done));
            Eio.Promise.resolve release_a_u ();
            must (Eio.Promise.await first_done);
            must (Eio.Promise.await second_done);
            let rec await_b () =
              if
                List.exists !notifications ~f:(fun change ->
                  P.Idempotency_key.equal change.key key_b
                  &&
                  match change.kind with
                  | Change.Resync_required -> true
                  | Command_changed | Approval_changed -> false)
              then ()
              else (
                Eio.Fiber.yield ();
                await_b ())
            in
            await_b ();
            hold_b := true;
            Hub.enqueue hub ~run_id ~key:key_b ~kind:Change.Command_changed;
            Eio.Promise.await entered_b;
            let retired_b, retired_b_u = Eio.Promise.create () in
            Eio.Fiber.fork ~sw (fun () ->
              Eio.Promise.resolve
                retired_b_u
                (Hub.unwatch hub ~connection ~run_id ~key:key_b));
            Connection_context.mark_initialized
              ~enabled_features:[ "developer.events" ]
              connection;
            let rewatches, rewatches_u = Eio.Promise.create () in
            Eio.Fiber.fork ~sw (fun () -> Eio.Promise.resolve rewatches_u (watch key_a));
            assert (not (Eio.Promise.is_resolved retired_b));
            assert (not (Eio.Promise.is_resolved rewatches));
            Eio.Promise.resolve release_b_u ();
            must (Eio.Promise.await retired_b);
            Eio.Promise.await rewatches;
            assert (not (Connection_context.authoring_closed connection));
            let old_a_count =
              List.count !notifications ~f:(fun change ->
                P.Idempotency_key.equal change.key key_a)
            in
            Hub.enqueue hub ~run_id ~key:key_a ~kind:Change.Command_changed;
            let rec await_new_a () =
              if
                List.count !notifications ~f:(fun change ->
                  P.Idempotency_key.equal change.key key_a)
                > old_a_count
              then ()
              else (
                Eio.Fiber.yield ();
                await_new_a ())
            in
            await_new_a ();
            must (Hub.close_connection_and_wait hub connection);
            assert (Connection_context.authoring_closed connection);
            must (Hub.close_and_wait hub))))
    ;;

    let%test_unit "failed publisher is connection-local and reported once" =
      Eio_main.run (fun env ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 8. (fun () ->
          Eio.Switch.run (fun sw ->
            let wire_limits = wire_limits () in
            let limits =
              must
                (Hub.Limits.create
                   ~max_connections:1
                   ~max_watches_per_connection:1
                   ~max_pending_hints_per_connection:1)
            in
            let hub = must (Hub.create ~sw ~limits ~wire_limits ~repl_wire_limits:None) in
            let principal =
              must
                (P.Principal.create
                   ~id:(must (P.Id.Principal.of_string "pri_failed_watcher"))
                   ~authentication_kind:"test"
                   ~scopes:(P.Scope.Set.of_list [ P.Scope.Developer_check ])
                   ~attributes:[])
            in
            let actor = Operator_authorization.trusted_local principal in
            let run_id = must (P.Id.Developer_run.of_string "dvr_failed_watcher") in
            let key = must (P.Idempotency_key.of_string "failed-watch") in
            let publisher_failure = Failure "publisher-a failed" in
            let entered, entered_u = Eio.Promise.create () in
            let release, release_u = Eio.Promise.create () in
            let make_connection ~connection_id ~publish_notification =
              let connection =
                Connection_context.create_authenticated
                  ~actor
                  ~connection_id
                  ~transport:In_memory
                  ~publish_notification
                  ~max_attachments:1
              in
              Connection_context.mark_initialized
                ~enabled_features:[ "developer.events" ]
                connection;
              connection
            in
            let a =
              make_connection ~connection_id:"publisher-a" ~publish_notification:(fun _ ->
                Eio.Promise.resolve entered_u ();
                Eio.Cancel.protect (fun () -> Eio.Promise.await release);
                raise publisher_failure)
            in
            let watch connection =
              Hub.watch
                hub
                ~connection
                ~actor
                ~run_id
                ~key
                ~authorize_key:(fun () -> Ok ())
                ~authorize_change:(fun _ -> Ok ())
              |> must
            in
            watch a;
            Hub.enqueue hub ~run_id ~key ~kind:Change.Command_changed;
            Eio.Promise.await entered;
            let close () =
              try
                match Hub.close_connection_and_wait hub a with
                | Ok () -> `Ok
                | Error _ -> failwith "unexpected typed close failure"
              with
              | exn -> `Exn exn
            in
            let first, first_u = Eio.Promise.create () in
            let second, second_u = Eio.Promise.create () in
            Eio.Fiber.fork ~sw (fun () -> Eio.Promise.resolve first_u (close ()));
            Eio.Fiber.fork ~sw (fun () -> Eio.Promise.resolve second_u (close ()));
            assert (not (Eio.Promise.is_resolved first));
            assert (not (Eio.Promise.is_resolved second));
            Eio.Promise.resolve release_u ();
            let first = Eio.Promise.await first in
            let second = Eio.Promise.await second in
            let observed_original = function
              | `Ok -> false
              | `Exn exn ->
                assert (phys_equal exn publisher_failure);
                true
            in
            assert (Int.equal (List.count [ first; second ] ~f:observed_original) 1);
            let b_notifications = ref 0 in
            let b =
              make_connection ~connection_id:"publisher-b" ~publish_notification:(fun _ ->
                Int.incr b_notifications)
            in
            watch b;
            Hub.enqueue hub ~run_id ~key ~kind:Change.Command_changed;
            let rec await_b () =
              if Int.equal !b_notifications 0
              then (
                Eio.Fiber.yield ();
                await_b ())
            in
            await_b ();
            must (Hub.close_connection_and_wait hub a);
            must (Hub.close_connection_and_wait hub b);
            must (Hub.close_and_wait hub))))
    ;;

    let%expect_test "Watch refresh converges and preserves post-barrier invalidation" =
      Eio_main.run (fun env ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 8. (fun () ->
          Eio.Switch.run (fun sw ->
            let wire_limits = wire_limits () in
            let limits =
              must
                (Hub.Limits.create
                   ~max_connections:1
                   ~max_watches_per_connection:2
                   ~max_pending_hints_per_connection:1)
            in
            let hub = must (Hub.create ~sw ~limits ~wire_limits ~repl_wire_limits:None) in
            let principal =
              must
                (P.Principal.create
                   ~id:(must (P.Id.Principal.of_string "pri_refresh_watcher"))
                   ~authentication_kind:"test"
                   ~scopes:(P.Scope.Set.of_list [ P.Scope.Developer_check ])
                   ~attributes:[])
            in
            let actor = Operator_authorization.trusted_local principal in
            let run_id = must (P.Id.Developer_run.of_string "dvr_refresh_case") in
            let key = must (P.Idempotency_key.of_string "refresh-key") in
            let marker = must (P.Idempotency_key.of_string "refresh-marker") in
            let old_entered, signal_old_entered = Eio.Promise.create () in
            let old_release, release_old = Eio.Promise.create () in
            let publishing, signal_publishing = Eio.Promise.create () in
            let publication_release, release_publication = Eio.Promise.create () in
            let hold_publication = ref false in
            let changes = ref [] in
            let connection =
              Connection_context.create_authenticated
                ~actor
                ~connection_id:"refresh-connection"
                ~transport:In_memory
                ~publish_notification:(function
                  | P.Envelope.Notification { params; _ } ->
                    let change = must (Change.of_json ~limits:wire_limits params) in
                    if !hold_publication && P.Idempotency_key.equal change.key key
                    then (
                      Eio.Promise.resolve signal_publishing ();
                      Eio.Promise.await publication_release);
                    changes := change :: !changes
                  | Request _ | Response _ -> failwith "expected change")
                ~max_attachments:1
            in
            Connection_context.mark_initialized
              ~enabled_features:[ "developer.events" ]
              connection;
            let watch key ~authorize_change =
              must
                (Hub.watch
                   hub
                   ~connection
                   ~actor
                   ~run_id
                   ~key
                   ~authorize_key:(fun () -> Ok ())
                   ~authorize_change)
            in
            let enqueue key = Hub.enqueue hub ~run_id ~key ~kind:Change.Command_changed in
            let count key =
              List.count !changes ~f:(fun change ->
                P.Idempotency_key.equal change.key key)
            in
            let await_count key expected =
              while count key < expected do
                Eio.Fiber.yield ()
              done
            in
            watch marker ~authorize_change:(fun _ -> Ok ());
            watch key ~authorize_change:(fun _ ->
              Eio.Promise.resolve signal_old_entered ();
              Eio.Promise.await old_release;
              Ok ());
            enqueue key;
            Eio.Promise.await old_entered;
            (* Both a queued hint and an overflow belong to the old baseline. *)
            enqueue key;
            enqueue key;
            let allowed = ref true in
            let refreshed_authorizations = ref 0 in
            let refresh () =
              watch key ~authorize_change:(fun _ ->
                Int.incr refreshed_authorizations;
                if !allowed
                then Ok ()
                else
                  Error
                    (P.Error.create
                       Permission_denied
                       ~message:"revoked"
                       ~retryable:false
                       ()))
            in
            refresh ();
            (* Post-ack overflow must survive the old authorization continuation. *)
            enqueue key;
            enqueue key;
            Eio.Promise.resolve release_old ();
            await_count key 1;
            assert (Int.equal !refreshed_authorizations 1);
            assert (
              List.for_all !changes ~f:(fun change ->
                match change.kind with
                | Change.Resync_required -> true
                | Command_changed | Approval_changed -> false));
            (* Repeated registration cannot create a second recovery hint. A
               marker traverses the same pump before checking quiescence. *)
            for _ = 1 to 3 do
              refresh ()
            done;
            enqueue marker;
            await_count marker 1;
            assert (Int.equal (count key) 1);
            (* A refreshed callback still enforces current disclosure. Restore
               the grant and explicitly register again after revocation. *)
            allowed := false;
            enqueue key;
            while !refreshed_authorizations < 2 do
              Eio.Fiber.yield ()
            done;
            assert (Int.equal (count key) 1);
            allowed := true;
            refresh ();
            enqueue key;
            await_count key 2;
            assert (Int.equal !refreshed_authorizations 3);
            (* Bytes already handed to a yielding publisher may finish after
               refresh, but its completion cannot erase a newer dirty slot. *)
            hold_publication := true;
            enqueue key;
            enqueue key;
            Eio.Promise.await publishing;
            refresh ();
            enqueue key;
            enqueue key;
            hold_publication := false;
            Eio.Promise.resolve release_publication ();
            await_count key 4;
            assert (Int.equal !refreshed_authorizations 5);
            must (Hub.close_connection_and_wait hub connection);
            must (Hub.close_and_wait hub))));
      print_endline
        "stale generation suppressed; post-refresh resync retained; repeat Watch idle; \
         authorization refreshed";
      [%expect
        {| stale generation suppressed; post-refresh resync retained; repeat Watch idle; authorization refreshed |}]
    ;;

    let%test_unit "full hint queue retains a key-scoped resync without restart" =
      Eio_main.run (fun env ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 8. (fun () ->
          Eio.Switch.run (fun sw ->
            let wire_limits = wire_limits () in
            let limits =
              must
                (Hub.Limits.create
                   ~max_connections:1
                   ~max_watches_per_connection:2
                   ~max_pending_hints_per_connection:1)
            in
            let hub = must (Hub.create ~sw ~limits ~wire_limits ~repl_wire_limits:None) in
            let principal =
              must
                (P.Principal.create
                   ~id:(must (P.Id.Principal.of_string "pri_overflow_watcher"))
                   ~authentication_kind:"test"
                   ~scopes:(P.Scope.Set.of_list [ P.Scope.Developer_check ])
                   ~attributes:[])
            in
            let actor = Operator_authorization.trusted_local principal in
            let run_id = must (P.Id.Developer_run.of_string "dvr_overflow_case") in
            let key_a = must (P.Idempotency_key.of_string "overflow-a") in
            let key_b = must (P.Idempotency_key.of_string "overflow-b") in
            let entered, entered_u = Eio.Promise.create () in
            let release, release_u = Eio.Promise.create () in
            let changes = ref [] in
            let connection =
              Connection_context.create_authenticated
                ~actor
                ~connection_id:"overflow-connection"
                ~transport:In_memory
                ~publish_notification:(function
                  | P.Envelope.Notification { params; _ } ->
                    let change = must (Change.of_json ~limits:wire_limits params) in
                    if P.Idempotency_key.equal change.key key_a
                    then (
                      Eio.Promise.resolve entered_u ();
                      Eio.Cancel.protect (fun () -> Eio.Promise.await release));
                    changes := change :: !changes
                  | Request _ | Response _ -> failwith "expected change")
                ~max_attachments:1
            in
            Connection_context.mark_initialized
              ~enabled_features:[ "developer.events" ]
              connection;
            List.iter [ key_a; key_b ] ~f:(fun key ->
              must
                (Hub.watch
                   hub
                   ~connection
                   ~actor
                   ~run_id
                   ~key
                   ~authorize_key:(fun () -> Ok ())
                   ~authorize_change:(fun _ -> Ok ())));
            Hub.enqueue hub ~run_id ~key:key_a ~kind:Change.Command_changed;
            Eio.Promise.await entered;
            Hub.enqueue hub ~run_id ~key:key_b ~kind:Change.Command_changed;
            Hub.enqueue hub ~run_id ~key:key_b ~kind:Change.Approval_changed;
            Eio.Promise.resolve release_u ();
            let rec await_resync () =
              if
                not
                  (List.exists !changes ~f:(fun change ->
                     P.Idempotency_key.equal change.key key_b
                     &&
                     match change.kind with
                     | Change.Resync_required -> true
                     | Command_changed | Approval_changed -> false))
              then (
                Eio.Fiber.yield ();
                await_resync ())
            in
            await_resync ();
            must (Hub.close_connection_and_wait hub connection);
            must (Hub.close_and_wait hub))))
    ;;
  end)
;;
