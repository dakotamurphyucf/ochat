open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module S = Agent_server

let activity_query server_id ?cursor ~archive () =
  P.Activity_query.create
    ~server_id
    ~catalog:(Catalog_service_tests.query ?cursor ~archive ())
    ~reasons:[]
    ~scan_limit:8
  |> protocol_ok
;;

let response_code = function
  | Ok _ -> "ok"
  | Error (error : P.Error.t) -> P.Error.code_to_string error.code
;;

let%expect_test
    "activity RPC observes retained work without activation and retries original \
     cancellation"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt_file = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Activity fixture.</developer>";
        let config = config root workspace prompt_file in
        let retained_session, retained_job, server_id, original_cancel, original_revision =
          Eio.Switch.run (fun sw ->
            let daemon = Catalog_service_tests.start_catalog_daemon sw env ~root config in
            let client = connection daemon (principal ()) in
            initialize client;
            let server_id = Agent_store.Session_store.server_id (S.Daemon.store daemon) in
            let views = C.Activity_views.create client ~server_id in
            let session, attachment = create_session client in
            let entry =
              S.Session_registry.find (S.Daemon.registry daemon) session.id
              |> Option.value_exn
            in
            let job =
              P.Job.
                { id = P.Id.Job.create ()
                ; session_id = session.id
                ; generation = session.generation
                ; kind = Model_call
                ; payload = `Object []
                ; status = Queued
                ; retry_policy = Never
                ; attempt = 2
                ; created_at = session.created_at
                ; started_at = None
                ; next_run_at = None
                ; completed_at = None
                ; result = None
                ; delivery = Not_required
                ; launch = None
                ; progress = None
                }
            in
            Agent_session.Session_actor.add_job entry.actor job |> protocol_ok |> ignore;
            let request =
              P.Session_work.Query.
                { session = P.Session_ref.create ~server_id ~session_id:session.id
                ; page = P.Page.Request.create ~limit:8 () |> protocol_ok
                }
            in
            let before = S.Session_registry.stats (S.Daemon.registry daemon) in
            let row =
              C.Activity_views.work_page views request
              |> protocol_ok
              |> fun page -> List.hd_exn page.P.Page.items
            in
            let observed =
              C.Activity_views.list_page
                views
                (activity_query server_id ~archive:Active ())
              |> protocol_ok
            in
            let immutable =
              Agent_session.Session_actor.snapshot entry.actor |> protocol_ok
            in
            let operation =
              P.Operation.
                { id = P.Id.Operation.create ()
                ; generation = immutable.session.generation
                ; kind = Compaction
                ; state =
                    Interrupted { reason = "private interruption"; retryable = false }
                ; started_at = immutable.session.created_at
                ; updated_at = immutable.session.updated_at
                }
            in
            let failed_session =
              { immutable.session with
                observed_state = Failed (P.Error.invalid_request "private failure")
              ; active_operation = Some operation
              }
            in
            let failed_snapshot =
              { immutable with session = failed_session; failure = None }
            in
            let failed_catalog =
              P.Session_catalog.
                { session = failed_session
                ; active_owner_principal_id = None
                ; archived = false
                ; effective_organization = P.Session_organization.Values.empty
                }
            in
            let projection =
              S.Activity_projection.create
                ~server_id
                ~principal:(principal ())
                ~now:failed_session.updated_at
            in
            let project snapshot =
              S.Activity_projection.observe
                projection
                snapshot
                ~catalog:failed_catalog
                ~transient:Unavailable
                ~usage:(List.hd_exn observed.P.Page.items).usage
              |> protocol_ok
            in
            let failure_rows row =
              List.count row.P.Session_activity.attention ~f:(fun attention ->
                P.Session_activity.Reason.equal attention.reason Failure)
            in
            let independent = project failed_snapshot in
            let overlapping =
              project
                { failed_snapshot with
                  failure = Some (P.Error.invalid_request "second private failure")
                }
            in
            print_s
              [%sexp
                { independent_failure_entities = (failure_rows independent : int)
                ; overlapping_failure_entities = (failure_rows overlapping : int)
                }];
            let after = S.Session_registry.stats (S.Daemon.registry daemon) in
            let stale =
              P.Command.Job_cancel
                { session_id = session.id
                ; attachment_id = attachment.id
                ; job_id = job.id
                ; expected_generation = Some session.generation
                ; expected_attempt = Some 1
                ; idempotency_key =
                    P.Idempotency_key.of_string "activity-stale" |> protocol_ok
                }
            in
            let stale_result = C.Connection.request_without_history client stale in
            let key = P.Idempotency_key.of_string "activity-cancel" |> protocol_ok in
            let cancelled =
              C.Activity_views.cancel_job
                views
                row
                ~attachment_id:attachment.id
                ~idempotency_key:key
              |> protocol_ok
            in
            Agent_session.Session_actor.change_job
              entry.actor
              ~attachment_id:attachment.id
              { job with attempt = 3; status = Queued }
            |> protocol_ok
            |> ignore;
            let retried =
              C.Activity_views.cancel_job
                views
                row
                ~attachment_id:attachment.id
                ~idempotency_key:key
              |> protocol_ok
            in
            let current_job =
              Agent_session.Session_actor.read_job entry.actor ~job_id:job.id
              |> protocol_ok
            in
            let newer_attempt_unchanged =
              Int.equal current_job.attempt 3
              &&
              match current_job.status with
              | Queued -> true
              | Running
              | Waiting_permission _
              | Waiting_completion _
              | Succeeded
              | Failed _
              | Cancelled
              | Interrupted _ -> false
            in
            let original_cancel =
              P.Command.Job_cancel
                { session_id = session.id
                ; attachment_id = attachment.id
                ; job_id = job.id
                ; expected_generation = Some row.generation
                ; expected_attempt = Some job.attempt
                ; idempotency_key = key
                }
            in
            let reduced =
              connection
                daemon
                (principal_with_scopes
                   "pri_restart_test"
                   (P.Scope.Set.of_list [ View_session_transcript ]))
            in
            initialize reduced;
            let denied =
              C.Connection.request_without_history reduced (Session_work request)
            in
            let denied_retry =
              C.Connection.request_without_history reduced original_cancel
            in
            C.Connection.close reduced;
            print_s
              [%sexp
                { no_activation = (Int.equal before.loaded after.loaded : bool)
                ; work_count = ((List.hd_exn observed.items).work_count : int)
                ; stale = (response_code stale_result : string)
                ; retry_original_revision =
                    (Int64.equal cancelled.mutation.revision retried.mutation.revision
                     : bool)
                ; newer_attempt_unchanged : bool
                ; denied = (response_code denied : string)
                ; denied_retry = (response_code denied_retry : string)
                }];
            let current =
              Agent_session.Session_actor.snapshot entry.actor |> protocol_ok
            in
            C.Connection.request_without_history
              client
              (Session_delete
                 { session_id = session.id
                 ; attachment_id = attachment.id
                 ; expected_revision = current.revision
                 ; policy = Archive
                 ; confirmation = P.Id.Session.to_string session.id
                 ; idempotency_key =
                     P.Idempotency_key.of_string "activity-archive" |> protocol_ok
                 })
            |> protocol_ok
            |> ignore;
            C.Connection.close client;
            S.Daemon.shutdown daemon |> protocol_ok;
            session.id, job.id, server_id, original_cancel, cancelled.mutation.revision)
        in
        Eio.Switch.run (fun sw ->
          let daemon = Catalog_service_tests.start_catalog_daemon sw env ~root config in
          let client = connection daemon (principal ()) in
          initialize client;
          let views = C.Activity_views.create client ~server_id in
          let before = S.Session_registry.stats (S.Daemon.registry daemon) in
          let page =
            C.Activity_views.list_page
              views
              (activity_query server_id ~archive:Archived ())
            |> protocol_ok
          in
          let row = List.hd_exn page.P.Page.items in
          let replayed =
            match
              C.Connection.request_without_history client original_cancel |> protocol_ok
            with
            | Job_cancel result -> Int64.equal result.mutation.revision original_revision
            | _ -> false
          in
          let after = S.Session_registry.stats (S.Daemon.registry daemon) in
          let work =
            C.Activity_views.work_page
              views
              P.Session_work.Query.
                { session = P.Session_ref.create ~server_id ~session_id:retained_session
                ; page = P.Page.Request.create ~limit:8 () |> protocol_ok
                }
            |> protocol_ok
          in
          let retained =
            List.exists work.items ~f:(fun (item : P.Session_work.t) ->
              P.Session_work.Key.equal item.key (Job { id = retained_job; attempt = 3 })
              &&
              match item.status with
              | Accepted -> true
              | Cancelled
              | Running
              | Waiting_approval
              | Waiting_work
              | Succeeded
              | Failed
              | Interrupted
              | Unsupported -> false)
          in
          print_s
            [%sexp
              { archived = (row.summary.archived : bool)
              ; archived_cached_retry_original = (replayed : bool)
              ; transient_unavailable =
                  ((match row.transient with
                    | Unavailable -> true
                    | Live _ -> false)
                   : bool)
              ; restart_retains_new_attempt = (retained : bool)
              ; no_activation = (Int.equal before.loaded after.loaded : bool)
              }];
          C.Connection.close client;
          S.Daemon.shutdown daemon |> protocol_ok)));
  [%expect
    {|
    ((independent_failure_entities 2) (overlapping_failure_entities 2))
    ((no_activation true) (work_count 1) (stale conflict)
     (retry_original_revision true) (newer_attempt_unchanged true)
     (denied permission_denied) (denied_retry permission_denied))
    ((archived true) (archived_cached_retry_original true)
     (transient_unavailable true) (restart_retains_new_attempt true)
     (no_activation true))
    |}]
;;
