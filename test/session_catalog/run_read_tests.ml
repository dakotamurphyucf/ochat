open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module S = Agent_server
module A = Agent_session

let permission_denied = function
  | Error { P.Error.code = Permission_denied; _ } -> true
  | Error _ | Ok _ -> false
;;

module Fixture = struct
  type t =
    { daemon : S.Daemon.t
    ; client : C.Connection.t
    ; entry : S.Session_registry.entry
    ; authorized : bool ref
    ; clients : C.Connection.t list ref
    ; session : P.Session.t
    ; reference : P.Session_ref.t
    ; admitted : P.Run_receipt.t
    ; views : C.Run_views.t
    ; request : P.Run_query.Lookup_request.t
    }
end

let with_admitted_workflow ~f =
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
          {|<developer>Run read fixture.</developer>
<script id="read_run" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event = fun ctx state event -> match event with
| `Session_start -> Task.bind(Run.finish(`Object([
  { key = "kind"; value = `String("finish") },
  { key = "terminal"; value = `Object([{ key = "kind"; value = `String("completed") }]) },
  { key = "relinquish"; value = `Array([]) }])), fun ignored -> Task.pure(state))
| _ -> Task.pure(state)
</script>|};
        Eio.Switch.run (fun sw ->
          let daemon =
            Catalog_service_tests.start_catalog_daemon
              sw
              env
              ~root
              (config root workspace prompt_file)
          in
          let clients = ref [] in
          Exn.protect
            ~finally:(fun () ->
              List.iter !clients ~f:C.Connection.close;
              S.Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              let authorized = ref true in
              let actor =
                Operator_authorization.guarded
                  ~principal:(principal ())
                  ~is_current:(fun () -> !authorized)
              in
              let client = Run_service_tests.authenticated_connection daemon ~actor in
              clients := client :: !clients;
              initialize client;
              let session, attachment = create_session client in
              let entry =
                S.Session_registry.find (S.Daemon.registry daemon) session.id
                |> Option.value_exn
              in
              let before = A.Session_actor.state entry.actor |> protocol_ok in
              let admitted =
                P.Run_start.create
                  ~session_id:session.id
                  ~attachment_id:attachment.id
                  ~generation:before.identity.generation
                  ~expected_revision:before.counters.revision
                  ~mode:Workflow
                  ~input:Authored_start
                  ~key:(P.Idempotency_key.of_string "read-run-start" |> protocol_ok)
                |> protocol_ok
                |> fun request ->
                C.Connection.request_without_history client (Session_run_start request)
                |> protocol_ok
                |> function
                | P.Method_result.Session_run_start receipt -> receipt
                | _ -> failwith "run admission"
              in
              let server_id =
                Agent_store.Session_store.server_id (S.Daemon.store daemon)
              in
              let reference = P.Session_ref.create ~server_id ~session_id:session.id in
              let views = C.Run_views.create client ~server_id in
              let request =
                P.Run_query.Lookup_request.
                  { session = reference; run_id = admitted.run_id }
              in
              f
                Fixture.
                  { daemon
                  ; client
                  ; entry
                  ; authorized
                  ; clients
                  ; session
                  ; reference
                  ; admitted
                  ; views
                  ; request
                  }))))
;;

let%expect_test
    "run read and scoped watch preserve host authority and same-revision recovery"
  =
  with_admitted_workflow
    ~f:
      (fun
        { Fixture.daemon
        ; client
        ; entry
        ; authorized
        ; clients
        ; session
        ; reference
        ; admitted
        ; views
        ; request
        }
      ->
      let snapshot =
        C.Admin.get_session client session.id
        |> protocol_ok
        |> C.Projection.install_snapshot
      in
      let callbacks = ref 0 in
      let errors = ref 0 in
      let callback = ref (fun (_ : C.Projection.t) -> ()) in
      let first, first_resolver = Eio.Promise.create () in
      let denied, denied_resolver = Eio.Promise.create () in
      let stale, stale_resolver = Eio.Promise.create () in
      let restored, restored_resolver = Eio.Promise.create () in
      Eio.Switch.run (fun watch_scope ->
        callback
        := C.Run_views.watch
             views
             ~sw:watch_scope
             ~request
             ~on_result:(fun outcome ->
               incr callbacks;
               match outcome with
               | P.Run_query.Outcome.Available view ->
                 if Int.equal !callbacks 1
                 then Eio.Promise.resolve first_resolver view
                 else if Int.equal !callbacks 2
                 then Eio.Promise.resolve restored_resolver view
                 else
                   failwith "unexpected successful run refresh after authority revocation"
               | Unavailable _ -> failwith "admitted run unavailable")
             ~on_error:(fun error ->
               incr errors;
               match error.P.Error.code with
               | Invalid_request -> Eio.Promise.resolve stale_resolver error
               | Permission_denied -> Eio.Promise.resolve denied_resolver error
               | _ -> failwith "unexpected watch error");
        !callback snapshot;
        let view = Eio.Promise.await first in
        assert (Option.exists view.admission_receipt ~f:(P.Run_receipt.equal admitted));
        !callback
          (C.Projection.mark_stale snapshot (P.Error.invalid_request "snapshot gap"));
        Eio.Promise.await stale |> ignore;
        !callback snapshot;
        let same_revision = Eio.Promise.await restored in
        assert (P.Run.equal same_revision.run view.run);
        S.Runtime_owner.with_background_runtime entry.runtime (fun runtime ->
          match runtime.moderator_activation with
          | Some activation -> activation.run ()
          | None -> Error (P.Error.invalid_request "missing startup capability"))
        |> protocol_ok
        |> ignore;
        let terminal_snapshot =
          C.Admin.get_session client session.id
          |> protocol_ok
          |> C.Projection.install_snapshot
        in
        authorized := false;
        !callback terminal_snapshot;
        let error = Eio.Promise.await denied in
        assert (
          match error.code with
          | Permission_denied -> true
          | _ -> false));
      authorized := true;
      let terminal = C.Run_views.lookup views request |> protocol_ok in
      let terminal_evidence =
        match terminal with
        | Available view ->
          Option.is_some view.terminal_receipt
          && P.Run.Lifecycle.equal view.run.lifecycle (Terminal (Completed None))
        | Unavailable _ -> false
      in
      !callback
        (C.Projection.mark_stale snapshot (P.Error.invalid_request "closed observer"));
      Eio.Fiber.yield ();
      let limited =
        principal_with_scopes
          "pri_restart_test"
          (P.Scope.Set.of_list [ View_session_transcript ])
      in
      let limited_client =
        Run_service_tests.authenticated_connection
          daemon
          ~actor:(Operator_authorization.trusted_local limited)
      in
      clients := limited_client :: !clients;
      initialize limited_client;
      let denied_without_security =
        C.Connection.request_without_history limited_client (Session_run request)
        |> permission_denied
      in
      let list =
        P.Run_query.Request.create
          ~session:reference
          ~page:(P.Page.Request.create ~limit:8 () |> protocol_ok)
        |> protocol_ok
        |> C.Run_views.page views
        |> protocol_ok
      in
      print_s
        [%sexp
          { terminal_evidence : bool
          ; listed_once = (Int.equal (List.length list.items) 1 : bool)
          ; denied_without_security : bool
          ; refreshes = (!callbacks : int)
          ; errors = (!errors : int)
          }]);
  [%expect
    {|
    ((terminal_evidence true) (listed_once true) (denied_without_security true)
     (refreshes 2) (errors 2))
    |}]
;;

let%expect_test
    "retained failed result metadata is independent of a running retry and excludes \
     payload"
  =
  with_admitted_workflow
    ~f:(fun { Fixture.session; reference; entry; views; request; _ } ->
      let terminal_run =
        match C.Run_views.lookup views request |> protocol_ok with
        | Available view -> view.run
        | Unavailable _ -> failwith "terminal run"
      in
      let job_id = P.Id.Job.create () in
      let first_work =
        P.Run_work.create
          ~key:(Retained (Job { id = job_id; attempt = 1 }))
          ~generation:terminal_run.source.generation
        |> protocol_ok
      in
      let retry_work =
        P.Run_work.create
          ~key:(Retained (Job { id = job_id; attempt = 2 }))
          ~generation:terminal_run.source.generation
        |> protocol_ok
      in
      let failed_proof =
        P.Run_work.Terminal.create ~work:first_work ~outcome:Failed ~revision:1L
        |> protocol_ok
      in
      let retry_run =
        P.Run.create
          ~id:(P.Id.Run.create ())
          ~session:reference
          ~principal_id:terminal_run.principal_id
          ~source:terminal_run.source
          ~mode:Workflow
          ~lifecycle:Active
          ~revision:1L
          ~owned_work:[ first_work; retry_work ]
          ~relinquished_work:[]
          ~terminal_work:[ failed_proof ]
          ~created_at:terminal_run.created_at
          ~updated_at:terminal_run.updated_at
        |> protocol_ok
      in
      let completion =
        P.Completion.Failed
          { code = "fixture.failure"
          ; message = "private failed payload"
          ; retryable = true
          ; details = `Null
          }
      in
      let content = P.Completion.to_json completion |> Jsonaf.to_string in
      let blob =
        P.Blob.Metadata.create
          ~id:(P.Id.Blob.create ())
          ~kind:File
          ~media_type:P.Job_artifact.media_type
          ~byte_length:(Int64.of_int (String.length content))
          ~digest:Digestif.SHA256.(digest_string content |> to_hex)
          ()
        |> protocol_ok
      in
      let artifact =
        P.Job_artifact.create
          ~session_id:session.id
          ~job_id
          ~generation:retry_run.source.generation
          ~attempt:1
          ~blob
        |> protocol_ok
      in
      let stored = P.Stored_completion.artifact artifact completion |> protocol_ok in
      let result =
        P.Job_result_reference.of_completion
          stored
          ~session_id:session.id
          ~job_id
          ~generation:retry_run.source.generation
          ~attempt:1
        |> protocol_ok
        |> P.Run_result_reference.of_job_result
        |> protocol_ok
      in
      let retry_view =
        P.Run_query.View.create
          ~run:retry_run
          ~result_references:[ result ]
          ~pending_action:None
          ~admission_receipt:None
          ~terminal_receipt:None
          ~session_revision:0L
          ~event_sequence:0L
        |> protocol_ok
      in
      let retry_json = P.Run_query.View.to_json retry_view in
      let retry_roundtrip = P.Run_query.View.of_json retry_json |> protocol_ok in
      let failed_reference =
        match retry_roundtrip.result_references with
        | [ Job reference ] ->
          P.Stored_completion.equal_outcome reference.outcome Failed
          && Int.equal reference.attempt 1
          && Option.is_some reference.artifact
        | [] | Operation _ :: _ | Job _ :: _ -> false
      in
      let payload_free =
        not
          (String.is_substring
             (Jsonaf.to_string retry_json)
             ~substring:"private failed payload")
      in
      let counterfeit =
        P.Job_result_reference.of_completion
          (Artifact { outcome = Succeeded; reference = artifact })
          ~session_id:session.id
          ~job_id
          ~generation:retry_run.source.generation
          ~attempt:1
        |> protocol_ok
        |> P.Run_result_reference.of_job_result
        |> protocol_ok
      in
      let rejects_false_success =
        Result.is_error
          (P.Run_query.View.create
             ~run:retry_run
             ~result_references:[ counterfeit ]
             ~pending_action:None
             ~admission_receipt:None
             ~terminal_receipt:None
             ~session_revision:0L
             ~event_sequence:0L)
      in
      let wake =
        P.Run_wake.create
          ~run_id:retry_run.id
          ~source:retry_run.source
          ~occurrence:(Job_completion { job_id; attempt = 1 })
        |> protocol_ok
      in
      let waiting =
        P.Run.create
          ~id:retry_run.id
          ~session:retry_run.session
          ~principal_id:retry_run.principal_id
          ~source:retry_run.source
          ~mode:retry_run.mode
          ~lifecycle:(Waiting wake)
          ~revision:retry_run.revision
          ~owned_work:retry_run.owned_work
          ~relinquished_work:[]
          ~terminal_work:retry_run.terminal_work
          ~created_at:retry_run.created_at
          ~updated_at:retry_run.updated_at
        |> protocol_ok
      in
      let frame =
        Chat_response.Background_delivery.of_json
          (`Object
              [ "session_id", P.Id.Session.to_json session.id
              ; "job_id", P.Id.Job.to_json job_id
              ; "generation", `String (Int.to_string retry_run.source.generation)
              ; "attempt", `String "1"
              ; "script_id", `String retry_run.source.observer.script_id
              ; "source_sha256", `String retry_run.source.observer.source_sha256
              ; "completed_at", P.Timestamp.to_json retry_run.updated_at
              ; "result", P.Stored_completion.to_json stored
              ])
        |> Result.ok_or_failwith
      in
      let claimed =
        A.Run_job_delivery.capture waiting ~frame
        |> protocol_ok
        |> A.Run_job_delivery.enqueue ~at:retry_run.updated_at
        |> protocol_ok
        |> A.Run_job_delivery.claim ~execution_id:(P.Id.Moderator_execution.create ())
        |> protocol_ok
      in
      let actual_state = A.Session_actor.state entry.actor |> protocol_ok in
      let actual_index = Option.value_exn actual_state.run_state in
      let raw_index =
        match A.Run_state.to_jsonaf actual_index with
        | `Object fields ->
          `Object
            (List.Assoc.add
               fields
               ~equal:String.equal
               "runs"
               (`Array
                   (List.map
                      (retry_run :: A.Run_state.runs actual_index)
                      ~f:P.Run.to_json)))
        | _ -> failwith "run index object"
      in
      let index =
        A.Run_state.of_jsonaf raw_index
        |> protocol_ok
        |> fun index -> A.Run_state.add_job_delivery index claimed |> protocol_ok
      in
      let latest =
        P.Job.
          { id = job_id
          ; session_id = session.id
          ; generation = retry_run.source.generation
          ; kind = Async_tool
          ; payload = `Object []
          ; status = Running
          ; retry_policy = Safe_retry { max_attempts = 2; backoff_ms = 0 }
          ; attempt = 2
          ; created_at = retry_run.created_at
          ; started_at = Some retry_run.updated_at
          ; next_run_at = None
          ; completed_at = None
          ; result = None
          ; delivery = Not_required
          ; launch = None
          ; progress = None
          }
      in
      let reader_state =
        { actual_state with run_state = Some index; jobs = latest :: actual_state.jobs }
      in
      A.Session_state.validate reader_state |> protocol_ok;
      let service =
        S.Run_read_service.create
          (principal ())
          ~server_id:(P.Session_ref.server_id reference)
          ~read:(fun _ -> Ok reader_state)
          ~pagination:(S.Pagination.create ())
        |> protocol_ok
      in
      let extracted =
        S.Run_read_service.lookup service { request with run_id = retry_run.id }
        |> protocol_ok
      in
      let extracted_failed =
        match extracted with
        | Available view ->
          P.Run.Lifecycle.equal view.run.lifecycle Active
          && List.exists view.result_references ~f:(function
            | Job value ->
              P.Id.Job.equal value.job_id job_id
              && Int.equal value.attempt 1
              && P.Stored_completion.equal_outcome value.outcome Failed
            | Operation _ -> false)
          && not
               (String.is_substring
                  (P.Run_query.View.to_json view |> Jsonaf.to_string)
                  ~substring:"private failed payload")
        | Unavailable _ -> false
      in
      let rejects_pending_wait =
        Result.is_error
          (P.Run_query.View.create
             ~run:retry_run
             ~result_references:[ result ]
             ~pending_action:(Some (Wait wake))
             ~admission_receipt:None
             ~terminal_receipt:None
             ~session_revision:0L
             ~event_sequence:0L)
      in
      print_s
        [%sexp
          { extracted_failed : bool
          ; rejects_pending_wait : bool
          ; failed_reference : bool
          ; payload_free : bool
          ; rejects_false_success : bool
          }]);
  [%expect
    {|
    ((extracted_failed true) (rejects_pending_wait true) (failed_reference true)
     (payload_free true) (rejects_false_success true))
    |}]
;;

let%expect_test
    "run reader filters before paging and binds cursors to current principal and scopes"
  =
  with_admitted_workflow ~f:(fun { Fixture.entry; reference; admitted; request; _ } ->
    let server_id = P.Session_ref.server_id reference in
    (* These additional admitted records are immutable reader fixtures,
                 not executions. Exercise filtering before signed paging. *)
    let actual_state = A.Session_actor.state entry.actor |> protocol_ok in
    let actual_index = Option.value_exn actual_state.run_state in
    let original = A.Run_state.find actual_index admitted.run_id |> Option.value_exn in
    let add_admitted index principal_id key =
      let run =
        P.Run.create
          ~id:(P.Id.Run.create ())
          ~session:reference
          ~principal_id
          ~source:original.source
          ~mode:Workflow
          ~lifecycle:Admitted
          ~revision:0L
          ~owned_work:[]
          ~relinquished_work:[]
          ~terminal_work:[]
          ~created_at:original.updated_at
          ~updated_at:original.updated_at
        |> protocol_ok
      in
      let receipt =
        P.Run_receipt.create
          ~run_id:run.id
          ~principal_id
          ~source:run.source
          ~key:(P.Idempotency_key.of_string key |> protocol_ok)
          ~request_sha256:(String.make 64 'b')
          ~kind:Admission
          ~run_revision:0L
          ~session_revision:actual_state.counters.revision
          ~committed_at:run.created_at
        |> protocol_ok
      in
      A.Run_state.commit index ~run ~receipt ~intent:None |> protocol_ok, run
    in
    let with_own, _ = add_admitted actual_index (principal ()).id "reader-own" in
    let with_foreign, foreign =
      add_admitted with_own (principal_with_id "pri_foreign_reader").id "reader-foreign"
    in
    let reader_state = { actual_state with run_state = Some with_foreign } in
    A.Session_state.validate reader_state |> protocol_ok;
    let reader_principal =
      principal_with_scopes
        "pri_restart_test"
        (P.Scope.Set.of_list [ View_session_transcript; View_security_state ])
    in
    let pagination = S.Pagination.create () in
    let service principal =
      S.Run_read_service.create
        principal
        ~server_id
        ~read:(fun _ -> Ok reader_state)
        ~pagination
      |> protocol_ok
    in
    let first_request =
      P.Run_query.Request.create
        ~session:reference
        ~page:(P.Page.Request.create ~limit:1 () |> protocol_ok)
      |> protocol_ok
    in
    let first_page =
      S.Run_read_service.list (service reader_principal) first_request |> protocol_ok
    in
    let next_request =
      P.Run_query.Request.create
        ~session:reference
        ~page:
          (P.Page.Request.create ~limit:1 ?cursor:first_page.next_cursor () |> protocol_ok)
      |> protocol_ok
    in
    let next_page =
      S.Run_read_service.list (service reader_principal) next_request |> protocol_ok
    in
    let filtered =
      Int.equal (List.length first_page.items + List.length next_page.items) 2
      && Option.is_none next_page.next_cursor
    in
    let foreign_unavailable =
      match
        S.Run_read_service.lookup
          (service reader_principal)
          { request with run_id = foreign.id }
        |> protocol_ok
      with
      | Unavailable id -> P.Id.Run.equal id foreign.id
      | Available _ -> false
    in
    let cursor_scope_change =
      Result.is_error (S.Run_read_service.list (service (principal ())) next_request)
    in
    let cursor_principal_change =
      Result.is_error
        (S.Run_read_service.list
           (service (principal_with_id "pri_foreign_reader"))
           next_request)
    in
    print_s
      [%sexp
        { filtered : bool
        ; foreign_unavailable : bool
        ; cursor_scope_change : bool
        ; cursor_principal_change : bool
        }]);
  [%expect
    {|
    ((filtered true) (foreign_unavailable true) (cursor_scope_change true)
     (cursor_principal_change true))
    |}]
;;

let%expect_test
    "run view revisions retain exact decimal boundaries and reject ambiguous encodings"
  =
  let source =
    P.Run_source.create
      ~observer:{ script_id = "codec"; source_sha256 = String.make 64 'a' }
      ~generation:0
      ~installation_epoch:1L
    |> protocol_ok
  in
  let reference =
    P.Session_ref.create
      ~server_id:(P.Id.Server.of_string "srv_run_codec" |> protocol_ok)
      ~session_id:(P.Id.Session.of_string "ses_run_codec" |> protocol_ok)
  in
  let now = P.Timestamp.of_string "2026-10-09T00:00:00Z" |> protocol_ok in
  let run =
    P.Run.create
      ~id:(P.Id.Run.of_string "run_codec" |> protocol_ok)
      ~session:reference
      ~principal_id:(P.Id.Principal.of_string "pri_run_codec" |> protocol_ok)
      ~source
      ~mode:Workflow
      ~lifecycle:Admitted
      ~revision:0L
      ~owned_work:[]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:now
      ~updated_at:now
    |> protocol_ok
  in
  let view revision =
    P.Run_query.View.create
      ~run
      ~result_references:[]
      ~pending_action:None
      ~admission_receipt:None
      ~terminal_receipt:None
      ~session_revision:revision
      ~event_sequence:revision
    |> protocol_ok
  in
  let roundtrip revision =
    match
      view revision
      |> P.Run_query.Outcome.available
      |> P.Run_query.Outcome.to_json
      |> P.Run_query.Outcome.of_json
      |> protocol_ok
    with
    | Available value ->
      Int64.equal value.session_revision revision
      && Int64.equal value.event_sequence revision
    | Unavailable _ -> false
  in
  let zero = view 0L |> P.Run_query.View.to_json in
  let replace field data =
    match zero with
    | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal field data)
    | _ -> failwith "view object"
  in
  let rejected =
    List.for_all [ "session_revision"; "event_sequence" ] ~f:(fun field ->
      List.for_all
        [ `String "00"
        ; `String "+1"
        ; `String "01"
        ; `String "0x1"
        ; `String "-1"
        ; `String "9223372036854775808"
        ; `Number "1"
        ; `Null
        ]
        ~f:(fun value -> Result.is_error (P.Run_query.View.of_json (replace field value))))
  in
  let strings =
    match zero with
    | `Object fields ->
      List.for_all [ "session_revision"; "event_sequence" ] ~f:(fun field ->
        match List.Assoc.find fields ~equal:String.equal field with
        | Some (`String "0") -> true
        | _ -> false)
    | _ -> false
  in
  print_s
    [%sexp
      { zero = (roundtrip 0L : bool)
      ; maximum = (roundtrip Int64.max_value : bool)
      ; strings : bool
      ; rejected : bool
      }];
  [%expect {| ((zero true) (maximum true) (strings true) (rejected true)) |}]
;;

let%expect_test "run authority revoked during an admitted cold read prevents disclosure" =
  with_admitted_workflow
    ~f:
      (fun
        { Fixture.daemon
        ; entry
        ; authorized
        ; client = _
        ; clients = _
        ; session
        ; reference
        ; admitted = _
        ; views
        ; request
        }
      ->
      let state = A.Session_actor.state entry.actor |> protocol_ok in
      let registry = S.Daemon.registry daemon in
      let removed = S.Session_registry.remove registry session.id |> Option.value_exn in
      S.Runtime_owner.close_and_wait removed.runtime;
      removed.close ();
      let indexed =
        Agent_store.Session_index.find_checked
          (Agent_store.Session_store.session_index (S.Daemon.store daemon))
          session.id
        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
        |> protocol_ok
        |> Option.value_exn
      in
      S.Session_registry.index registry indexed;
      let entered, entered_resolver = Eio.Promise.create () in
      let release, release_resolver = Eio.Promise.create () in
      let reads = ref 0 in
      S.Session_registry.install_reader registry (fun _ ->
        incr reads;
        Eio.Promise.resolve entered_resolver ();
        Eio.Promise.await release;
        Ok state);
      let denied_lookup =
        Eio.Switch.run (fun sw ->
          let result =
            Eio.Fiber.fork_promise ~sw (fun () -> C.Run_views.lookup views request)
          in
          Eio.Promise.await entered;
          authorized := false;
          Eio.Promise.resolve release_resolver ();
          Eio.Promise.await_exn result |> permission_denied)
      in
      let page_request =
        P.Run_query.Request.create
          ~session:reference
          ~page:(P.Page.Request.create ~limit:1 () |> protocol_ok)
        |> protocol_ok
      in
      let denied_page = C.Run_views.page views page_request |> permission_denied in
      print_s
        [%sexp
          { denied_lookup : bool
          ; denied_page : bool
          ; reads = (!reads : int)
          ; cold = (Option.is_none (S.Session_registry.find registry session.id) : bool)
          }]);
  [%expect {| ((denied_lookup true) (denied_page true) (reads 1) (cold true)) |}]
;;
