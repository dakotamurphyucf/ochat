open! Core
module P = Agent_protocol
module S = Agent_server
module Q = P.Inference_query
module F = Inference_ledger_tests.Ledger_fixture

let ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (P.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let principal scopes =
  P.Principal.create
    ~id:(P.Id.Principal.of_string "pri_query" |> ok)
    ~authentication_kind:"local"
    ~attributes:[]
    ~scopes:(P.Scope.Set.of_list scopes)
  |> ok
;;

let request ?cursor ?(limit = 1) ?(configuration = false) () =
  Q.Request.create
    ~session_id:F.sid
    ~page:(P.Page.Request.create ~limit ?cursor () |> ok)
    ~include_configuration:configuration
    ~include_diagnostics:false
  |> ok
;;

let code result =
  Result.error result |> Option.value_exn |> fun error -> error.P.Error.code
;;

let%expect_test
    "inference cursors bind actual authority/query and fence accounting changes"
  =
  Mirage_crypto_rng_unix.use_default ();
  let signer = S.Pagination.create () in
  let p = principal [ View_session_transcript ] in
  let bind p request generation revision =
    S.Pagination.Inference.binding
      ~principal:p
      ~request
      ~generation
      ~accounting_revision:revision
    |> ok
  in
  let binding = bind p (request ()) 0 1L in
  let cursor = S.Pagination.Inference.cursor signer binding ~after_ordinal:42L |> ok in
  [%test_eq: int64] 42L (S.Pagination.Inference.after signer binding (Some cursor) |> ok);
  let same_authority =
    { p with
      attributes = [ "credential", "never-bound-or-disclosed" ]
    ; authentication_kind = "oauth"
    }
  in
  [%test_eq: int64]
    42L
    (S.Pagination.Inference.after
       signer
       (bind same_authority (request ()) 0 1L)
       (Some cursor)
     |> ok);
  List.iter
    [ bind (principal [ View_session_transcript; Diagnostics ]) (request ()) 0 1L
    ; bind p (request ~limit:2 ()) 0 1L
    ; bind p (request ~configuration:true ()) 0 1L
    ; bind { p with id = P.Id.Principal.of_string "pri_other" |> ok } (request ()) 0 1L
    ]
    ~f:(fun binding ->
      [%test_eq: P.Error.code]
        Cursor_expired
        (code (S.Pagination.Inference.after signer binding (Some cursor))));
  List.iter
    [ bind p (request ()) 1 1L; bind p (request ()) 0 2L ]
    ~f:(fun binding ->
      match S.Pagination.Inference.after signer binding (Some cursor) with
      | Error error ->
        [%test_eq: P.Error.code] Conflict error.code;
        assert (
          Document_schema.Json.equal error.data (`Object [ "restart_required", `True ]))
      | Ok _ -> failwith "changed accounting accepted");
  [%test_eq: P.Error.code]
    Cursor_expired
    (code (S.Pagination.Inference.after (S.Pagination.create ()) binding (Some cursor)));
  let changed = Bytes.of_string (P.Page.Cursor.to_string cursor) in
  Bytes.set changed 0 (if Char.equal (Bytes.get changed 0) 'A' then 'B' else 'A');
  let tampered = P.Page.Cursor.of_string (Bytes.to_string changed) |> ok in
  [%test_eq: P.Error.code]
    Cursor_expired
    (code (S.Pagination.Inference.after signer binding (Some tampered)));
  assert (String.length (P.Page.Cursor.to_string cursor) <= 2048);
  print_endline
    "authority/query/restart expire; generation/revision require restart; no attributes \
     retained";
  [%expect
    {| authority/query/restart expire; generation/revision require restart; no attributes retained |}]
;;

let%expect_test "detailed inference disclosure is authorized before reader IO" =
  let visible = principal [ View_session_transcript ] in
  let summary = P.Command.Session_inference_summary { session_id = F.sid } in
  assert (Result.is_ok (S.Authorization.authorize (principal []) summary));
  let rows = P.Command.Session_inference_observations (request ()) in
  [%test_eq: P.Error.code]
    Permission_denied
    (code (S.Authorization.authorize (principal []) rows));
  assert (Result.is_ok (S.Authorization.authorize visible rows));
  let detail =
    P.Command.Session_inference_observations (request ~configuration:true ())
  in
  [%test_eq: P.Error.code]
    Permission_denied
    (code (S.Authorization.authorize visible detail));
  assert (
    Result.is_ok
      (S.Authorization.authorize
         (principal [ Diagnostics; View_session_transcript ])
         detail));
  print_endline
    "summary has separate visibility check; rows need transcript; details also \
     diagnostics";
  [%expect
    {| summary has separate visibility check; rows need transcript; details also diagnostics |}]
;;

let%expect_test
    "query byte policy measures escaped actual RPC id and whole signed-cursor envelope"
  =
  Mirage_crypto_rng_unix.use_default ();
  let ledger, _, _ = F.admit (F.ledger ()) in
  let row = List.hd_exn (Agent_session.Inference_ledger.rows ledger) in
  let row =
    Agent_session.Inference_ledger.row_view
      row
      ~include_configuration:true
      ~include_diagnostics:true
  in
  let cursor =
    let binding =
      S.Pagination.Inference.binding
        ~principal:(principal [ Diagnostics; View_session_transcript ])
        ~request:(request ~configuration:true ())
        ~generation:0
        ~accounting_revision:1L
      |> ok
    in
    S.Pagination.Inference.cursor (S.Pagination.create ()) binding ~after_ordinal:1L |> ok
  in
  let response =
    Q.Response.create
      ~summary:(Agent_session.Inference_ledger.summary ledger)
      ~attempts:{ items = [ row ]; next_cursor = Some cursor }
      ~max_bytes:(16 * 1024 * 1024)
    |> ok
  in
  let result =
    P.Public.Result.Non_history
      (P.Public.Result.Non_history.of_internal (Session_inference_observations response)
       |> ok)
  in
  let id = P.Envelope.Request_id.of_json (`String "correlation\"\\\n😀") |> ok in
  let envelope = P.Envelope.success ~id (P.Public.Result.to_json result) in
  let exact = String.length (Jsonaf.to_string (P.Envelope.to_json envelope)) in
  let policy = S.Inference_query_budget.Policy.create ~max_envelope_bytes:exact |> ok in
  let budget = S.Inference_query_budget.for_request policy id |> ok in
  [%test_eq: int]
    (String.length (Jsonaf.to_string (P.Public.Result.to_json result)))
    (S.Inference_query_budget.max_result_bytes budget);
  assert (Result.is_ok (S.Inference_query_budget.validate_result budget result));
  assert (Result.is_ok (S.Inference_query_budget.validate_envelope policy envelope));
  let tight =
    S.Inference_query_budget.Policy.create ~max_envelope_bytes:(exact - 1) |> ok
  in
  let tight_budget = S.Inference_query_budget.for_request tight id |> ok in
  [%test_eq: P.Error.code]
    Resource_limit
    (code (S.Inference_query_budget.validate_result tight_budget result));
  [%test_eq: P.Error.code]
    Resource_limit
    (code (S.Inference_query_budget.validate_envelope tight envelope));
  let builder =
    Q.Response.Builder.create
      ~summary:(Agent_session.Inference_ledger.summary ledger)
      ~max_bytes:(S.Inference_query_budget.max_result_bytes tight_budget)
    |> ok
  in
  [%test_eq: P.Error.code]
    Resource_limit
    (code (Q.Response.Builder.add builder row ~next_cursor:(Some cursor)));
  let empty =
    Q.Response.create
      ~summary:(Agent_session.Inference_ledger.summary ledger)
      ~attempts:{ items = []; next_cursor = None }
      ~max_bytes:(16 * 1024 * 1024)
    |> ok
  in
  let empty_bytes = String.length (Jsonaf.to_string (Q.Response.to_json empty)) in
  assert (
    Result.is_ok
      (Q.Response.Builder.create
         ~summary:(Agent_session.Inference_ledger.summary ledger)
         ~max_bytes:empty_bytes));
  [%test_eq: P.Error.code]
    Resource_limit
    (code
       (Q.Response.Builder.create
          ~summary:(Agent_session.Inference_ledger.summary ledger)
          ~max_bytes:(empty_bytes - 1)));
  let tiny_allowance = String.length (Jsonaf.to_string (Q.Response.to_json empty)) + 1 in
  let escaped_cursor = P.Page.Cursor.of_string (String.make 2048 '\"') |> ok in
  assert (
    String.length (Jsonaf.to_string (P.Page.Cursor.to_json escaped_cursor))
    > tiny_allowance);
  let tiny_builder =
    Q.Response.Builder.create
      ~summary:(Agent_session.Inference_ledger.summary ledger)
      ~max_bytes:tiny_allowance
    |> ok
  in
  [%test_eq: P.Error.code]
    Resource_limit
    (code (Q.Response.Builder.add tiny_builder row ~next_cursor:(Some escaped_cursor)));
  let later_builder =
    Q.Response.Builder.create
      ~summary:(Agent_session.Inference_ledger.summary ledger)
      ~max_bytes:(String.length (Jsonaf.to_string (Q.Response.to_json response)))
    |> ok
  in
  let later_builder =
    Q.Response.Builder.add later_builder row ~next_cursor:(Some cursor)
    |> ok
    |> Option.value_exn
  in
  let next, next_handle, _ = F.admit ledger in
  let next_row =
    Agent_session.Inference_ledger.find
      next
      ~ordinal:(Agent_session.Inference_ledger.Handle.ordinal next_handle)
    |> Option.value_exn
    |> fun row ->
    Agent_session.Inference_ledger.row_view
      row
      ~include_configuration:true
      ~include_diagnostics:true
  in
  assert (
    Option.is_none
      (Q.Response.Builder.add later_builder next_row ~next_cursor:(Some escaped_cursor)
       |> ok));
  let retained_page =
    Q.Response.Builder.finish later_builder |> ok |> Q.Response.attempts
  in
  [%test_eq: int64 list] [ 1L ] (List.map retained_page.items ~f:Q.Attempt.ordinal);
  assert (Option.equal P.Page.Cursor.equal retained_page.next_cursor (Some cursor));
  let huge_id = P.Envelope.Request_id.of_json (`String (String.make 4096 'x')) |> ok in
  let tiny = S.Inference_query_budget.Policy.create ~max_envelope_bytes:128 |> ok in
  assert (Result.is_error (S.Inference_query_budget.for_request tiny huge_id));
  assert (
    Result.is_error
      (S.Inference_query_budget.validate_envelope
         tiny
         (P.Envelope.failure ~id:huge_id (P.Error.invalid_request "bounded error"))));
  print_endline
    "exact envelope fits; one byte less rejects result/first row; oversized correlation \
     fails closed";
  [%expect
    {| exact envelope fits; one byte less rejects result/first row; oversized correlation fails closed |}]
;;

let%expect_test
    "indexed Pending reads authorize before IO and again on actual state without \
     activation"
  =
  let module Fixtures = Agent_session_test.Fixtures in
  Fixtures.with_actor_workspace (fun _env workspace_instance ->
    let p = principal [ View_session_transcript ] in
    let state =
      Fixtures.actor_state
        ~workspace_instance
        ~liveness:Process_bound
        ~start_immediately:false
    in
    let state =
      { state with
        identity = { state.identity with creating_principal = Some p.id }
      ; runtime_initialization = Pending { fresh_history = true }
      }
    in
    let registry = S.Session_registry.create () in
    let index : Agent_store.Session_index.Entry.t =
      { session = Agent_session.Session_state.summary state
      ; runnable_job_count = 0
      ; deliverable_job_count = 0
      ; earliest_schedule_due = None
      ; owner_grace_deadline = None
      ; pending_initial_start = false
      ; archived = false
      ; lifecycle_revision = Agent_store.Session_archive_record.Revision.zero
      ; admission = Automatic
      }
    in
    S.Session_registry.index registry index;
    let reads = ref 0
    and loads = ref 0 in
    S.Session_registry.install_loader registry (fun _ ->
      Int.incr loads;
      failwith "query activated loader");
    S.Session_registry.install_reader registry (fun _ ->
      Int.incr reads;
      Ok state);
    let denied _ =
      Error (P.Error.create Permission_denied ~message:"hidden" ~retryable:false ())
    in
    [%test_eq: P.Error.code]
      Permission_denied
      (code
         (S.Session_registry.read_state
            registry
            ~authorize:denied
            state.identity.session_id));
    [%test_eq: int] 0 !reads;
    let authorized = ref 0 in
    let authorize (summary : P.Session.t) =
      Int.incr authorized;
      if Option.equal P.Id.Principal.equal summary.creator (Some p.id)
      then Ok ()
      else denied summary
    in
    let read =
      S.Session_registry.read_state registry ~authorize state.identity.session_id |> ok
    in
    assert (
      Agent_session.Session_state.Runtime_initialization.equal
        read.runtime_initialization
        state.runtime_initialization);
    assert (P.Session.equal_desired_state read.lifecycle.desired Stopped);
    [%test_eq: int] 2 !authorized;
    [%test_eq: int] 1 !reads;
    [%test_eq: int] 0 !loads;
    let actual =
      { state with
        identity =
          { state.identity with
            creating_principal = Some (P.Id.Principal.of_string "pri_other" |> ok)
          }
      }
    in
    S.Session_registry.install_reader registry (fun _ ->
      Int.incr reads;
      Ok actual);
    [%test_eq: P.Error.code]
      Permission_denied
      (code (S.Session_registry.read_state registry ~authorize state.identity.session_id));
    [%test_eq: int] 4 !authorized;
    let stats = S.Session_registry.stats registry in
    [%test_eq: int] 0 stats.loaded;
    [%test_eq: int] 1 stats.indexed;
    S.Session_registry.install_reader registry (fun _ ->
      Ok { state with identity = { state.identity with session_id = F.sid } });
    [%test_eq: P.Error.code]
      Persistence_error
      (code (S.Session_registry.read_state registry ~authorize state.identity.session_id));
    S.Session_registry.shutdown registry);
  print_endline
    "denied before IO; actual identity/visibility rechecked; Pending/stopped and \
     registry unchanged";
  [%expect
    {| denied before IO; actual identity/visibility rechecked; Pending/stopped and registry unchanged |}]
;;

let%expect_test "cancelled immutable read releases registry ownership for the next read" =
  let module Fixtures = Agent_session_test.Fixtures in
  Fixtures.with_actor_workspace (fun _env workspace_instance ->
    let state =
      Fixtures.actor_state
        ~workspace_instance
        ~liveness:Process_bound
        ~start_immediately:false
    in
    let registry = S.Session_registry.create () in
    let index : Agent_store.Session_index.Entry.t =
      { session = Agent_session.Session_state.summary state
      ; runnable_job_count = 0
      ; deliverable_job_count = 0
      ; earliest_schedule_due = None
      ; owner_grace_deadline = None
      ; pending_initial_start = false
      ; archived = false
      ; lifecycle_revision = Agent_store.Session_archive_record.Revision.zero
      ; admission = Automatic
      }
    in
    S.Session_registry.index registry index;
    let entered, resolver = Eio.Promise.create () in
    let blocked, _ = Eio.Promise.create () in
    let cleaned = ref false in
    S.Session_registry.install_reader registry (fun _ ->
      Exn.protect
        ~finally:(fun () -> cleaned := true)
        ~f:(fun () ->
          Eio.Promise.resolve resolver ();
          Eio.Promise.await blocked;
          Ok state));
    Eio.Fiber.first
      (fun () ->
         ignore
           (S.Session_registry.read_state
              registry
              ~authorize:(fun _ -> Ok ())
              state.identity.session_id
            : (Agent_session.Session_state.t, P.Error.t) Result.t))
      (fun () -> Eio.Promise.await entered);
    assert !cleaned;
    S.Session_registry.install_reader registry (fun _ -> Ok state);
    ignore
      (S.Session_registry.read_state
         registry
         ~authorize:(fun _ -> Ok ())
         state.identity.session_id
       |> ok
       : Agent_session.Session_state.t);
    S.Session_registry.shutdown registry);
  print_endline "cancellation reaches reader cleanup and leaves no retained lock";
  [%expect {| cancellation reaches reader cleanup and leaves no retained lock |}]
;;
