open! Core
module P = Agent_protocol
module T = P.Developer_trace_wire

let must = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let document_limit bytes =
  match
    Document_schema.Limits.create
      ~max_bytes:bytes
      ~max_depth:32
      ~max_fields:512
      ~max_nodes:1024
  with
  | Ok value -> value
  | Error _ -> failwith "invalid trace test document limit"
;;

let limits =
  T.Limits.create
    ~request:(document_limit 4096)
    ~page:(document_limit 8192)
    ~response:(document_limit 16384)
    ~max_page_items:2
    ~max_cursor_bytes:1024
  |> must
;;

let capabilities =
  T.Capabilities.create ~methods:T.Request.supported_methods ~limits |> must
;;

let generator = P.Id.Generator.create ~bytes:(fun length -> String.make length 't')

let initialization ?(trace = true) ?(authentication_kind = "embedded") () =
  let implementation =
    P.Initialize.Implementation.create ~name:"trace-test" ~version:"1" |> must
  in
  let principal =
    P.Principal.create
      ~id:(P.Id.Principal.create_with generator)
      ~authentication_kind
      ~scopes:P.Scope.Set.empty
      ~attributes:[]
    |> must
  in
  P.Initialize.Response.create
    ~protocol_name:"ochat.agent"
    ~selected_version:P.Version.current
    ~implementation
    ~server_id:(P.Id.Server.create_with generator)
    ~enabled_features:(if trace then [ T.Features.trace ] else [])
    ~extensions:None
    ~principal
    ~limits:
      { max_request_bytes = 65536
      ; max_event_bytes = 65536
      ; max_page_size = 1000
      ; max_attachments_per_connection = 8
      }
    ~event_retention:
      { minimum_age_ms = 1000; maximum_events = 1024; oldest_replayable_sequence = None }
    ~timing:
      { heartbeat_interval_ms = 1000
      ; owner_lease_duration_ms = 1000
      ; owner_renew_after_ms = 500
      ; disconnect_grace_default_ms = 100
      }
    ~server_time:(P.Timestamp.of_string "2026-10-20T00:00:00Z" |> must)
  |> must
;;

let initialize connection response ~trace =
  let implementation =
    P.Initialize.Implementation.create ~name:"trace-test" ~version:"1" |> must
  in
  let request =
    P.Initialize.Request.create
      ~implementation
      ~protocol_min:P.Version.current
      ~protocol_max:P.Version.current
      ~features:(if trace then [ T.Features.trace ] else [])
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:65536
      ()
    |> must
  in
  match
    Agent_client.Connection.request_without_history
      connection
      (Protocol_initialize request)
  with
  | Ok (Protocol_initialize _) -> ()
  | Ok _ -> failwith "unexpected Initialize result"
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let public_initialized response =
  P.Public.Result.Non_history.of_internal (Protocol_initialize response)
  |> must
  |> fun value -> P.Public.Result.Non_history value
;;

let page_request installed_limits =
  let lookup =
    P.Developer_common.Lookup.create
      ~run_id:(P.Id.Developer_run.create_with generator)
      ~key:(P.Idempotency_key.of_string "trace-key" |> must)
  in
  T.Page_request.create
    ~limits:installed_limits
    ~locator:(T.Locator.Command lookup)
    ~after:None
    ~limit:2
  |> must
;;

let connection response ~bootstrap_count ~page_count ~page_json =
  Agent_client.In_memory.create_with_developer_repl_trace
    ~request:(function
      | P.Command.Protocol_initialize _ -> Ok (public_initialized response)
      | _ -> Error (P.Error.invalid_request "unexpected public command"))
    ~request_developer:(fun ~bound:_ _ ->
      Error (P.Error.invalid_request "unexpected Developer request"))
    ~request_repl:(fun ~max_request_bytes:_ ~bound:_ _ ->
      Error (P.Error.invalid_request "unexpected REPL request"))
    ~request_trace:(fun ~max_request_bytes:_ ~bound request ->
      match request, bound with
      | T.Request.Capabilities, Agent_client.Transport.Trace_response_bound.Bootstrap ->
        Int.incr bootstrap_count;
        Ok (T.Capabilities.to_json capabilities)
      | T.Request.Page _, Installed _ ->
        Int.incr page_count;
        Ok !page_json
      | _ -> Error (P.Error.invalid_request "wrong trace transport bound"))
    ~notifications:(Eio.Stream.create 8)
    ~close:Fn.id
;;

let%expect_test "trace client checks installed generation, bounds and read-only reconnect"
  =
  Eio_main.run (fun _ ->
    let response = initialization () in
    let bootstrap_count = ref 0
    and page_count = ref 0 in
    let page_json = ref (`Object [ "kind", `String "unavailable" ]) in
    let physical = connection response ~bootstrap_count ~page_count ~page_json in
    initialize physical response ~trace:true;
    let client = Agent_client.Developer_trace_connection.create physical |> must in
    assert (Int.equal !bootstrap_count 1);
    ignore
      (Agent_client.Connection.trace_capabilities physical |> must : T.Capabilities.t);
    assert (Int.equal !bootstrap_count 1);
    let page_request =
      Agent_client.Developer_trace_connection.capabilities client
      |> T.Capabilities.limits
      |> page_request
    in
    (match Agent_client.Developer_trace_connection.page client page_request with
     | Ok T.Availability.Unavailable -> ()
     | Ok _ | Error _ -> failwith "unavailable trace result changed");
    page_json := `String "malformed";
    assert (
      Result.is_error (Agent_client.Developer_trace_connection.page client page_request));
    page_json := `String (String.make 20000 'x');
    assert (
      Result.is_error (Agent_client.Developer_trace_connection.page client page_request));
    assert (Int.equal !page_count 3);
    initialize physical response ~trace:true;
    assert (
      Result.is_error (Agent_client.Developer_trace_connection.page client page_request));
    let refreshed = Agent_client.Developer_trace_connection.create physical |> must in
    assert (Int.equal !bootstrap_count 2);
    page_json := `Object [ "kind", `String "unavailable" ];
    ignore (Agent_client.Developer_trace_connection.page refreshed page_request |> must);
    let replacement = connection response ~bootstrap_count ~page_count ~page_json in
    initialize replacement response ~trace:true;
    let moved =
      Agent_client.Developer_trace_connection.reconnect client replacement |> must
    in
    ignore (Agent_client.Developer_trace_connection.page moved page_request |> must);
    let foreign = initialization ~authentication_kind:"bearer" () in
    let foreign_connection = connection foreign ~bootstrap_count ~page_count ~page_json in
    initialize foreign_connection foreign ~trace:true;
    assert (
      Result.is_error
        (Agent_client.Developer_trace_connection.reconnect client foreign_connection));
    Agent_client.Connection.close physical;
    Agent_client.Connection.close replacement;
    Agent_client.Connection.close foreign_connection;
    print_endline "trace generation, bounds, and same-identity reconnect enforced");
  [%expect {| trace generation, bounds, and same-identity reconnect enforced |}]
;;

let%expect_test "unselected trace feature prevents bootstrap handoff" =
  Eio_main.run (fun _ ->
    let response = initialization ~trace:false () in
    let bootstrap_count = ref 0
    and page_count = ref 0 in
    let page_json = ref `Null in
    let physical = connection response ~bootstrap_count ~page_count ~page_json in
    initialize physical response ~trace:false;
    assert (Result.is_error (Agent_client.Developer_trace_connection.create physical));
    assert (Int.equal !bootstrap_count 0);
    Agent_client.Connection.close physical;
    print_endline "unselected trace feature sent no request");
  [%expect {| unselected trace feature sent no request |}]
;;

let%expect_test "retained page must match full request and requested item count" =
  Eio_main.run (fun _ ->
    let response = initialization () in
    let bootstrap_count = ref 0
    and page_count = ref 0 in
    let page_json = ref `Null in
    let physical = connection response ~bootstrap_count ~page_count ~page_json in
    initialize physical response ~trace:true;
    let client = Agent_client.Developer_trace_connection.create physical |> must in
    let installed =
      T.Capabilities.limits (Agent_client.Developer_trace_connection.capabilities client)
    in
    let request = page_request installed in
    let locator = T.Page_request.locator request in
    let other_locator =
      T.Locator.Command
        (P.Developer_common.Lookup.create
           ~run_id:(P.Id.Developer_run.create_with generator)
           ~key:(P.Idempotency_key.of_string "other-key" |> must))
    in
    let digest = Chatml_source.Digest.of_hex (String.make 64 'a') |> Or_error.ok_exn in
    let header =
      T.Header.create
        ~execution_id:"trace-execution"
        ~admission:T.Header.Submitted_capture
        ~submission_fingerprint:digest
        ~selection_fingerprint:digest
        ~graph_fingerprint:None
        ~contract_fingerprint:None
        ~environment_fingerprint:None
        ~sources:[]
      |> must
    in
    let cursor locator sequence =
      T.Cursor.create
        ~limits:installed
        ~locator
        ~execution_id:(T.Header.execution_id header)
        ~submission_fingerprint:digest
        ~selection_fingerprint:digest
        ~after_sequence:sequence
      |> must
    in
    let page ~locator ~after ~events ~last_sequence ~resume =
      T.Page.create
        ~limits:installed
        ~locator
        ~after
        ~header
        ~events
        ~last_sequence
        ~omitted_after_prefix:None
        ~last_retained_site_context:None
        ~omitted_site_count:0
        ~state:T.Page.Incomplete
        ~resume
        ~next:None
      |> must
      |> fun page ->
      T.Response.Page (T.Availability.Retained page)
      |> fun response -> T.Response.to_json response ~limits:installed |> must
    in
    page_json
    := page ~locator:other_locator ~after:None ~events:[] ~last_sequence:0L ~resume:None;
    assert (Result.is_error (Agent_client.Developer_trace_connection.page client request));
    let after = cursor locator 1L in
    page_json
    := page ~locator ~after:(Some after) ~events:[] ~last_sequence:1L ~resume:(Some after);
    assert (Result.is_error (Agent_client.Developer_trace_connection.page client request));
    let events =
      List.map [ 1L; 2L ] ~f:(fun sequence ->
        T.Event.create
          ~sequence
          ~kind:T.Event.Statement_entered
          ~site:None
          ~correlation:None
        |> must)
    in
    page_json
    := page
         ~locator
         ~after:None
         ~events
         ~last_sequence:2L
         ~resume:(Some (cursor locator 2L));
    let one_item_request =
      T.Page_request.create ~limits:installed ~locator ~after:None ~limit:1 |> must
    in
    assert (
      Result.is_error
        (Agent_client.Developer_trace_connection.page client one_item_request));
    assert (Int.equal !page_count 3);
    Agent_client.Connection.close physical;
    print_endline
      "retained locator, full after cursor and item count stayed request-affine");
  [%expect {| retained locator, full after cursor and item count stayed request-affine |}]
;;
