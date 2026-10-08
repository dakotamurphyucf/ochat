open! Core
open Ledger_fixture
module C = Agent_client.Connection
module V = Agent_client.Inference_views

let public value =
  P.Public.Result.Non_history.of_internal value
  |> protocol_ok
  |> fun value -> P.Public.Result.Non_history value
;;

let initialization features =
  let implementation =
    P.Initialize.Implementation.create ~name:"test" ~version:"1" |> protocol_ok
  in
  let principal =
    P.Principal.create
      ~id:(P.Id.Principal.of_string "pri_inference" |> protocol_ok)
      ~authentication_kind:"embedded"
      ~scopes:P.Scope.Set.empty
      ~attributes:[]
    |> protocol_ok
  in
  P.Initialize.Response.create
    ~protocol_name:"ochat.agent"
    ~selected_version:P.Version.current
    ~implementation
    ~server_id:(P.Id.Server.of_string "srv_inference" |> protocol_ok)
    ~enabled_features:features
    ~extensions:None
    ~principal
    ~limits:
      { max_request_bytes = 16 * 1024 * 1024
      ; max_event_bytes = 16 * 1024 * 1024
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
    ~server_time:(P.Timestamp.of_string "2026-10-07T00:00:00Z" |> protocol_ok)
  |> protocol_ok
;;

let make_connection ~features ~initialize_result =
  let summary = L.summary (ledger ()) in
  let response =
    Q.Response.create
      ~summary
      ~attempts:{ items = []; next_cursor = None }
      ~max_bytes:(16 * 1024 * 1024)
    |> protocol_ok
  in
  let reads = ref 0 in
  let connection =
    Agent_client.Transport.create
      ~request:(function
        | P.Command.Protocol_initialize _ -> initialize_result (initialization features)
        | Session_inference_summary _ ->
          Int.incr reads;
          Ok (public (Session_inference_summary summary))
        | Session_inference_observations _ ->
          Int.incr reads;
          Ok (public (Session_inference_observations response))
        | _ -> Error (P.Error.invalid_request "unexpected test command"))
      ~next_notification:(fun () -> None)
      ~close:Fn.id
    |> C.create
  in
  connection, reads
;;

let initialize connection =
  Agent_client.Session_handle.initialize
    connection
    ~implementation_name:"test"
    ~implementation_version:"1"
;;

let request ~configuration ~diagnostics =
  Q.Request.create
    ~session_id:sid
    ~page:(P.Page.Request.create ~limit:128 () |> protocol_ok)
    ~include_configuration:configuration
    ~include_diagnostics:diagnostics
  |> protocol_ok
;;

let%expect_test "SDK queries use actual selected support and preserve server denial" =
  Eio_main.run (fun _ ->
    let connection, reads =
      make_connection
        ~features:[ Q.Features.observations ]
        ~initialize_result:(fun response -> Ok (public (Protocol_initialize response)))
    in
    assert (Result.is_error (V.summary connection sid));
    assert (!reads = 0);
    ignore (initialize connection |> protocol_ok : P.Initialize.Response.t);
    ignore (V.summary connection sid |> protocol_ok : Q.Summary.t);
    ignore
      (V.observations connection (request ~configuration:false ~diagnostics:false)
       |> protocol_ok
       : Q.Response.t);
    assert (!reads = 2);
    assert (
      Result.is_error
        (V.observations connection (request ~configuration:true ~diagnostics:false)));
    assert (!reads = 2);
    let denied =
      P.Error.create
        Permission_denied
        ~message:"session is not visible"
        ~retryable:false
        ()
    in
    let connection =
      Agent_client.Transport.create
        ~request:(function
          | P.Command.Protocol_initialize _ ->
            Ok (public (Protocol_initialize (initialization Q.Features.all)))
          | Session_inference_summary _ -> Error denied
          | _ -> Error (P.Error.invalid_request "unexpected test command"))
        ~next_notification:(fun () -> None)
        ~close:Fn.id
      |> C.create
    in
    ignore (initialize connection |> protocol_ok : P.Initialize.Response.t);
    match V.summary connection sid with
    | Error error -> assert (P.Error.equal_code error.code Permission_denied)
    | Ok _ -> failwith "server denial was lost");
  print_endline "selected support checked; authority remains server-owned";
  [%expect {| selected support checked; authority remains server-owned |}]
;;

let%expect_test
    "failed invalid unrelated and closed initialization never establishes support"
  =
  Eio_main.run (fun _ ->
    let exercise initialize_result =
      let connection, reads =
        make_connection ~features:Q.Features.all ~initialize_result
      in
      ignore (initialize connection : (P.Initialize.Response.t, P.Error.t) Result.t);
      assert (Option.is_none (C.initialization connection));
      assert (Result.is_error (V.summary connection sid));
      assert (!reads = 0)
    in
    exercise (fun _ -> Error (P.Error.invalid_request "initialize refused"));
    exercise (fun response ->
      Ok
        (public
           (Protocol_initialize
              { response with enabled_features = [ "unrequested.feature" ] })));
    exercise (fun response ->
      Ok
        (public
           (Protocol_initialize { response with selected_version = P.Version.initial })));
    exercise (fun _ -> Ok (public (Session_inference_summary (L.summary (ledger ())))));
    let connection, _ =
      make_connection ~features:Q.Features.all ~initialize_result:(fun response ->
        Ok (public (Protocol_initialize response)))
    in
    ignore (initialize connection |> protocol_ok : P.Initialize.Response.t);
    C.close connection;
    assert (Option.is_none (C.initialization connection)));
  print_endline "only valid initialize acknowledges support";
  [%expect {| only valid initialize acknowledges support |}]
;;

let%expect_test "new method codecs public whitelist and sexp admission remain bounded" =
  let commands =
    [ P.Command.Session_inference_summary { session_id = sid }
    ; Session_inference_observations (request ~configuration:false ~diagnostics:false)
    ]
  in
  List.iter commands ~f:(fun command ->
    let method_ = P.Command.method_name command in
    assert (List.mem P.Command.supported_methods method_ ~equal:String.equal);
    let decoded =
      P.Command.of_method_and_params ~method_ ~params:(P.Command.params command)
      |> protocol_ok
    in
    assert (Jsonaf.exactly_equal (P.Command.params command) (P.Command.params decoded)));
  let summary = L.summary (ledger ()) in
  let result = public (Session_inference_summary summary) in
  P.Public.Result.validate result |> protocol_ok;
  let decoded =
    P.Public.Result.of_json
      ~method_:"session.inference_summary"
      (P.Public.Result.to_json result)
    |> protocol_ok
  in
  assert (
    Jsonaf.exactly_equal
      (P.Public.Result.to_json result)
      (P.Public.Result.to_json decoded));
  let restored =
    P.Method_result.t_of_sexp
      (P.Method_result.sexp_of_t (Session_inference_summary summary))
  in
  assert (
    Jsonaf.exactly_equal (P.Method_result.to_json restored) (Q.Summary.to_json summary));
  let malformed =
    add (Q.Summary.to_json summary) "future" (`String (String.make 8192 'x'))
  in
  assert (
    Result.is_error
      (P.Public.Result.of_json ~method_:"session.inference_summary" malformed));
  assert (Exn.does_raise (fun () -> Q.Summary.t_of_sexp (Jsonaf.sexp_of_t malformed)));
  print_endline "additive dispatch and validating sexp/read boundaries";
  [%expect {| additive dispatch and validating sexp/read boundaries |}]
;;
