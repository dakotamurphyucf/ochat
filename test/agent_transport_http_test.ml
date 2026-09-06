open! Core
module P = Piaf

let http_reply ?(status = "200 OK") ?(headers = "") body =
  sprintf
    "HTTP/1.1 %s\r\nConnection: close\r\nContent-Length: %d\r\n%s\r\n%s"
    status
    (String.length body)
    headers
    body
;;

let consume_http_headers reader =
  let rec loop length =
    match Eio.Buf_read.line reader |> String.strip with
    | "" -> ignore (Eio.Buf_read.take length reader : string)
    | line ->
      let length =
        match String.lsplit2 line ~on:':' with
        | Some (name, value) when String.Caseless.equal name "content-length" ->
          String.strip value |> Int.of_string
        | _ -> length
      in
      loop length
  in
  loop 0
;;

let event_fixture_handler requests event_response flow _address =
  let reader = Eio.Buf_read.of_flow flow ~max_size:16384 in
  let request = Eio.Buf_read.line reader in
  consume_http_headers reader;
  requests := request :: !requests;
  let response =
    if String.is_prefix request ~prefix:"POST /v1/rpc "
    then
      http_reply
        ~headers:"ochat-connection-id: fixture-connection\r\n"
        {|{"jsonrpc":"2.0","id":1,"result":{"server_time":"2026-09-06T00:00:00Z","ready":true,"draining":false}}|}
    else event_response
  in
  Eio.Flow.copy_string response flow
;;

let event_fixture ~sw env event_response requests =
  let listener =
    Eio.Net.listen
      ~sw
      ~backlog:8
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | `Unix _ -> failwith "expected TCP fixture"
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Net.run_server
      listener
      (event_fixture_handler requests event_response)
      ~on_error:(function
      | Eio.Io _ -> ()
      | exn -> raise exn));
  port
;;

let with_event_fixture env event_response f =
  Eio.Switch.run (fun sw ->
    let requests = ref [] in
    let port = event_fixture ~sw env event_response requests in
    let connection =
      Agent_transport_http.Client.connect
        ~sw
        ~env
        ~uri:(Uri.of_string (sprintf "http://127.0.0.1:%d" port))
        ~bearer_token:None
        ~notification_capacity:16
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum [%sexp (error : Agent_protocol.Error.t)])
      |> Result.ok_or_failwith
    in
    Exn.protect
      ~f:(fun () -> f connection requests)
      ~finally:(fun () -> Agent_client.Connection.close connection))
;;

let assert_stream_terminated connection requests expected =
  let module C = Agent_client.Connection in
  assert (Result.is_ok (C.request connection (Protocol_ping { payload = None })));
  let rec drain count =
    match C.next_notification connection with
    | None -> count
    | Some _ -> drain (count + 1)
  in
  assert (drain 0 = expected);
  (match C.request connection (Protocol_ping { payload = None }) with
   | Error { code = Interrupted; _ } -> ()
   | _ -> failwith "failed event transport accepted another RPC");
  C.close connection;
  assert (List.count !requests ~f:(fun line -> String.is_prefix line ~prefix:"GET ") = 1);
  assert (List.count !requests ~f:(fun line -> String.is_prefix line ~prefix:"POST ") = 1);
  assert (
    not (List.exists !requests ~f:(fun line -> String.is_prefix line ~prefix:"DELETE ")))
;;

let%test_unit "HTTP event termination is observable and cannot silently retry" =
  let notice =
    {|data: {"jsonrpc":"2.0","method":"fixture.notice","params":{}}|} ^ "\n\n"
  in
  List.iter
    [ http_reply "", 0
    ; http_reply notice, 1
    ; http_reply ~status:"503 Unavailable" "", 0
    ; http_reply "data: not-json\n\n", 0
    ; "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Length: 100\r\n\r\nshort", 0
    ]
    ~f:(fun (response, expected) ->
      Eio_main.run (fun env ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
          with_event_fixture env response (fun connection requests ->
            assert_stream_terminated connection requests expected))))
;;

let held_event_handler gate flow _address =
  let reader = Eio.Buf_read.of_flow flow ~max_size:16384 in
  let request = Eio.Buf_read.line reader in
  consume_http_headers reader;
  if String.is_prefix request ~prefix:"GET /v1/events "
  then (
    let notice =
      "data: {\"jsonrpc\":\"2.0\",\"method\":\"fixture.notice\",\"params\":{}}\n\n"
    in
    Eio.Flow.copy_string
      (sprintf
         "HTTP/1.1 200 OK\r\n\
          Content-Type: text/event-stream\r\n\
          Transfer-Encoding: chunked\r\n\
          \r\n\
          %x\r\n\
          %s\r\n"
         (String.length notice)
         notice)
      flow;
    Eio.Promise.await gate)
  else if String.is_prefix request ~prefix:"POST /v1/rpc "
  then
    Eio.Flow.copy_string
      (http_reply
         ~headers:"ochat-connection-id: fixture-connection\r\n"
         {|{"jsonrpc":"2.0","id":1,"result":{"server_time":"2026-09-06T00:00:00Z","ready":true,"draining":false}}|})
      flow
  else Eio.Flow.copy_string (http_reply "") flow
;;

let with_held_events env f =
  Eio.Switch.run (fun server_sw ->
    let gate, _resolver = Eio.Promise.create () in
    let listener =
      Eio.Net.listen
        ~sw:server_sw
        ~backlog:8
        (Eio.Stdenv.net env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    let port =
      match Eio.Net.listening_addr listener with
      | `Tcp (_, port) -> port
      | `Unix _ -> assert false
    in
    Eio.Fiber.fork_daemon ~sw:server_sw (fun () ->
      Eio.Net.run_server listener (held_event_handler gate) ~on_error:(function
        | Eio.Io _ -> ()
        | exn -> raise exn));
    f port)
;;

let%test_unit "closing an active HTTP SSE client releases its owning switch" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
      with_held_events env (fun port ->
        Eio.Switch.run (fun sw ->
          let connection =
            Agent_transport_http.Client.connect
              ~sw
              ~env
              ~uri:(Uri.of_string (sprintf "http://127.0.0.1:%d" port))
              ~bearer_token:None
              ~notification_capacity:16
            |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
            |> Result.ok_or_failwith
          in
          assert (
            Result.is_ok
              (Agent_client.Connection.request
                 connection
                 (Protocol_ping { payload = None })));
          assert (Option.is_some (Agent_client.Connection.next_notification connection));
          Agent_client.Connection.close connection))))
;;

let%test_unit "HTTP lifetime cancellation releases partial initialization" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
      Eio.Switch.run (fun sw ->
        let started, resolve_started = Eio.Promise.create () in
        let released = ref false in
        Eio.Fiber.first
          (fun () ->
             ignore
               (Agent_transport_http.Client_lifetime.start ~sw (fun lifetime ->
                  Eio.Switch.on_release
                    (Agent_transport_http.Client_lifetime.switch lifetime)
                    (fun () -> released := true);
                  Eio.Promise.resolve resolve_started ();
                  Eio.Fiber.await_cancel ())
                : (unit, Agent_protocol.Error.t) result))
          (fun () -> Eio.Promise.await started);
        assert !released)))
;;

let tracked_lifetime sw released =
  Agent_transport_http.Client_lifetime.start ~sw (fun lifetime ->
    Eio.Switch.on_release
      (Agent_transport_http.Client_lifetime.switch lifetime)
      (fun () -> released := true);
    Ok lifetime)
  |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
  |> Result.ok_or_failwith
;;

let%test_unit "HTTP lifetimes close independently and idempotently" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
      Eio.Switch.run (fun sw ->
        let first_released, second_released = ref false, ref false in
        let first = tracked_lifetime sw first_released in
        let second = tracked_lifetime sw second_released in
        Agent_transport_http.Client_lifetime.close first;
        Agent_transport_http.Client_lifetime.close first;
        assert !first_released;
        assert (not !second_released);
        Eio.Switch.check (Agent_transport_http.Client_lifetime.switch second);
        Agent_transport_http.Client_lifetime.close second;
        assert !second_released)))
;;

let%test_unit "HTTP initialization errors release partial resources" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
      Eio.Switch.run (fun sw ->
        List.iter [ false; true ] ~f:(fun raises ->
          let released = ref false in
          let result =
            Agent_transport_http.Client_lifetime.start ~sw (fun lifetime ->
              Eio.Switch.on_release
                (Agent_transport_http.Client_lifetime.switch lifetime)
                (fun () -> released := true);
              if raises then failwith "fixture initialization failure";
              Error
                (Agent_protocol.Error.create
                   Interrupted
                   ~message:"fixture"
                   ~retryable:true
                   ()))
          in
          assert (Result.is_error result);
          assert !released))))
;;

let%test_unit "parent cancellation releases an unclosed HTTP lifetime" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
      let started, resolve_started = Eio.Promise.create () in
      let released = ref false in
      Eio.Fiber.first
        (fun () ->
           Eio.Switch.run (fun sw ->
             ignore
               (tracked_lifetime sw released : Agent_transport_http.Client_lifetime.t);
             Eio.Promise.resolve resolve_started ();
             Eio.Fiber.await_cancel ()))
        (fun () -> Eio.Promise.await started);
      assert !released))
;;

let request ?(headers = []) ?(body = "") ?(meth = `POST) target =
  P.Request.create
    ~scheme:`HTTP
    ~version:P.Versions.HTTP.HTTP_1_1
    ~headers:(P.Headers.of_list headers)
    ~meth
    ~body:(P.Body.of_string body)
    target
;;

let error_code = function
  | Ok _ -> "ok"
  | Error error -> Agent_protocol.Error.code_to_string error.Agent_protocol.Error.code
;;

let summarize = function
  | Error error -> Agent_protocol.Error.code_to_string error.Agent_protocol.Error.code
  | Ok (Agent_transport_http.Rpc_body.Single (Request request)) ->
    "single:" ^ request.method_
  | Ok (Single (Notification notification)) -> "notification:" ^ notification.method_
  | Ok (Single (Response _)) -> "response"
  | Ok (Batch envelopes) ->
    List.map envelopes ~f:(function
      | Agent_protocol.Envelope.Request request -> request.method_
      | Notification notification -> notification.method_
      | Response _ -> "response")
    |> String.concat ~sep:","
    |> ( ^ ) "batch:"
;;

let%expect_test "HTTP RPC body decodes single and ordered batch requests" =
  let single = {|{"jsonrpc":"2.0","id":1,"method":"protocol.ping","params":{}}|} in
  let batch =
    {|[{"jsonrpc":"2.0","id":1,"method":"protocol.initialize","params":{}},{"jsonrpc":"2.0","id":2,"method":"server.info","params":{}},{"jsonrpc":"2.0","method":"protocol.ping","params":{}}]|}
  in
  Agent_transport_http.Rpc_body.parse ~max_batch_size:8 single
  |> summarize
  |> print_endline;
  Agent_transport_http.Rpc_body.parse ~max_batch_size:8 batch
  |> summarize
  |> print_endline;
  [%expect
    {|
    single:protocol.ping
    batch:protocol.initialize,server.info,protocol.ping |}]
;;

let%expect_test "HTTP RPC body rejects empty, oversized, and duplicate-field batches" =
  let values =
    [ "[]"
    ; {|[{"jsonrpc":"2.0","id":1,"method":"protocol.ping","params":{}},{"jsonrpc":"2.0","id":2,"method":"protocol.ping","params":{}}]|}
    ; {|[{"jsonrpc":"2.0","id":1,"method":"protocol.ping","params":{"value":1,"value":2}}]|}
    ]
  in
  List.iter values ~f:(fun body ->
    Agent_transport_http.Rpc_body.parse ~max_batch_size:1 body
    |> summarize
    |> print_endline);
  [%expect
    {|
    invalid_request
    resource_limit
    invalid_request |}]
;;

let%expect_test "HTTP request contract validates headers, bearer syntax, body, and cursor"
  =
  let valid =
    request
      ~headers:
        [ "content-type", "application/json; charset=utf-8"
        ; "ochat-protocol-version", "1.0"
        ; "authorization", "Bearer secret-token"
        ]
      ~body:"{}"
      "/v1/rpc"
  in
  let wrong_charset =
    request ~headers:[ "content-type", "application/json; charset=latin1" ] "/v1/rpc"
  in
  let missing_version =
    request ~headers:[ "content-type", "application/json" ] "/v1/rpc"
  in
  let malformed_bearer =
    request ~headers:[ "authorization", "Basic credential" ] "/v1/rpc"
  in
  let ambiguous_bearer =
    request
      ~headers:[ "authorization", "Bearer first"; "authorization", "Bearer second" ]
      "/v1/rpc"
  in
  let oversized = request ~headers:[ "content-length", "100" ] ~body:"small" "/v1/rpc" in
  let disagreeing_cursor =
    request
      ~headers:[ "last-event-id", "4" ]
      ~meth:`GET
      "/v1/sessions/session/events?after_sequence=5"
  in
  let bearer =
    Agent_transport_http.Request_contract.bearer_token valid
    |> Result.ok
    |> Option.join
    |> Option.value_map ~default:false ~f:(String.equal "secret-token")
  in
  print_s
    [%sexp
      { content_type =
          (Agent_transport_http.Request_contract.require_json_content_type valid
           |> error_code
           : string)
      ; protocol_version =
          (Agent_transport_http.Request_contract.require_protocol_version valid
           |> error_code
           : string)
      ; bearer : bool
      ; wrong_charset =
          (Agent_transport_http.Request_contract.require_json_content_type wrong_charset
           |> error_code
           : string)
      ; missing_version =
          (Agent_transport_http.Request_contract.require_protocol_version missing_version
           |> error_code
           : string)
      ; malformed_bearer =
          (Agent_transport_http.Request_contract.bearer_token malformed_bearer
           |> Result.is_error
           : bool)
      ; ambiguous_bearer =
          (Agent_transport_http.Request_contract.bearer_token ambiguous_bearer
           |> Result.is_error
           : bool)
      ; oversized =
          (Agent_transport_http.Request_contract.request_body ~max_body_bytes:16 oversized
           |> error_code
           : string)
      ; disagreeing_cursor =
          (Agent_transport_http.Request_contract.event_cursor disagreeing_cursor
           |> error_code
           : string)
      }];
  [%expect
    {|
    ((content_type ok) (protocol_version ok) (bearer true)
     (wrong_charset invalid_request) (missing_version incompatible_protocol)
     (malformed_bearer true) (ambiguous_bearer true) (oversized resource_limit)
     (disagreeing_cursor invalid_request))
    |}]
;;

let%expect_test "HTTP RPC body rejects invalid UTF-8 and excessive JSON depth" =
  let invalid_utf8 = Bytes.of_string "{}" in
  Bytes.set invalid_utf8 0 (Char.of_int_exn 0xFF);
  let deep = String.make 65 '[' ^ "{}" ^ String.make 65 ']' in
  List.iter
    [ Bytes.to_string invalid_utf8; deep ]
    ~f:(fun body ->
      Agent_transport_http.Rpc_body.parse ~max_batch_size:128 body
      |> error_code
      |> print_endline);
  [%expect
    {|
    invalid_request
    resource_limit |}]
;;
