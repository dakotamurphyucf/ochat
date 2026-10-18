open! Core
module P = Agent_protocol
module R = P.Developer_repl_wire
module T = Developer_connection_tests

let limits =
  R.Limits.create
    ~request:(T.document_limit 4096)
    ~result:(T.document_limit 4096)
    ~page:(T.document_limit 2048)
    ~response:(T.document_limit 8192)
    ~max_source_bytes:512
    ~max_reference_bytes:384
    ~max_output_bytes:512
    ~max_page_items:2
    ~max_event_bytes:128
    ~max_registry_entries:16
  |> T.must
;;

let capabilities =
  R.Capabilities.create
    ~methods:[ "developer.repl.capabilities"; "developer.repl.observe" ]
    ~limits
  |> T.must
;;

let public_initialized response =
  P.Public.Result.Non_history.of_internal (Protocol_initialize response)
  |> T.must
  |> fun value -> P.Public.Result.Non_history value
;;

let lookup =
  R.Lookup.create
    ~instance:(R.Wire_id.Instance.of_string "ri_fixture" |> T.must)
    ~key:(P.Idempotency_key.of_string "repl-observe" |> T.must)
;;

let%expect_test
    "closed REPL lane rejects malformed, oversized, unsupported, and stale-generation \
     responses"
  =
  Eio_main.run (fun _ ->
    let initialized = T.initialization ~repl:true () in
    let bootstrap_count = ref 0 in
    let observe_count = ref 0 in
    let observed = ref (`Object [ "found", `False ]) in
    let connection =
      Agent_client.In_memory.create_with_developer_repl
        ~request:(function
          | P.Command.Protocol_initialize _ -> Ok (public_initialized initialized)
          | _ -> Error (P.Error.invalid_request "unexpected public command"))
        ~request_developer:(fun ~bound:_ _ ->
          Error (P.Error.invalid_request "unexpected Developer request"))
        ~request_repl:(fun ~max_request_bytes:_ ~bound request ->
          match request, bound with
          | R.Request.Capabilities, Agent_client.Transport.Repl_response_bound.Bootstrap
            ->
            Int.incr bootstrap_count;
            Ok (R.Capabilities.to_json capabilities)
          | R.Request.Observe _, Installed _ ->
            Int.incr observe_count;
            Ok !observed
          | _ -> Error (P.Error.invalid_request "wrong REPL transport bound"))
        ~notifications:(Eio.Stream.create 8)
        ~close:Fn.id
    in
    T.initialize ~repl:true connection initialized;
    let installed = Agent_client.Connection.repl_capabilities connection |> T.must in
    (* Decoding produces a fresh, validated limits object; only that exact
       installed object is accepted by Connection.request_repl. *)
    let installed_limits = R.Capabilities.limits installed in
    assert (Int.equal !bootstrap_count 1);
    ignore
      (Agent_client.Connection.repl_capabilities connection |> T.must : R.Capabilities.t);
    assert (Int.equal !bootstrap_count 1);
    let observe () =
      Agent_client.Connection.request_repl
        connection
        ~limits:installed_limits
        (R.Request.Observe lookup)
    in
    assert (
      Result.is_error
        (Agent_client.Connection.request_repl
           connection
           ~limits
           (R.Request.Observe lookup)));
    (match observe () with
     | Ok (R.Response.Observe None) -> ()
     | Ok _ | Error _ -> failwith "valid REPL Observe was not accepted");
    observed := `Object [ "found", `True ];
    assert (Result.is_error (observe ()));
    observed := `String (String.make 9000 'x');
    assert (Result.is_error (observe ()));
    assert (
      Result.is_error
        (Agent_client.Connection.request_repl
           connection
           ~limits:installed_limits
           (R.Request.Submit
              (R.Submit.create
                 ~limits:installed_limits
                 ~instance:(R.Lookup.instance lookup)
                 ~epoch:(R.Wire_id.Epoch.of_string "re_fixture" |> T.must)
                 ~expected_revision:(R.Wire_id.Revision.of_string "rv_fixture" |> T.must)
                 ~key:(R.Lookup.key lookup)
                 ~source:"1"
               |> T.must))));
    assert (
      Result.is_error
        (Agent_client.Connection.request_repl
           connection
           ~limits:installed_limits
           (R.Request.Watch (R.Watch_key.Repl lookup))));
    assert (Int.equal !observe_count 3);
    (* A second successful Initialize creates a new physical response object,
       even when it describes the same logical host/principal. *)
    T.initialize ~repl:true connection initialized;
    assert (Result.is_error (observe ()));
    let fresh = Agent_client.Connection.repl_capabilities connection |> T.must in
    assert (Int.equal !bootstrap_count 2);
    observed := `Object [ "found", `False ];
    (match
       Agent_client.Connection.request_repl
         connection
         ~limits:(R.Capabilities.limits fresh)
         (R.Request.Observe lookup)
     with
     | Ok (R.Response.Observe None) -> ()
     | Ok _ | Error _ -> failwith "fresh REPL generation was not accepted");
    Agent_client.Connection.close connection;
    assert (Result.is_error (Agent_client.Connection.repl_capabilities connection));
    print_endline
      "REPL bootstrap cached; exact limits and method gated; malformed and oversized \
       rejected; generation invalidated; close rejected");
  [%expect
    {| REPL bootstrap cached; exact limits and method gated; malformed and oversized rejected; generation invalidated; close rejected |}]
;;

let%expect_test "REPL feature selection is required before bootstrap" =
  Eio_main.run (fun _ ->
    let initialized = T.initialization () in
    let bootstrap = ref 0 in
    let connection =
      Agent_client.In_memory.create_with_developer_repl
        ~request:(function
          | P.Command.Protocol_initialize _ -> Ok (public_initialized initialized)
          | _ -> Error (P.Error.invalid_request "unexpected public command"))
        ~request_developer:(fun ~bound:_ _ ->
          Error (P.Error.invalid_request "unexpected Developer request"))
        ~request_repl:(fun ~max_request_bytes:_ ~bound:_ _ ->
          Int.incr bootstrap;
          Ok (R.Capabilities.to_json capabilities))
        ~notifications:(Eio.Stream.create 8)
        ~close:Fn.id
    in
    T.initialize connection initialized;
    assert (Result.is_error (Agent_client.Connection.repl_capabilities connection));
    assert (Int.equal !bootstrap 0);
    Agent_client.Connection.close connection;
    print_endline "unselected REPL feature sent no bootstrap request");
  [%expect {| unselected REPL feature sent no bootstrap request |}]
;;

let%expect_test "Unix REPL lane bounds a raw result and keeps request correlation" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root =
      Filename.concat
        "/tmp"
        ("ochat-repl-transport-"
         ^ (P.Id.Transaction.create () |> P.Id.Transaction.to_string))
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / root) in
    Eio.Path.mkdir ~perm:0o700 path;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path)
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let socket_path = Filename.concat root "repl.sock" in
          let listener =
            Eio.Net.listen ~sw ~backlog:1 (Eio.Stdenv.net env) (`Unix socket_path)
          in
          let initialized = T.initialization ~repl:true () in
          let seen = ref 0 in
          Eio.Fiber.fork ~sw (fun () ->
            let flow, _ = Eio.Net.accept ~sw listener in
            let reader = Eio.Buf_read.of_flow flow ~max_size:131072 in
            List.iter
              [ ( "protocol.initialize"
                , P.Public.Result.to_json (public_initialized initialized) )
              ; "developer.repl.capabilities", R.Capabilities.to_json capabilities
              ; "developer.repl.observe", `Object [ "found", `False ]
              ; "developer.repl.observe", `String (String.make 9000 'x')
              ; "developer.repl.observe", `Object [ "found", `False ]
              ]
              ~f:(fun (expected_method, result) ->
                let request =
                  Eio.Buf_read.line reader
                  |> Jsonaf.of_string
                  |> P.Envelope.of_json
                  |> T.must
                in
                match request with
                | P.Envelope.Request request ->
                  assert (String.equal request.method_ expected_method);
                  Int.incr seen;
                  P.Envelope.success ~id:request.id result
                  |> P.Envelope.to_json
                  |> Jsonaf.to_string
                  |> fun line -> Eio.Flow.copy_string (line ^ "\n") flow
                | Notification _ | Response _ -> failwith "expected socket request");
            Eio.Flow.close flow);
          let connection =
            Agent_transport_socket.Client.connect
              ~sw
              ~net:(Eio.Stdenv.net env)
              ~socket_path
              ~max_line_length:131072
              ~notification_capacity:8
          in
          Exn.protect
            ~finally:(fun () -> Agent_client.Connection.close connection)
            ~f:(fun () ->
              T.initialize ~repl:true connection initialized;
              let capabilities =
                Agent_client.Connection.repl_capabilities connection |> T.must
              in
              let request () =
                Agent_client.Connection.request_repl
                  connection
                  ~limits:(R.Capabilities.limits capabilities)
                  (R.Request.Observe lookup)
              in
              (match request () with
               | Ok (R.Response.Observe None) -> ()
               | Ok _ | Error _ -> failwith "valid socket Observe was lost");
              assert (Result.is_error (request ()));
              (match request () with
               | Ok (R.Response.Observe None) -> ()
               | Ok _ | Error _ -> failwith "socket correlation was not retained");
              assert (Int.equal !seen 5);
              print_endline
                "Unix REPL constructor exercised; oversized raw result rejected; next \
                 response correlated"))));
  [%expect
    {| Unix REPL constructor exercised; oversized raw result rejected; next response correlated |}]
;;

let%expect_test "Unix Developer bootstrap fits its exact physical line boundary" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root =
      Filename.concat
        "/tmp"
        ("ochat-developer-bootstrap-"
         ^ (P.Id.Transaction.create () |> P.Id.Transaction.to_string))
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / root) in
    Eio.Path.mkdir ~perm:0o700 path;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path)
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let socket_path = Filename.concat root "bootstrap.sock" in
          let listener =
            Eio.Net.listen ~sw ~backlog:2 (Eio.Stdenv.net env) (`Unix socket_path)
          in
          let id = P.Envelope.Request_id.of_json (`Number "2") |> T.must in
          let success_framing =
            P.Envelope.success ~id `Null
            |> P.Envelope.to_json
            |> Jsonaf.to_string
            |> String.length
            |> fun length -> length - 4
          in
          let empty_error = P.Error.invalid_request "" in
          let error_bytes =
            P.Error.to_json empty_error |> Jsonaf.to_string |> String.length
          in
          let error_framing =
            P.Envelope.failure ~id empty_error
            |> P.Envelope.to_json
            |> Jsonaf.to_string
            |> String.length
            |> fun length -> length - error_bytes
          in
          let module Bound = Agent_client.Transport.Developer_response_bound in
          let line_length =
            Int.max
              (Bound.max_result_bytes Bootstrap + success_framing)
              (Bound.max_error_bytes + error_framing)
          in
          let initialized = T.initialization () in
          let first_done, first_done_resolver = Eio.Promise.create () in
          let capabilities_seen = ref 0 in
          Eio.Fiber.fork ~sw (fun () ->
            List.iter [ false; true ] ~f:(fun expect_capabilities ->
              let flow, _ = Eio.Net.accept ~sw listener in
              let reader = Eio.Buf_read.of_flow flow ~max_size:line_length in
              let read expected_method result =
                match
                  Eio.Buf_read.line reader
                  |> Jsonaf.of_string
                  |> P.Envelope.of_json
                  |> T.must
                with
                | P.Envelope.Request request ->
                  assert (String.equal request.method_ expected_method);
                  if String.equal expected_method "developer.capabilities"
                  then Int.incr capabilities_seen;
                  P.Envelope.success ~id:request.id result
                  |> P.Envelope.to_json
                  |> Jsonaf.to_string
                  |> fun line -> Eio.Flow.copy_string (line ^ "\n") flow
                | Notification _ | Response _ -> failwith "expected socket request"
              in
              read
                "protocol.initialize"
                (P.Public.Result.to_json (public_initialized initialized));
              if expect_capabilities
              then
                read
                  "developer.capabilities"
                  (P.Developer_common.Capabilities.to_json T.capabilities)
              else Eio.Promise.await first_done;
              Eio.Flow.close flow));
          let connect max_line_length =
            Agent_transport_socket.Client.connect
              ~sw
              ~net:(Eio.Stdenv.net env)
              ~socket_path
              ~max_line_length
              ~notification_capacity:8
          in
          let first = connect (line_length - 1) in
          T.initialize first initialized;
          (match Agent_client.Connection.developer_capabilities first with
           | Error error ->
             assert (
               String.is_substring
                 error.message
                 ~substring:"closed response exceeds socket line limit")
           | Ok _ -> failwith "undersized socket line admitted bootstrap");
          Eio.Promise.resolve first_done_resolver ();
          Agent_client.Connection.close first;
          let second = connect line_length in
          T.initialize second initialized;
          let installed =
            Agent_client.Connection.developer_capabilities second |> T.must
          in
          assert (
            P.Developer_common.Capabilities.supports
              installed
              P.Developer_common.Method.Observe);
          assert (Int.equal !capabilities_seen 1);
          Agent_client.Connection.close second;
          print_endline "Developer bootstrap rejects N-1 and roundtrips at N")));
  [%expect {| Developer bootstrap rejects N-1 and roundtrips at N |}]
;;

let%expect_test "framed stdio gateway dispatches the closed REPL family" =
  Eio_main.run (fun _ ->
    Eio.Switch.run (fun sw ->
      let initialized = T.initialization ~repl:true () in
      let repl_calls = ref 0 in
      let connection =
        Agent_client.In_memory.create_with_developer_repl
          ~request:(function
            | P.Command.Protocol_initialize _ -> Ok (public_initialized initialized)
            | _ -> Error (P.Error.invalid_request "unexpected public command"))
          ~request_developer:(fun ~bound:_ _ ->
            Error (P.Error.invalid_request "unexpected Developer request"))
          ~request_repl:(fun ~max_request_bytes:_ ~bound:_ request ->
            Int.incr repl_calls;
            match request with
            | R.Request.Capabilities -> Ok (R.Capabilities.to_json capabilities)
            | Observe _ -> Ok (`Object [ "found", `False ])
            | _ -> Error (P.Error.invalid_request "unexpected REPL method"))
          ~notifications:(Eio.Stream.create 8)
          ~close:Fn.id
      in
      let line id method_ params =
        P.Envelope.request
          ~id:(P.Envelope.Request_id.of_json (`Number (Int.to_string id)) |> T.must)
          ~method_
          ~params
          ()
        |> P.Envelope.to_json
        |> Jsonaf.to_string
      in
      let input =
        String.concat
          ~sep:"\n"
          [ {|{"jsonrpc":"2.0","id":1,"method":"protocol.initialize","params":{"implementation":{"name":"repl-gateway-test","version":"1"},"protocol_min":{"major":2,"minor":0},"protocol_max":{"major":2,"minor":0},"features":["developer.run","developer.repl"],"event_encodings":["json"],"max_inbound_event_bytes":1048576}}|}
          ; line 2 "developer.repl.capabilities" (`Object [])
          ; line 3 "developer.repl.observe" (R.Lookup.to_json lookup)
          ; ""
          ]
      in
      let output = Buffer.create 1024 in
      let errors = ref [] in
      Agent_transport_stdio.Gateway.run
        ~sw
        ~connection
        ~input:(Eio.Flow.string_source input)
        ~output:(Eio.Flow.buffer_sink output)
        ~max_line_length:131072
        ~outgoing_capacity:8
        ~on_error:(fun error -> errors := error.P.Error.code :: !errors);
      let responses =
        Buffer.contents output
        |> String.split_lines
        |> List.filter ~f:(Fn.non String.is_empty)
        |> List.map ~f:(fun line ->
          match Jsonaf.of_string line |> P.Envelope.of_json with
          | Ok (Response { outcome = Ok _; _ }) -> true
          | Ok (Response { outcome = Error _; _ })
          | Ok (Request _ | Notification _)
          | Error _ -> false)
      in
      assert (List.equal Bool.equal responses [ true; true; true ]);
      assert (Int.equal !repl_calls 2);
      assert (List.is_empty !errors);
      print_endline
        "stdio REPL capabilities and Observe dispatched through typed Connection"));
  [%expect {| stdio REPL capabilities and Observe dispatched through typed Connection |}]
;;
