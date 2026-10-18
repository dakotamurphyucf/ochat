open! Core
module P = Agent_protocol
module C = P.Developer_common
module R = P.Developer_repl_wire
module T = Developer_connection_tests
module RT = Repl_transport_tests

let consume_request reader =
  let rec headers length =
    match Eio.Buf_read.line reader |> String.strip with
    | "" -> Eio.Buf_read.take length reader
    | line ->
      let length =
        match String.lsplit2 line ~on:':' with
        | Some (name, value) when String.Caseless.equal name "content-length" ->
          String.strip value |> Int.of_string
        | Some _ | None -> length
      in
      headers length
  in
  match Eio.Buf_read.line reader with
  | exception End_of_file -> None
  | _ -> Some (headers 0)
;;

let reply body =
  sprintf
    "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Length: %d\r\n\r\n%s"
    (String.length body)
    body
;;

let%expect_test "Developer HTTP success and error responses have separate finite bounds" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Switch.run (fun sw ->
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
          | `Unix _ -> failwith "expected TCP listener"
        in
        let requests = ref 0 in
        let response_mode = ref `Oversized_success in
        let initialized = T.initialization () in
        let public_initialized =
          P.Public.Result.Non_history.of_internal (Protocol_initialize initialized)
          |> T.must
          |> fun value -> P.Public.Result.Non_history value
        in
        Eio.Fiber.fork_daemon ~sw (fun () ->
          Eio.Net.run_server
            listener
            (fun flow _ ->
               let reader = Eio.Buf_read.of_flow flow ~max_size:16384 in
               match consume_request reader with
               | None -> ()
               | Some body ->
                 let envelope =
                   body |> Jsonaf.of_string |> P.Envelope.of_json |> T.must
                 in
                 (match envelope with
                  | P.Envelope.Request request ->
                    Int.incr requests;
                    let response =
                      match request.method_ with
                      | "protocol.initialize" ->
                        P.Envelope.success
                          ~id:request.id
                          (P.Public.Result.to_json public_initialized)
                      | "developer.capabilities" ->
                        P.Envelope.success
                          ~id:request.id
                          (C.Capabilities.to_json T.capabilities)
                      | "developer.observe" ->
                        (match !response_mode with
                         | `Oversized_success ->
                           P.Envelope.success
                             ~id:request.id
                             (`String (String.make 5000 'x'))
                         | `Allowed_error ->
                           P.Envelope.failure
                             ~id:request.id
                             (P.Error.create
                                Invalid_request
                                ~message:(String.make 5000 'e')
                                ~retryable:false
                                ())
                         | `Oversized_error ->
                           P.Envelope.failure
                             ~id:request.id
                             (P.Error.create
                                Invalid_request
                                ~message:(String.make 70_000 'e')
                                ~retryable:false
                                ()))
                      | _ -> failwith "unexpected HTTP method"
                    in
                    P.Envelope.to_json response
                    |> Jsonaf.to_string
                    |> reply
                    |> Fn.flip Eio.Flow.copy_string flow
                  | Notification _ | Response _ -> failwith "expected request"))
            ~on_error:(function
              | Eio.Io _ -> ()
              | exn -> raise exn));
        let connect () =
          Agent_transport_http.Client.connect
            ~sw
            ~env
            ~uri:(Uri.of_string (sprintf "http://127.0.0.1:%d" port))
            ~bearer_token:None
            ~notification_capacity:8
          |> T.must
        in
        let connection = connect () in
        Exn.protect
          ~f:(fun () ->
            T.initialize connection initialized;
            let developer =
              Agent_client.Developer_connection.create connection |> T.must
            in
            let submission = T.submit () in
            let lookup =
              C.Lookup.create
                ~run_id:(C.Submit.run_id submission)
                ~key:(C.Submit.key submission)
            in
            assert (
              Result.is_error
                (Agent_client.Developer_connection.send
                   developer
                   (P.Developer_wire.Request.Observe lookup)));
            assert (Int.equal !requests 3);
            assert (
              Result.is_error
                (Agent_client.Developer_connection.send
                   developer
                   (P.Developer_wire.Request.Observe lookup)));
            assert (Int.equal !requests 3);
            response_mode := `Allowed_error;
            let second = connect () in
            Exn.protect
              ~f:(fun () ->
                T.initialize second initialized;
                let developer =
                  Agent_client.Developer_connection.create second |> T.must
                in
                (match
                   Agent_client.Developer_connection.send
                     developer
                     (P.Developer_wire.Request.Observe lookup)
                 with
                 | Error { code = Invalid_request; message; _ } ->
                   assert (String.length message = 5000)
                 | Error error ->
                   raise_s [%sexp "unexpected bounded error", (error : P.Error.t)]
                 | Ok _ -> failwith "large valid error was not returned");
                response_mode := `Oversized_error;
                assert (
                  Result.is_error
                    (Agent_client.Developer_connection.send
                       developer
                       (P.Developer_wire.Request.Observe lookup)));
                assert (Int.equal !requests 7);
                assert (
                  Result.is_error
                    (Agent_client.Developer_connection.send
                       developer
                       (P.Developer_wire.Request.Observe lookup)));
                assert (Int.equal !requests 7);
                print_endline
                  "success overflow rejected; larger valid error accepted; error \
                   overflow retired connection")
              ~finally:(fun () -> Agent_client.Connection.close second))
          ~finally:(fun () -> Agent_client.Connection.close connection))));
  [%expect
    {| success overflow rejected; larger valid error accepted; error overflow retired connection |}]
;;

let%expect_test
    "REPL HTTP enforces separate params, generated envelope, result, and error bounds"
  =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Switch.run (fun sw ->
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
          | `Unix _ -> failwith "expected TCP listener"
        in
        let initialized = ref (T.initialization ~repl:true ()) in
        let requests = ref 0 in
        let response_mode = ref `Valid in
        Eio.Fiber.fork_daemon ~sw (fun () ->
          Eio.Net.run_server
            listener
            (fun flow _ ->
               let reader = Eio.Buf_read.of_flow flow ~max_size:16384 in
               match consume_request reader with
               | None -> ()
               | Some body ->
                 let envelope =
                   body |> Jsonaf.of_string |> P.Envelope.of_json |> T.must
                 in
                 (match envelope with
                  | P.Envelope.Request request ->
                    Int.incr requests;
                    let response =
                      match request.method_ with
                      | "protocol.initialize" ->
                        P.Envelope.success
                          ~id:request.id
                          (P.Public.Result.to_json (RT.public_initialized !initialized))
                      | "developer.repl.capabilities" ->
                        P.Envelope.success
                          ~id:request.id
                          (R.Capabilities.to_json RT.capabilities)
                      | "developer.repl.observe" ->
                        (match !response_mode with
                         | `Valid ->
                           P.Envelope.success ~id:request.id (`Object [ "found", `False ])
                         | `Oversized_success ->
                           P.Envelope.success
                             ~id:request.id
                             (`String (String.make 9000 'x'))
                         | `Allowed_error ->
                           P.Envelope.failure
                             ~id:request.id
                             (P.Error.invalid_request (String.make 5000 'e'))
                         | `Oversized_error ->
                           P.Envelope.failure
                             ~id:request.id
                             (P.Error.invalid_request (String.make 70_000 'e')))
                      | _ -> failwith "unexpected HTTP method"
                    in
                    P.Envelope.to_json response
                    |> Jsonaf.to_string
                    |> reply
                    |> Fn.flip Eio.Flow.copy_string flow
                  | Notification _ | Response _ -> failwith "expected request"))
            ~on_error:(function
              | Eio.Io _ -> ()
              | exn -> raise exn));
        let connect () =
          Agent_transport_http.Client.connect
            ~sw
            ~env
            ~uri:(Uri.of_string (sprintf "http://127.0.0.1:%d" port))
            ~bearer_token:None
            ~notification_capacity:8
          |> T.must
        in
        let observe connection =
          Result.bind
            (Agent_client.Developer_repl_connection.create connection)
            ~f:(fun client ->
              Agent_client.Developer_repl_connection.observe client RT.lookup)
        in
        let first = connect () in
        Exn.protect
          ~f:(fun () ->
            T.initialize ~repl:true first !initialized;
            (match observe first with
             | Ok None -> ()
             | Ok _ | Error _ -> failwith "valid REPL HTTP response failed");
            response_mode := `Oversized_success;
            assert (Result.is_error (observe first));
            assert (Int.equal !requests 4))
          ~finally:(fun () -> Agent_client.Connection.close first);
        let request_bytes method_ params id =
          P.Envelope.request
            ~id:(P.Envelope.Request_id.of_json (`Number (Int.to_string id)) |> T.must)
            ~method_
            ~params
            ()
          |> P.Envelope.to_json
          |> Jsonaf.to_string
          |> String.length
        in
        let capability_bytes =
          request_bytes "developer.repl.capabilities" (`Object []) 2
        in
        let observe_bytes =
          request_bytes
            "developer.repl.observe"
            (R.Request.params (R.Request.Observe RT.lookup))
            3
        in
        assert (observe_bytes > capability_bytes);
        initialized
        := T.initialization ~repl:true ~max_request_bytes:(observe_bytes - 1) ();
        let second = connect () in
        Exn.protect
          ~f:(fun () ->
            T.initialize ~repl:true second !initialized;
            assert (Result.is_error (observe second));
            assert (Int.equal !requests 6))
          ~finally:(fun () -> Agent_client.Connection.close second);
        response_mode := `Valid;
        initialized := T.initialization ~repl:true ~max_request_bytes:observe_bytes ();
        let exact = connect () in
        Exn.protect
          ~f:(fun () ->
            T.initialize ~repl:true exact !initialized;
            (match observe exact with
             | Ok None -> ()
             | Ok _ -> failwith "exact REPL HTTP frame budget returned wrong result"
             | Error failure ->
               failwith ("exact REPL HTTP frame budget: " ^ failure.message));
            assert (Int.equal !requests 9))
          ~finally:(fun () -> Agent_client.Connection.close exact);
        initialized := T.initialization ~repl:true ();
        response_mode := `Allowed_error;
        let third = connect () in
        Exn.protect
          ~f:(fun () ->
            T.initialize ~repl:true third !initialized;
            (match observe third with
             | Error { code = Invalid_request; message; _ } ->
               assert (String.length message = 5000)
             | Error _ | Ok _ -> failwith "valid REPL error was not returned");
            response_mode := `Oversized_error;
            assert (Result.is_error (observe third));
            assert (Int.equal !requests 13);
            assert (Result.is_error (observe third));
            assert (Int.equal !requests 13))
          ~finally:(fun () -> Agent_client.Connection.close third);
        print_endline
          "REPL HTTP success overflow retired; generated frame budget enforced at exact \
           boundary; valid error accepted; error overflow retired")));
  [%expect
    {| REPL HTTP success overflow retired; generated frame budget enforced at exact boundary; valid error accepted; error overflow retired |}]
;;
