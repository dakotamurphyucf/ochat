open! Core

let http_reply ?(status = "200 OK") ?(headers = "") body =
  sprintf
    "HTTP/1.1 %s\r\nConnection: close\r\nContent-Length: %d\r\n%s\r\n%s"
    status
    (String.length body)
    headers
    body
;;

module Repl_fixture = struct
  module A = Agent_protocol
  module R = A.Developer_repl_wire
  module C = Agent_client.Connection

  let ok = function
    | Ok value -> value
    | Error error -> raise_s [%sexp (error : A.Error.t)]
  ;;

  let limits =
    let document bytes =
      match
        Document_schema.Limits.create
          ~max_bytes:bytes
          ~max_depth:32
          ~max_fields:256
          ~max_nodes:512
      with
      | Ok value -> value
      | Error _ -> failwith "invalid HTTP REPL fixture document bound"
    in
    R.Limits.create
      ~request:(document 4096)
      ~result:(document 4096)
      ~page:(document 2048)
      ~response:(document 8192)
      ~max_source_bytes:512
      ~max_reference_bytes:384
      ~max_output_bytes:512
      ~max_page_items:2
      ~max_event_bytes:128
      ~max_registry_entries:16
    |> ok
  ;;

  let instance = R.Wire_id.Instance.of_string "ri_http_fixture" |> ok
  let key = A.Idempotency_key.of_string "http-control" |> ok
  let lookup = R.Lookup.create ~instance ~key
  let generator = A.Id.Generator.create ~bytes:(fun length -> String.make length 'h')

  let capabilities =
    R.Capabilities.create
      ~methods:
        [ R.Request.method_name R.Request.Capabilities
        ; R.Request.method_name (R.Request.Observe lookup)
        ; R.Request.method_name (R.Request.Cancel_submission lookup)
        ; R.Request.method_name
            (R.Request.Task_cancel_and_wait
               (A.Developer_common.Lookup.create
                  ~run_id:(A.Id.Developer_run.create_with generator)
                  ~key))
        ; "developer.repl.submit"
        ]
      ~limits
    |> ok
  ;;

  let initialized =
    let implementation =
      A.Initialize.Implementation.create ~name:"http-control-fixture" ~version:"1" |> ok
    in
    let principal =
      A.Principal.create
        ~id:(A.Id.Principal.create_with generator)
        ~authentication_kind:"http.static_bearer"
        ~scopes:A.Scope.Set.empty
        ~attributes:[]
      |> ok
    in
    A.Initialize.Response.create
      ~protocol_name:"ochat.agent"
      ~selected_version:A.Version.current
      ~implementation
      ~server_id:(A.Id.Server.create_with generator)
      ~enabled_features:[ R.Features.repl ]
      ~extensions:None
      ~principal
      ~limits:
        { max_request_bytes = 16 * 1024 * 1024
        ; max_event_bytes = 16 * 1024 * 1024
        ; max_page_size = 1000
        ; max_attachments_per_connection = 8
        }
      ~event_retention:
        { minimum_age_ms = 1000
        ; maximum_events = 1024
        ; oldest_replayable_sequence = None
        }
      ~timing:
        { heartbeat_interval_ms = 1000
        ; owner_lease_duration_ms = 1000
        ; owner_renew_after_ms = 500
        ; disconnect_grace_default_ms = 100
        }
      ~server_time:(A.Timestamp.of_string "2026-10-20T00:00:00Z" |> ok)
    |> ok
  ;;

  let initialize connection =
    let request =
      A.Initialize.Request.create
        ~implementation:
          (A.Initialize.Implementation.create ~name:"http-control-fixture" ~version:"1"
           |> ok)
        ~protocol_min:A.Version.current
        ~protocol_max:A.Version.current
        ~features:[ R.Features.repl ]
        ~event_encodings:[ Json ]
        ~max_inbound_event_bytes:4096
        ()
      |> ok
    in
    (match C.request_without_history connection (Protocol_initialize request) |> ok with
     | Protocol_initialize _ -> ()
     | _ -> failwith "HTTP fixture Initialize changed method");
    let initialized_as = Option.value_exn (C.initialization connection) in
    let caps = C.repl_capabilities connection |> ok in
    initialized_as, R.Capabilities.limits caps
  ;;

  let success id result =
    A.Envelope.success
      ~id:(A.Envelope.Request_id.of_json (`Number (Int.to_string id)) |> ok)
      result
    |> A.Envelope.to_json
    |> Jsonaf.to_string
  ;;

  let read_request flow =
    let reader = Eio.Buf_read.of_flow flow ~max_size:16384 in
    let request = Eio.Buf_read.line reader in
    let rec headers length =
      match Eio.Buf_read.line reader |> String.strip with
      | "" -> request, Eio.Buf_read.take length reader
      | line ->
        let length =
          match String.lsplit2 line ~on:':' with
          | Some (name, value) when String.Caseless.equal name "content-length" ->
            String.strip value |> Int.of_string
          | _ -> length
        in
        headers length
    in
    headers 0
  ;;

  let with_server env handler f =
    Eio.Switch.run (fun server_sw ->
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
        | `Unix _ -> failwith "expected TCP fixture"
      in
      Eio.Fiber.fork_daemon ~sw:server_sw (fun () ->
        Eio.Net.run_server listener handler ~on_error:(function
          | End_of_file | Eio.Io _ -> ()
          | exn -> raise exn));
      Eio.Switch.run (fun sw ->
        let connection =
          Agent_transport_http.Client.connect
            ~sw
            ~env
            ~uri:(Uri.of_string (sprintf "http://127.0.0.1:%d" port))
            ~bearer_token:None
            ~notification_capacity:8
          |> ok
        in
        Exn.protect ~f:(fun () -> f sw connection) ~finally:(fun () -> C.close connection)))
  ;;
end

let%test_unit "HTTP REPL control requires and preserves one logical connection identity" =
  let module F = Repl_fixture in
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      List.iter [ `Absent; `Changed ] ~f:(fun case ->
        let posts = ref 0 in
        let deletes = ref 0 in
        let handler flow _address =
          let request, _body = F.read_request flow in
          if String.is_prefix request ~prefix:"POST /v1/rpc "
          then (
            incr posts;
            let result =
              if Int.equal !posts 1
              then F.A.Initialize.Response.to_json F.initialized
              else if Int.equal !posts 2
              then F.R.Capabilities.to_json F.capabilities
              else `Object []
            in
            let headers =
              match case, !posts with
              | `Absent, _ -> ""
              | `Changed, 3 -> "ochat-connection-id: foreign-connection\r\n"
              | `Changed, _ -> "ochat-connection-id: fixture-connection\r\n"
            in
            Eio.Flow.copy_string (http_reply ~headers (F.success !posts result)) flow)
          else if String.is_prefix request ~prefix:"GET /v1/events "
          then (
            Eio.Flow.copy_string
              "HTTP/1.1 200 OK\r\n\
               Content-Type: text/event-stream\r\n\
               Transfer-Encoding: chunked\r\n\
               \r\n"
              flow;
            Eio.Fiber.await_cancel ())
          else if String.is_prefix request ~prefix:"DELETE /v1/connection "
          then (
            incr deletes;
            Eio.Flow.copy_string (http_reply "") flow)
          else failwith "unexpected HTTP fixture request"
        in
        F.with_server env handler (fun _sw connection ->
          let initialized_as, limits = F.initialize connection in
          (match
             F.C.request_repl_control
               connection
               ~initialized_as
               ~limits
               (F.C.Repl_control.Cancel_submission F.lookup)
           with
           | Error _ -> ()
           | Ok _ -> failwith "invalid logical identity admitted REPL control");
          (match case with
           | `Absent -> assert (!posts = 2)
           | `Changed ->
             assert (!posts = 3);
             assert (
               Result.is_error
                 (F.C.request_repl connection ~limits (F.R.Request.Observe F.lookup))));
          F.C.close connection;
          assert (!deletes = 0)))))
;;

let%test_unit "cancelled HTTP REPL control retires the owner promptly" =
  let module F = Repl_fixture in
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      let control_entered, signal_control_entered = Eio.Promise.create () in
      let control_closed, signal_control_closed = Eio.Promise.create () in
      let posts = ref 0 in
      let deletes = ref 0 in
      let handler flow _address =
        let request, _body = F.read_request flow in
        if String.is_prefix request ~prefix:"POST /v1/rpc "
        then (
          incr posts;
          match !posts with
          | (1 | 2) as id ->
            let result =
              if Int.equal id 1
              then F.A.Initialize.Response.to_json F.initialized
              else F.R.Capabilities.to_json F.capabilities
            in
            Eio.Flow.copy_string
              (http_reply
                 ~headers:"ochat-connection-id: fixture-connection\r\n"
                 (F.success id result))
              flow
          | 3 ->
            Eio.Promise.resolve signal_control_entered ();
            let reader = Eio.Buf_read.of_flow flow ~max_size:1024 in
            (match Eio.Buf_read.line reader with
             | _ -> failwith "retired HTTP control accepted another request"
             | (exception End_of_file) | (exception Eio.Io _) -> ());
            Eio.Promise.resolve signal_control_closed ()
          | _ -> failwith "retired HTTP owner sent another POST")
        else if String.is_prefix request ~prefix:"GET /v1/events "
        then (
          Eio.Flow.copy_string
            "HTTP/1.1 200 OK\r\n\
             Content-Type: text/event-stream\r\n\
             Transfer-Encoding: chunked\r\n\
             \r\n"
            flow;
          Eio.Fiber.await_cancel ())
        else if String.is_prefix request ~prefix:"DELETE /v1/connection "
        then (
          incr deletes;
          Eio.Flow.copy_string (http_reply "") flow)
        else failwith "unexpected HTTP control fixture request"
      in
      F.with_server env handler (fun _sw connection ->
        let initialized_as, limits = F.initialize connection in
        let cancelled = ref false in
        Eio.Fiber.first
          (fun () ->
             match
               F.C.request_repl_control
                 connection
                 ~initialized_as
                 ~limits
                 (F.C.Repl_control.Cancel_submission F.lookup)
             with
             | _ -> failwith "cancelled HTTP control returned an ordinary receipt"
             | exception (Eio.Cancel.Cancelled _ as exn) ->
               let backtrace = Stdlib.Printexc.get_raw_backtrace () in
               cancelled := true;
               Exn.raise_with_original_backtrace exn backtrace)
          (fun () -> Eio.Promise.await control_entered);
        assert !cancelled;
        Eio.Promise.await control_closed;
        assert (
          Result.is_error
            (F.C.request_repl connection ~limits (F.R.Request.Observe F.lookup)));
        F.C.close connection;
        assert (!posts = 3);
        assert (!deletes = 0))))
;;

let%test_unit "HTTP close joins held ordinary and REPL control RPC lanes" =
  let module F = Repl_fixture in
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      let ordinary_entered, signal_ordinary_entered = Eio.Promise.create () in
      let control_entered, signal_control_entered = Eio.Promise.create () in
      let ordinary_done, signal_ordinary_done = Eio.Promise.create () in
      let control_done, signal_control_done = Eio.Promise.create () in
      let posts = ref 0 in
      let deletes = ref 0 in
      let handler flow _address =
        let request, _body = F.read_request flow in
        if String.is_prefix request ~prefix:"POST /v1/rpc "
        then (
          incr posts;
          match !posts with
          | (1 | 2) as id ->
            let result =
              if Int.equal id 1
              then F.A.Initialize.Response.to_json F.initialized
              else F.R.Capabilities.to_json F.capabilities
            in
            Eio.Flow.copy_string
              (http_reply
                 ~headers:"ochat-connection-id: fixture-connection\r\n"
                 (F.success id result))
              flow
          | 3 ->
            Eio.Promise.resolve signal_ordinary_entered ();
            Eio.Fiber.await_cancel ()
          | 4 ->
            Eio.Promise.resolve signal_control_entered ();
            Eio.Fiber.await_cancel ()
          | _ -> failwith "HTTP held fixture accepted an extra POST")
        else if String.is_prefix request ~prefix:"GET /v1/events "
        then (
          Eio.Flow.copy_string
            "HTTP/1.1 200 OK\r\n\
             Content-Type: text/event-stream\r\n\
             Transfer-Encoding: chunked\r\n\
             \r\n"
            flow;
          Eio.Fiber.await_cancel ())
        else if String.is_prefix request ~prefix:"DELETE /v1/connection "
        then (
          incr deletes;
          Eio.Flow.copy_string (http_reply "") flow)
        else failwith "unexpected HTTP held fixture request"
      in
      F.with_server env handler (fun sw connection ->
        let initialized_as, limits = F.initialize connection in
        let submit =
          F.R.Submit.create
            ~limits
            ~instance:F.instance
            ~epoch:(F.R.Wire_id.Epoch.of_string "re_http_fixture" |> F.ok)
            ~expected_revision:(F.R.Wire_id.Revision.of_string "rv_http_fixture" |> F.ok)
            ~key:F.key
            ~source:"1"
          |> F.ok
        in
        Eio.Fiber.fork ~sw (fun () ->
          Exn.protect
            ~f:(fun () ->
              ignore
                (Result.try_with (fun () ->
                   F.C.request_repl connection ~limits (F.R.Request.Submit submit))
                 : ((F.R.Response.t, F.A.Error.t) result, exn) result))
            ~finally:(fun () -> Eio.Promise.resolve signal_ordinary_done ()));
        Eio.Promise.await ordinary_entered;
        Eio.Fiber.fork ~sw (fun () ->
          Exn.protect
            ~f:(fun () ->
              ignore
                (Result.try_with (fun () ->
                   F.C.request_repl_control
                     connection
                     ~initialized_as
                     ~limits
                     (F.C.Repl_control.Cancel_submission F.lookup))
                 : ((F.R.Response.t, F.A.Error.t) result, exn) result))
            ~finally:(fun () -> Eio.Promise.resolve signal_control_done ()));
        Eio.Promise.await control_entered;
        F.C.close connection;
        Eio.Promise.await ordinary_done;
        Eio.Promise.await control_done;
        assert (!posts = 4);
        assert (!deletes = 0))))
;;
