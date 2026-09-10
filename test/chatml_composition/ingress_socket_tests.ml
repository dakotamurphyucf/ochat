open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module Socket = Agent_transport_socket

let sources =
  [ ( "agent.chatmd"
    , [%blob "../chatml_extensibility_fixtures/x08-external-completion/agent.chatmd"] )
  ; ( "any.json"
    , [%blob "../chatml_extensibility_fixtures/x08-external-completion/any.json"] )
  ; ( "string.json"
    , [%blob "../chatml_extensibility_fixtures/x08-external-completion/string.json"] )
  ]
;;

let registration_from_output snapshot =
  List.find_map snapshot.P.Snapshot.canonical_history.entries ~f:(fun entry ->
    match
      Agent_session.History_codec.of_protocol entry |> protocol_ok |> History_entry.item
    with
    | Openai.Responses.Item.Function_call_output
        { call_id = "watch-call"; output = Text text; _ } ->
      (match Jsonaf.of_string text |> P.Invocation.outcome_of_json |> protocol_ok with
       | Pending (Subscription _, acknowledgement) ->
         let fields = P.Json_codec.fields acknowledgement |> protocol_ok in
         let id =
           P.Json_codec.required_as fields "registration_id" P.Id.Capability.of_json
           |> protocol_ok
         in
         let namespace =
           P.Json_codec.required_as fields "namespace" P.Json_codec.string |> protocol_ok
         in
         Some (id, namespace)
       | _ -> failwith "watch did not acknowledge its subscription")
    | _ -> None)
  |> Option.value_exn
;;

let initialize_request () =
  let implementation =
    P.Initialize.Implementation.create ~name:"ingress-helper" ~version:"test"
    |> protocol_ok
  in
  P.Initialize.Request.create
    ~implementation
    ~protocol_min:P.Version.ingress_minimum
    ~protocol_max:P.Version.current
    ~features:[]
    ~event_encodings:[ Json ]
    ~max_inbound_event_bytes:1048576
    ()
  |> protocol_ok
;;

let run_helper env socket_path payload =
  let envelopes =
    List.mapi
      [ "protocol.initialize", P.Initialize.Request.to_json (initialize_request ())
      ; "ingress.submit", payload
      ]
      ~f:(fun index (method_, params) ->
        let id =
          P.Envelope.Request_id.of_json (`Number (Int.to_string index)) |> protocol_ok
        in
        P.Envelope.request ~id ~method_ ~params ()
        |> P.Envelope.to_json
        |> Jsonaf.to_string)
  in
  let errors = Buffer.create 128 in
  let output =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      Eio.Process.parse_out
        (Eio.Stdenv.process_mgr env)
        Eio.Buf_read.take_all
        ~stdin:(Eio.Flow.string_source (String.concat ~sep:"\n" envelopes ^ "\n"))
        ~stderr:(Eio.Flow.buffer_sink errors)
        [ "../../bin/ochat_agent_stdio.exe"; "--connect"; "unix://" ^ socket_path ])
  in
  assert (Buffer.length errors = 0);
  let responses =
    String.split_lines output
    |> List.map ~f:(fun line ->
      match Jsonaf.of_string line |> P.Envelope.of_json |> protocol_ok with
      | Response response -> response.outcome |> protocol_ok
      | _ -> failwith "helper emitted a non-response")
  in
  match responses with
  | [ initialized; accepted ] ->
    let initialized = P.Initialize.Response.of_json initialized |> protocol_ok in
    [%test_eq: P.Scope.Set.t]
      (P.Scope.Set.of_list [ Submit_ingress ])
      initialized.principal.scopes;
    P.Ingress.Acknowledgement.of_json accepted |> protocol_ok
  | _ -> failwith "helper did not return exactly two protocol responses"
;;

let%expect_test
    "Unix peer-authenticated ingress preserves acknowledgements across helper reconnect \
     without granting transcript access"
  =
  let socket_path = ref "" in
  let model_received = ref false in
  let granted_scopes = ref scopes in
  let connect ~sw ~env ~root daemon =
    let path = Filename.concat root "ingress.sock" in
    socket_path := path;
    Socket.Server.prepare_path ~env ~socket_path:path |> protocol_ok;
    let listener =
      Eio.Net.listen ~sw ~reuse_addr:true ~backlog:8 (Eio.Stdenv.net env) (`Unix path)
    in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server
        ~on_error:raise
        listener
        (Socket.Server.serve
           ~dispatcher:(Agent_server.Daemon.dispatcher daemon)
           ~close_connection:(Agent_server.Daemon.close_connection daemon)
           ~authenticate:(fun flow _ ->
             Socket.Peer_credentials.authenticate_same_user ~scopes:!granted_scopes flow)
           ~max_line_length:(2 * 1024 * 1024)
           ~outgoing_capacity:128
           ~max_attachments:16
           ~on_protocol_error:(fun error -> raise_s [%sexp (error : P.Error.t)])));
    Socket.Client.connect
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~socket_path:path
      ~max_line_length:(2 * 1024 * 1024)
      ~notification_capacity:128
  in
  with_daemon
    ~connect
    ~sources
    ~expect_moderator:true
    ~expected_requests:3
    ~inspect_request:(fun request _ -> if request = 3 then model_received := true)
    ~calls:[ "watch-call", "watch", `Null ]
    ~after_turn:(fun env handle entry ->
      let snapshot = H.projection handle |> Agent_client.Projection.snapshot in
      let registration_id, namespace = registration_from_output snapshot in
      let producer = Option.value_exn snapshot.session.creator in
      let request : P.Ingress.Submit_request.t =
        { session_id = H.session_id handle
        ; registration_id
        ; namespace
        ; idempotency_key = P.Idempotency_key.of_string "socket-result" |> protocol_ok
        ; payload =
            `Object [ "type", `String "Permission_resolved"; "value", `String "ready" ]
        }
      in
      granted_scopes := P.Scope.Set.of_list [ Submit_ingress ];
      let wire f =
        Eio.Switch.run (fun sw ->
          let flow = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix !socket_path) in
          let input = Eio.Buf_read.of_flow flow ~max_size:(2 * 1024 * 1024) in
          let serial = ref 0 in
          let call method_ params =
            Int.incr serial;
            let id =
              P.Envelope.Request_id.of_json (`Number (Int.to_string !serial))
              |> protocol_ok
            in
            let envelope = P.Envelope.request ~id ~method_ ~params () in
            Eio.Flow.copy_string
              (Jsonaf.to_string (P.Envelope.to_json envelope) ^ "\n")
              flow;
            match
              Eio.Buf_read.line input
              |> Jsonaf.of_string
              |> P.Envelope.of_json
              |> protocol_ok
            with
            | Response response when P.Envelope.Request_id.compare id response.id = 0 ->
              response.outcome
            | _ -> failwith "unexpected socket response correlation"
          in
          let initialized =
            call
              "protocol.initialize"
              (P.Initialize.Request.to_json (initialize_request ()))
            |> protocol_ok
            |> P.Initialize.Response.of_json
            |> protocol_ok
          in
          [%test_eq: P.Scope.Set.t]
            (P.Scope.Set.of_list [ Submit_ingress ])
            initialized.principal.scopes;
          assert (P.Id.Principal.equal initialized.principal.id producer);
          f call)
      in
      let denied label = function
        | Error error ->
          print_s [%sexp (label : string), (error.P.Error.code : P.Error.code)]
        | Ok _ -> failwith (label ^ " was accepted")
      in
      let encoded = P.Ingress.Submit_request.to_json request in
      wire (fun call ->
        denied
          "transcript"
          (call
             "session.get"
             (`Object [ "session_id", P.Id.Session.to_json request.session_id ]));
        let forged =
          match encoded with
          | `Object fields ->
            `Object (("producer", P.Id.Principal.to_json producer) :: fields)
          | _ -> assert false
        in
        denied "caller producer field" (call "ingress.submit" forged);
        denied
          "wrong namespace"
          (call
             "ingress.submit"
             (P.Ingress.Submit_request.to_json
                { request with namespace = "external.other" }));
        denied
          "invalid completion schema"
          (call
             "ingress.submit"
             (P.Ingress.Submit_request.to_json { request with payload = `Object [] })));
      let first = run_helper env !socket_path encoded in
      Subscription_tests.await_subscription env entry;
      let retried = run_helper env !socket_path encoded in
      assert (P.Ingress.Acknowledgement.equal first retried);
      wire (fun call ->
        let retried =
          call "ingress.submit" encoded
          |> protocol_ok
          |> P.Ingress.Acknowledgement.of_json
          |> protocol_ok
        in
        assert (P.Ingress.Acknowledgement.equal first retried);
        denied
          "changed retry"
          (call
             "ingress.submit"
             (P.Ingress.Submit_request.to_json
                { request with payload = `Object [ "value", `String "different" ] }))))
    ~settle:(fun env entry ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
        let rec wait () =
          let state = A.state entry.Agent_server.Session_registry.actor |> protocol_ok in
          match !model_received, state.active_operation with
          | true, None -> ()
          | _ ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
            wait ()
        in
        wait ()))
    (fun state ->
       [%test_eq: int] 1 (List.length (List.hd_exn state.ingress_registrations).receipts);
       assert (List.is_empty state.permissions);
       let notifications =
         List.filter state.conversation.canonical_history ~f:(fun entry ->
           match entry.P.History.provenance with
           | Runtime_notification _ -> true
           | _ -> false)
       in
       [%test_eq: int] 1 (List.length notifications);
       (match
          Agent_session.History_codec.of_protocol (List.hd_exn notifications)
          |> protocol_ok
          |> History_entry.item
        with
        | Input_message { role = User; _ } -> ()
        | _ -> failwith "notification used an unsupported provider role");
       [%test_eq: int]
         1
         (List.count state.moderator_executions ~f:(fun execution ->
            P.Moderator_execution.equal_phase execution.context.phase Internal_event));
       print_endline
         "one data handler; identical acknowledgement after reconnect; no native \
          permission event");
  [%expect
    {|
    (transcript Permission_denied)
    ("caller producer field" Invalid_request)
    ("wrong namespace" Permission_denied)
    ("invalid completion schema" Invalid_request)
    ("changed retry" Conflict)
    one data handler; identical acknowledgement after reconnect; no native permission event
    |}]
;;
