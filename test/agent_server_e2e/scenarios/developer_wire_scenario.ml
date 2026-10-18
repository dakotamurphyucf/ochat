open! Core
module P = Agent_protocol
module S = Agent_server
module D = P.Developer_common
module W = P.Developer_wire

let fail message = raise_s [%sexp "developer transport E2E failed", (message : string)]
let require condition message = if not condition then fail message

let ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "developer transport protocol error", (error : P.Error.t)]
;;

let transport_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "developer transport failure", (error : string)]
;;

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    port)
;;

let wire_limits () =
  let document ~bytes ~depth ~fields ~nodes =
    match
      Document_schema.Limits.create
        ~max_bytes:bytes
        ~max_depth:depth
        ~max_fields:fields
        ~max_nodes:nodes
    with
    | Ok value -> value
    | Error _ -> fail "invalid Developer wire fixture limits"
  in
  D.Limits.create
    ~request:(document ~bytes:(1024 * 1024) ~depth:16 ~fields:1024 ~nodes:2048)
    ~receipt:(document ~bytes:(1024 * 1024) ~depth:16 ~fields:1024 ~nodes:2048)
    ~response:(document ~bytes:(2 * 1024 * 1024) ~depth:24 ~fields:2048 ~nodes:4096)
    ~approval_page:(document ~bytes:(1024 * 1024) ~depth:16 ~fields:1024 ~nodes:2048)
    ~max_output_bytes:65536
    ~max_fixture_steps:4
    ~max_fixture_bytes:4096
    ~max_approval_display_bytes:4096
    ~max_approval_decision_reason_bytes:1024
    ~max_approval_page:8
    ~max_event_bytes:4096
  |> ok
;;

type raw_client =
  { send : P.Envelope.t -> P.Envelope.t
  ; close : unit -> unit
  ; mutable next_id : int
  }

let call client ~method_ ~params =
  let id = P.Envelope.Request_id.of_json (`Number (Int.to_string client.next_id)) |> ok in
  client.next_id <- client.next_id + 1;
  let request = P.Envelope.request ~id ~method_ ~params () in
  match client.send request with
  | P.Envelope.Response { id = received; outcome } ->
    require (P.Envelope.Request_id.compare id received = 0) "request ID changed";
    ok outcome
  | P.Envelope.Request _ | P.Envelope.Notification _ ->
    fail "request did not receive a response"
;;

let call_authoring client request =
  let method_ = P.Authoring_wire.Request.method_name request in
  let result = call client ~method_ ~params:(P.Authoring_wire.Request.params request) in
  P.Authoring_wire.Response.of_method_and_json ~method_ result |> ok
;;

let call_developer client ~limits request =
  let method_ = W.Request.method_name request in
  let result = call client ~method_ ~params:(W.Request.params request) in
  W.Response.of_method_and_json ~limits ~method_ result |> ok
;;

let initialize client =
  let implementation =
    P.Initialize.Implementation.create ~name:"developer-wire-e2e" ~version:"1" |> ok
  in
  let request =
    P.Initialize.Request.create
      ~implementation
      ~protocol_min:P.Version.current
      ~protocol_max:P.Version.current
      ~features:
        [ P.Authoring.Features.core; P.Authoring.Features.semantic; D.Features.run ]
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(2 * 1024 * 1024)
      ()
    |> ok
  in
  let response =
    call
      client
      ~method_:"protocol.initialize"
      ~params:(P.Initialize.Request.to_json request)
    |> P.Initialize.Response.of_json
    |> ok
  in
  require
    (List.mem response.enabled_features D.Features.run ~equal:String.equal)
    "Developer run feature was not selected"
;;

let open_and_analyze client =
  let source = {|let main input = Task.pure(input)|} in
  let opened =
    P.Authoring.Context.Open_request.create_supplied_document
      ~member:"draft.chatmd"
      ~default_environment:None
      ~contents:
        ("<script id=\"s\" language=\"chatml\" kind=\"tool\">" ^ source ^ "</script>")
    |> ok
  in
  let expected =
    match call_authoring client (P.Authoring_wire.Request.Context_open opened) with
    | P.Authoring_wire.Response.Context_open context ->
      P.Authoring.Context.Expected.of_view context
    | _ -> fail "Authoring Context_open returned another method"
  in
  let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
  let analyze =
    P.Authoring.Analysis.Analyze.Request.create ~context:expected ~target:selector |> ok
  in
  (match call_authoring client (P.Authoring_wire.Request.Analyze analyze) with
   | P.Authoring_wire.Response.Analyze _ -> ()
   | _ -> fail "Authoring Analyze returned another method");
  expected, selector
;;

let qualify_client client =
  Exn.protect
    ~f:(fun () ->
      initialize client;
      let caps =
        match call_developer client ~limits:(wire_limits ()) W.Request.Capabilities with
        | W.Response.Capabilities caps -> caps
        | _ -> fail "Developer capabilities returned another method"
      in
      require (D.Capabilities.supports caps D.Method.Submit) "Submit is not installed";
      require (D.Capabilities.supports caps D.Method.Observe) "Observe is not installed";
      require
        (D.Capabilities.supports caps D.Method.Await_terminal)
        "Await_terminal is not installed";
      let limits = D.Capabilities.limits caps in
      require
        (String.equal
           (D.Limits.to_json limits |> Jsonaf.to_string)
           (D.Limits.to_json (wire_limits ()) |> Jsonaf.to_string))
        "advertised Developer limits differ from installed policy";
      let expected, selector = open_and_analyze client in
      let route =
        D.Route.document_script ~limits:(D.Limits.request limits) ~expected ~selector
        |> ok
      in
      let run_id = P.Id.Developer_run.create () in
      let key = P.Idempotency_key.of_string "developer-wire-check" |> ok in
      let input = `Object [ "transport", `String "raw" ] in
      let submit =
        D.Submit.create
          ~limits
          ~run_id
          ~key
          ~kind:P.Developer_receipt.Kind.Check
          ~route
          ~target:D.Target.One_off_v1
          ~prompt:None
          ~workspace:None
          ~entrypoint:"main"
          ~input
          ~fixtures:None
          ~requested_execution:None
          ~requested_max_output_bytes:None
        |> ok
      in
      let submitted =
        match call_developer client ~limits (W.Request.Submit submit) with
        | W.Response.Submit view -> view
        | _ -> fail "Developer Submit returned another method"
      in
      let lookup = D.Lookup.create ~run_id ~key in
      let terminal =
        match call_developer client ~limits (W.Request.Await_terminal lookup) with
        | W.Response.Await_terminal view -> view
        | _ -> fail "Developer Await_terminal returned another method"
      in
      let observed =
        match call_developer client ~limits (W.Request.Observe lookup) with
        | W.Response.Observe (Some view) -> view
        | W.Response.Observe None -> fail "submitted Developer Check was not observable"
        | _ -> fail "Developer Observe returned another method"
      in
      require
        (P.Developer_command_id.equal
           (D.View.command_id submitted)
           (D.View.command_id observed))
        "Submit and Observe identified different commands";
      require
        (P.Developer_command_id.equal
           (D.View.command_id terminal)
           (D.View.command_id observed))
        "Await and Observe identified different commands";
      require
        (P.Id.Developer_run.equal (D.View.owner observed).run_id run_id)
        "observed Developer owner belongs to another run";
      require
        (P.Idempotency_key.equal (D.View.key observed) key)
        "observed Developer key changed";
      require
        (Chatml_source.Digest.equal
           (D.View.request_fingerprint submitted)
           (D.View.request_fingerprint observed))
        "Submit and Observe request fingerprints differ";
      require
        (Chatml_source.Digest.equal
           (D.View.capture_fingerprint submitted)
           (D.View.capture_fingerprint observed))
        "Submit and Observe capture fingerprints differ";
      match D.View.phase observed with
      | D.View.Terminal { execution_started = false; receipt } ->
        (match P.Developer_receipt.value receipt with
         | P.Developer_receipt.Check P.Developer_receipt.Check.Ready -> ()
         | _ -> fail "Developer Check did not produce Ready")
      | _ -> fail "Developer Check was not terminal")
    ~finally:client.close
;;

let http_client ~sw env fixture =
  let driver =
    Support.Http_driver.create
      ~sw
      ~env
      ~port:(Support.Config_fixture.http_port fixture)
      ~token:(Some (Support.Config_fixture.public_token fixture))
    |> transport_ok
  in
  { next_id = 1
  ; send =
      (fun request ->
        let response =
          Support.Http_driver.rpc_raw
            driver
            (P.Envelope.to_json request |> Jsonaf.to_string)
          |> transport_ok
        in
        require (Int.equal response.status 200) "HTTP Developer RPC failed";
        Jsonaf.parse response.body |> Or_error.ok_exn |> P.Envelope.of_json |> ok)
  ; close = (fun () -> Support.Http_driver.shutdown driver)
  }
;;

let rec read_unix_response raw clock remaining =
  if remaining = 0 then fail "too many Unix notifications before response";
  match Support.Unix_driver.read_envelope raw ~clock ~timeout_seconds:5. with
  | Support.Unix_driver.Envelope (P.Envelope.Notification _) ->
    read_unix_response raw clock (remaining - 1)
  | Support.Unix_driver.Envelope response -> response
  | Support.Unix_driver.End_of_file
  | Support.Unix_driver.Timeout
  | Support.Unix_driver.Invalid_response _ -> fail "Unix Developer response unavailable"
;;

let unix_client ~sw env fixture =
  let raw =
    Support.Unix_driver.connect_raw
      ~sw
      ~env
      ~socket_path:(Support.Config_fixture.unix_socket fixture)
      ~max_response_bytes:(2 * 1024 * 1024)
  in
  { next_id = 1
  ; send =
      (fun request ->
        Support.Unix_driver.send_line raw (P.Envelope.to_json request |> Jsonaf.to_string);
        read_unix_response raw (Eio.Stdenv.clock env) 4)
  ; close = (fun () -> Support.Unix_driver.close_raw raw)
  }
;;

let start_socket_listener ~sw env fixture daemon =
  let path = Support.Config_fixture.unix_socket fixture in
  Agent_transport_socket.Server.prepare_path ~env ~socket_path:path |> ok;
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Switch.run (fun listener_sw ->
      Agent_transport_socket.Server.run
        ~sw:listener_sw
        ~net:(Eio.Stdenv.net env)
        ~socket_path:path
        ~backlog:16
        ~dispatcher:(S.Daemon.dispatcher daemon)
        ~close_connection:(S.Daemon.close_connection daemon)
        ~authenticate:(fun flow _ ->
          Agent_transport_socket.Peer_credentials.authenticate_same_user_actor
            ~scopes:Support.Config_fixture.all_scopes
            flow)
        ~max_line_length:(16 * 1024 * 1024)
        ~outgoing_capacity:32
        ~max_attachments:
          S.Daemon.default_options.protocol_limits.max_attachments_per_connection
        ~on_error:raise
        ~on_protocol_error:(fun error ->
          raise_s [%sexp "Developer socket protocol error", (error : P.Error.t)]));
    `Stop_daemon);
  let rec await_socket attempts =
    if attempts = 0 then fail "Developer Unix listener did not become ready";
    match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
    | `Socket -> ()
    | _ ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
      await_socket (attempts - 1)
  in
  await_socket 250
;;

let run env ~case =
  (match case with
   | None -> ()
   | Some case -> raise_s [%sexp "unknown Developer wire E2E case", (case : string)]);
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
    Support.Temporary_environment.with_
      ~scenario:"developer-wire"
      ~env
      (fun environment ->
         let fixture =
           Support.Config_fixture.create
             environment
             ~name:"developer-wire"
             ~http_port:(reserve_port env)
         in
         Support.Config_fixture.grant_public_all_scopes fixture;
         let developer = Support.Developer_fixture.developer_configuration () in
         let portable =
           S.Daemon.Developer_configuration.Portable.create
             ~wire_limits:(wire_limits ())
             ~max_connections:4
             ~max_watches_per_connection:4
             ~max_pending_hints_per_connection:8
           |> ok
         in
         let options =
           { (Support.Daemon_host.with_offline_inference S.Daemon.default_options) with
             authoring = Some (Support.Developer_fixture.semantic_authoring_policy env)
           ; developer =
               Some
                 (S.Daemon.Developer_configuration.with_portable_wire developer portable)
           }
         in
         Support.Daemon_host.with_ env fixture ~options (fun sw daemon ->
           start_socket_listener ~sw env fixture daemon;
           qualify_client (http_client ~sw env fixture);
           qualify_client (unix_client ~sw env fixture))));
  Eio.Flow.copy_string "developer wire transports passed\n" (Eio.Stdenv.stdout env)
;;
