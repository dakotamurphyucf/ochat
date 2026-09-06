open Core
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Http_driver = Support.Http_driver
module Port_reservation = Support.Port_reservation
module Temporary_environment = Support.Temporary_environment
module Unix_driver = Support.Unix_driver

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
;;

let result_ok = function
  | Ok value -> value
  | Error message -> raise_s [%sexp "HTTP operation failed", (message : string)]
;;

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Port_reservation.create ~sw ~env in
    let port = Port_reservation.port reservation in
    Port_reservation.release reservation;
    port)
;;

let fixture env environment name =
  Config_fixture.create environment ~name ~http_port:(reserve_port env)
;;

let readiness_failure daemon error =
  raise_s
    [%sexp
      "daemon did not become ready"
    , { error : Daemon_process.readiness_error
      ; stdout = ((Daemon_process.stdout daemon).contents : string)
      ; stderr = ((Daemon_process.stderr daemon).contents : string)
      }]
;;

let stop_daemon env daemon =
  match Daemon_process.result daemon with
  | Some _ -> ()
  | None -> ignore (Daemon_process.stop daemon ~env ~grace_seconds:1.)
;;

let with_daemon ~sw env fixture config_path f =
  let daemon = Daemon_process.start ~sw ~env ~fixture ~config_path in
  let health =
    match Daemon_process.wait_ready daemon ~env ~timeout_seconds:5. with
    | Ok health -> health
    | Error error -> readiness_failure daemon error
  in
  Exn.protect ~f:(fun () -> f daemon health) ~finally:(fun () -> stop_daemon env daemon)
;;

let with_fixture_daemon env environment name f =
  let fixture = fixture env environment name in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (Config_fixture.config_path fixture) (fun daemon health ->
      f sw fixture daemon health))
;;

let with_client ~sw env fixture ?token f =
  let token = Option.value token ~default:(Config_fixture.admin_token fixture) in
  let client =
    Http_driver.create
      ~sw
      ~env
      ~port:(Config_fixture.http_port fixture)
      ~token:(Some token)
    |> result_ok
  in
  Exn.protect ~f:(fun () -> f client) ~finally:(fun () -> Http_driver.shutdown client)
;;

let raw_client ~sw env fixture =
  Http_driver.create ~sw ~env ~port:(Config_fixture.http_port fixture) ~token:None
  |> result_ok
;;

let initialize_body id =
  sprintf
    {|{"jsonrpc":"2.0","id":%d,"method":"protocol.initialize","params":{"implementation":{"name":"agent-server-e2e-http","version":"dev"},"protocol_min":{"major":1,"minor":0},"protocol_max":{"major":1,"minor":0},"features":[],"event_encodings":["json"],"max_inbound_event_bytes":16777216}}|}
    id
;;

let require_status response expected label =
  require
    (Int.equal response.Http_driver.status expected)
    (sprintf "%s returned HTTP %d instead of %d" label response.status expected)
;;

let test_static_auth_matrix env environment =
  with_fixture_daemon env environment "http-auth" (fun sw fixture _daemon _health ->
    let client = raw_client ~sw env fixture in
    let request label authorization expected =
      Http_driver.rpc_raw client ~headers:authorization (initialize_body 1)
      |> result_ok
      |> fun response -> require_status response expected label
    in
    request "absent bearer" [] 401;
    request "unknown bearer" [ "authorization", "Bearer unknown" ] 401;
    request "malformed bearer" [ "authorization", "Basic credential" ] 401;
    request
      "duplicate bearer"
      [ "authorization", "Bearer first"; "authorization", "Bearer second" ]
      401;
    request
      "valid bearer"
      [ "authorization", "Bearer " ^ Config_fixture.admin_token fixture ]
      200;
    require
      (Option.is_some (Http_driver.connection_id client))
      "valid bearer did not establish a logical connection";
    with_client ~sw env fixture (fun closer ->
      Http_driver.set_connection_id closer (Http_driver.connection_id client);
      Http_driver.close_connection closer
      |> result_ok
      |> fun response -> require_status response 200 "connection close"))
;;

let test_initialize_ping env environment =
  with_fixture_daemon env environment "http-initialize" (fun sw fixture _daemon health ->
    require health.Agent_protocol.Health.Response.ready "daemon health was not ready";
    with_client ~sw env fixture (fun client ->
      let initialized, response = Http_driver.initialize client |> protocol_ok in
      require_status response 200 "initialize";
      require
        (String.equal initialized.protocol_name "ochat.agent")
        "HTTP protocol name differs";
      let connection_id = Http_driver.connection_id client in
      require (Option.is_some connection_id) "initialize omitted the connection ID";
      let payload = `Object [ "probe", `String "http-e2e" ] in
      let rpc =
        Http_driver.request client (Protocol_ping { payload = Some payload })
        |> protocol_ok
      in
      require
        (Option.equal
           String.equal
           connection_id
           (Piaf.Headers.get rpc.response.headers "ochat-connection-id"))
        "ping changed or omitted the logical connection ID";
      match rpc.result with
      | Protocol_ping ping ->
        require ping.ready "HTTP ping did not report ready";
        require
          (Option.equal
             (fun left right ->
                String.equal (Jsonaf.to_string left) (Jsonaf.to_string right))
             ping.payload
             (Some payload))
          "HTTP ping payload changed"
      | _ -> fail "protocol.ping returned the wrong result variant"))
;;

let response_ids response =
  let json =
    Result.try_with (fun () -> Jsonaf.of_string response.Http_driver.body)
    |> Result.ok_exn
  in
  match json with
  | `Array values ->
    List.map values ~f:(fun value ->
      match Jsonaf.member_exn "id" value with
      | `Number id -> Int.of_string id
      | _ -> fail "batch response ID was not numeric")
  | _ -> fail "ordered batch response was not an array"
;;

let test_ordered_batch env environment =
  with_fixture_daemon env environment "http-batch" (fun sw fixture _daemon _health ->
    let client = raw_client ~sw env fixture in
    let body =
      sprintf
        "[%s,%s,%s]"
        (initialize_body 1)
        {|{"jsonrpc":"2.0","id":2,"method":"protocol.ping","params":{}}|}
        {|{"jsonrpc":"2.0","id":3,"method":"server.info","params":{}}|}
    in
    let headers = [ "authorization", "Bearer " ^ Config_fixture.admin_token fixture ] in
    let response = Http_driver.rpc_raw client ~headers body |> result_ok in
    require_status response 200 "ordered batch";
    require
      (List.equal Int.equal (response_ids response) [ 1; 2; 3 ])
      "batch responses did not preserve request order";
    Http_driver.close_connection client |> result_ok |> ignore)
;;

let test_notification_omission env environment =
  with_fixture_daemon
    env
    environment
    "http-notification"
    (fun sw fixture _daemon _health ->
       with_client ~sw env fixture (fun client ->
         ignore
           (Http_driver.initialize client |> protocol_ok
            : Agent_protocol.Initialize.Response.t * Http_driver.response);
         let response =
           Http_driver.notify client (Protocol_ping { payload = None }) |> result_ok
         in
         require_status response 204 "notification-only RPC";
         require (String.is_empty response.body) "notification-only RPC returned a body";
         let rpc =
           Http_driver.request client (Protocol_ping { payload = None }) |> protocol_ok
         in
         match rpc.result with
         | Protocol_ping ping ->
           require ping.ready "connection failed after a notification"
         | _ -> fail "post-notification ping returned the wrong result variant"))
;;

let page_request () = Agent_protocol.Page.Request.create ~limit:100 () |> protocol_ok

let catalog client =
  let prompt_request =
    Agent_protocol.Prompt.List_request.
      { page = page_request (); enabled = Some true; available = Some true }
  in
  let workspace_request =
    Agent_protocol.Workspace.List_request.
      { page = page_request (); kind = None; access = None; available = Some true }
  in
  let prompt =
    match
      (Http_driver.request client (Prompt_list prompt_request) |> protocol_ok).result
    with
    | Prompt_list page -> List.hd_exn page.items
    | _ -> fail "prompt.list returned the wrong result variant"
  in
  let workspace =
    match
      (Http_driver.request client (Workspace_list workspace_request) |> protocol_ok)
        .result
    with
    | Workspace_list page ->
      List.find_exn page.items ~f:(fun workspace ->
        String.equal workspace.name "physical")
    | _ -> fail "workspace.list returned the wrong result variant"
  in
  prompt, workspace
;;

let session_spec client =
  let prompt, workspace = catalog client in
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog prompt.id)
    ~workspace:(Configured workspace.id)
    ~liveness:Detached
    ~persistence:Durable
    ~permission_profile:"unattended"
    ~start_immediately:false
    ~labels:[ "suite", "http-e2e" ]
    ()
  |> protocol_ok
;;

let idempotency_key name = Agent_protocol.Idempotency_key.of_string name |> protocol_ok

let create_session client ~mode ~subscribe ~key =
  let request =
    Agent_protocol.Session.Create_request.
      { spec = session_spec client
      ; requested_mode = mode
      ; subscribe
      ; idempotency_key = idempotency_key key
      }
  in
  match (Http_driver.request client (Session_create request) |> protocol_ok).result with
  | Session_create created -> created
  | _ -> fail "session.create returned the wrong result variant"
;;

let detach_session client session attachment key =
  let request =
    Agent_protocol.Session.Detach_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; idempotency_key = idempotency_key key
      }
  in
  match (Http_driver.request client (Session_detach request) |> protocol_ok).result with
  | Session_detach mutation -> mutation
  | _ -> fail "session.detach returned the wrong result variant"
;;

let start_session client session attachment key =
  let request =
    Agent_protocol.Session.Start_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; queue_if_limited = false
      ; idempotency_key = idempotency_key key
      }
  in
  match (Http_driver.request client (Session_start request) |> protocol_ok).result with
  | Session_start mutation -> mutation
  | _ -> fail "session.start returned the wrong result variant"
;;

let sse_event env stream =
  Http_driver.Sse.next stream ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:5.
  |> result_ok
;;

let durable_sse_event env stream =
  let frame = sse_event env stream in
  require
    (Option.equal String.equal frame.event (Some "session.event"))
    "SSE durable event name differs";
  let event =
    Result.try_with (fun () -> Jsonaf.of_string frame.data)
    |> Result.map_error ~f:Exn.to_string
    |> Result.bind ~f:(fun json ->
      Agent_protocol.Event.Durable.of_json json
      |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message))
    |> result_ok
  in
  require
    (Option.equal String.equal frame.id (Some (Int64.to_string event.sequence)))
    "SSE frame ID differs from its durable sequence";
  event
;;

let rec collect_sse_through env stream previous through events =
  if Int64.(previous >= through)
  then List.rev events
  else (
    let event = durable_sse_event env stream in
    require
      (Int64.equal event.sequence Int64.(previous + 1L))
      "SSE durable event sequence was not contiguous";
    collect_sse_through env stream event.sequence through (event :: events))
;;

let snapshot_equal left right =
  String.equal
    (Agent_protocol.Snapshot.to_json left |> Jsonaf.to_string)
    (Agent_protocol.Snapshot.to_json right |> Jsonaf.to_string)
;;

let test_per_session_sse env environment =
  with_fixture_daemon
    env
    environment
    "http-session-sse"
    (fun sw fixture _daemon _health ->
       with_client ~sw env fixture (fun client ->
         ignore (Http_driver.initialize client |> protocol_ok);
         let created =
           create_session client ~mode:(Some Read_write) ~subscribe:false ~key:"create"
         in
         let attachment = (Option.value_exn created.attachment).attachment in
         let initial, _ =
           Http_driver.get_snapshot client created.session.id |> result_ok
         in
         let stream, response =
           Http_driver.open_session_events
             client
             ~sw
             ~session_id:created.session.id
             ~after_sequence:initial.latest_event_sequence
             ()
           |> result_ok
         in
         require_status response 200 "per-session SSE";
         let mutation = start_session client created.session attachment "start" in
         let events =
           collect_sse_through
             env
             stream
             initial.latest_event_sequence
             mutation.session.latest_event_sequence
             []
         in
         let projection =
           List.fold
             events
             ~init:(Agent_client.Projection.install_snapshot initial)
             ~f:(fun projection event ->
               Agent_client.Projection.apply_event projection event |> protocol_ok)
         in
         let final, _ = Http_driver.get_snapshot client created.session.id |> result_ok in
         let projected = Agent_client.Projection.snapshot projection in
         if not (snapshot_equal projected final)
         then
           raise_s
             [%sexp
               "SSE event projection differs from the authoritative snapshot"
             , { projected : Agent_protocol.Snapshot.t
               ; final : Agent_protocol.Snapshot.t
               }];
         Http_driver.Sse.close stream))
;;

let test_rpc_while_sse_open env environment =
  with_fixture_daemon env environment "http-dual" (fun sw fixture _daemon _health ->
    with_client ~sw env fixture (fun client ->
      ignore (Http_driver.initialize client |> protocol_ok);
      let created =
        create_session client ~mode:(Some Read_write) ~subscribe:true ~key:"create"
      in
      let stream, response = Http_driver.open_connection_events client ~sw |> result_ok in
      require_status response 200 "logical-connection SSE";
      let ping =
        Http_driver.request client (Protocol_ping { payload = None }) |> protocol_ok
      in
      (match ping.result with
       | Protocol_ping response -> require response.ready "RPC stalled while SSE was open"
       | _ -> fail "dual-connection ping returned the wrong result variant");
      let attachment = (Option.value_exn created.attachment).attachment in
      ignore (start_session client created.session attachment "start");
      let frame = sse_event env stream in
      let envelope =
        Result.try_with (fun () -> Jsonaf.of_string frame.data)
        |> Result.map_error ~f:Exn.to_string
        |> Result.bind ~f:(fun json ->
          Agent_protocol.Envelope.of_json json
          |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message))
        |> result_ok
      in
      (match envelope with
       | Notification { method_ = "session.event"; params } ->
         let event = Agent_protocol.Event.Durable.of_json params |> protocol_ok in
         require
           (Agent_protocol.Id.Session.compare event.session_id created.session.id = 0)
           "logical SSE delivered an event for another session"
       | _ -> fail "logical SSE did not deliver a session notification");
      Http_driver.Sse.close stream))
;;

let test_sse_cursor_replay env environment =
  with_fixture_daemon env environment "http-replay" (fun sw fixture _daemon _health ->
    with_client ~sw env fixture (fun client ->
      ignore (Http_driver.initialize client |> protocol_ok);
      let created =
        create_session client ~mode:(Some Read_write) ~subscribe:false ~key:"create"
      in
      let attachment = (Option.value_exn created.attachment).attachment in
      let mutation = start_session client created.session attachment "start" in
      let first_stream, _ =
        Http_driver.open_session_events
          client
          ~sw
          ~session_id:created.session.id
          ~after_sequence:0L
          ()
        |> result_ok
      in
      let first = durable_sse_event env first_stream in
      Http_driver.Sse.close first_stream;
      require
        Int64.(first.sequence < mutation.session.latest_event_sequence)
        "replay fixture produced only one event";
      let replay, _ =
        Http_driver.open_session_events
          client
          ~sw
          ~session_id:created.session.id
          ~last_event_id:first.sequence
          ()
        |> result_ok
      in
      let events =
        collect_sse_through
          env
          replay
          first.sequence
          mutation.session.latest_event_sequence
          []
      in
      require (not (List.is_empty events)) "cursor reconnect returned no replay events";
      require
        (Int64.equal (List.hd_exn events).sequence Int64.(first.sequence + 1L))
        "cursor reconnect duplicated or skipped the first replay event";
      Http_driver.Sse.close replay))
;;

let single_attachment_config fixture =
  Config_fixture.configuration fixture ()
  |> String.substr_replace_first
       ~pattern:"(max_attachments_per_session 16)"
       ~with_:"(max_attachments_per_session 1)"
  |> String.substr_replace_first
       ~pattern:"(idle_connection_timeout_ms 5000)"
       ~with_:"(idle_connection_timeout_ms 200)"
  |> Config_fixture.write_configuration fixture ~name:"single-attachment.sexp"
;;

let rec attach_after_cleanup env connection session_id deadline =
  let request =
    Agent_protocol.Session.Attach_request.
      { session_id
      ; requested_mode = Read_only
      ; subscribe = false
      ; after_sequence = None
      ; reclaim_token = None
      ; idempotency_key =
          idempotency_key
            (Agent_protocol.Id.Transaction.create ()
             |> Agent_protocol.Id.Transaction.to_string)
      }
  in
  match Agent_client.Connection.request connection (Session_attach request) with
  | Ok (Session_attach attached) -> attached
  | Error error when Agent_protocol.Error.equal_code error.code Resource_limit ->
    if Float.(Eio.Time.now (Eio.Stdenv.clock env) >= deadline)
    then
      raise_s
        [%sexp
          "SSE observer attachment was not cleaned", (error : Agent_protocol.Error.t)]
    else (
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
      attach_after_cleanup env connection session_id deadline)
  | Ok _ -> fail "cleanup attach returned the wrong result variant"
  | Error error ->
    raise_s [%sexp "cleanup attach failed", (error : Agent_protocol.Error.t)]
;;

let test_sse_cleanup env environment =
  let fixture = fixture env environment "http-cleanup" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (single_attachment_config fixture) (fun _daemon _health ->
      with_client ~sw env fixture (fun client ->
        ignore (Http_driver.initialize client |> protocol_ok);
        let created = create_session client ~mode:None ~subscribe:false ~key:"create" in
        let stream, _ =
          Http_driver.open_session_events client ~sw ~session_id:created.session.id ()
          |> result_ok
        in
        Http_driver.Sse.close stream;
        let connection =
          Unix_driver.connect ~sw ~env ~socket_path:(Config_fixture.unix_socket fixture)
        in
        Exn.protect
          ~f:(fun () ->
            ignore (Unix_driver.initialize connection |> protocol_ok);
            let deadline = Eio.Time.now (Eio.Stdenv.clock env) +. 2. in
            ignore (attach_after_cleanup env connection created.session.id deadline))
          ~finally:(fun () -> Agent_client.Connection.close connection))))
;;

let limited_http_config fixture ~max_connections ~idle_timeout_ms name =
  Config_fixture.configuration fixture ()
  |> String.substr_replace_first
       ~pattern:"(max_connections 32)"
       ~with_:(sprintf "(max_connections %d)" max_connections)
  |> String.substr_replace_first
       ~pattern:"(idle_connection_timeout_ms 5000)"
       ~with_:(sprintf "(idle_connection_timeout_ms %d)" idle_timeout_ms)
  |> Config_fixture.write_configuration fixture ~name
;;

let with_new_client ~sw env fixture f =
  let client =
    Http_driver.create
      ~sw
      ~env
      ~port:(Config_fixture.http_port fixture)
      ~token:(Some (Config_fixture.admin_token fixture))
    |> result_ok
  in
  Exn.protect ~f:(fun () -> f client) ~finally:(fun () -> Http_driver.shutdown client)
;;

let require_error_code (result : (_, Agent_protocol.Error.t) result) code label =
  match result with
  | Error error when Agent_protocol.Error.equal_code error.code code -> ()
  | Error error ->
    raise_s [%sexp (label : string), "unexpected error", (error : Agent_protocol.Error.t)]
  | Ok _ -> fail (label ^ " unexpectedly succeeded")
;;

let test_connection_limit env environment =
  let fixture = fixture env environment "http-limit" in
  let config =
    limited_http_config fixture ~max_connections:2 ~idle_timeout_ms:5000 "limit.sexp"
  in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture config (fun _daemon _health ->
      with_new_client ~sw env fixture (fun first ->
        with_new_client ~sw env fixture (fun second ->
          with_new_client ~sw env fixture (fun third ->
            ignore (Http_driver.initialize first |> protocol_ok);
            ignore (Http_driver.initialize second |> protocol_ok);
            require_error_code
              (Http_driver.initialize third)
              Resource_limit
              "third HTTP connection";
            Http_driver.close_connection first |> result_ok |> ignore;
            ignore (Http_driver.initialize third |> protocol_ok))))))
;;

let test_idle_reaping env environment =
  let fixture = fixture env environment "http-idle" in
  let config =
    limited_http_config fixture ~max_connections:1 ~idle_timeout_ms:200 "idle.sexp"
  in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture config (fun _daemon _health ->
      with_new_client ~sw env fixture (fun stale ->
        ignore (Http_driver.initialize stale |> protocol_ok);
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.6;
        require_error_code
          (Http_driver.request stale (Protocol_ping { payload = None }))
          Invalid_request
          "reaped HTTP connection";
        with_new_client ~sw env fixture (fun replacement ->
          ignore (Http_driver.initialize replacement |> protocol_ok)))))
;;

let error_from_response response =
  Result.try_with (fun () -> Jsonaf.of_string response.Http_driver.body)
  |> Result.map_error ~f:Exn.to_string
  |> Result.bind ~f:(fun json ->
    Agent_protocol.Error.of_json json
    |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message))
  |> result_ok
;;

let test_malformed_body env environment =
  with_fixture_daemon env environment "http-malformed" (fun sw fixture _daemon _health ->
    with_client ~sw env fixture (fun client ->
      ignore (Http_driver.initialize client |> protocol_ok);
      let response = Http_driver.rpc_raw client {|{"jsonrpc":|} |> result_ok in
      require_status response 400 "malformed RPC body";
      require
        (Agent_protocol.Error.equal_code
           (error_from_response response).code
           Invalid_request)
        "malformed body returned the wrong protocol error";
      match
        (Http_driver.request client (Protocol_ping { payload = None }) |> protocol_ok)
          .result
      with
      | Protocol_ping ping ->
        require ping.ready "malformed body damaged the logical connection"
      | _ -> fail "post-malformed ping returned the wrong result variant"))
;;

let test_oversized_body env environment =
  with_fixture_daemon env environment "http-oversized" (fun sw fixture _daemon _health ->
    with_client ~sw env fixture (fun client ->
      let body = String.make ((16 * 1024 * 1024) + 1) 'x' in
      let response = Http_driver.rpc_raw client body |> result_ok in
      require_status response 400 "oversized RPC body";
      require
        (Agent_protocol.Error.equal_code
           (error_from_response response).code
           Resource_limit)
        "oversized body returned the wrong protocol error";
      with_new_client ~sw env fixture (fun healthy ->
        let initialized, _ = Http_driver.initialize healthy |> protocol_ok in
        require
          (String.equal initialized.protocol_name "ochat.agent")
          "daemon stopped accepting RPC after an oversized body")))
;;

let cases =
  [ "http.static-auth-matrix", test_static_auth_matrix
  ; "http.initialize-ping", test_initialize_ping
  ; "http.ordered-batch", test_ordered_batch
  ; "http.notification-omission", test_notification_omission
  ; "http.per-session-sse", test_per_session_sse
  ; "http.rpc-while-sse-open", test_rpc_while_sse_open
  ; "http.sse-cursor-replay", test_sse_cursor_replay
  ; "http.sse-cleanup", test_sse_cleanup
  ; "http.connection-limit", test_connection_limit
  ; "http.idle-reaping", test_idle_reaping
  ; "http.malformed-body", test_malformed_body
  ; "http.oversized-body", test_oversized_body
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown http-transport case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"http-transport" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (_name, test) -> test env environment);
    print_s
      [%sexp
        { scenario = ("http-transport" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }])
;;
