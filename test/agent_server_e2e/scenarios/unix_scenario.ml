open Core
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Port_reservation = Support.Port_reservation
module Process_manager = Support.Process_manager
module Temporary_environment = Support.Temporary_environment
module Unix_driver = Support.Unix_driver

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
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

let stop_if_running env daemon =
  match Daemon_process.result daemon with
  | Some _ -> ()
  | None -> ignore (Daemon_process.stop daemon ~env ~grace_seconds:1.)
;;

let with_daemon env fixture config_path f =
  Eio.Switch.run (fun sw ->
    let daemon = Daemon_process.start ~sw ~env ~fixture ~config_path in
    let health =
      match Daemon_process.wait_ready daemon ~env ~timeout_seconds:5. with
      | Ok health -> health
      | Error error -> readiness_failure daemon error
    in
    Exn.protect
      ~f:(fun () -> f daemon health)
      ~finally:(fun () -> stop_if_running env daemon))
;;

let with_fixture_daemon env environment name f =
  let fixture = fixture env environment name in
  with_daemon env fixture (Config_fixture.config_path fixture) (f fixture)
;;

let with_connection env fixture f =
  Eio.Switch.run (fun sw ->
    let connection =
      Unix_driver.connect ~sw ~env ~socket_path:(Config_fixture.unix_socket fixture)
    in
    Exn.protect
      ~f:(fun () -> f connection)
      ~finally:(fun () -> Agent_client.Connection.close connection))
;;

let initialize connection = Unix_driver.initialize connection |> protocol_ok
let idempotency_key name = Agent_protocol.Idempotency_key.of_string name |> protocol_ok

let catalog connection =
  let prompts = Agent_client.Catalog.prompts connection |> protocol_ok in
  let workspaces = Agent_client.Catalog.workspaces connection |> protocol_ok in
  prompts, workspaces
;;

let find_named values name name_of kind =
  List.find values ~f:(fun value -> String.equal (name_of value) name)
  |> Option.value_or_thunk ~default:(fun () -> fail (kind ^ " is missing: " ^ name))
;;

let prompt_and_workspace connection =
  let prompts, workspaces = catalog connection in
  let prompt =
    find_named prompts "smoke" (fun value -> value.Agent_protocol.Prompt.name) "prompt"
  in
  let workspace =
    find_named
      workspaces
      "physical"
      (fun value -> value.Agent_protocol.Workspace.name)
      "workspace"
  in
  prompt, workspace
;;

let session_spec connection =
  let prompt, workspace = prompt_and_workspace connection in
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog prompt.id)
    ~workspace:(Configured workspace.id)
    ~liveness:Detached
    ~persistence:Durable
    ~permission_profile:"unattended"
    ~start_immediately:false
    ~labels:[ "suite", "unix-e2e" ]
    ()
  |> protocol_ok
;;

let create_session connection ~mode ~subscribe ~key =
  let request =
    Agent_protocol.Session.Create_request.
      { spec = session_spec connection
      ; requested_mode = mode
      ; subscribe
      ; idempotency_key = idempotency_key key
      }
  in
  match Agent_client.Connection.request connection (Session_create request) with
  | Ok (Session_create created) -> created
  | Ok _ -> fail "session.create returned the wrong result variant"
  | Error error ->
    raise_s [%sexp "session.create failed", (error : Agent_protocol.Error.t)]
;;

let attach connection ~session_id ~mode ~subscribe ~after_sequence ~key =
  let request =
    Agent_protocol.Session.Attach_request.
      { session_id
      ; requested_mode = mode
      ; subscribe
      ; after_sequence
      ; reclaim_token = None
      ; idempotency_key = idempotency_key key
      }
  in
  match Agent_client.Connection.request connection (Session_attach request) with
  | Ok (Session_attach attached) -> Ok attached
  | Ok _ -> Error (Agent_protocol.Error.invalid_request "unexpected attach result")
  | Error error -> Error error
;;

let start_session connection session attachment key =
  let request =
    Agent_protocol.Session.Start_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; queue_if_limited = false
      ; idempotency_key = idempotency_key key
      }
  in
  match Agent_client.Connection.request connection (Session_start request) with
  | Ok (Session_start result) -> result.session
  | Ok _ -> fail "session.start returned the wrong result variant"
  | Error error ->
    raise_s [%sexp "session.start failed", (error : Agent_protocol.Error.t)]
;;

let durable_event envelope =
  match envelope with
  | Agent_protocol.Envelope.Notification { method_ = "session.event"; params } ->
    Agent_protocol.Event.Durable.of_json params |> protocol_ok
  | envelope ->
    raise_s [%sexp "unexpected notification", (envelope : Agent_protocol.Envelope.t)]
;;

let next_event env connection =
  match
    Unix_driver.next_notification
      connection
      ~clock:(Eio.Stdenv.clock env)
      ~timeout_seconds:2.
  with
  | `Notification envelope -> durable_event envelope
  | `Closed -> fail "notification connection closed"
  | `Timeout -> fail "timed out waiting for a session event"
;;

let rec collect_events_through env connection ~previous_sequence ~through_sequence events =
  if Int64.(previous_sequence >= through_sequence)
  then List.rev events
  else (
    let event = next_event env connection in
    let expected = Int64.(previous_sequence + 1L) in
    require
      (Int64.equal event.sequence expected)
      "subscriber event sequence was not contiguous";
    collect_events_through
      env
      connection
      ~previous_sequence:event.sequence
      ~through_sequence
      (event :: events))
;;

let test_initialize_ping env environment =
  with_fixture_daemon env environment "unix-initialize" (fun fixture _daemon health ->
    require health.Agent_protocol.Health.Response.ready "daemon health was not ready";
    with_connection env fixture (fun connection ->
      let initialized = initialize connection in
      require
        (String.equal initialized.protocol_name "ochat.agent")
        "protocol name differs";
      let payload = `Object [ "probe", `String "unix-e2e" ] in
      let ping = Unix_driver.ping connection ~payload:(Some payload) |> protocol_ok in
      require ping.ready "protocol ping did not report ready";
      require (not ping.draining) "protocol ping reported draining";
      require
        (Option.equal
           (fun first second ->
              String.equal (Jsonaf.to_string first) (Jsonaf.to_string second))
           ping.payload
           (Some payload))
        "ping payload changed"))
;;

let test_peer_principal_stability env environment =
  with_fixture_daemon env environment "unix-principal" (fun fixture _daemon _health ->
    let connect_once () =
      with_connection env fixture (fun connection -> (initialize connection).principal)
    in
    let first = connect_once () in
    let second = connect_once () in
    require
      (Agent_protocol.Id.Principal.compare first.id second.id = 0)
      "same-user principal changed between connections";
    require
      (String.equal first.authentication_kind "unix.peer")
      "Unix authentication kind differs";
    let expected_uid = Agent_transport_socket.Peer_credentials.effective_uid () in
    let actual_uid = List.Assoc.find first.attributes "unix.uid" ~equal:String.equal in
    require
      (Option.equal String.equal actual_uid (Some (Int.to_string expected_uid)))
      "Unix principal UID attribute differs")
;;

let test_catalog_list env environment =
  with_fixture_daemon env environment "unix-catalog" (fun fixture _daemon _health ->
    with_connection env fixture (fun connection ->
      ignore (initialize connection : Agent_protocol.Initialize.Response.t);
      let prompts, workspaces = catalog connection in
      require (List.length prompts = 1) "prompt catalog size differs";
      require (List.length workspaces = 2) "workspace catalog size differs";
      let prompt, workspace = prompt_and_workspace connection in
      require prompt.enabled "smoke prompt is disabled";
      require
        (Agent_protocol.Prompt.equal_availability prompt.availability Available)
        "smoke prompt is unavailable";
      require
        (Agent_protocol.Workspace.equal_kind workspace.kind Physical)
        "physical workspace kind differs"))
;;

let test_create_attach_subscribe env environment =
  with_fixture_daemon env environment "unix-subscribe" (fun fixture _daemon _health ->
    with_connection env fixture (fun writer ->
      ignore (initialize writer : Agent_protocol.Initialize.Response.t);
      let created =
        create_session writer ~mode:(Some Read_write) ~subscribe:false ~key:"create"
      in
      let writer_attachment =
        Option.value_exn created.attachment
        |> fun (value : Agent_protocol.Method_result.Attach.t) -> value.attachment
      in
      with_connection env fixture (fun reader ->
        ignore (initialize reader : Agent_protocol.Initialize.Response.t);
        let attached =
          attach
            reader
            ~session_id:created.session.id
            ~mode:Read_only
            ~subscribe:true
            ~after_sequence:(Some created.session.latest_event_sequence)
            ~key:"attach"
          |> protocol_ok
        in
        require
          (Agent_protocol.Session.equal_attachment_mode
             attached.attachment.mode
             Read_only)
          "read-only attachment mode changed";
        let started = start_session writer created.session writer_attachment "start" in
        let events =
          collect_events_through
            env
            reader
            ~previous_sequence:created.session.latest_event_sequence
            ~through_sequence:started.latest_event_sequence
            []
        in
        require (not (List.is_empty events)) "session.start published no durable event";
        let event = List.last_exn events in
        require
          (Agent_protocol.Id.Session.compare event.session_id started.id = 0)
          "subscriber received an event for another session";
        require
          (Int64.equal event.sequence started.latest_event_sequence)
          "subscriber cursor does not match acknowledged mutation")))
;;

let first_replay_event attached =
  match attached.Agent_protocol.Method_result.Attach.replay with
  | Events (event :: _) -> event
  | Events [] -> fail "attach replay was empty"
  | Current -> fail "attach replay unexpectedly returned current"
  | Snapshot _ -> fail "attach replay unexpectedly required a snapshot"
;;

let test_session_created_first env environment =
  with_fixture_daemon env environment "unix-created-first" (fun fixture _daemon _health ->
    with_connection env fixture (fun connection ->
      ignore (initialize connection : Agent_protocol.Initialize.Response.t);
      let created = create_session connection ~mode:None ~subscribe:false ~key:"create" in
      let attached =
        attach
          connection
          ~session_id:created.session.id
          ~mode:Read_only
          ~subscribe:false
          ~after_sequence:(Some 0L)
          ~key:"replay"
        |> protocol_ok
      in
      let event = first_replay_event attached in
      require
        (Agent_protocol.Event.Durable.equal_kind event.kind Session_created)
        "session.created was not the first durable event";
      require (Int64.equal event.sequence 1L) "first durable event sequence was not one"))
;;

let request_line id method_ params =
  let id =
    Agent_protocol.Envelope.Request_id.of_json (`Number (Int.to_string id)) |> protocol_ok
  in
  Agent_protocol.Envelope.request ~id ~method_ ~params ()
  |> Agent_protocol.Envelope.to_json
  |> Jsonaf.to_string
;;

let initialize_line =
  {|{"jsonrpc":"2.0","id":1,"method":"protocol.initialize","params":{"implementation":{"name":"unix-e2e-raw","version":"dev"},"protocol_min":{"major":1,"minor":0},"protocol_max":{"major":1,"minor":0},"features":[],"event_encodings":["json"],"max_inbound_event_bytes":16777216}}|}
;;

let require_success_response = function
  | Unix_driver.Envelope (Response { outcome = Ok _; _ }) -> ()
  | result ->
    raise_s [%sexp "expected successful response", (result : Unix_driver.raw_read)]
;;

let test_malformed_line env environment =
  with_fixture_daemon env environment "unix-malformed" (fun fixture _daemon _health ->
    Eio.Switch.run (fun sw ->
      let raw =
        Unix_driver.connect_raw
          ~sw
          ~env
          ~socket_path:(Config_fixture.unix_socket fixture)
          ~max_response_bytes:65_536
      in
      Unix_driver.send_line raw initialize_line;
      Unix_driver.read_envelope raw ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:2.
      |> require_success_response;
      Unix_driver.send_line raw {|{"jsonrpc":"2.0","id":2,"method":17,"params":{}}|};
      let malformed =
        Unix_driver.read_envelope raw ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:2.
      in
      (match malformed with
       | Envelope (Response { outcome = Error error; _ }) ->
         require
           (Agent_protocol.Error.equal_code error.code Invalid_request)
           "malformed envelope returned the wrong typed error"
       | result ->
         raise_s
           [%sexp "expected malformed-envelope failure", (result : Unix_driver.raw_read)]);
      Unix_driver.send_line raw (request_line 3 "protocol.ping" (`Object []));
      Unix_driver.read_envelope raw ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:2.
      |> require_success_response;
      Unix_driver.close_raw raw))
;;

let test_oversized_line env environment =
  with_fixture_daemon env environment "unix-oversized" (fun fixture daemon _health ->
    Eio.Switch.run (fun sw ->
      let raw =
        Unix_driver.connect_raw
          ~sw
          ~env
          ~socket_path:(Config_fixture.unix_socket fixture)
          ~max_response_bytes:65_536
      in
      Unix_driver.send_line raw initialize_line;
      Unix_driver.read_envelope raw ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:2.
      |> require_success_response;
      let oversized = String.make ((16 * 1024 * 1024) + 1) 'x' in
      ignore
        (Result.try_with (fun () -> Unix_driver.send_line raw oversized)
         : (unit, exn) result);
      let closed =
        match
          Unix_driver.read_envelope raw ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:5.
        with
        | Unix_driver.End_of_file -> true
        | Envelope _ | Timeout | Invalid_response _ -> false
        (* Linux can reset a socket closed with unread oversized input. Both
           EOF and reset establish rejection; a response or timeout does not. *)
        | exception Eio.Io (Eio.Net.E (Connection_reset _), _) -> true
      in
      require closed "oversized line did not close connection");
    let health =
      Daemon_process.health daemon ~env ~token:(Config_fixture.admin_token fixture)
      |> Result.ok_or_failwith
    in
    require health.ready "oversized-line rejection made the daemon unready")
;;

let single_attachment_config fixture =
  Config_fixture.configuration fixture ()
  |> String.substr_replace_first
       ~pattern:"(max_attachments_per_session 16)"
       ~with_:"(max_attachments_per_session 1)"
  |> Config_fixture.write_configuration fixture ~name:"single-attachment.sexp"
;;

let rec attach_until_cleaned env connection session_id deadline =
  match
    attach
      connection
      ~session_id
      ~mode:Read_only
      ~subscribe:false
      ~after_sequence:None
      ~key:
        (Agent_protocol.Id.Transaction.create ()
         |> Agent_protocol.Id.Transaction.to_string)
  with
  | Ok attached -> attached
  | Error error when Agent_protocol.Error.equal_code error.code Resource_limit ->
    if Float.(Eio.Time.now (Eio.Stdenv.clock env) >= deadline)
    then
      raise_s
        [%sexp
          "abrupt-disconnect attachment was not cleaned", (error : Agent_protocol.Error.t)]
    else (
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
      attach_until_cleaned env connection session_id deadline)
  | Error error ->
    raise_s [%sexp "cleanup verification attach failed", (error : Agent_protocol.Error.t)]
;;

let test_abrupt_disconnect_cleanup env environment =
  let fixture = fixture env environment "unix-abrupt" in
  with_daemon env fixture (single_attachment_config fixture) (fun _daemon _health ->
    let session_id =
      with_connection env fixture (fun connection ->
        ignore (initialize connection : Agent_protocol.Initialize.Response.t);
        let created =
          create_session connection ~mode:(Some Read_write) ~subscribe:false ~key:"create"
        in
        created.session.id)
    in
    with_connection env fixture (fun connection ->
      ignore (initialize connection : Agent_protocol.Initialize.Response.t);
      let deadline = Eio.Time.now (Eio.Stdenv.clock env) +. 2. in
      let attached = attach_until_cleaned env connection session_id deadline in
      require
        (Agent_protocol.Session.equal_attachment_mode attached.attachment.mode Read_only)
        "replacement attachment mode changed"))
;;

let cases =
  [ "unix.initialize-ping", test_initialize_ping
  ; "unix.peer-principal-stability", test_peer_principal_stability
  ; "unix.catalog-list", test_catalog_list
  ; "unix.create-attach-subscribe", test_create_attach_subscribe
  ; "unix.session-created-first", test_session_created_first
  ; "unix.malformed-line", test_malformed_line
  ; "unix.oversized-line", test_oversized_line
  ; "unix.abrupt-disconnect-cleanup", test_abrupt_disconnect_cleanup
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown unix-transport case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"unix-transport" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (_name, test) -> test env environment);
    print_s
      [%sexp
        { scenario = ("unix-transport" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }])
;;
