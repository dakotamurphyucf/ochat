open Core
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Port_reservation = Support.Port_reservation
module Temporary_environment = Support.Temporary_environment
module Unix_driver = Support.Unix_driver

type session =
  { id : Agent_protocol.Id.Session.t
  ; writer : Agent_protocol.Session.Attachment.t
  ; revision : int64
  ; sequence : int64
  }

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message
let last_stage = ref "not started"

let trace env stage =
  last_stage := stage;
  if Option.equal String.equal (Sys.getenv "OCHAT_E2E_TRACE_LIFETIME") (Some "1")
  then Eio.Flow.copy_string ("multi-client: " ^ stage ^ "\n") (Eio.Stdenv.stdout env)
;;

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
;;

let request connection command =
  Agent_client.Connection.request connection command |> protocol_ok
;;

let idempotency_key value = Agent_protocol.Idempotency_key.of_string value |> protocol_ok

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Port_reservation.create ~sw ~env in
    let port = Port_reservation.port reservation in
    Port_reservation.release reservation;
    port)
;;

let fixture env environment name =
  let fixture = Config_fixture.create environment ~name ~http_port:(reserve_port env) in
  Config_fixture.grant_public_all_scopes fixture;
  fixture
;;

let approval_prompt =
  {|
<developer>Request one deterministic shell approval when a turn starts.</developer>
<shell_access id="approval" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false"
      child_processes="false" arbitrary_code="false" privilege_change="false"/>
  <resolver allow_relative_search_path="false">
    <executable id="pwd" path="/bin/pwd" trusted="true"/>
  </resolver>
  <backends merge="replace">
    <direct when="macos"/>
    <direct when="linux"/>
  </backends>
  <policy default="ask"/>
  <approvals provider="ui" unavailable="deny" scopes="once" durable="false"/>
  <audit format="jsonl" path="${session_dir}/authority-audit.jsonl"
      content="full" failure="deny_start"/>
</shell_access>
<moderator_runtime shell_runtime="approval"/>
<script id="approval-probe" language="chatml" kind="moderator">
  type state = int
  type event =
    [ `Session_start | `Session_resume | `Item_appended(item) | `Turn_start | `Turn_end ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx state event ->
      match event with
      | `Session_start -> Task.pure(state + 1)
      | `Session_resume -> Task.pure(state)
      | `Item_appended(_) -> Task.pure(state)
      | `Turn_start ->
        Task.bind(Process.run("/bin/pwd", []), fun output ->
        Task.pure(state + 1))
      | `Turn_end -> Task.pure(state)
</script>
|}
;;

let background_prompt =
  {|
<developer>Run one deterministic background event without a provider.</developer>
<script id="background" language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Session_resume | `Wake ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx state event ->
      match event with
      | `Session_start ->
        Task.bind(Schedule.after_ms(100, `Wake), fun schedule_id ->
        Task.pure(state + 1))
      | `Session_resume -> Task.pure(state)
      | `Wake -> Task.pure(state + 1)
</script>
|}
;;

let write_prompt fixture contents =
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path
       (Config_fixture.environment fixture)
       (Config_fixture.prompt_path fixture))
    contents
;;

let configure_approval_fixture fixture =
  write_prompt fixture approval_prompt;
  let configuration =
    Config_fixture.configuration fixture ()
    |> String.substr_replace_all
         ~pattern:"(tool_default deny)"
         ~with_:"(tool_default ask)"
    |> String.substr_replace_all
         ~pattern:"(manifest_authorization deny)"
         ~with_:"(manifest_authorization assume_authorized)"
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path
       (Config_fixture.environment fixture)
       (Config_fixture.config_path fixture))
    configuration
;;

let stop_daemon env daemon =
  match Daemon_process.result daemon with
  | Some _ -> ()
  | None -> ignore (Daemon_process.stop daemon ~env ~grace_seconds:1.)
;;

let wait_ready daemon env =
  match Daemon_process.wait_ready daemon ~env ~timeout_seconds:5. with
  | Ok _ -> ()
  | Error error ->
    raise_s
      [%sexp
        "daemon did not become ready"
      , (error : Daemon_process.readiness_error)
      , ((Daemon_process.stderr daemon).contents : string)]
;;

let initialize connection =
  ignore
    (Unix_driver.initialize connection |> protocol_ok
     : Agent_protocol.Initialize.Response.t)
;;

let connect_unix ~sw env fixture =
  let connection =
    Unix_driver.connect ~sw ~env ~socket_path:(Config_fixture.unix_socket fixture)
  in
  initialize connection;
  connection
;;

let connect_http ~sw env fixture =
  let uri =
    Uri.of_string (sprintf "http://127.0.0.1:%d" (Config_fixture.http_port fixture))
  in
  let connection =
    Agent_transport_http.Client.connect
      ~sw
      ~env
      ~uri
      ~bearer_token:(Some (Config_fixture.public_token fixture))
      ~notification_capacity:1_024
    |> protocol_ok
  in
  initialize connection;
  connection
;;

let blackhole_api_url ~sw env =
  let listener =
    Eio.Net.listen
      ~sw
      ~reuse_addr:false
      ~reuse_port:false
      ~backlog:1
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  match Eio.Net.listening_addr listener with
  | `Tcp (_, port) -> sprintf "http://127.0.0.1:%d" port
  | `Unix path -> raise_s [%sexp "loopback listener returned Unix path", (path : string)]
;;

let with_daemon env fixture f =
  let result =
    Eio.Switch.run (fun sw ->
      let config_path = Config_fixture.config_path fixture in
      let environment_overrides =
        [ "OPENAI_API_KEY", "e2e-multi-client"; "API_URL", blackhole_api_url ~sw env ]
      in
      let daemon =
        Daemon_process.start_with_environment_overrides
          ~sw
          ~env
          ~fixture
          ~environment_overrides
          ~config_path
      in
      Exn.protect
        ~f:(fun () ->
          wait_ready daemon env;
          trace env "daemon ready";
          let result = f sw in
          trace env "client scenario returned";
          result)
        ~finally:(fun () ->
          trace env "stopping daemon";
          stop_daemon env daemon;
          trace env "daemon stopped"))
  in
  trace env "daemon switch exited";
  result
;;

let with_clients ~sw env fixture f =
  let unix = connect_unix ~sw env fixture in
  let http = connect_http ~sw env fixture in
  Exn.protect
    ~f:(fun () -> f unix http)
    ~finally:(fun () ->
      trace env "closing HTTP client";
      Agent_client.Connection.close http;
      trace env "closing Unix client";
      Agent_client.Connection.close unix;
      trace env "clients closed")
;;

let catalog_prompt connection =
  Agent_client.Catalog.prompts connection
  |> protocol_ok
  |> List.find_exn ~f:(fun prompt -> String.equal prompt.name "smoke")
;;

let catalog_workspace connection =
  Agent_client.Catalog.workspaces connection
  |> protocol_ok
  |> List.find_exn ~f:(fun workspace -> String.equal workspace.name "physical")
;;

let session_spec
      ?(liveness = Agent_protocol.Session.Detached)
      ?(permission_profile = "unattended")
      connection
  =
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog (catalog_prompt connection).id)
    ~workspace:(Configured (catalog_workspace connection).id)
    ~liveness
    ~persistence:Durable
    ~permission_profile
    ~start_immediately:false
    ~labels:[ "suite", "multi-client" ]
    ()
  |> protocol_ok
;;

let session_of_created (created : Agent_protocol.Method_result.Create.t) =
  let attached = Option.value_exn created.attachment in
  { id = created.session.id
  ; writer = attached.attachment
  ; revision = created.session.revision
  ; sequence = created.session.latest_event_sequence
  }
;;

let create_session
      ?(mode = Agent_protocol.Session.Read_write)
      ?(liveness = Agent_protocol.Session.Detached)
      ?(permission_profile = "unattended")
      connection
      ~subscribe
      key
  =
  let create_request =
    Agent_protocol.Session.Create_request.
      { spec = session_spec connection ~liveness ~permission_profile
      ; requested_mode = Some mode
      ; subscribe
      ; idempotency_key = idempotency_key key
      }
  in
  match request connection (Session_create create_request) with
  | Session_create created -> session_of_created created
  | _ -> fail "session.create returned the wrong result variant"
;;

let attach connection session ~mode ~subscribe key =
  let attach_request =
    Agent_protocol.Session.Attach_request.
      { session_id = session.id
      ; requested_mode = mode
      ; subscribe
      ; after_sequence = Some session.sequence
      ; reclaim_token = None
      ; idempotency_key = idempotency_key key
      }
  in
  match request connection (Session_attach attach_request) with
  | Session_attach attached -> attached.attachment
  | _ -> fail "session.attach returned the wrong result variant"
;;

let start_session connection session key =
  let command =
    Agent_protocol.Command.Session_start
      { session_id = session.id
      ; attachment_id = session.writer.id
      ; queue_if_limited = false
      ; idempotency_key = idempotency_key key
      }
  in
  match request connection command with
  | Session_start result ->
    { session with
      revision = result.session.revision
    ; sequence = result.session.latest_event_sequence
    }
  | _ -> fail "session.start returned the wrong result variant"
;;

let stop_session connection session attachment key =
  let command =
    Agent_protocol.Command.Session_stop
      { session_id = session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; mode = Cancel
      ; idempotency_key = idempotency_key key
      }
  in
  match request connection command with
  | Session_stop _ -> ()
  | _ -> fail "session.stop returned the wrong result variant"
;;

let send_command session attachment text key =
  Agent_protocol.Command.Session_send_message
    { session_id = session.id
    ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
    ; content = { kind = Plain_text; text; attachments = [] }
    ; idempotency_key = idempotency_key key
    }
;;

let send connection session attachment text key =
  match request connection (send_command session attachment text key) with
  | Session_send_message sent -> sent
  | _ -> fail "session.send_message returned the wrong result variant"
;;

let get_session connection session_id =
  match request connection (Session_get { session_id; history = None }) with
  | Session_get snapshot -> snapshot.session
  | _ -> fail "session.get returned the wrong result variant"
;;

let page_request () = Agent_protocol.Page.Request.create ~limit:100 () |> protocol_ok

let permissions connection session_id state =
  let list_request =
    Agent_protocol.Permission.List_request.
      { session_id; page = page_request (); state = Some state }
  in
  match request connection (Permission_list list_request) with
  | Permission_list page -> page.items
  | _ -> fail "permission.list returned the wrong result variant"
;;

let rec await_pending_permission env connection session_id attempts =
  match permissions connection session_id Pending with
  | permission :: _ -> permission
  | [] when attempts > 0 ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_pending_permission env connection session_id (attempts - 1)
  | [] -> fail "session did not publish a pending permission"
;;

let respond_command session attachment permission choice key =
  Agent_protocol.Command.Permission_respond
    Agent_protocol.Permission.Respond_request.
      { session_id = session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; permission_id = permission.Agent_protocol.Permission.id
      ; permission_generation = permission.generation
      ; choice
      ; reason = Some "multi-client E2E response"
      ; idempotency_key = idempotency_key key
      }
;;

let respond connection session attachment permission choice key =
  Agent_client.Connection.request
    connection
    (respond_command session attachment permission choice key)
;;

let require_permission_race_result first second =
  let outcomes = [ first; second ] in
  let winners =
    List.count outcomes ~f:(function
      | Ok (Agent_protocol.Method_result.Permission_respond _) -> true
      | Ok _ | Error _ -> false)
  in
  let losers =
    List.count outcomes ~f:(function
      | Error (error : Agent_protocol.Error.t) ->
        Agent_protocol.Error.equal_code error.code Already_resolved
      | Ok _ -> false)
  in
  require (Int.equal winners 1) "permission race did not have exactly one winner";
  require (Int.equal losers 1) "permission race loser was not already_resolved"
;;

let history_ids_are_unique
      (sent_a : Agent_protocol.Method_result.Send_message.t)
      (sent_b : Agent_protocol.Method_result.Send_message.t)
  =
  Agent_protocol.History.Id.compare sent_a.history_id sent_b.history_id <> 0
;;

let get_snapshot connection session_id =
  match request connection (Session_get { session_id; history = None }) with
  | Session_get snapshot -> snapshot
  | _ -> fail "session.get returned wrong result"
;;

let assert_accepted_messages snapshot sent_a sent_b =
  let expected sent text =
    Agent_session.History_codec.user_text
      ~id:sent.Agent_protocol.Method_result.Send_message.history_id
      text
    |> Agent_session.History_codec.to_protocol
  in
  let accepted =
    [ sent_a, expected sent_a "from-unix"; sent_b, expected sent_b "from-http" ]
  in
  let expected =
    List.sort accepted ~compare:(fun (a, _) (b, _) ->
      Int64.compare a.mutation.revision b.mutation.revision)
    |> List.map ~f:snd
  in
  let canonical =
    snapshot.Agent_protocol.Snapshot.canonical_history.entries @ snapshot.deferred_entries
  in
  let actual =
    List.filter canonical ~f:(fun entry ->
      Agent_protocol.History.equal_role entry.role User)
  in
  if
    not
      (List.equal
         String.equal
         (List.map actual ~f:(fun entry ->
            Agent_protocol.History.entry_to_json entry |> Jsonaf.to_string))
         (List.map expected ~f:(fun entry ->
            Agent_protocol.History.entry_to_json entry |> Jsonaf.to_string)))
  then
    raise_s
      [%sexp
        "canonical writer messages differ"
      , (expected : Agent_protocol.History.entry list)
      , (snapshot : Agent_protocol.Snapshot.t)]
;;

let test_concurrent_messages env environment =
  let fixture = fixture env environment "multi-writers" in
  let session_id, sent_a, sent_b =
    with_daemon env fixture (fun sw ->
      with_clients ~sw env fixture (fun unix http ->
        let session = create_session unix ~subscribe:false "writers:create" in
        let http_writer =
          attach http session ~mode:Read_write ~subscribe:false "attach"
        in
        let session = start_session unix session "writers:start" in
        let sent_a, sent_b =
          Eio.Fiber.pair
            (fun () -> send unix session session.writer "from-unix" "send-unix")
            (fun () -> send http session http_writer "from-http" "send-http")
        in
        require
          (history_ids_are_unique sent_a sent_b)
          "concurrent messages reused history ID";
        require
          Int64.(sent_a.mutation.revision <> sent_b.mutation.revision)
          "concurrent messages did not receive one actor acceptance order";
        stop_session unix session session.writer "stop-writers";
        assert_accepted_messages (get_snapshot unix session.id) sent_a sent_b;
        assert_accepted_messages (get_snapshot http session.id) sent_a sent_b;
        session.id, sent_a, sent_b))
  in
  with_daemon env fixture (fun sw ->
    with_clients ~sw env fixture (fun unix http ->
      assert_accepted_messages (get_snapshot unix session_id) sent_a sent_b;
      assert_accepted_messages (get_snapshot http session_id) sent_a sent_b))
;;

let durable_notification = function
  | Agent_protocol.Envelope.Notification { method_ = "session.event"; params } ->
    Agent_protocol.Event.Durable.of_json params |> protocol_ok |> Option.some
  | Notification _ -> None
  | Request _ | Response _ -> fail "notification stream returned a non-notification"
;;

let rec next_event env connection session_id =
  match
    Eio.Time.with_timeout (Eio.Stdenv.clock env) 5. (fun () ->
      Ok (Agent_client.Connection.next_notification connection))
  with
  | Error `Timeout -> fail "timed out waiting for session event"
  | Ok None -> fail "notification connection closed"
  | Ok (Some envelope) ->
    (match durable_notification envelope with
     | Some event when Agent_protocol.Id.Session.compare event.session_id session_id = 0
       -> event
     | Some _ | None -> next_event env connection session_id)
;;

let rec collect_through env connection session_id previous through events =
  if Int64.(previous >= through)
  then List.rev events
  else (
    let event = next_event env connection session_id in
    require Int64.(event.sequence = previous + 1L) "observer event gap";
    collect_through env connection session_id event.sequence through (event :: events))
;;

let event_signature event = Agent_protocol.Event.Durable.to_json event |> Jsonaf.to_string

let test_same_event_order env environment =
  let fixture = fixture env environment "multi-observers" in
  try
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
      with_daemon env fixture (fun sw ->
        with_clients ~sw env fixture (fun unix http ->
          let session = create_session unix ~subscribe:true "observers:create" in
          ignore (attach http session ~mode:Read_only ~subscribe:true "observer" : _);
          let base_sequence = session.sequence in
          let session = start_session unix session "observers:start" in
          let sent = send unix session session.writer "ordered" "ordered-send" in
          let through = sent.mutation.latest_event_sequence in
          let unix_events =
            collect_through env unix session.id base_sequence through []
          in
          let http_events =
            collect_through env http session.id base_sequence through []
          in
          require
            (List.equal
               String.equal
               (List.map unix_events ~f:event_signature)
               (List.map http_events ~f:event_signature))
            "mixed-transport observers received different durable event order";
          stop_session unix session session.writer "stop-observers")))
  with
  | Eio.Time.Timeout -> fail ("observer teardown timed out after: " ^ !last_stage)
;;

let test_observer_rejected env environment =
  let fixture = fixture env environment "multi-readonly" in
  with_daemon env fixture (fun sw ->
    with_clients ~sw env fixture (fun unix http ->
      let session = create_session unix ~subscribe:false "readonly:create" in
      let reader = attach http session ~mode:Read_only ~subscribe:false "reader" in
      let before = get_session unix session.id in
      (match
         Agent_client.Connection.request http (send_command session reader "no" "reject")
       with
       | Error _ -> ()
       | Ok _ -> fail "read-only observer mutation succeeded");
      let after = get_session unix session.id in
      require
        (Int64.equal before.revision after.revision)
        "rejected observer advanced revision";
      require
        (Int64.equal before.latest_event_sequence after.latest_event_sequence)
        "rejected observer advanced event sequence"))
;;

let test_permission_race env environment =
  let fixture = fixture env environment "multi-permission-race" in
  configure_approval_fixture fixture;
  with_daemon env fixture (fun sw ->
    with_clients ~sw env fixture (fun unix http ->
      let session = create_session unix ~subscribe:false "permission-race:create" in
      let http_writer =
        attach http session ~mode:Read_write ~subscribe:false "permission-race:attach"
      in
      let session = start_session unix session "permission-race:start" in
      ignore
        (send unix session session.writer "request approval" "permission-race:send"
         : Agent_protocol.Method_result.Send_message.t);
      let permission = await_pending_permission env unix session.id 250 in
      let first, second =
        Eio.Fiber.pair
          (fun () ->
             respond unix session session.writer permission Deny "permission:unix")
          (fun () -> respond http session http_writer permission Deny "permission:http")
      in
      require_permission_race_result first second;
      require
        (Int.equal (List.length (permissions unix session.id Denied)) 1)
        "permission race did not durably resolve exactly one permission";
      stop_session unix session session.writer "permission-race:stop"))
;;

let require_permission_denied = function
  | Error (error : Agent_protocol.Error.t) ->
    require
      (Agent_protocol.Error.equal_code error.code Permission_denied)
      "read-only permission response returned the wrong error"
  | Ok _ -> fail "read-only attachment resolved a permission"
;;

let require_unchanged_session
      (before : Agent_protocol.Session.t)
      (after : Agent_protocol.Session.t)
  =
  require
    (Int64.equal before.Agent_protocol.Session.revision after.revision)
    "read-only permission response advanced revision";
  require
    (Int64.equal before.latest_event_sequence after.latest_event_sequence)
    "read-only permission response advanced event sequence"
;;

let resolve_permission connection session permission key =
  ignore
    (respond connection session session.writer permission Deny key |> protocol_ok
     : Agent_protocol.Method_result.t)
;;

let test_read_only_permission_rejected env environment =
  let fixture = fixture env environment "multi-permission-readonly" in
  configure_approval_fixture fixture;
  with_daemon env fixture (fun sw ->
    with_clients ~sw env fixture (fun unix http ->
      let session = create_session unix ~subscribe:false "permission-reader:create" in
      let reader =
        attach http session ~mode:Read_only ~subscribe:false "permission-reader:attach"
      in
      let session = start_session unix session "permission-reader:start" in
      ignore
        (send unix session session.writer "request approval" "permission-reader:send"
         : Agent_protocol.Method_result.Send_message.t);
      let permission = await_pending_permission env unix session.id 250 in
      let before = get_session unix session.id in
      respond http session reader permission Deny "permission-reader:reject"
      |> require_permission_denied;
      let after = get_session unix session.id in
      require_unchanged_session before after;
      resolve_permission unix session permission "permission-reader:resolve";
      stop_session unix session session.writer "permission-reader:stop"))
;;

let desired_running session =
  Agent_protocol.Session.equal_desired_state
    session.Agent_protocol.Session.desired_state
    Running
;;

let fully_stopped session =
  Agent_protocol.Session.equal_desired_state
    session.Agent_protocol.Session.desired_state
    Stopped
  &&
  match session.observed_state with
  | Stopped -> true
  | Queued_for_slot
  | Starting
  | Recovering
  | Idle
  | Running_turn _
  | Compacting _
  | Waiting_for_permission _
  | Stopping
  | Failed _ -> false
;;

let rec await_stopped env connection session_id attempts =
  let session = get_session connection session_id in
  if fully_stopped session
  then session
  else if attempts = 0
  then fail "owner-bound session did not stop after disconnect grace"
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_stopped env connection session_id (attempts - 1))
;;

let close_connections connections = List.iter connections ~f:Agent_client.Connection.close

let observe_send env observer session sent =
  ignore
    (collect_through
       env
       observer
       session.id
       session.sequence
       sent.Agent_protocol.Method_result.Send_message.mutation.latest_event_sequence
       []
     : Agent_protocol.Event.Durable.t list)
;;

let test_writer_disconnect_continues env environment =
  let fixture = fixture env environment "multi-detach-writer" in
  with_daemon env fixture (fun sw ->
    let unix = connect_unix ~sw env fixture in
    let http = connect_http ~sw env fixture in
    Exn.protect
      ~f:(fun () ->
        let session = create_session unix ~subscribe:false "detach-writer:create" in
        let survivor =
          attach http session ~mode:Read_write ~subscribe:false "detach-writer:attach"
        in
        let session = start_session unix session "detach-writer:start" in
        Agent_client.Connection.close unix;
        require
          (desired_running (get_session http session.id))
          "writer close stopped session";
        ignore
          (send http session survivor "after writer close" "detach-writer:send"
           : Agent_protocol.Method_result.Send_message.t);
        stop_session http session survivor "detach-writer:stop")
      ~finally:(fun () -> close_connections [ http; unix ]))
;;

let test_observer_survives_writer_disconnect env environment =
  let fixture = fixture env environment "multi-detach-observer" in
  with_daemon env fixture (fun sw ->
    let first = connect_unix ~sw env fixture in
    let observer = connect_http ~sw env fixture in
    let survivor = connect_unix ~sw env fixture in
    Exn.protect
      ~f:(fun () ->
        let session = create_session first ~subscribe:false "detach-observer:create" in
        let writer = attach survivor session ~mode:Read_write ~subscribe:false "writer" in
        let session = start_session first session "detach-observer:start" in
        ignore (attach observer session ~mode:Read_only ~subscribe:true "observer" : _);
        Agent_client.Connection.close first;
        let sent =
          send survivor session writer "observer remains" "detach-observer:send"
        in
        observe_send env observer session sent;
        stop_session survivor session writer "detach-observer:stop")
      ~finally:(fun () -> close_connections [ survivor; observer; first ]))
;;

let owner_liveness =
  Agent_protocol.Session.Owner_bound { disconnect_grace_ms = 150; stop_mode = Graceful }
;;

let verify_detached_mode env observer detached_owner =
  let detached =
    create_session detached_owner ~subscribe:false "owner-mode:detached-create"
  in
  let writer =
    attach observer detached ~mode:Read_write ~subscribe:false "detached-writer"
  in
  let detached = start_session detached_owner detached "owner-mode:detached-start" in
  Agent_client.Connection.close detached_owner;
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.2;
  require
    (desired_running (get_session observer detached.id))
    "detached session followed owner-bound disconnect behavior";
  stop_session observer detached writer "owner-mode:detached-stop"
;;

let verify_owner_mode env observer owner =
  let bounded =
    create_session
      owner
      ~mode:Owner_read_write
      ~liveness:owner_liveness
      ~subscribe:false
      "owner-mode:bounded-create"
  in
  ignore (attach observer bounded ~mode:Read_only ~subscribe:false "bounded-reader" : _);
  let bounded = start_session owner bounded "owner-mode:bounded-start" in
  Agent_client.Connection.close owner;
  ignore (await_stopped env observer bounded.id 100 : Agent_protocol.Session.t)
;;

let test_owner_mode_dependent env environment =
  let fixture = fixture env environment "multi-detach-owner-mode" in
  write_prompt fixture background_prompt;
  with_daemon env fixture (fun sw ->
    let detached_owner = connect_unix ~sw env fixture in
    let observer = connect_http ~sw env fixture in
    let owner = connect_unix ~sw env fixture in
    Exn.protect
      ~f:(fun () ->
        verify_detached_mode env observer detached_owner;
        verify_owner_mode env observer owner)
      ~finally:(fun () -> close_connections [ owner; observer; detached_owner ]))
;;

let cases =
  [ "writers.concurrent-messages", test_concurrent_messages
  ; "observer.same-event-order", test_same_event_order
  ; "observer.mutations-rejected", test_observer_rejected
  ; "approval.two-writers-race", test_permission_race
  ; "approval.read-only-rejected", test_read_only_permission_rejected
  ; "detach.writer-continues", test_writer_disconnect_continues
  ; "detach.observer-continues", test_observer_survives_writer_disconnect
  ; "detach.owner-mode-dependent", test_owner_mode_dependent
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown multi-client case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"multi-client" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (name, test) ->
      try test env environment with
      | exn ->
        raise_s [%sexp "multi-client E2E case failed", (name : string), (exn : Exn.t)]);
    print_s
      [%sexp
        { scenario = ("multi-client" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }])
;;
