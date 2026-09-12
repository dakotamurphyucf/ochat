open Core
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Http_driver = Support.Http_driver
module Port_reservation = Support.Port_reservation
module Temporary_environment = Support.Temporary_environment

type session =
  { summary : Agent_protocol.Session.t
  ; attachment : Agent_protocol.Session.Attachment.t
  }

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
;;

let result_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "HTTP operation failed", (error : string)]
;;

let idempotency_key value = Agent_protocol.Idempotency_key.of_string value |> protocol_ok

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Port_reservation.create ~sw ~env in
    let port = Port_reservation.port reservation in
    Port_reservation.release reservation;
    port)
;;

let prompt =
  {|
<developer>Provide deterministic lifecycle events without a model.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event =
    [ `Session_start | `Session_resume | `Item_appended(item) | `Turn_start | `Turn_end ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx state event ->
      match event with
      | `Session_start -> Task.pure(state + 1)
      | `Session_resume -> Task.pure(state + 1)
      | `Item_appended(_) -> Task.pure(state)
      | `Turn_start -> Task.pure(state + 1)
      | `Turn_end -> Task.pure(state)
</script>
|}
;;

let fixture ?subscriber_capacity env environment name ~retained_events =
  let fixture = Config_fixture.create environment ~name ~http_port:(reserve_port env) in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment (Config_fixture.prompt_path fixture))
    prompt;
  let configuration =
    Config_fixture.configuration fixture ()
    |> String.substr_replace_first
         ~pattern:"(max_events_per_session 1000)"
         ~with_:(sprintf "(max_events_per_session %d)" retained_events)
    |> fun configuration ->
    Option.value_map subscriber_capacity ~default:configuration ~f:(fun capacity ->
      String.substr_replace_first
        configuration
        ~pattern:"(subscriber_queue_capacity 16)"
        ~with_:(sprintf "(subscriber_queue_capacity %d)" capacity))
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment (Config_fixture.config_path fixture))
    configuration;
  fixture
;;

let stop_daemon env daemon =
  match Daemon_process.result daemon with
  | Some _ -> ()
  | None -> ignore (Daemon_process.stop daemon ~env ~grace_seconds:1.)
;;

let wait_ready env daemon =
  match Daemon_process.wait_ready daemon ~env ~timeout_seconds:5. with
  | Ok _ -> ()
  | Error error ->
    raise_s
      [%sexp
        "daemon did not become ready"
      , (error : Daemon_process.readiness_error)
      , ((Daemon_process.stderr daemon).contents : string)]
;;

let with_daemon env fixture f =
  Eio.Switch.run (fun sw ->
    let daemon =
      Daemon_process.start
        ~sw
        ~env
        ~fixture
        ~config_path:(Config_fixture.config_path fixture)
    in
    Exn.protect
      ~f:(fun () ->
        wait_ready env daemon;
        f sw)
      ~finally:(fun () -> stop_daemon env daemon))
;;

let with_client ~sw env fixture f =
  let client =
    Http_driver.create
      ~sw
      ~env
      ~port:(Config_fixture.http_port fixture)
      ~token:(Some (Config_fixture.admin_token fixture))
    |> result_ok
  in
  Exn.protect
    ~f:(fun () ->
      ignore (Http_driver.initialize client |> protocol_ok : _);
      f client)
    ~finally:(fun () -> Http_driver.shutdown client)
;;

let page_request () = Agent_protocol.Page.Request.create ~limit:100 () |> protocol_ok

let catalog client =
  let prompts =
    Agent_protocol.Prompt.List_request.
      { page = page_request (); enabled = Some true; available = Some true }
  in
  let workspaces =
    Agent_protocol.Workspace.List_request.
      { page = page_request (); kind = None; access = None; available = Some true }
  in
  let prompt =
    match (Http_driver.request client (Prompt_list prompts) |> protocol_ok).result with
    | Prompt_list page -> List.hd_exn page.items
    | _ -> fail "prompt.list returned the wrong result"
  in
  let workspace =
    match
      (Http_driver.request client (Workspace_list workspaces) |> protocol_ok).result
    with
    | Workspace_list page -> List.hd_exn page.items
    | _ -> fail "workspace.list returned the wrong result"
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
    ~labels:[ "suite", "replay" ]
    ()
  |> protocol_ok
;;

let create_session client key =
  let request =
    Agent_protocol.Session.Create_request.
      { spec = session_spec client
      ; requested_mode = Some Read_write
      ; subscribe = false
      ; idempotency_key = idempotency_key key
      }
  in
  match (Http_driver.request client (Session_create request) |> protocol_ok).result with
  | Session_create created ->
    { summary = created.session
    ; attachment = (Option.value_exn created.attachment).attachment
    }
  | _ -> fail "session.create returned the wrong result"
;;

let lifecycle_command session start key =
  if start
  then
    Agent_protocol.Command.Session_start
      { session_id = session.summary.id
      ; attachment_id = session.attachment.id
      ; queue_if_limited = false
      ; idempotency_key = idempotency_key key
      }
  else
    Session_stop
      { session_id = session.summary.id
      ; attachment_id = session.attachment.id
      ; mode = Graceful
      ; idempotency_key = idempotency_key key
      }
;;

let change_lifecycle client session start key =
  let result = Http_driver.request client (lifecycle_command session start key) in
  match (result |> protocol_ok).result with
  | Session_start mutation | Session_stop mutation ->
    { session with summary = mutation.session }
  | _ -> fail "lifecycle command returned the wrong result"
;;

let generate_events ?(prefix = "cycle") client session cycles =
  List.range 0 cycles
  |> List.fold ~init:session ~f:(fun session index ->
    let key action = sprintf "%s:%s:%d" prefix action index in
    let session = change_lifecycle client session true (key "start") in
    change_lifecycle client session false (key "stop"))
;;

let durable_event env stream =
  let frame =
    Http_driver.Sse.next stream ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:5.
    |> result_ok
  in
  let json = Jsonaf.of_string frame.data in
  Agent_protocol.Event.Durable.of_json json |> protocol_ok
;;

let rec collect_through env stream previous through events =
  if Int64.(previous >= through)
  then List.rev events
  else (
    let event = durable_event env stream in
    require Int64.(event.sequence = previous + 1L) "replay event gap";
    collect_through env stream event.sequence through (event :: events))
;;

let open_events client ~sw session_id cursor =
  Http_driver.open_session_events client ~sw ~session_id ~after_sequence:cursor ()
  |> result_ok
  |> fst
;;

let connect_typed ?(notification_capacity = 128) ~sw env fixture =
  let uri =
    Uri.of_string (sprintf "http://127.0.0.1:%d" (Config_fixture.http_port fixture))
  in
  let connection =
    Agent_transport_http.Client.connect
      ~sw
      ~env
      ~uri
      ~bearer_token:(Some (Config_fixture.admin_token fixture))
      ~notification_capacity
    |> protocol_ok
  in
  ignore
    (Agent_client.Session_handle.initialize
       connection
       ~implementation_name:"replay-e2e"
       ~implementation_version:"dev"
     |> protocol_ok
     : Agent_protocol.Initialize.Response.t);
  connection
;;

let snapshots_equal left right =
  String.equal
    (Agent_protocol.Snapshot.to_json left |> Jsonaf.to_string)
    (Agent_protocol.Snapshot.to_json right |> Jsonaf.to_string)
;;

let send_message client session text key =
  let request =
    Agent_protocol.Session.Send_message_request.
      { session_id = session.summary.id
      ; attachment_id = session.attachment.id
      ; content = { kind = Plain_text; text; attachments = [] }
      ; idempotency_key = idempotency_key key
      }
  in
  match
    (Http_driver.request client (Session_send_message request) |> protocol_ok).result
  with
  | Session_send_message sent -> sent
  | _ -> fail "session.send_message returned the wrong result"
;;

let await_stopped env client session =
  let rec wait () =
    let snapshot, _ = Http_driver.get_snapshot client session.summary.id |> result_ok in
    match snapshot.session.observed_state, snapshot.session.active_operation with
    | Stopped, None -> { session with summary = snapshot.session }
    | _ ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      wait ()
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. wait
;;

let require_submitted_content client session sent text =
  let snapshot, _ = Http_driver.get_snapshot client session.summary.id |> result_ok in
  let expected =
    Agent_session.History_codec.user_text
      ~id:sent.Agent_protocol.Method_result.Send_message.history_id
      text
    |> Agent_session.History_codec.to_protocol
  in
  let actual =
    List.filter snapshot.canonical_history.entries ~f:(fun entry ->
      Agent_protocol.History.Id.compare entry.id sent.history_id = 0)
  in
  require
    (Sexp.equal
       ([%sexp_of: Agent_protocol.History.entry list] actual)
       ([%sexp_of: Agent_protocol.History.entry list] [ expected ]))
    "replay fixture lost, duplicated, or altered its submitted content"
;;

let generate_content env client session key =
  let session = change_lifecycle client session true (key ^ ":start") in
  let sent = send_message client session key (key ^ ":message") in
  ignore
    (Http_driver.request
       client
       (Session_stop
          { session_id = session.summary.id
          ; attachment_id = session.attachment.id
          ; mode = Cancel
          ; idempotency_key = idempotency_key (key ^ ":stop")
          })
     |> protocol_ok
     : Http_driver.rpc_response);
  let session = await_stopped env client session in
  require_submitted_content client session sent key;
  session
;;

let require_projection_matches writer session projected =
  let authoritative, _ =
    Http_driver.get_snapshot writer session.summary.id |> result_ok
  in
  if not (snapshots_equal projected authoritative)
  then
    raise_s
      [%sexp
        "client projection differs from authoritative snapshot"
      , (projected : Agent_protocol.Snapshot.t)
      , (authoritative : Agent_protocol.Snapshot.t)]
;;

let pressure_payload index =
  (* Thirty-two messages still overflow the deliberately stalled subscriber's
     transport/queue buffers. Keep retained history below the journal frame
     bound too: moderator snapshots can repeat that history within one commit. *)
  sprintf "pressure-%02d:%s" index (String.make (64 * 1024) (Char.of_int_exn 120))
;;

let send_pressure_messages client session ~count =
  List.range 0 count
  |> List.fold ~init:session.summary.latest_event_sequence ~f:(fun sequence index ->
    let sent =
      send_message
        client
        session
        (pressure_payload index)
        (sprintf "backpressure:message:%d" index)
    in
    Int64.max sequence sent.mutation.latest_event_sequence)
;;

let rec await_snapshot_required env stream attempts =
  if attempts = 0
  then false
  else (
    match
      Http_driver.Sse.next stream ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:5.
    with
    | Error _ -> false
    | Ok frame ->
      if Option.value_map frame.event ~default:false ~f:(String.equal "snapshot.required")
      then true
      else await_snapshot_required env stream (attempts - 1))
;;

type backpressure_observation =
  { slow_snapshot_required : bool
  ; healthy_reached_latest : bool
  ; writer_completed : bool
  }

let rec await_projection env projection target attempts =
  let snapshot = projection () |> Agent_client.Projection.snapshot in
  if Int64.(snapshot.latest_event_sequence >= target)
  then snapshot
  else if attempts = 0
  then fail "client projection did not reach the durable target"
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    await_projection env projection target (attempts - 1))
;;

let attach_healthy ~sw env fixture writer session =
  let previous, _ = Http_driver.get_snapshot writer session.summary.id |> result_ok in
  let connection = connect_typed ~notification_capacity:2048 ~sw env fixture in
  let handle =
    Agent_client.Session_handle.attach
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~connection
      ~session_id:session.summary.id
      ~mode:Read_only
      ~subscribe:true
      ~after_sequence:previous.latest_event_sequence
      ~previous_projection:(Agent_client.Projection.install_snapshot previous)
      ()
    |> protocol_ok
  in
  let snapshot, _ = Http_driver.get_snapshot writer session.summary.id |> result_ok in
  connection, handle, snapshot.latest_event_sequence
;;

let open_slow_stream ~sw writer session =
  let initial, _ = Http_driver.get_snapshot writer session.summary.id |> result_ok in
  Http_driver.open_session_events
    writer
    ~sw
    ~session_id:session.summary.id
    ~buffer_capacity:1
    ~after_sequence:initial.latest_event_sequence
    ()
  |> result_ok
  |> fst
;;

let with_pressure_clients ~sw env fixture writer session f =
  let stream = open_slow_stream ~sw writer session in
  Exn.protect
    ~f:(fun () ->
      let connection, handle, cursor = attach_healthy ~sw env fixture writer session in
      Exn.protect
        ~f:(fun () -> f stream handle cursor)
        ~finally:(fun () ->
          Agent_client.Session_handle.close handle;
          Agent_client.Connection.close connection))
    ~finally:(fun () -> Http_driver.Sse.close stream)
;;

let observe_pressure env writer session stream handle cursor =
  let latest = send_pressure_messages writer session ~count:32 in
  let projected =
    await_projection
      env
      (fun () -> Agent_client.Session_handle.projection handle)
      latest
      500
  in
  { slow_snapshot_required = await_snapshot_required env stream 64
  ; healthy_reached_latest = Int64.(projected.latest_event_sequence >= latest)
  ; writer_completed = Int64.(latest > cursor)
  }
;;

let observe_backpressure ~sw env fixture writer session =
  with_pressure_clients ~sw env fixture writer session (fun stream handle cursor ->
    observe_pressure env writer session stream handle cursor)
;;

let with_backpressure_observation env environment name f =
  let fixture =
    fixture ~subscriber_capacity:16 env environment name ~retained_events:1024
  in
  with_daemon env fixture (fun sw ->
    with_client ~sw env fixture (fun writer ->
      let created = create_session writer (name ^ ":create") in
      let session = change_lifecycle writer created true (name ^ ":start") in
      f (observe_backpressure ~sw env fixture writer session)))
;;

let test_slow_subscriber_disconnect env environment =
  with_backpressure_observation env environment "backpressure-slow" (fun observation ->
    require
      observation.slow_snapshot_required
      "slow subscriber was not detached with snapshot.required")
;;

let test_healthy_client_progress env environment =
  with_backpressure_observation env environment "backpressure-healthy" (fun observation ->
    require
      observation.writer_completed
      "writer commands stalled behind the slow subscriber";
    require
      observation.healthy_reached_latest
      "healthy subscriber did not reach the writer's latest durable event")
;;

let test_contiguous_cursor env environment =
  let fixture = fixture env environment "replay-contiguous" ~retained_events:64 in
  with_daemon env fixture (fun sw ->
    with_client ~sw env fixture (fun client ->
      let created = create_session client "contiguous:create" in
      let cursor = created.summary.latest_event_sequence in
      let session = generate_events client created 3 in
      let stream = open_events client ~sw session.summary.id cursor in
      let events =
        collect_through env stream cursor session.summary.latest_event_sequence []
      in
      require (not (List.is_empty events)) "retained cursor replay was empty";
      Http_driver.Sse.close stream))
;;

let rec take_events env stream count events =
  if count = 0
  then List.rev events
  else take_events env stream (count - 1) (durable_event env stream :: events)
;;

let sequences events =
  List.map events ~f:(fun (event : Agent_protocol.Event.Durable.t) -> event.sequence)
;;

let test_no_duplicates env environment =
  let fixture = fixture env environment "replay-no-duplicates" ~retained_events:64 in
  with_daemon env fixture (fun sw ->
    with_client ~sw env fixture (fun client ->
      let session =
        create_session client "duplicates:create" |> Fn.flip (generate_events client) 4
      in
      let first_stream = open_events client ~sw session.summary.id 0L in
      let first = take_events env first_stream 3 [] in
      Http_driver.Sse.close first_stream;
      let cursor = (List.last_exn first).sequence in
      let replay = open_events client ~sw session.summary.id cursor in
      let rest =
        collect_through env replay cursor session.summary.latest_event_sequence []
      in
      let combined = sequences (first @ rest) in
      require
        (Option.is_none (List.find_a_dup combined ~compare:Int64.compare))
        "replay duplicated an event";
      require
        Int64.(List.last_exn combined = session.summary.latest_event_sequence)
        "replay lost terminal events";
      Http_driver.Sse.close replay))
;;

let require_expired_cursor = function
  | Error message ->
    require
      (String.is_substring message ~substring:"HTTP 409")
      "expired cursor returned the wrong HTTP status"
  | Ok (stream, _) ->
    Http_driver.Sse.close stream;
    fail "expired cursor unexpectedly opened an event stream"
;;

let require_snapshot_current client session =
  let snapshot, _ = Http_driver.get_snapshot client session.summary.id |> result_ok in
  require
    Int64.(snapshot.latest_event_sequence = session.summary.latest_event_sequence)
    "snapshot did not cover the expired cursor"
;;

let test_expired_cursor env environment =
  let fixture = fixture env environment "replay-expired" ~retained_events:4 in
  with_daemon env fixture (fun sw ->
    with_client ~sw env fixture (fun client ->
      let session =
        create_session client "expired:create" |> Fn.flip (generate_events client) 4
      in
      let result =
        Http_driver.open_session_events
          client
          ~sw
          ~session_id:session.summary.id
          ~after_sequence:0L
          ()
      in
      require_expired_cursor result;
      require_snapshot_current client session))
;;

let attach_from_snapshot ~sw env connection session_id snapshot ~subscribe =
  Agent_client.Session_handle.attach
    ~sw
    ~clock:(Eio.Stdenv.clock env)
    ~connection
    ~session_id
    ~mode:Read_only
    ~subscribe
    ~after_sequence:snapshot.Agent_protocol.Snapshot.latest_event_sequence
    ~previous_projection:(Agent_client.Projection.install_snapshot snapshot)
    ()
  |> protocol_ok
;;

let require_snapshot_replaced writer session handle =
  let authoritative, _ =
    Http_driver.get_snapshot writer session.summary.id |> result_ok
  in
  let replaced =
    Agent_client.Session_handle.projection handle |> Agent_client.Projection.snapshot
  in
  require
    (snapshots_equal replaced authoritative)
    "expired replay did not replace the client snapshot"
;;

let test_snapshot_replacement env environment =
  let fixture = fixture env environment "replay-snapshot" ~retained_events:4 in
  with_daemon env fixture (fun sw ->
    with_client ~sw env fixture (fun writer ->
      let created = create_session writer "snapshot:create" in
      let stale, _ = Http_driver.get_snapshot writer created.summary.id |> result_ok in
      let session = generate_events writer created 4 in
      let connection = connect_typed ~sw env fixture in
      let handle =
        attach_from_snapshot ~sw env connection session.summary.id stale ~subscribe:false
      in
      require_snapshot_replaced writer session handle;
      Agent_client.Session_handle.close handle;
      Agent_client.Connection.close connection))
;;

let reconnect_status_connected client =
  match Agent_client.Reconnect.status client with
  | Connected -> true
  | Reconnecting _ | Disconnected | Failed _ -> false
;;

let make_reconnect_client ~sw env fixture session_id =
  let current = ref (connect_typed ~sw env fixture) in
  let reconnect () =
    let connection = connect_typed ~sw env fixture in
    current := connection;
    Ok connection
  in
  let client =
    Agent_client.Reconnect.attach
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~connection:!current
      ~reconnect:(Some reconnect)
      ~session_id
      ~mode:Read_only
      ()
    |> protocol_ok
  in
  current, client
;;

let reconnect_once env writer current client session iteration =
  Agent_client.Connection.close !current;
  let session = generate_content env writer session (sprintf "reconnect:%d" iteration) in
  ignore
    (await_projection
       env
       (fun () -> Agent_client.Reconnect.projection client)
       session.summary.latest_event_sequence
       300
     : Agent_protocol.Snapshot.t);
  require (reconnect_status_connected client) "reconnect client did not recover";
  let session =
    generate_events ~prefix:(sprintf "reconnected:%d" iteration) writer session 1
  in
  let projected =
    await_projection
      env
      (fun () -> Agent_client.Reconnect.projection client)
      session.summary.latest_event_sequence
      300
  in
  require_projection_matches writer session projected;
  session
;;

let test_repeated_reconnect env environment =
  let fixture = fixture env environment "replay-reconnect" ~retained_events:64 in
  with_daemon env fixture (fun sw ->
    with_client ~sw env fixture (fun writer ->
      let created = create_session writer "reconnect:create" in
      let current, client = make_reconnect_client ~sw env fixture created.summary.id in
      Exn.protect
        ~f:(fun () ->
          let session =
            List.range 0 3
            |> List.fold ~init:created ~f:(reconnect_once env writer current client)
          in
          require
            Int64.(
              session.summary.latest_event_sequence
              > created.summary.latest_event_sequence)
            "reconnect fixture produced no durable progress")
        ~finally:(fun () -> Agent_client.Reconnect.close client)))
;;

let race_live_boundary ~sw env connection writer created stale =
  let selected, selected_resolver = Eio.Promise.create () in
  let release, release_resolver = Eio.Promise.create () in
  let request command =
    let response = Agent_client.Connection.request connection command in
    (match command, response with
     | Session_attach _, Ok (Session_attach attached) ->
       require
         (match attached.replay with
          | Events (_ :: _) -> true
          | _ -> false)
         "boundary fixture did not select retained replay events";
       Eio.Promise.resolve selected_resolver attached.latest_event_sequence;
       Eio.Promise.await release
     | _ -> ());
    response
  in
  let gated =
    Agent_client.Transport.create
      ~request
      ~next_notification:(fun () -> Agent_client.Connection.next_notification connection)
      ~close:(fun () -> Agent_client.Connection.close connection)
    |> Agent_client.Connection.create
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
    Eio.Fiber.pair
      (fun () ->
         attach_from_snapshot ~sw env gated created.summary.id stale ~subscribe:true)
      (fun () ->
         let replay_end = Eio.Promise.await selected in
         let session = generate_content env writer created "boundary:live" in
         require
           Int64.(session.summary.latest_event_sequence > replay_end)
           "no mutation committed during the replay/live handoff";
         Eio.Promise.resolve release_resolver ();
         session))
;;

let require_boundary_current env writer handle session =
  let projected =
    await_projection
      env
      (fun () -> Agent_client.Session_handle.projection handle)
      session.summary.latest_event_sequence
      300
  in
  require
    Int64.(projected.latest_event_sequence = session.summary.latest_event_sequence)
    "live boundary duplicated or skipped durable events";
  require
    (Option.is_none (Agent_client.Session_handle.last_error handle))
    "live boundary produced a projection error";
  require_projection_matches writer session projected
;;

let test_live_boundary_race env environment =
  let fixture = fixture env environment "replay-live-boundary" ~retained_events:64 in
  with_daemon env fixture (fun sw ->
    with_client ~sw env fixture (fun writer ->
      let created = create_session writer "boundary:create" in
      let stale, _ = Http_driver.get_snapshot writer created.summary.id |> result_ok in
      let created = generate_content env writer created "boundary:replay" in
      let connection = connect_typed ~sw env fixture in
      let handle, session = race_live_boundary ~sw env connection writer created stale in
      require_boundary_current env writer handle session;
      Agent_client.Session_handle.close handle;
      Agent_client.Connection.close connection))
;;

let cases =
  [ "replay.contiguous-cursor", test_contiguous_cursor
  ; "replay.no-duplicates", test_no_duplicates
  ; "replay.expired-cursor", test_expired_cursor
  ; "snapshot.replacement", test_snapshot_replacement
  ; "snapshot.live-boundary-race", test_live_boundary_race
  ; "reconnect.repeated", test_repeated_reconnect
  ; "backpressure.slow-subscriber-disconnect", test_slow_subscriber_disconnect
  ; "backpressure.healthy-client-progress", test_healthy_client_progress
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown replay case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"replay-backpressure" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (name, test) ->
      try test env environment with
      | exn -> raise_s [%sexp "replay E2E case failed", (name : string), (exn : Exn.t)]);
    print_s
      [%sexp
        { scenario = ("replay-backpressure" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }])
;;
