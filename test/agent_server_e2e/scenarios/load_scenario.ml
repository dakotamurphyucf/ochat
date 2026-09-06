open Core
module F = Support.Background_fixture
module L = Support.Load_fixture
module H = Support.Http_driver
module R = Support.Load_report
module C = Support.Config_fixture

let count = L.integer
let number n = `Number (Int.to_string n)
let parallel_map ~max_fibers values ~f = Eio.Fiber.List.map ~max_fibers f values
let parallel_iter ~max_fibers values ~f = Eio.Fiber.List.iter ~max_fibers f values

let report env name f =
  let report = R.create env name in
  R.record report env "started" [ "scenario", `String name ];
  try
    f report;
    printf "load.report=%s\n%!" (R.finish report)
  with
  | exn ->
    R.fail report;
    raise exn
;;

let sessions client total =
  List.init total ~f:(fun index -> F.create client (sprintf "create-%d" index))
;;

let update_cursor cursor frame =
  F.require
    (not (Option.equal String.equal frame.H.Sse.event (Some "snapshot.required")))
    "healthy SSE observer required a snapshot";
  if Option.is_some frame.id
  then (
    let event =
      Jsonaf.of_string frame.data |> Agent_protocol.Event.Durable.of_json |> F.protocol_ok
    in
    F.require Int64.(event.sequence = !cursor + 1L) "SSE gap or duplicate";
    cursor := event.sequence)
;;

let watch_events ~sw env stream closing initial =
  let cursor = ref initial in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      if not !closing
      then (
        match H.Sse.next stream ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:20. with
        | Error "timed out waiting for an SSE event" -> loop ()
        | Error error -> if not !closing then failwith error
        | Ok frame ->
          update_cursor cursor frame;
          loop ())
    in
    loop ();
    `Stop_daemon);
  cursor
;;

let share total count index = (count / total) + if index < count % total then 1 else 0

let session_commands env client total commands (index, session) =
  let count = share total commands index in
  let schedules =
    List.init count ~f:(fun command ->
      F.schedule client session (sprintf "load-%d-%d" index command) "Wake" 0)
  in
  L.await_schedules env client session count;
  L.stop client session (sprintf "stop-%d" index);
  L.detach client session (sprintf "detach-%d" index);
  schedules
;;

let command_batch env client total commands indices =
  let batch =
    List.map indices ~f:(fun index -> index, F.create client (sprintf "create-%d" index))
  in
  List.concat_map batch ~f:(session_commands env client total commands)
;;

let assert_identities schedules expected =
  let unique =
    List.dedup_and_sort schedules ~compare:(fun a b ->
      Agent_protocol.Id.Schedule.compare a.Agent_protocol.Schedule.id b.id)
  in
  F.require (List.length unique = expected) "duplicate schedule identities"
;;

let commands env report =
  L.with_daemon env (fun _ fixture daemon client ->
    let total = count "OCHAT_E2E_LOAD_SESSIONS" 100 in
    let commands = count "OCHAT_E2E_LOAD_COMMANDS" 1000 in
    let started = L.now env in
    let schedules =
      List.range 0 total
      |> List.chunks_of ~length:50
      |> List.concat_map ~f:(command_batch env client total commands)
    in
    assert_identities schedules commands;
    ignore
      (R.sample
         report
         env
         fixture
         daemon
         client
         "sessions-commands"
         [ "sessions", number total
         ; "durable_commands", number commands
         ; "delivery_seconds", `Number (Float.to_string (L.now env -. started))
         ]
       : int * int))
;;

let observer_client ~sw env fixture summary =
  let client =
    H.create ~sw ~env ~port:(C.http_port fixture) ~token:(Some (C.admin_token fixture))
    |> F.result_ok
  in
  ignore (H.initialize client |> F.protocol_ok : _);
  let key =
    Agent_protocol.Id.Attachment.create () |> Agent_protocol.Id.Attachment.to_string
  in
  let session = L.attach client summary key in
  client, F.snapshot client session
;;

let keepalive ~sw env client closing =
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      Eio.Time.sleep (Eio.Stdenv.clock env) 10.;
      if not !closing
      then (
        ignore (L.health client : Agent_protocol.Health.Response.t);
        loop ())
    in
    loop ();
    `Stop_daemon)
;;

let open_observer ~sw env fixture summary =
  let client, snapshot = observer_client ~sw env fixture summary in
  let stream, response =
    H.open_session_events
      client
      ~sw
      ~session_id:summary.Agent_protocol.Session.id
      ~buffer_capacity:1
      ~after_sequence:snapshot.latest_event_sequence
      ()
    |> F.result_ok
  in
  F.require (response.status = 200) "SSE did not open";
  let closing = ref false in
  let cursor = watch_events ~sw env stream closing snapshot.latest_event_sequence in
  keepalive ~sw env client closing;
  client, stream, cursor, closing
;;

let open_observers ~sw env fixture sessions clients =
  let opened = ref 0 in
  parallel_map
    ~max_fibers:20
    (List.range 0 (List.length sessions * clients))
    ~f:(fun index ->
      let observer =
        open_observer ~sw env fixture (List.nth_exn sessions (index / clients)).F.summary
      in
      incr opened;
      if !opened % 50 = 0 then printf "fanout.opened=%d\n%!" !opened;
      index / clients, observer)
;;

let await_fanout env writer sessions observers =
  List.iteri sessions ~f:(fun index session ->
    ignore
      (F.schedule writer session (sprintf "fanout-%d" index) "Wake" 0
       : Agent_protocol.Schedule.t);
    L.await_schedules env writer session 1);
  let sequences =
    List.map sessions ~f:(fun session ->
      (F.snapshot writer session).latest_event_sequence)
  in
  L.wait env "all SSE observers catch up" (fun () ->
    List.for_all observers ~f:(fun (index, (_, _, cursor, _)) ->
      Int64.equal !cursor (List.nth_exn sequences index)))
;;

let close_observer (_, (client, stream, _, closing)) =
  closing := true;
  H.Sse.close stream;
  F.close_client client;
  H.shutdown client
;;

let attachments env report =
  L.with_daemon env (fun sw fixture daemon writer ->
    let groups = count "OCHAT_E2E_LOAD_ACTIVE_SESSIONS" 25 in
    let clients = count "OCHAT_E2E_LOAD_CLIENTS_PER_SESSION" 20 in
    let sessions = sessions writer groups in
    let observers = open_observers ~sw env fixture sessions clients in
    await_fanout env writer sessions observers;
    ignore
      (R.sample
         report
         env
         fixture
         daemon
         writer
         "attachments-sse"
         [ "clients", number (groups * clients)
         ; "sse_streams", number (groups * clients)
         ]
       : int * int);
    List.iter observers ~f:close_observer)
;;

let reconnect_once ~sw env fixture writer session index cycle =
  let key = sprintf "reconnect-%d-%d" index cycle in
  ignore (F.schedule writer session key "Wake" 0 : Agent_protocol.Schedule.t);
  ignore (F.schedule writer session (key ^ "-extra") "Wake" 0 : Agent_protocol.Schedule.t);
  L.await_schedules env writer session ((cycle + 1) * 2);
  F.with_client ~sw env fixture (fun client ->
    let attached, replay = F.attach client session.F.summary (key ^ "-attach") in
    let snapshot = F.snapshot client attached in
    F.require (List.length snapshot.schedules = (cycle + 1) * 2) "reconnect state drift";
    F.require
      (List.length snapshot.canonical_history.entries = 1)
      "reconnect changed history";
    F.close_client client;
    match replay with
    | Snapshot _ -> true
    | Events _ | Current -> false)
;;

let reconnect_sessions ~sw env fixture writer total cycles sessions =
  let replays = ref 0
  and snapshots = ref 0 in
  parallel_iter
    ~max_fibers:10
    (List.mapi sessions ~f:(fun i s -> i, s))
    ~f:(fun (index, session) ->
      for cycle = 0 to share total cycles index - 1 do
        if reconnect_once ~sw env fixture writer session index cycle
        then incr snapshots
        else incr replays;
        let completed = !replays + !snapshots in
        if completed % 100 = 0 then printf "reconnect.completed=%d\n%!" completed
      done);
  F.require (!replays + !snapshots = cycles) "reconnect count differs";
  if cycles >= total * 20
  then F.require (!replays > 0 && !snapshots > 0) "both replay paths must be exercised";
  !replays, !snapshots
;;

let reconnect env report =
  L.with_daemon env (fun sw fixture daemon writer ->
    let total = count "OCHAT_E2E_LOAD_RECONNECT_SESSIONS" 50 in
    let cycles = count "OCHAT_E2E_LOAD_RECONNECTS" 1000 in
    let sessions = sessions writer total in
    let replays, snapshots =
      reconnect_sessions ~sw env fixture writer total cycles sessions
    in
    ignore
      (R.sample
         report
         env
         fixture
         daemon
         writer
         "reconnect-storm"
         [ "sessions", number total
         ; "cycles", number cycles
         ; "retained_replays", number replays
         ; "snapshots", number snapshots
         ]
       : int * int))
;;

let unload_cycle env client cycle =
  let sessions =
    List.init 25 ~f:(fun index -> F.create client (sprintf "unload-%d-%d" cycle index))
  in
  List.iteri sessions ~f:(fun index session ->
    L.stop client session (sprintf "stop-%d-%d" cycle index);
    L.detach client session (sprintf "detach-%d-%d" cycle index));
  L.wait env "actor unload" (fun () -> L.loaded client = 0)
;;

let unload env report =
  L.with_daemon env (fun _ fixture daemon client ->
    let baseline_rss, baseline_fd =
      R.sample report env fixture daemon client "unload-baseline" []
    in
    for cycle = 0 to 2 do
      unload_cycle env client cycle;
      let rss, descriptors =
        R.sample report env fixture daemon client (sprintf "unload-settled-%d" cycle) []
      in
      F.require (descriptors <= baseline_fd + 16) "descriptors did not settle";
      F.require (rss <= baseline_rss + 262144) "settled RSS exceeded 256 MiB allowance"
    done)
;;

let attachments_and_backpressure env report =
  attachments env report;
  Replay_scenario.run env ~case:(Some "backpressure.slow-subscriber-disconnect");
  Replay_scenario.run env ~case:(Some "backpressure.healthy-client-progress");
  R.record
    report
    env
    "backpressure-supplemental"
    [ "slow_subscriber_disconnect_checks", number 1
    ; "healthy_client_progress_checks", number 1
    ]
;;

let capacity env report =
  Background_scenario.run_load_capacity env ~record:(fun fixture daemon client label ->
    ignore (R.sample report env fixture daemon client label [] : int * int))
;;

let cases =
  [ "load.sessions-commands", commands
  ; "load.attachments-sse-slow-readers", attachments_and_backpressure
  ; "load.reconnect-storm", reconnect
  ; "load.actor-unload-memory-settle", unload
  ; "load.jobs-schedules-capacity", capacity
  ]
;;

let run env ~case =
  let selected =
    match case with
    | None -> cases
    | Some name ->
      List.filter cases ~f:(fun (candidate, _) -> String.equal candidate name)
  in
  F.require (not (List.is_empty selected)) "unknown load case";
  List.iter selected ~f:(fun (name, run) -> report env name (run env))
;;
