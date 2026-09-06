open Core
module F = Support.Background_fixture
module L = Support.Load_fixture
module R = Support.Load_report
module C = Support.Config_fixture
module D = Support.Daemon_process
module P = Support.Tui_stream_provider
module M = Support.Tui_manual_provider

type t =
  { env : Eio_unix.Stdenv.base
  ; sw : Eio.Switch.t
  ; fixture : C.t
  ; daemon : D.t ref
  ; provider : P.t
  ; manual : M.t
  ; port : int
  ; report : R.t
  ; session : Agent_protocol.Session.t
  ; cursor : int64 option ref
  }

let configure fixture =
  let nested = Filename.concat (C.physical_workspace fixture) "nested.chatmd" in
  F.save fixture nested "<developer>Reply with background-result.</developer>";
  let payload =
    Jsonaf.to_string
      (`Object
          [ "prompt", `String nested
          ; "input", `String "soak-background-input"
          ; "is_local", `True
          ])
  in
  F.save
    fixture
    (C.prompt_path fixture)
    (sprintf
       {|<developer>Isolated soak counter.</developer>
<script language="chatml" kind="moderator">
type state = int
type event = [ `Session_start | `Session_resume | `Go | `Wake
  | `Model_job_succeeded(string, string, json) | `Model_job_failed(string, string, string) ]
let initial_state = 0
let on_event : context -> state -> event -> state task = fun ctx state event ->
  match event with
  | `Go -> Task.bind(Model.spawn("agent_prompt_v1", Json.parse(%S)), fun id -> Task.pure(state))
  | `Model_job_succeeded(id, recipe, result) -> Task.pure(state + 1)
  | `Model_job_failed(id, recipe, message) -> Task.bind(Runtime.end_session(message), fun ignored -> Task.pure(state))
  | _ -> Task.pure(state)
</script>|}
       payload)
;;

let summary ~sw env fixture =
  F.with_client ~sw env fixture (fun client ->
    let session = F.create client "soak-create" in
    F.close_client client;
    session.summary)
;;

let with_client t f = F.with_client ~sw:t.sw t.env t.fixture f

let attach t client name =
  let session, replay = F.attach_after client t.session name !(t.cursor) in
  let kind =
    match replay with
    | Agent_protocol.Method_result.Attach.Snapshot _ -> "snapshot"
    | Events _ -> "events"
    | Current -> "current"
  in
  R.record t.report t.env "attach" [ "replay_kind", `String kind ];
  session
;;

let begin_job t index =
  with_client t (fun client ->
    let session = attach t client (sprintf "soak-attach-%d" index) in
    let started = L.now t.env in
    ignore
      (F.schedule client session (sprintf "soak-go-%d" index) "Go" 0
       : Agent_protocol.Schedule.t);
    R.record
      t.report
      t.env
      "schedule-ack"
      [ "durable_command_ack_seconds", `Number (Float.to_string (L.now t.env -. started))
      ];
    L.wait t.env "soak running job" (fun () ->
      List.exists (F.snapshot client session).jobs ~f:(fun j ->
        Poly.equal j.status Running));
    F.close_client client)
;;

let all_delivered snapshot expected =
  List.length snapshot.Agent_protocol.Snapshot.jobs = expected
  && List.for_all snapshot.jobs ~f:(fun j ->
    Poly.equal j.status Succeeded
    &&
    match j.delivery with
    | Delivered _ -> true
    | _ -> false)
;;

let scheduler_lag schedules =
  List.fold schedules ~init:0. ~f:(fun maximum schedule ->
    Option.value_map
      schedule.Agent_protocol.Schedule.last_delivery_at
      ~default:maximum
      ~f:(fun delivered ->
        let lag =
          Time_ns.diff
            (Agent_protocol.Timestamp.to_time_ns delivered)
            (Agent_protocol.Timestamp.to_time_ns schedule.next_due_at)
          |> Time_ns.Span.to_sec
        in
        Float.max maximum lag))
;;

let validate_snapshot snapshot =
  F.require
    (List.length snapshot.Agent_protocol.Snapshot.canonical_history.entries = 1)
    "soak history grew unexpectedly";
  F.require
    ((not snapshot.halted) && Option.is_none snapshot.failure)
    "orchestrator halted or failed";
  F.require
    (Poly.equal snapshot.session.observed_state Idle)
    "root turn unexpectedly active"
;;

let await_counter t session expected =
  L.wait t.env "persisted moderator counter" (fun () ->
    Option.exists (F.checkpoint t.env t.fixture session) ~f:(fun state ->
      String.equal (F.moderator_state state) (Int.to_string expected)))
;;

let collect_snapshot t client session index =
  L.wait t.env "soak delivered jobs" (fun () ->
    all_delivered (F.snapshot client session) (index + 1));
  ignore
    (F.schedule client session (sprintf "soak-wake-%d" index) "Wake" 0
     : Agent_protocol.Schedule.t);
  L.await_schedules t.env client session ((index + 1) * 2);
  let snapshot = F.snapshot client session in
  validate_snapshot snapshot;
  await_counter t session (index + 1);
  t.cursor := Some snapshot.latest_event_sequence;
  snapshot
;;

let sample_snapshot t client index started snapshot =
  let number n = `Number (Int.to_string n) in
  R.sample
    t.report
    t.env
    t.fixture
    !(t.daemon)
    client
    (sprintf "cycle-%d" index)
    [ "jobs", number (List.length snapshot.Agent_protocol.Snapshot.jobs)
    ; "schedules", number (List.length snapshot.schedules)
    ; ( "job_queue_depth"
      , number (List.count snapshot.jobs ~f:(fun job -> Poly.equal job.status Queued)) )
    ; "fixture_rw_attachments", number 1
    ; ( "scheduler_lag_max_seconds"
      , `Number (Float.to_string (scheduler_lag snapshot.schedules)) )
    ; "job_delivery_seconds", `Number (Float.to_string (L.now t.env -. started))
    ; "event_sequence", `Number (Int64.to_string snapshot.latest_event_sequence)
    ]
;;

let collect t index started =
  with_client t (fun client ->
    let session = attach t client (sprintf "soak-delivered-%d" index) in
    let snapshot = collect_snapshot t client session index in
    let metrics = sample_snapshot t client index started snapshot in
    F.close_client client;
    L.wait t.env "no remaining client attachments" (fun () ->
      Option.exists (F.checkpoint t.env t.fixture session) ~f:(fun state ->
        List.is_empty state.attachments));
    metrics)
;;

let restart t crash =
  if crash
  then (
    D.signal !(t.daemon) Stdlib.Sys.sigkill;
    L.wait t.env "killed daemon exit" (fun () -> Option.is_some (D.result !(t.daemon))))
  else F.stop t.env !(t.daemon);
  let started = L.now t.env in
  t.daemon := F.start ~sw:t.sw t.env t.fixture t.port;
  L.now t.env -. started
;;

let maybe_restart t index count =
  if (index + 1) % 5 <> 0
  then count
  else (
    let crash = count % 2 = 1 in
    let recovery = restart t crash in
    R.record
      t.report
      t.env
      "restart"
      [ "restart_count", `Number (Int.to_string (count + 1))
      ; "kind", `String (if crash then "sigkill" else "graceful")
      ; "recovery_seconds", `Number (Float.to_string recovery)
      ];
    printf "soak.restart count=%d recovery_seconds=%.3f\n%!" (count + 1) recovery;
    count + 1)
;;

let cycle t index baseline =
  let started = L.now t.env in
  begin_job t index;
  ignore (P.await_request t.provider t.env index : P.request);
  M.respond t.manual ~env:t.env ~action:"background" ~index;
  let rss, fd = collect t index started in
  Option.iter baseline ~f:(fun (base_rss, base_fd) ->
    F.require (rss <= base_rss + 262144) "soak RSS grew beyond 256 MiB allowance";
    F.require (fd <= base_fd + 32) "soak descriptor leak");
  rss, fd
;;

let run_loop t seconds interval =
  let started = L.now t.env in
  let rec loop index restarts baseline =
    let rss, fd = cycle t index baseline in
    let restarts = maybe_restart t index restarts in
    printf
      "soak.cycle=%d elapsed_seconds=%.1f jobs=%d rss_kib=%d descriptors=%d\n%!"
      index
      (L.now t.env -. started)
      (index + 1)
      rss
      fd;
    if Float.(L.now t.env -. started >= seconds) && restarts >= 2
    then ()
    else (
      Eio.Time.sleep (Eio.Stdenv.clock t.env) interval;
      loop (index + 1) restarts (Some (Option.value baseline ~default:(rss, fd))))
  in
  loop 0 0 None
;;

let duration short =
  F.require
    (short || Poly.equal (Sys.getenv "OCHAT_E2E_ALLOW_SOAK") (Some "1"))
    "soak requires OCHAT_E2E_ALLOW_SOAK=1";
  F.require
    (short || Poly.equal (Sys.getenv "OCHAT_E2E_RUNNER_PROFILE") (Some "isolated"))
    "soak requires OCHAT_E2E_RUNNER_PROFILE=isolated";
  let seconds =
    if short
    then L.integer "OCHAT_E2E_SOAK_SELF_CHECK_SECONDS" 10
    else L.integer "OCHAT_E2E_SOAK_SECONDS" 3600
  in
  F.require (short || seconds >= 3600) "required soak must run for at least one hour";
  seconds
;;

let maintenance t short =
  if not short
  then (
    Load_scenario.unload t.env t.report;
    Data_integrity_scenario.run
      t.env
      ~case:(Some "daemon-timer.retention-active-protection");
    R.record t.report t.env "retention-active-protection-passed" [])
;;

let execute t short seconds =
  R.record
    t.report
    t.env
    "configuration"
    [ "required_seconds", `Number (Int.to_string seconds)
    ; "cycle_interval_seconds", `Number (if short then "0.1" else "20")
    ; "restart_every_cycles", `Number "5"
    ; "rss_allowance_kib", `Number "262144"
    ; "descriptor_allowance", `Number "32"
    ];
  Eio.Fiber.both
    (fun () -> run_loop t (Float.of_int seconds) (if short then 0.1 else 20.))
    (fun () -> maintenance t short);
  R.record t.report t.env "complete" []
;;

let with_daemon env temporary report short seconds =
  Eio.Switch.run (fun sw ->
    let fixture = L.configure env temporary in
    configure fixture;
    let port = F.reserve_port env in
    let provider = P.start ~sw ~env ~port in
    let manual = M.create provider in
    let daemon = ref (F.start ~sw env fixture port) in
    Exn.protect
      ~f:(fun () ->
        let session = summary ~sw env fixture in
        let t =
          { env
          ; sw
          ; fixture
          ; daemon
          ; provider
          ; manual
          ; port
          ; report
          ; session
          ; cursor = ref None
          }
        in
        execute t short seconds)
      ~finally:(fun () -> F.stop env !daemon))
;;

let run env ~case =
  F.require
    (Option.is_none case || Poly.equal case (Some "self-check"))
    "unknown soak case";
  let short = Poly.equal case (Some "self-check") in
  let seconds = duration short in
  Load_scenario.report
    env
    (if short then "soak-self-check" else "soak")
    (fun report ->
       Support.Temporary_environment.with_ ~scenario:"soak" ~env (fun temporary ->
         with_daemon env temporary report short seconds))
;;
