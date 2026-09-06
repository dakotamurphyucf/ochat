open Core
module F = Support.Background_fixture
module Provider = Support.Background_provider
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Temporary_environment = Support.Temporary_environment

let payload fixture =
  let path =
    Filename.concat (Config_fixture.physical_workspace fixture) "nested.chatmd"
  in
  F.save fixture path "<developer>Reply with background-result.</developer>";
  `Object
    [ "prompt", `String path; "input", `String "background-input"; "is_local", `True ]
  |> Jsonaf.to_string
  |> sprintf "Json.parse(%S)"
;;

let spawn input =
  sprintf
    {|Task.bind(Model.spawn("agent_prompt_v1", %s), fun id ->
    Task.pure(state ++ "spawn:" ++ id ++ ";"))|}
    input
;;

let call input =
  sprintf
    {|Task.bind(Model.call("agent_prompt_v1", %s), fun result ->
    match result with
    | `Ok(value) -> Task.pure(state ++ "call-ok:" ++ Json.stringify(value) ++ ";")
    | `Error(message) -> Task.pure(state ++ "call-error;")
    | `Refused(message) -> Task.pure(state ++ "call-refused;"))|}
    input
;;

let prompt action =
  sprintf
    {|
<developer>Run deterministic background orchestration.</developer>
<script language="chatml" kind="moderator">
  type state = string
  type event = [ `Session_start | `Session_resume | `Go | `Check | `Wake | `Halt
    | `Model_job_succeeded(string, string, json)
    | `Model_job_failed(string, string, string) ]
  let initial_state = ""
  let on_event : context -> state -> event -> state task =
    fun ctx state event -> match event with
    | `Session_start -> Task.pure(state)
    | `Session_resume -> Task.pure(state)
    | `Go -> %s
    | `Wake -> Task.pure(state ++ "wake;")
    | `Halt -> Task.bind(Runtime.end_session("suppressed"), fun ignored -> Task.pure(state))
    | `Check -> Task.bind(Runtime.end_session(state), fun ignored -> Task.pure(state))
    | `Model_job_succeeded(id, recipe, result) ->
      Task.pure(state ++ "ok:" ++ id ++ ":" ++ Json.stringify(result) ++ ";")
    | `Model_job_failed(id, recipe, message) ->
      Task.pure(state ++ "error:" ++ id ++ ":" ++ message ++ ";")
</script>
|}
    action
;;

let fixture env environment name action =
  let fixture = Config_fixture.create environment ~name ~http_port:(F.reserve_port env) in
  F.save fixture (Config_fixture.prompt_path fixture) (prompt (action (payload fixture)));
  let config =
    Config_fixture.configuration fixture ()
    |> String.substr_replace_first
         ~pattern:"(snapshot_every_events 10)"
         ~with_:"(snapshot_every_events 1)"
    |> String.substr_replace_first
         ~pattern:"(shutdown_grace_ms 1000)"
         ~with_:"(shutdown_grace_ms 1000) (job_limits ((daemon_total 1) (per_session 1)))"
  in
  F.save fixture (Config_fixture.config_path fixture) config;
  fixture
;;

let with_fixture env fixture f =
  Eio.Switch.run (fun sw ->
    let port = F.reserve_port env in
    let provider = Provider.start ~sw ~env ~port in
    let daemon = F.start ~sw env fixture port in
    Exn.protect
      ~f:(fun () -> f sw fixture provider daemon port)
      ~finally:(fun () -> F.stop env daemon))
;;

let with_daemon env environment name action f =
  with_fixture env (fixture env environment name action) f
;;

let await_requests env provider count =
  F.await env "nested provider request" (fun () ->
    if Provider.request_count provider >= count then Some () else None)
;;

let is_delivered (job : Agent_protocol.Job.t) =
  match job.delivery with
  | Delivered _ -> true
  | Pending | Not_required -> false
;;

let is_terminal (job : Agent_protocol.Job.t) =
  match job.status with
  | Succeeded | Failed _ | Cancelled | Interrupted _ -> true
  | Queued | Running | Waiting_permission _ -> false
;;

let is_complete snapshot =
  (not (List.is_empty snapshot.Agent_protocol.Snapshot.jobs))
  && List.for_all snapshot.jobs ~f:(fun job ->
    is_terminal job
    && (is_delivered job
        ||
        match job.delivery with
        | Not_required -> true
        | _ -> false))
;;

let completed env client session =
  F.await_snapshot env client session "terminal jobs and delivery" is_complete
;;

let check env client session name =
  ignore
    (F.schedule client session (name ^ ":check") "Check" 0 : Agent_protocol.Schedule.t);
  let snapshot =
    F.await_snapshot env client session "moderator Check outcome" (fun s -> s.halted)
  in
  F.require
    (Option.is_none snapshot.failure)
    "moderator failed instead of observing outcomes";
  snapshot, Option.value_exn snapshot.halt_reason
;;

let await_checkpoint env fixture session predicate =
  try
    F.await env "durable background checkpoint" (fun () ->
      Option.filter (F.checkpoint env fixture session) ~f:predicate)
  with
  | exn ->
    let last = F.checkpoint env fixture session in
    raise_s
      [%sexp
        "background checkpoint wait failed"
      , (exn : Exn.t)
      , (Option.map last ~f:(fun state ->
           state.jobs, state.attachments, F.moderator_state state)
         : (Agent_protocol.Job.t list * Agent_protocol.Session.Attachment.t list * string)
             option)]
;;

let await_zero_clients env fixture session =
  await_checkpoint env fixture session (fun state ->
    List.is_empty state.attachments
    && (not (List.is_empty state.jobs))
    && List.for_all state.jobs ~f:(fun job ->
      is_terminal job
      && (is_delivered job
          ||
          match job.delivery with
          | Not_required -> true
          | _ -> false))
    && String.is_substring (F.moderator_state state) ~substring:"ok:")
;;

let job_events events id =
  List.filter_map events ~f:(fun (event : Agent_protocol.Event.Durable.t) ->
    match
      Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
      |> F.protocol_ok
    with
    | Job_state_changed job when Agent_protocol.Id.Job.compare job.id id = 0 ->
      Some (event, job)
    | _ -> None)
;;

let assert_order events =
  List.iter
    (List.zip_exn (List.drop_last_exn events) (List.tl_exn events))
    ~f:(fun (left, right) ->
      F.require
        Int64.(left.Agent_protocol.Event.Durable.sequence < right.sequence)
        "durable event sequences are not strictly increasing";
      F.require Int64.(left.revision <= right.revision) "durable revisions regressed")
;;

let assert_delivery_trace changes delivery =
  let deliveries = List.filter changes ~f:(fun (_, job) -> is_delivered job) in
  F.require
    (List.length deliveries = delivery)
    "completion delivery was missing or duplicated";
  Option.iter (List.hd deliveries) ~f:(fun (delivered, _) ->
    F.require
      (List.exists changes ~f:(fun (event, job) ->
         is_terminal job
         && (not (is_delivered job))
         && Int64.(event.Agent_protocol.Event.Durable.sequence < delivered.sequence)))
      "completion was delivered before its durable terminal state")
;;

let assert_job_trace events (job : Agent_protocol.Job.t) ~attempt ~delivery =
  let changes = job_events events job.id in
  let first = snd (List.hd_exn changes) in
  F.require
    (match first.status with
     | Queued -> first.attempt = 0
     | _ -> false)
    "model intent was not durable before execution";
  F.require
    (List.exists changes ~f:(fun (_, j) ->
       match j.status with
       | Running -> j.attempt = attempt
       | _ -> false))
    "durable running claim/attempt is missing";
  F.require (job.attempt = attempt) "terminal attempt count changed";
  F.require (Option.is_some job.completed_at) "terminal completion timestamp missing";
  assert_delivery_trace changes delivery
;;

let assert_identity session (job : Agent_protocol.Job.t) =
  F.require
    (Agent_protocol.Id.Session.compare session.F.summary.id job.session_id = 0)
    "job changed session identity";
  F.require (session.summary.generation = job.generation) "job changed generation";
  F.require (Agent_protocol.Job.equal_kind job.kind Model_call) "job was not a model call"
;;

let assert_success (job : Agent_protocol.Job.t) =
  (match job.status with
   | Succeeded -> ()
   | _ -> raise_s [%sexp "nested model did not succeed", (job : Agent_protocol.Job.t)]);
  let result = Option.value_exn job.result in
  F.require
    (match Jsonaf.member "final_text" result with
     | Some (`String "background-result") -> true
     | _ -> false)
    "nested provider text was not preserved in the job result";
  result
;;

let begin_work client name =
  let session = F.create client (name ^ ":create") in
  ignore (F.schedule client session (name ^ ":go") "Go" 0 : Agent_protocol.Schedule.t);
  session
;;

let success_text job result ~is_call =
  let id = Agent_protocol.Id.Job.to_string job.Agent_protocol.Job.id in
  if is_call
  then "call-ok:" ^ Jsonaf.to_string result ^ ";"
  else "spawn:" ^ id ^ ";ok:" ^ id ^ ":" ^ Jsonaf.to_string result ^ ";"
;;

let verify_success env client session name durable ~is_call =
  let session, _ = F.attach client session.F.summary (name ^ ":attach") in
  let job = List.hd_exn (completed env client session).jobs in
  let expected = success_text job (assert_success job) ~is_call in
  let _, observed = check env client session name in
  F.require
    (String.equal observed expected)
    "moderator did not observe exact model success and ID";
  F.require
    (String.equal (F.moderator_state durable) expected)
    "model outcome was not checkpointed with zero clients";
  let events = F.events client session (name ^ ":replay") in
  assert_order events;
  assert_identity session job;
  assert_job_trace events job ~attempt:1 ~delivery:(if is_call then 0 else 1)
;;

let success_case env environment ~is_call =
  let name = if is_call then "call.success" else "spawn.zero-client-success" in
  with_daemon
    env
    environment
    name
    (if is_call then call else spawn)
    (fun sw fixture provider _daemon _port ->
       let session =
         F.with_client ~sw env fixture (fun client ->
           let session = begin_work client name in
           await_requests env provider 1;
           F.close_client client;
           session)
       in
       Provider.release provider ~index:0;
       let durable = await_zero_clients env fixture session in
       F.require (Option.is_none durable.failure) "zero-client moderator failed";
       F.with_client ~sw env fixture (fun client ->
         verify_success env client session name durable ~is_call);
       F.require (Provider.request_count provider = 1) "nested model ran more than once")
;;

let verify_failure env client session name ~is_call =
  let job = List.hd_exn (completed env client session).jobs in
  let message =
    match job.status with
    | Failed error -> error.message
    | _ -> failwith "job did not fail"
  in
  let _, observed = check env client session name in
  let id = Agent_protocol.Id.Job.to_string job.id in
  let expected =
    if is_call
    then "call-error;"
    else "spawn:" ^ id ^ ";error:" ^ id ^ ":" ^ message ^ ";"
  in
  F.require
    (String.equal observed expected)
    "moderator did not observe the durable model failure";
  let events = F.events client session (name ^ ":replay") in
  assert_order events;
  assert_identity session job;
  assert_job_trace events job ~attempt:1 ~delivery:(if is_call then 0 else 1)
;;

let failure_case env environment ~is_call =
  let name = if is_call then "call.failure" else "spawn.failure" in
  let action input =
    if is_call
    then
      call
        (String.substr_replace_all input ~pattern:"nested.chatmd" ~with_:"missing.chatmd")
    else spawn "`Null"
  in
  with_daemon env environment name action (fun sw fixture provider _daemon _port ->
    F.with_client ~sw env fixture (fun client ->
      let session = begin_work client name in
      verify_failure env client session name ~is_call;
      F.require
        (Provider.request_count provider = 0)
        "invalid payload reached the provider"))
;;

let cancel client session job name =
  let request =
    Agent_protocol.Job.Cancel_request.
      { session_id = session.F.summary.id
      ; attachment_id = session.attachment_id
      ; job_id = job.Agent_protocol.Job.id
      ; idempotency_key = F.key name
      }
  in
  match F.request client (Job_cancel request) with
  | Job_cancel result -> result
  | _ -> failwith "unexpected cancel result"
;;

let verify_cancel env client session name =
  let job = List.hd_exn (completed env client session).jobs in
  F.require
    (match job.status with
     | Cancelled -> true
     | _ -> false)
    "late provider result overwrote cancellation";
  let _, observed = check env client session name in
  let id = Agent_protocol.Id.Job.to_string job.id in
  F.require
    (String.equal observed ("spawn:" ^ id ^ ";error:" ^ id ^ ":job was cancelled;"))
    "moderator did not observe cancellation exactly once";
  assert_job_trace (F.events client session (name ^ ":replay")) job ~attempt:1 ~delivery:1
;;

let cancel_case env environment =
  let name = "spawn.running-cancel" in
  with_daemon env environment name spawn (fun sw fixture provider _daemon _port ->
    F.with_client ~sw env fixture (fun client ->
      let session = begin_work client name in
      await_requests env provider 1;
      let job = List.hd_exn (F.snapshot client session).jobs in
      F.require
        (match job.status with
         | Running -> job.attempt = 1
         | _ -> false)
        "cancel target was not executing";
      let first = cancel client session job (name ^ ":cancel") in
      let second = cancel client session job (name ^ ":cancel") in
      F.require
        (Agent_protocol.Id.Job.compare first.job.id second.job.id = 0)
        "idempotent cancel changed job ID";
      Provider.release provider ~index:0;
      verify_cancel env client session name))
;;

let cancel_blocked_case env environment =
  let name = "spawn.cancel-releases-blocked-capacity" in
  with_daemon env environment name spawn (fun sw fixture provider _daemon _port ->
    F.with_client ~sw env fixture (fun client ->
      let next = F.create client (name ^ ":next") in
      let session = begin_work client name in
      await_requests env provider 1;
      let job = List.hd_exn (F.snapshot client session).jobs in
      ignore
        (cancel client session job (name ^ ":cancel")
         : Agent_protocol.Job.Cancel_result.t);
      Exn.protect
        ~finally:(fun () -> Provider.release provider ~index:0)
        ~f:(fun () ->
          ignore
            (F.schedule client next (name ^ ":next-go") "Go" 0
             : Agent_protocol.Schedule.t);
          await_requests env provider 2;
          verify_cancel env client session name;
          Provider.release provider ~index:1;
          let finished = completed env client next in
          ignore (assert_success (List.hd_exn finished.jobs) : Jsonaf.t))))
;;

let call_cancel_case env environment =
  let name = "call.cancel-blocked-provider" in
  with_daemon env environment name call (fun sw fixture provider _daemon _port ->
    F.with_client ~sw env fixture (fun client ->
      let session = begin_work client name in
      await_requests env provider 1;
      let job = List.hd_exn (F.snapshot client session).jobs in
      ignore
        (cancel client session job (name ^ ":cancel")
         : Agent_protocol.Job.Cancel_result.t);
      Exn.protect
        ~finally:(fun () -> Provider.release provider ~index:0)
        ~f:(fun () ->
          let _, observed = check env client session name in
          F.require
            (String.equal observed "call-error;")
            "cancelled synchronous call did not return exactly one error";
          let job = List.hd_exn (F.snapshot client session).jobs in
          F.require
            (match job.status, job.delivery with
             | Cancelled, Not_required -> true
             | _ -> false)
            "synchronous cancellation generated an asynchronous completion")))
;;

let two_spawns input =
  sprintf
    {|Task.bind(Model.spawn("agent_prompt_v1", %s), fun first ->
    Task.bind(Model.spawn("agent_prompt_v1", %s), fun second ->
    Task.pure(state ++ "spawn:" ++ first ++ ";spawn:" ++ second ++ ";")))|}
    input
    input
;;

let is_running (job : Agent_protocol.Job.t) =
  match job.status with
  | Running -> true
  | _ -> false
;;

let stop_blocked_case env environment =
  let name = "stop.cancel-blocked-jobs" in
  with_daemon env environment name two_spawns (fun sw fixture provider _daemon _port ->
    F.with_client ~sw env fixture (fun client ->
      let session = begin_work client name in
      await_requests env provider 1;
      Exn.protect
        ~finally:(fun () -> Provider.release provider ~index:0)
        ~f:(fun () ->
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 3. (fun () ->
            ignore
              (F.request
                 client
                 (Session_stop
                    { session_id = session.summary.id
                    ; attachment_id = session.attachment_id
                    ; mode = Cancel
                    ; idempotency_key = F.key (name ^ ":stop")
                    })
               : Agent_protocol.Method_result.t));
          let state = F.snapshot client session in
          F.require
            (List.length state.jobs = 2
             && List.for_all state.jobs ~f:(fun job ->
               match job.status with
               | Cancelled | Interrupted _ -> true
               | _ -> false))
            "cancelling stop left runnable jobs";
          F.require (Provider.request_count provider = 1) "queued job ran after stop")))
;;

let is_queued (job : Agent_protocol.Job.t) =
  match job.status with
  | Queued -> true
  | _ -> false
;;

let running_and_queued env client session provider =
  await_requests env provider 1;
  let state =
    F.await_snapshot
      env
      client
      session
      "capacity-bound running and queued jobs"
      (fun state -> List.length state.jobs = 2 && List.count state.jobs ~f:is_running = 1)
  in
  let running = List.find_exn state.jobs ~f:is_running in
  let queued = List.find_exn state.jobs ~f:is_queued in
  F.require
    (running.attempt = 1 && queued.attempt = 0)
    "capacity waiting consumed an attempt";
  F.require
    (Provider.request_count provider = 1)
    "daemon exceeded configured job capacity";
  running, queued
;;

let reuse_capacity env client session provider name =
  ignore (F.schedule client session (name ^ ":again") "Go" 0 : Agent_protocol.Schedule.t);
  await_requests env provider 2;
  F.require
    (Provider.request_count provider = 2)
    "released capacity did not admit exactly one next job";
  Provider.release provider ~index:1;
  await_requests env provider 3;
  Provider.release provider ~index:2
;;

let verify_capacity env client session provider queued name =
  let state = completed env client session in
  F.require (List.length state.jobs = 4) "capacity exercise lost job identities";
  let _, observed = check env client session name in
  let id = Agent_protocol.Id.Job.to_string queued.Agent_protocol.Job.id in
  F.require
    (String.is_substring observed ~substring:("error:" ^ id ^ ":job was cancelled;"))
    "moderator missed queued cancellation";
  let events = F.events client session (name ^ ":replay") in
  assert_order events;
  List.iter
    (List.filter state.jobs ~f:(fun job ->
       Agent_protocol.Id.Job.compare job.id queued.id <> 0))
    ~f:(fun job ->
      ignore (assert_success job : Jsonaf.t);
      assert_job_trace events job ~attempt:1 ~delivery:1);
  F.require
    (Provider.request_count provider = 3)
    "cancelled queued job reached the provider"
;;

let cancel_queued env client session provider queued name =
  ignore
    (cancel client session queued (name ^ ":cancel") : Agent_protocol.Job.Cancel_result.t);
  Provider.release provider ~index:0;
  let state = completed env client session in
  let cancelled =
    List.find_exn state.jobs ~f:(fun job ->
      Agent_protocol.Id.Job.compare job.id queued.id = 0)
  in
  F.require
    (cancelled.attempt = 0
     &&
     match cancelled.status with
     | Cancelled -> true
     | _ -> false)
    "queued cancellation executed or consumed an attempt"
;;

let capacity_case env environment =
  let name = "spawn.capacity-queued-cancel" in
  with_daemon env environment name two_spawns (fun sw fixture provider _daemon _port ->
    F.with_client ~sw env fixture (fun client ->
      let session = begin_work client name in
      let running, queued = running_and_queued env client session provider in
      cancel_queued env client session provider queued name;
      reuse_capacity env client session provider name;
      verify_capacity env client session provider queued name;
      F.require
        (List.exists (F.snapshot client session).jobs ~f:(fun job ->
           Agent_protocol.Id.Job.compare job.id running.id = 0))
        "first running job disappeared"))
;;

let capacity_fixture env environment dimension =
  let fixture = fixture env environment dimension two_spawns in
  let fields =
    [ "daemon_total"
    ; "per_principal"
    ; "per_prompt"
    ; "per_workspace"
    ; "per_session"
    ; "per_kind"
    ]
  in
  let limits =
    List.map fields ~f:(fun name ->
      sprintf "(%s %d)" name (if String.equal name dimension then 1 else 64))
    |> String.concat ~sep:" "
  in
  let path =
    Temporary_environment.path environment (Config_fixture.config_path fixture)
  in
  let config =
    Eio.Path.load path
    |> String.substr_replace_first
         ~pattern:"(job_limits ((daemon_total 1) (per_session 1)))"
         ~with_:(sprintf "(job_limits (%s (max_nested_depth 0)))" limits)
  in
  F.save fixture (Config_fixture.config_path fixture) config;
  fixture
;;

let capacity_dimension env environment dimension record =
  with_fixture
    env
    (capacity_fixture env environment dimension)
    (fun sw fixture provider daemon _ ->
       F.with_client ~sw env fixture (fun client ->
         let session = begin_work client dimension in
         let _, queued = running_and_queued env client session provider in
         record fixture daemon client (dimension ^ "-saturated");
         cancel_queued env client session provider queued dimension;
         reuse_capacity env client session provider dimension;
         verify_capacity env client session provider queued dimension;
         record fixture daemon client (dimension ^ "-released")))
;;

let run_load_capacity env ~record =
  List.iter
    [ "daemon_total"
    ; "per_principal"
    ; "per_prompt"
    ; "per_workspace"
    ; "per_session"
    ; "per_kind"
    ]
    ~f:(fun dimension ->
      Temporary_environment.with_
        ~scenario:("load-capacity-" ^ dimension)
        ~env
        (fun environment -> capacity_dimension env environment dimension record))
;;

let same_job state (job : Agent_protocol.Job.t) =
  List.find_exn state.Agent_protocol.Snapshot.jobs ~f:(fun candidate ->
    Agent_protocol.Id.Job.compare candidate.id job.id = 0)
;;

let schedule_delivered (schedule : Agent_protocol.Schedule.t) =
  match schedule.status with
  | Delivered -> schedule.delivery_count = 1
  | _ -> false
;;

let has_observed_job observed (job : Agent_protocol.Job.t) =
  let prefix =
    match job.status with
    | Succeeded -> "ok:"
    | _ -> "error:"
  in
  let pattern = prefix ^ Agent_protocol.Id.Job.to_string job.id ^ ":" in
  List.length (String.substr_index_all observed ~pattern ~may_overlap:false) = 1
;;

let checkpoint_ready env fixture session schedule =
  await_checkpoint env fixture session (fun state ->
    let observed = F.moderator_state state in
    List.for_all state.jobs ~f:(fun job -> is_terminal job && is_delivered job)
    && List.for_all state.jobs ~f:(has_observed_job observed)
    && List.exists state.schedules ~f:(fun candidate ->
      Agent_protocol.Id.Schedule.compare candidate.id schedule.Agent_protocol.Schedule.id
      = 0
      && schedule_delivered candidate)
    && String.is_substring observed ~substring:"wake;")
;;

let assert_checkpoint_equal before after =
  F.require
    (String.equal (F.moderator_state before) (F.moderator_state after))
    "restart duplicated or lost a moderator-observed outcome";
  F.require
    (Sexp.equal
       ([%sexp_of: Agent_protocol.Job.t list] before.jobs)
       ([%sexp_of: Agent_protocol.Job.t list] after.jobs))
    "restart changed completed job IDs, attempts, or delivery timestamps";
  F.require
    (Sexp.equal
       ([%sexp_of: Agent_protocol.Schedule.t list] before.schedules)
       ([%sexp_of: Agent_protocol.Schedule.t list] after.schedules))
    "restart changed delivered schedule state"
;;

let with_restart ~sw env fixture provider_port f =
  let daemon = F.start ~sw env fixture provider_port in
  Exn.protect ~f:(fun () -> f daemon) ~finally:(fun () -> F.stop env daemon)
;;

let wait_past_due env schedule =
  let now = Agent_protocol.Timestamp.now () |> Agent_protocol.Timestamp.to_time_ns in
  let due =
    Agent_protocol.Timestamp.to_time_ns schedule.Agent_protocol.Schedule.next_due_at
  in
  Eio.Time.sleep
    (Eio.Stdenv.clock env)
    (Float.max 0. (Time_ns.diff due now |> Time_ns.Span.to_sec))
;;

let schedule_changes events schedule =
  List.filter_map events ~f:(fun event ->
    match
      Agent_protocol.Event.Durable.Payload.of_json
        ~kind:event.Agent_protocol.Event.Durable.kind
        event.payload
      |> F.protocol_ok
    with
    | Schedule_created candidate
    | Schedule_state_changed candidate
    | Schedule_cancelled candidate
      when Agent_protocol.Id.Schedule.compare
             candidate.id
             schedule.Agent_protocol.Schedule.id
           = 0 -> Some candidate
    | _ -> None)
;;

let assert_schedule_trace events schedule =
  let changes = schedule_changes events schedule in
  F.require
    (match (List.hd_exn changes).status with
     | Scheduled -> true
     | _ -> false)
    "schedule intent missing before delivery";
  F.require
    (List.count changes ~f:schedule_delivered = 1)
    "schedule durable delivery was duplicated";
  F.require
    (List.exists changes ~f:(fun schedule ->
       match schedule.status with
       | Delivering -> true
       | _ -> false))
    "schedule delivery claim was not persisted"
;;

let prepare_completed_restart env client provider name =
  let session = begin_work client name in
  await_requests env provider 1;
  Provider.release provider ~index:0;
  let state = completed env client session in
  let wake = F.schedule client session (name ^ ":wake") "Wake" 1_500 in
  let events = F.events client session (name ^ ":before-restart") in
  F.close_client client;
  session, wake, state, events
;;

let continued_events client session name before =
  let sequence = (List.last_exn before).Agent_protocol.Event.Durable.sequence in
  before @ F.events_since client session name sequence
;;

let inspect_completed_restart env client session before name events =
  let state = F.snapshot client session in
  let job = same_job state (List.hd_exn before.Agent_protocol.Snapshot.jobs) in
  ignore (assert_success job : Jsonaf.t);
  let events = continued_events client session (name ^ ":replay") events in
  assert_job_trace events job ~attempt:1 ~delivery:1;
  F.close_client client;
  events
;;

let inspect_stable_restart env client session after wake name events =
  let session, _ = F.attach client session.F.summary (name ^ ":attach-again") in
  let _, observed = check env client session name in
  F.require
    (String.equal observed (F.moderator_state after))
    "resume redelivered a completion";
  assert_schedule_trace
    (continued_events client session (name ^ ":final-replay") events)
    wake
;;

let first_completed_restart ~sw env fixture provider session wake before name port events =
  with_restart ~sw env fixture port (fun _ ->
    let checkpoint = checkpoint_ready env fixture session wake in
    F.require (List.is_empty checkpoint.attachments) "restart delivery required a client";
    F.require (Provider.request_count provider = 1) "completed job reran after restart";
    let events =
      F.with_client ~sw env fixture (fun client ->
        inspect_completed_restart env client session before name events)
    in
    checkpoint, events)
;;

let graceful_restart_case env environment =
  let name = "restart.completed-jobs-scheduled-wake" in
  with_daemon env environment name spawn (fun sw fixture provider daemon port ->
    let session, wake, before, events =
      F.with_client ~sw env fixture (fun client ->
        prepare_completed_restart env client provider name)
    in
    F.stop env daemon;
    wait_past_due env wake;
    let after, events =
      first_completed_restart
        ~sw
        env
        fixture
        provider
        session
        wake
        before
        name
        port
        events
    in
    with_restart ~sw env fixture port (fun _ ->
      let stable = checkpoint_ready env fixture session wake in
      assert_checkpoint_equal after stable;
      F.with_client ~sw env fixture (fun client ->
        inspect_stable_restart env client session after wake name events);
      F.require
        (Provider.request_count provider = 1)
        "second restart reran a completed job"))
;;

let kill_daemon env daemon =
  Daemon_process.signal daemon Stdlib.Sys.sigkill;
  let result =
    F.await env "killed daemon reaped" (fun () -> Daemon_process.result daemon)
  in
  F.require
    (match result.exit with
     | Signaled _ -> true
     | Exited _ -> false)
    "daemon did not undergo a real process crash"
;;

let assert_recovered_jobs state running queued =
  let interrupted = same_job state running in
  let resumed = same_job state queued in
  F.require
    (match interrupted.status with
     | Interrupted _ -> true
     | _ -> false)
    "running job was rerun or lost instead of interrupted";
  F.require
    (interrupted.attempt = 1 && resumed.attempt = 1)
    "restart changed attempt semantics";
  ignore (assert_success resumed : Jsonaf.t)
;;

let assert_interruption_observed observed checkpoint running =
  F.require
    (String.equal observed (F.moderator_state checkpoint))
    "restart moderator state diverged from checkpoint";
  F.require
    (String.is_substring
       observed
       ~substring:
         ("error:"
          ^ Agent_protocol.Id.Job.to_string running.Agent_protocol.Job.id
          ^ ":daemon restarted while the job was running;"))
    "moderator missed interrupted job identity and reason"
;;

let inspect_crash_restart
      env
      client
      session
      running
      queued
      checkpoint
      wake
      name
      before_events
  =
  let session, _ = F.attach client session.F.summary (name ^ ":attach") in
  let state = completed env client session in
  assert_recovered_jobs state running queued;
  let _, observed = check env client session name in
  assert_interruption_observed observed checkpoint running;
  let events = continued_events client session (name ^ ":replay") before_events in
  assert_order events;
  List.iter state.jobs ~f:(fun job ->
    assert_identity session job;
    assert_job_trace events job ~attempt:1 ~delivery:1);
  assert_schedule_trace events wake
;;

let assert_running_checkpoint env fixture session running =
  ignore
    (await_checkpoint env fixture session (fun state ->
       List.length state.jobs = 2
       && List.exists state.jobs ~f:(fun job ->
         Agent_protocol.Id.Job.compare job.id running.Agent_protocol.Job.id = 0
         && is_running job))
     : Agent_session.Session_state.t)
;;

let crash_restart_case env environment =
  let name = "restart.running-interrupted-queued-resumed" in
  with_daemon env environment name two_spawns (fun sw fixture provider daemon port ->
    let session, running, queued, wake, events =
      F.with_client ~sw env fixture (fun client ->
        let session = begin_work client name in
        let running, queued = running_and_queued env client session provider in
        let wake = F.schedule client session (name ^ ":wake") "Wake" 1_000 in
        let events = F.events client session (name ^ ":before-crash") in
        F.close_client client;
        session, running, queued, wake, events)
    in
    assert_running_checkpoint env fixture session running;
    kill_daemon env daemon;
    Provider.release provider ~index:0;
    wait_past_due env wake;
    with_restart ~sw env fixture port (fun _ ->
      await_requests env provider 2;
      Provider.release provider ~index:1;
      let checkpoint = checkpoint_ready env fixture session wake in
      F.with_client ~sw env fixture (fun client ->
        inspect_crash_restart
          env
          client
          session
          running
          queued
          checkpoint
          wake
          name
          events;
        F.require
          (Provider.request_count provider = 2)
          "restart reran an interrupted model request")))
;;

let same_schedule schedules (schedule : Agent_protocol.Schedule.t) =
  List.find_exn schedules ~f:(fun candidate ->
    Agent_protocol.Id.Schedule.compare candidate.Agent_protocol.Schedule.id schedule.id
    = 0)
;;

let create_overdue client name =
  let session = F.create client (name ^ ":create") in
  let schedule suffix policy =
    F.schedule_with_policy client session (name ^ suffix) "Wake" 1_500 policy
  in
  let deliver = schedule ":deliver" Deliver_once_immediately in
  let skip = schedule ":skip" Skip_if_expired in
  let fail = schedule ":fail" Fail in
  let events = F.events client session (name ^ ":before-restart") in
  F.close_client client;
  session, deliver, skip, fail, events
;;

let assert_misfires schedules skip fail =
  let skipped = same_schedule schedules skip in
  let failed = same_schedule schedules fail in
  F.require
    (match skipped.status with
     | Delivered -> skipped.delivery_count = 0 && Option.is_none skipped.last_delivery_at
     | _ -> false)
    "Skip_if_expired was delivered to the moderator";
  F.require
    (match failed.status with
     | Failed error ->
       Agent_protocol.Error.equal_code error.code Interrupted
       && failed.delivery_count = 0
       && String.equal error.message "schedule expired while the daemon was unavailable"
     | _ -> false)
    "Fail misfire did not persist the expected interruption"
;;

let inspect_overdue env client session deliver name before_events =
  let session, _ = F.attach client session.F.summary (name ^ ":attach") in
  let _, observed = check env client session name in
  F.require
    (String.equal observed "wake;")
    "overdue schedule policies produced extra moderator events";
  let events = continued_events client session (name ^ ":replay") before_events in
  assert_order events;
  assert_schedule_trace events deliver
;;

let overdue_case env environment =
  let name = "restart.schedule-overdue-policies" in
  with_daemon env environment name spawn (fun sw fixture provider daemon port ->
    let session, deliver, skip, fail, events =
      F.with_client ~sw env fixture (fun client -> create_overdue client name)
    in
    F.stop env daemon;
    wait_past_due env fail;
    with_restart ~sw env fixture port (fun _ ->
      let checkpoint =
        await_checkpoint env fixture session (fun state ->
          schedule_delivered (same_schedule state.schedules deliver)
          && String.equal (F.moderator_state state) "wake;")
      in
      assert_misfires checkpoint.schedules skip fail;
      F.with_client ~sw env fixture (fun client ->
        inspect_overdue env client session deliver name events);
      F.require
        (Provider.request_count provider = 0)
        "schedule-only recovery invoked a model"))
;;

let suppression_prompt fixture =
  let source = prompt (spawn (payload fixture)) in
  let pattern =
    {|Task.pure(state ++ "ok:" ++ id ++ ":" ++ Json.stringify(result) ++ ";")|}
  in
  let with_ =
    {|Task.bind(Runtime.end_session("suppressed"), fun ignored ->
        Task.pure(state ++ "ok:" ++ id ++ ":" ++ Json.stringify(result) ++ ";"))|}
  in
  F.save
    fixture
    (Config_fixture.prompt_path fixture)
    (String.substr_replace_first source ~pattern ~with_)
;;

let start_suppression env client provider name =
  let session = begin_work client name in
  await_requests env provider 1;
  Provider.release provider ~index:0;
  let halted =
    F.await_snapshot env client session "end_session after model completion" (fun state ->
      state.halted)
  in
  F.require
    (Option.equal String.equal halted.halt_reason (Some "suppressed"))
    "wrong end_session reason";
  let wake = F.schedule client session (name ^ ":wake") "Wake" 0 in
  F.close_client client;
  session, wake
;;

let is_suppressed_schedule (schedule : Agent_protocol.Schedule.t) =
  match schedule.status with
  | Failed error ->
    Agent_protocol.Error.equal_code error.code Invalid_state
    && String.equal error.message "Session has ended"
    && schedule.delivery_count = 0
    && Option.is_none schedule.last_delivery_at
  | Scheduled | Delivering | Delivered | Cancelled -> false
;;

let suppression_case env environment =
  let name = "moderator.end-session-suppresses-timer" in
  let fixture = fixture env environment name spawn in
  suppression_prompt fixture;
  with_fixture env fixture (fun sw fixture provider daemon _port ->
    let session, wake =
      F.with_client ~sw env fixture (fun client ->
        start_suppression env client provider name)
    in
    let checkpoint =
      await_checkpoint env fixture session (fun state ->
        List.for_all state.jobs ~f:(fun job -> is_terminal job && is_delivered job)
        && is_suppressed_schedule (same_schedule state.schedules wake))
    in
    F.require checkpoint.halted "late completion cleared the halt";
    let job = List.hd_exn checkpoint.jobs in
    ignore (assert_success job : Jsonaf.t);
    let expected = success_text job (Option.value_exn job.result) ~is_call:false in
    F.require
      (String.equal (F.moderator_state checkpoint) expected)
      "halted moderator observed a late timer";
    F.require
      (Provider.request_count provider = 1)
      "end_session started another model request";
    F.stop env daemon;
    let durable = F.checkpoint env fixture session |> Option.value_exn in
    F.require
      (String.equal (F.moderator_state durable) expected)
      "shutdown delivered a suppressed event")
;;

let cases =
  [ "spawn.zero-client-success", success_case ~is_call:false
  ; "call.success", success_case ~is_call:true
  ; "spawn.failure", failure_case ~is_call:false
  ; "call.failure", failure_case ~is_call:true
  ; "spawn.running-cancel", cancel_case
  ; "spawn.cancel-releases-blocked-capacity", cancel_blocked_case
  ; "call.cancel-blocked-provider", call_cancel_case
  ; "stop.cancel-blocked-jobs", stop_blocked_case
  ; "retry.backoff-reopen-late-completion", Job_recovery_checks.run
  ; "spawn.capacity-queued-cancel", capacity_case
  ; "restart.completed-jobs-scheduled-wake", graceful_restart_case
  ; "restart.running-interrupted-queued-resumed", crash_restart_case
  ; "restart.schedule-overdue-policies", overdue_case
  ; "moderator.end-session-suppresses-timer", suppression_case
  ]
;;

let run env ~case =
  let selected =
    match case with
    | None -> cases
    | Some name -> [ name, List.Assoc.find_exn cases name ~equal:String.equal ]
  in
  Temporary_environment.with_
    ~scenario:"background-orchestration"
    ~env
    (fun environment ->
       List.iter selected ~f:(fun (name, test) ->
         try test env environment with
         | exn -> raise_s [%sexp "background case failed", (name : string), (exn : Exn.t)]);
       let result =
         [%sexp
           { scenario = ("background-orchestration" : string)
           ; passed_cases = (List.map selected ~f:fst : string list)
           }]
       in
       Eio.Flow.copy_string (Sexp.to_string_hum result ^ "\n") (Eio.Stdenv.stdout env))
;;
