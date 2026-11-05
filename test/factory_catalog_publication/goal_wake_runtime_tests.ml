open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Resume = Goal_resume_runtime_tests
module Staging = Goal_resume_tests
module Manual = Goal_job_runtime_tests
module G = P.Goal
module Service = Agent_server.Goal_run_service
module Fault = Goal_wake_fault_support

(* Actual executing host and timer scheduler. Both due clocks are controlled;
   the production scheduler owns source loading, producer ACK and callback claim. *)
let context_file = "goal-wake-context.json"

let prompt =
  {|<developer>Preserve one actual Goal wait across a cold wake.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="goal_wake" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let field value key = match Json.get_field(value, key) with
| `Some(found) -> found
| `None -> `Null
let read_context () =
  let* read = Tool.call("read_file", `Object([
    {key = "root"; value = `String("data")},
    {key = "file"; value = `String("goal-wake-context.json")}
  ])) in
  match read with
  | `Error(message) -> Task.fail(message)
  | `Ok(`String(text)) ->
    let lines = String.split(text, "\n") in
    Task.pure(Json.parse(lines[2]))
  | `Ok(_) -> Task.fail("read_file did not return actual text")
let on_event ctx state event = match event with
| `Session_resume ->
  if state == 0 then
    let* context = read_context() in
    let* id = Schedule.after_ms(600000, `String("545 actual cold timer")) in
    let* schedule = Schedule.get(id) in
    let owner = field(schedule, "ownership") in
    let wake = `Object([
      {key = "run_id"; value = field(context, "run_id")},
      {key = "source"; value = field(context, "source")},
      {key = "occurrence"; value = `Object([
        {key = "kind"; value = `String("delivered_timer")},
        {key = "schedule_id"; value = `String(id)},
        {key = "delivery_count"; value = `Number(1.0)},
        {key = "creator"; value = `Object([
          {key = "kind"; value = field(owner, "creator_type")},
          {key = "id"; value = field(owner, "creator_id")}
        ])}
      ])}
    ]) in
    Task.bind(Run.wait(wake), fun ignored -> Task.pure(state + 1))
  else Task.pure(state)
| `Internal_event(_) ->
  let* context = read_context() in
  Task.bind(Goal.finish(field(context, "finish")), fun ignored -> Task.pure(state + 1))
| _ -> Task.pure(state)
</script>|}
;;

let write_context env root run finish =
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / root / context_file)
    (Jsonaf.to_string
       (`Object
           ([ "run_id", P.Id.Run.to_json run.P.Run.id
            ; "source", P.Run_source.to_json run.source
            ]
            @ Option.to_list (Option.map finish ~f:(fun intent -> "finish", intent))))
     ^ "\n")
;;

let waiting current admission =
  match (Resume.run current admission).lifecycle with
  | Waiting wake -> wake
  | Admitted | Active | Terminal _ -> failwith "actual Goal did not retain its Wait"
;;

let one_schedule current =
  Resume.one_actual "actual owned timer" current.A.Session_state.schedules
;;

let queued_receipt current id =
  List.find current.A.Session_state.moderator_executions ~f:(fun receipt ->
    P.Id.Moderator_execution.equal receipt.P.Moderator_execution.context.id id)
  |> Option.value_exn
;;

let assert_native current admission event =
  match
    List.filter current.A.Session_state.invocations ~f:(fun invocation ->
      String.equal invocation.P.Invocation.context.tool_name "read_file"
      && Option.exists
           invocation.parent_event
           ~f:(P.Id.Moderator_execution.equal event.P.Moderator_execution.context.id))
  with
  | [ invocation ] ->
    (match invocation.inference_origin with
     | Some (Run id) -> assert (P.Id.Run.equal id admission.P.Run_receipt.run_id)
     | Some Unbound | None -> failwith "queued native effect lost its actual Run");
    (match invocation.status with
     | Resolved (Complete _) | Published (Complete _) -> ()
     | Admitted | Dispatching | Resolved _ | Published _ ->
       raise_s [%sexp "native queued effect failed", (invocation : P.Invocation.t)])
  | invocations ->
    raise_s [%sexp "exact queued native effect", (invocations : P.Invocation.t list)]
;;

let with_due_clocks (env : Eio_unix.Stdenv.base) advance : Eio_unix.Stdenv.base =
  let mono_clock, _, _, advance_mono = controlled_monotonic_clock env#mono_clock in
  let clock, _, _, advance_wall =
    controlled_wall_clock env#clock ~initial:(Eio.Time.now env#clock)
  in
  (advance
   := fun seconds ->
        advance_wall (Eio.Time.now clock +. seconds);
        advance_mono seconds);
  object
    method clock = clock
    method mono_clock = mono_clock
    method domain_mgr = env#domain_mgr
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method process_mgr = env#process_mgr
    method fs = env#fs
    method cwd = env#cwd
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;

module Scenario = struct
  type t =
    | Complete
    | Stop
    | Claim_failure of Fault.Cut.t
end

let run (scenario : Scenario.t) =
  let fault = Fault.State.create () in
  let construction_armed = ref false in
  let construction_entered, construction_enter = Eio.Promise.create () in
  let construction_release, construction_release_u = Eio.Promise.create () in
  let construction_released = ref false in
  let construction_hits = ref 0 in
  let release_construction () =
    if not !construction_released
    then (
      construction_released := true;
      Eio.Promise.resolve construction_release_u ())
  in
  let advance = ref (fun _ -> failwith "due clocks are not installed") in
  let recovery_env = ref None in
  let counters : Retry_support.counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr counters.providers;
      completed_response "545 initial Goal turn")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          if !construction_armed
          then (
            construction_armed := false;
            Int.incr construction_hits;
            Eio.Promise.resolve construction_enter ();
            Eio.Promise.await construction_release);
          base.runtime_inference_ports actor)
    }
  in
  Creation_tests.with_daemon
    ~wrap_env:(fun env ->
      let wrapped = with_due_clocks env advance in
      recovery_env := Some wrapped;
      match scenario with
      | Complete | Stop -> wrapped
      | Claim_failure cut ->
        Crash_fault_io.wrap
          wrapped
          ~boundary:(Fault.Cut.boundary cut)
          ~matches:(fun path ->
            String.is_substring path ~substring:"/journal/"
            && String.is_suffix path ~suffix:".log")
          ~reached:(Fault.State.reached fault env))
    ~max_root_agents:
      (match scenario with
       | Complete | Claim_failure _ -> 1
       | Stop -> 2)
    ~prompt_text:prompt
    ~qualify_chatml_extensions:true
    ~sources:[ context_file, "{}\n" ]
    ~policy
    (fun env sw root daemon ->
       let phase = ref "initial runtime" in
       let observed = ref None in
       try
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
           Source.with_loaded_entry
             daemon
             (fun ~entry ~runtime:initial_runtime ~revision:_ ->
                phase := "create Goal";
                let session, created = Goal.create_goal daemon entry in
                observed := Some created;
                phase := "start Goal episode";
                let admission =
                  Resume.start_episode daemon entry original session created
                in
                phase := "initial Goal turn";
                let initial = Goal.await_input env entry counters in
                [%test_eq: int] 1 !(counters.providers);
                let initial_startup = Manual.startup initial in
                [%test_eq: int] 1 (List.length initial_startup);
                let attachment, _ =
                  Actor.attach entry.actor ~mode:Read_write ~subscribe:false
                  |> protocol_ok
                in
                protect_cleanup
                  ~finally:(fun () ->
                    release_construction ();
                    Actor.detach entry.actor attachment.id |> protocol_ok)
                  (fun () ->
                     Goal_admitted_guard_tests.command
                       daemon
                       entry
                       ~key:"goal-wake-initial-hold"
                       (Hold (Goal.goal_id, Operator_pause))
                     |> protocol_ok
                     |> ignore;
                     Actor.stop entry.actor ~attachment_id:attachment.id ~mode:Graceful
                     |> protocol_ok
                     |> ignore;
                     Owner.unload_and_wait entry.runtime |> protocol_ok;
                     assert (not (Owner.is_loaded entry.runtime));
                     phase := "stopped initial runtime";
                     let stopped = Resume.checkpoint entry in
                     let original_run = Resume.run stopped admission in
                     write_context env root original_run None;
                     let operator = Staging.guarded Staging.resume_principal (ref true) in
                     let request =
                       Staging.exact_request stopped ~key:"goal-wake-first-passive-resume"
                     in
                     phase := "explicit resume into Wait";
                     let receipt, wake =
                       match
                         Service.resume_episode
                           (Resume.public daemon)
                           ~entry
                           ~actor:operator
                           ~session
                           ~request
                         |> protocol_ok
                       with
                       | Resumed { receipt; continuation = Waiting wake } -> receipt, wake
                       | Resumed { continuation = Started _ | Settled _; _ } | Retained _
                         -> failwith "setup did not write its actual passive Wait"
                       | Resume_failed { error; _ } | Rejected error | Uncertain error ->
                         raise_s [%sexp "setup resume failed", (error : P.Error.t)]
                     in
                     phase := "passive Wait snapshot";
                     let passive = Resume.settled_snapshot entry in
                     Staging.assert_receipt request stopped receipt;
                     assert (P.Run_wake.equal wake (waiting passive admission));
                     assert (G.State.equal (Open Enabled) (G.state (Goal.goal passive)));
                     let pending_timer = one_schedule passive in
                     [%test_eq: int] 0 pending_timer.delivery_count;
                     (match pending_timer.status with
                      | Scheduled -> ()
                      | Delivering | Delivered | Cancelled | Failed _ ->
                        failwith "the genuine timer ran before its due clock");
                     [%test_eq: int] 1 !(counters.providers);
                     [%test_eq: int] 2 (List.length (Manual.startup passive));
                     (* This idle resource unload preserves the actual Enabled Wait.
                   A second public Stop would truthfully Hold the Goal, and could
                   only qualify suspension rather than fresh wake execution. *)
                     Owner.unload_and_wait entry.runtime |> protocol_ok;
                     assert (not (Owner.is_loaded entry.runtime));
                     phase := "cold runtime snapshot";
                     let cold = Resume.checkpoint entry in
                     assert (G.State.equal (Open Enabled) (G.state (Goal.goal cold)));
                     assert (P.Run_wake.equal wake (waiting cold admission));
                     assert (
                       P.Run.equal
                         (Resume.run passive admission)
                         (Resume.run cold admission));
                     let evidence =
                       G.Reason.of_string "The actual queued timer callback completed"
                       |> protocol_ok
                     in
                     let finish =
                       P.Goal_achievement.Intent.create
                         ~expected_goal_revision:(G.revision (Goal.goal cold))
                         ~evidence
                         ~action:(Finish { terminal = Completed None; relinquish = [] })
                       |> protocol_ok
                       |> P.Goal_achievement.Intent.to_json
                     in
                     write_context env root (Resume.run cold admission) (Some finish);
                     (* Advance both actual due-clock inputs. The fixture timeout
                      uses the original environment; no producer row is edited. *)
                     (match scenario with
                      | Complete -> ()
                      | Stop -> construction_armed := true
                      | Claim_failure _ ->
                        fault.selected <- Some { state = cold; admission; wake });
                     !advance 601.;
                     match scenario with
                     | Stop ->
                       phase := "actual scheduler cold construction policy barrier";
                       Eio.Promise.await construction_entered;
                       [%test_eq: int] 1 !construction_hits;
                       (* This actual Factory callback runs under the retained
                          source/resource lease, outside Actor and Owner locks.
                          Cancellation escapes directly rather than being
                          converted by a native tool's error wrapper. *)
                       let constructing = Resume.checkpoint entry in
                       assert (P.Run_wake.equal wake (waiting constructing admission));
                       assert (not (Owner.source_lane_idle entry.runtime));
                       assert (not (Owner.is_loaded entry.runtime));
                       [%test_eq: int] 0 (one_schedule constructing).delivery_count;
                       [%test_eq: int] 1 !(counters.providers);
                       Actor.stop entry.actor ~attachment_id:attachment.id ~mode:Cancel
                       |> protocol_ok
                       |> ignore;
                       phase := "join cancelled scheduler source";
                       Owner.unload_and_wait entry.runtime |> protocol_ok;
                       release_construction ();
                       let rec await_source_join () =
                         if not (Owner.source_lane_idle entry.runtime)
                         then (
                           Eio.Fiber.yield ();
                           await_source_join ())
                       in
                       await_source_join ();
                       let stopped = Resume.checkpoint entry in
                       observed := Some stopped;
                       assert (not (Owner.is_loaded entry.runtime));
                       (match G.state (Goal.goal stopped) with
                        | Open (Held Operator_pause) -> ()
                        | Open (Enabled | Held _) | Closed (Achieved _ | Abandoned _) ->
                          failwith "Stop did not preserve its actual operator hold");
                       assert (Option.is_none (G.achievement_request (Goal.goal stopped)));
                       assert (Option.is_none (G.request_settlement (Goal.goal stopped)));
                       let before_stop = Resume.run constructing admission in
                       let interrupted = Resume.run stopped admission in
                       assert (P.Id.Run.equal before_stop.id interrupted.id);
                       (match interrupted.lifecycle with
                        | Terminal Interrupted -> ()
                        | Admitted | Active | Waiting _ | Terminal _ ->
                          raise_s
                            [%sexp
                              "cancelled awaited producer did not interrupt its Run"
                            , (interrupted : P.Run.t)]);
                       [%test_eq: int64]
                         Int64.(succ before_stop.revision)
                         interrupted.revision;
                       let timer = one_schedule stopped in
                       assert (P.Id.Schedule.equal pending_timer.id timer.id);
                       (match timer.status with
                        | Cancelled -> ()
                        | Scheduled | Delivering | Delivered | Failed _ ->
                          raise_s
                            [%sexp
                              "Stop did not cancel its producer", (timer : P.Schedule.t)]);
                       [%test_eq: int] 0 timer.delivery_count;
                       assert (Option.is_none timer.last_delivery_at);
                       let schedule_work =
                         P.Run_work.create
                           ~key:(Retained (Schedule timer.id))
                           ~generation:timer.generation
                         |> protocol_ok
                       in
                       let cancelled_proof =
                         List.filter interrupted.terminal_work ~f:(fun proof ->
                           P.Run_work.equal proof.P.Run_work.Terminal.work schedule_work)
                         |> Resume.one_actual "cancelled awaited Schedule proof"
                       in
                       assert (
                         P.Run_work.Terminal.equal_outcome
                           cancelled_proof.outcome
                           Cancelled);
                       [%test_eq: int64] interrupted.revision cancelled_proof.revision;
                       List.iter before_stop.terminal_work ~f:(fun previous ->
                         assert (
                           List.mem
                             interrupted.terminal_work
                             previous
                             ~equal:P.Run_work.Terminal.equal));
                       List.iter interrupted.terminal_work ~f:(fun proof ->
                         if
                           not
                             (List.mem
                                before_stop.terminal_work
                                proof
                                ~equal:P.Run_work.Terminal.equal)
                         then
                           assert (
                             P.Run_work.Terminal.equal proof cancelled_proof
                             || P.Run_work.Terminal.equal_outcome
                                  proof.outcome
                                  Unconfirmed));
                       let terminal_receipt =
                         A.Run_state.receipts (Option.value_exn stopped.run_state)
                         |> List.filter ~f:(fun receipt ->
                           P.Run_receipt.Kind.equal receipt.P.Run_receipt.kind Terminal
                           && P.Id.Run.equal receipt.run_id interrupted.id)
                         |> Resume.one_actual "actual interrupted Run terminal receipt"
                       in
                       assert (
                         P.Id.Principal.equal
                           admission.principal_id
                           terminal_receipt.principal_id);
                       assert (
                         P.Run_source.equal interrupted.source terminal_receipt.source);
                       [%test_eq: int64]
                         interrupted.revision
                         terminal_receipt.run_revision;
                       assert (
                         P.Timestamp.equal
                           interrupted.updated_at
                           terminal_receipt.committed_at);
                       assert (Option.is_none stopped.active_operation);
                       assert (
                         List.equal
                           P.Delivery.equal
                           constructing.deliveries
                           stopped.deliveries);
                       [%test_eq: int]
                         (List.length constructing.invocations)
                         (List.length stopped.invocations);
                       assert (
                         A.Goal_accounting.equal
                           (Goal.accounting constructing)
                           (Goal.accounting stopped));
                       assert (
                         List.for_all stopped.moderator_executions ~f:(fun receipt ->
                           not
                             (P.Moderator_execution.equal_phase
                                receipt.context.phase
                                Internal_event)));
                       Resume.assert_episode cold stopped admission;
                       [%test_eq: int] 1 !(counters.providers);
                       (* A real second session and provider turn proves that
                          local runtime cancellation did not fail the shared
                          daemon/scheduler switch. No reflection getter or
                          synthetic scheduler is used. *)
                       phase := "independent actual session after local cancellation";
                       Source.with_loaded_entry
                         daemon
                         (fun ~entry:sibling ~runtime:_ ~revision:_ ->
                            let sibling_session, sibling_before =
                              Goal.create_goal daemon sibling
                            in
                            let sibling_admission =
                              Resume.start_episode
                                daemon
                                sibling
                                original
                                sibling_session
                                sibling_before
                            in
                            let sibling_final =
                              Resume.await_turns
                                ~providers:2
                                env
                                sibling
                                counters
                                ~completed:1
                            in
                            assert (
                              P.Id.Run.equal
                                sibling_admission.run_id
                                (Resume.run sibling_final sibling_admission).id);
                            [%test_eq: int] 2 !(counters.providers));
                       [%test_eq: int] 1 !construction_hits;
                       print_endline
                         "local Stop joins actual cold wake construction; an independent \
                          real session still executes"
                     | Claim_failure cut ->
                       phase := "await actual queued Goal claim writer failure";
                       let rec await_failure () =
                         let current = Resume.checkpoint entry in
                         observed := Some current;
                         match fault.candidate with
                         | Some candidate when Owner.source_lane_idle entry.runtime ->
                           current, candidate
                         | Some _ | None ->
                           Eio.Fiber.yield ();
                           await_failure ()
                       in
                       let confirmed, candidate = await_failure () in
                       [%test_eq: int] 1 fault.hits;
                       [%test_eq: int64]
                         (Int64.pred candidate.transaction.session_revision)
                         confirmed.counters.revision;
                       assert (P.Run_wake.equal wake (waiting confirmed admission));
                       Fault.assert_no_queued_effect confirmed cold counters;
                       assert (
                         not
                           (List.exists confirmed.moderator_executions ~f:(fun receipt ->
                              P.Id.Moderator_execution.equal
                                receipt.context.id
                                candidate.executing.context.id)));
                       let timer = one_schedule confirmed in
                       (match timer.status with
                        | Delivered -> ()
                        | Scheduled | Delivering | Cancelled | Failed _ ->
                          failwith "actual timer publication was not acknowledged");
                       [%test_eq: int] 1 timer.delivery_count;
                       phase := "physical reopen after unacknowledged Goal claim";
                       Fault.reopen_claim_fault
                         ~cut
                         ~recovery_env:(Option.value_exn !recovery_env)
                         ~before_reopen:confirmed
                         env
                         sw
                         root
                         daemon
                         policy
                         entry
                         counters
                         admission
                         cold
                         wake
                         candidate;
                       (* The scheduler correctly keeps this local write failure
                          inside its worker. Preserve its actual selected I/O cause
                          through the poisoned fixture's outer cleanup. *)
                       raise (Fault.Observed_claim_fault (Option.value_exn fault.failure))
                     | Complete ->
                       let rec await_timer () =
                         let current = Resume.checkpoint entry in
                         observed := Some current;
                         List.iter current.moderator_executions ~f:(fun receipt ->
                           match receipt.P.Moderator_execution.status with
                           | Failed _ | Interrupted _ ->
                             raise_s
                               [%sexp
                                 "timer callback failed"
                               , (receipt : P.Moderator_execution.t)
                               , (Resume.run current admission : P.Run.t)
                               , (current.invocations : P.Invocation.t list)
                               , (current.schedules : P.Schedule.t list)]
                           | Running | Completed _ -> ());
                         let completed =
                           List.filter current.moderator_executions ~f:(fun receipt ->
                             P.Moderator_execution.equal_phase
                               receipt.context.phase
                               Internal_event
                             &&
                             match receipt.status with
                             | Completed _ -> true
                             | Running | Failed _ | Interrupted _ -> false)
                         in
                         match (Resume.run current admission).lifecycle, completed with
                         | Terminal (Completed _), [ event ]
                           when Owner.source_lane_idle entry.runtime -> current, event
                         | Terminal (Completed _), _
                         | Admitted, _
                         | Active, _
                         | Waiting _, _ ->
                           Eio.Fiber.yield ();
                           await_timer ()
                         | Terminal _, _ ->
                           raise_s
                             [%sexp
                               "timer Run failed"
                             , (Resume.run current admission : P.Run.t)]
                       in
                       phase := "await actual timer scheduler";
                       let final, event = await_timer () in
                       let completed = queued_receipt final event.context.id in
                       assert (
                         P.Moderator_execution.equal_phase
                           Internal_event
                           completed.context.phase);
                       (match completed.status with
                        | Completed _ -> ()
                        | Running | Failed _ | Interrupted _ ->
                          raise_s
                            [%sexp
                              "queued callback did not complete"
                            , (completed : P.Moderator_execution.t)]);
                       assert_native final admission completed;
                       Resume.assert_episode cold final admission;
                       (match
                          ( (Resume.run final admission).lifecycle
                          , G.state (Goal.goal final) )
                        with
                        | Terminal (Completed _), Closed (Achieved _) -> ()
                        | _ ->
                          failwith "actual queued Goal.finish did not settle the episode");
                       let terminal_timer = one_schedule final in
                       assert (P.Id.Schedule.equal pending_timer.id terminal_timer.id);
                       [%test_eq: int] 1 terminal_timer.delivery_count;
                       (match terminal_timer.status with
                        | Delivered -> ()
                        | Scheduled | Delivering | Cancelled | Failed _ ->
                          failwith "actual timer producer did not finish");
                       assert (
                         A.Goal_accounting.equal
                           (Goal.accounting cold)
                           (Goal.accounting final));
                       [%test_eq: int] 1 !(counters.providers);
                       [%test_eq: int] 3 (List.length (Manual.startup final));
                       Owner.with_loaded_runtime entry.runtime (fun runtime ->
                         assert (not (phys_equal initial_runtime runtime));
                         Ok ())
                       |> protocol_ok;
                       let before_replay = Resume.settled_snapshot entry in
                       let counts =
                         !(counters.providers), List.length before_replay.invocations
                       in
                       let rec reject_replay () =
                         match
                           Factory.deliver_goal_wake (Daemon.factory daemon) entry ~wake
                         with
                         | Error error
                           when P.Error.equal_code error.code Conflict && error.retryable
                           ->
                           (* A real background poll can borrow the runtime after
                              the idle observation above. Join that bounded
                              contention; still require an actual No_work result. *)
                           Eio.Fiber.yield ();
                           reject_replay ()
                         | Error error -> raise_s [%sexp (error : P.Error.t)]
                         | Ok No_work -> ()
                         | Ok (Suspended_goal _ | Handed_off _) ->
                           failwith "old wake replayed"
                       in
                       reject_replay ();
                       Existing.assert_same before_replay entry;
                       [%test_eq: int * int]
                         counts
                         (!(counters.providers), List.length (state entry).invocations);
                       print_endline
                         "one cold timer claim survives native effect and actual \
                          Goal.finish; old wake is data-only")))
       with
       | Eio.Time.Timeout as exn ->
         print_s [%sexp "wake fixture timeout", (!phase : string)];
         Option.iter !observed ~f:(fun current ->
           print_s
             [%sexp
               (current.lifecycle : A.Session_state.Lifecycle.t)
             , (current.runtime_initialization : A.Session_state.Runtime_initialization.t)
             , (current.moderator_executions : P.Moderator_execution.t list)
             , (current.schedules : P.Schedule.t list)
             , (Option.value_map current.run_state ~default:[] ~f:A.Run_state.runs
                : P.Run.t list)]);
         raise exn);
  ()
;;

let%expect_test "cold Goal timer wake keeps its real claim for the first native effect" =
  run Complete;
  [%expect
    {| one cold timer claim survives native effect and actual Goal.finish; old wake is data-only |}]
;;

let%expect_test
    "local Stop during actual cold wake construction cannot fail the shared scheduler"
  =
  run Stop;
  [%expect
    {| local Stop joins actual cold wake construction; an independent real session still executes |}]
;;

let%expect_test "failed Goal queued-claim ACK never transfers effects across recovery" =
  List.iter
    Fault.Cut.[ Before_sync; After_sync ]
    ~f:(fun cut ->
      (match run (Claim_failure cut) with
       | () -> failwith "selected Goal queued-claim writer fault was not reached"
       | exception Fault.Observed_claim_fault failure ->
         (match failure with
          | Core_unix.Unix_error (EIO, operation, _) ->
            assert (String.equal operation "545 Goal wake claim sync")
          | _ -> Exn.reraise failure "unexpected Goal queued-claim failure"));
      print_s [%sexp (cut : Fault.Cut.t)]);
  [%expect
    {|
    Before_sync
    After_sync
    |}]
;;
