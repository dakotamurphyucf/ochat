open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Service = Agent_server.Goal_run_service
module G = P.Goal
module Graph = Agent_server.Graph_tracking
module Ledger = A.Inference_ledger
module Runtime = A.Runtime_builder

(* Trusted lower-host composition: Operator_only supplies a genuine Factory and
   registry without an automatic Job dispatcher. Explicit Actor/Factory ports
   below own the real admission/claim. This is not an Operator_only client route
   or a claim that the production scheduler has been tested. *)
let marker = "545_LOADED_GOAL_JOB"

let prompt =
  {|<developer>Keep one genuine owned Model Job for the trusted host.</developer>
<tool name="job_recipe" agent="held-job.chatmd" local/>
<script id="owner" language="chatml" kind="moderator">
let initial_state = 0
let on_event ctx state event = match event with
| `Turn_start ->
  if state == 0 then
    let* child = Model.spawn("agent_prompt_v1", `Object([
      {key = "prompt"; value = `String("held-job.chatmd")},
      {key = "input"; value = `String("545_LOADED_GOAL_JOB")},
      {key = "is_local"; value = `Bool(true)}
    ])) in
    Task.pure(state + 1)
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
;;

let one_job current =
  match
    List.filter current.A.Session_state.jobs ~f:(fun job ->
      P.Job.equal_kind Model_call job.kind
      && Option.equal
           Jsonaf.exactly_equal
           (Jsonaf.member "payload" job.payload |> Option.bind ~f:(Jsonaf.member "input"))
           (Some (`String marker)))
  with
  | [ job ] -> job
  | jobs -> raise_s [%sexp "exact actual spawned Goal Job", (jobs : P.Job.t list)]
;;

let startup current =
  List.filter current.A.Session_state.moderator_executions ~f:(fun receipt ->
    match receipt.context.phase with
    | Session_start | Session_resume -> true
    | Turn_start
    | Message_appended
    | Pre_tool_call
    | Post_tool_response
    | Turn_end
    | Internal_event -> false)
;;

let run current admission =
  A.Run_state.find
    (Option.value_exn current.A.Session_state.run_state)
    admission.P.Run_receipt.run_id
  |> Option.value_exn
;;

let prepared runtime =
  let context =
    Runtime.inference_execution runtime |> Inference_client.Execution.context
  in
  let request =
    Inference.Request.create
      ~target:(Inference_runtime.Context.target context)
      ~history:[]
      ~tools:[]
      ~assets:[]
      ~limits:Transcript.Admission.default
    |> function
    | Ok request -> request
    | Error error -> raise_s [%sexp (error : Inference.Request.Error.t)]
  in
  context, request
;;

let fresh_denial ~sw entry runtime services policy =
  let upstream =
    policy.Factory.runtime_inference_ports entry.Registry.actor
    |> function
    | Ok upstream -> upstream
    | Error error -> raise_s [%sexp (error : Inference_runtime.Preparation_error.t)]
  in
  let graph =
    Graph.create
      entry.actor
      ~source:
        (Transcript.Source_id.of_string "goal-loaded-job-hold" |> Result.ok_or_failwith)
      ~upstream
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () ->
      Graph.seal graph |> protocol_ok;
      Graph.finish graph |> protocol_ok)
    (fun () ->
       let context, request = prepared runtime in
       let identity =
         Graph.bind_identity graph ~allocate:services.Actor.allocate_inference
       in
       match
         Inference_client.run
           context
           ~sw
           ~identity
           ~relation:Root
           ~request
           ~before_dispatch:ignore
           ~on_attempt:(Graph.on_attempt graph)
           ~on_observation:(Graph.on_observation graph)
           ~on_completion:(Graph.on_completion graph)
           ~on_event:ignore
       with
       | exception Graph.Rejected error ->
         [%test_eq: P.Error.code] Permission_denied error.code;
         [%test_eq: bool] false error.retryable;
         error
       | Ok _ -> failwith "Held Job admitted fresh inference"
       | Error error -> raise_s [%sexp (error : Inference_client.Error.t)])
;;

let with_loaded_manual_entry daemon f =
  let entry =
    Factory.create_session
      (Daemon.factory daemon)
      ~command_audit:None
      ~principal
      (create_request ~start_immediately:true ())
    |> protocol_ok
  in
  (match
     Registry.add
       (Daemon.registry daemon)
       ~session_id:(state entry).identity.session_id
       entry
   with
   | Ok () -> ()
   | Error error ->
     entry.close ();
     raise_s [%sexp (error : P.Error.t)]);
  protect_cleanup ~finally:entry.close (fun () ->
    Owner.ensure_loaded_authenticated entry.runtime ~actor:original |> protocol_ok;
    let current = state entry in
    (match current.lifecycle.observed, current.runtime_initialization with
     | Idle, Ready -> ()
     | _ ->
       raise_s
         [%sexp
           "actual legacy constructor did not settle"
         , (current.lifecycle : A.Session_state.Lifecycle.t)
         , (current.runtime_initialization : A.Session_state.Runtime_initialization.t)]);
    let runtime =
      Owner.with_loaded_runtime entry.runtime (fun runtime -> Ok runtime) |> protocol_ok
    in
    f entry runtime)
;;

(* Only wall-clock sampling is controlled. The fixture's real timeout clock
   and all sleeps retain the actual Eio scheduler. *)
let with_wall_clock (env : Eio_unix.Stdenv.base) wall : Eio_unix.Stdenv.base =
  wall := Eio.Time.now env#clock;
  let module Clock = struct
    type t = unit
    type time = float

    let now () = !wall
    let sleep_until () at = Eio.Time.sleep_until env#clock at
  end
  in
  let clock = Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock)) in
  object
    method clock = clock
    method mono_clock = env#mono_clock
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

type scenario =
  | Held_before_claim
  | Claimed_then_held

let exercise scenario =
  let wall = ref 0. in
  let original_live = ref true in
  let episode_actor =
    Operator_authorization.guarded ~principal ~is_current:(fun () -> !original_live)
  in
  let cross_deadline = ref None in
  let authorizations = ref 0 in
  let crossed_deadline = ref false in
  let counters : Retry_support.counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr counters.providers;
      completed_response "545 original episode complete")
  in
  Creation_tests.with_daemon
    ~startup_mode:Operator_only
    ~wrap_env:(fun env -> with_wall_clock env wall)
    ~prompt_text:prompt
    ~sources:[ "held-job.chatmd", "<developer>Offline owned recipe.</developer>" ]
    ~policy
    (fun env sw _root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         with_loaded_manual_entry daemon (fun entry runtime ->
           let session, before = Goal.create_goal daemon entry in
           let headless =
             A.Run_admission_request.Headless.create
               ~session_id:before.identity.session_id
               ~generation:before.identity.generation
               ~expected_revision:before.counters.revision
               ~mode:Workflow
               ~input:
                 (User_submission { kind = Plain_text; text = marker; attachments = [] })
               ~key:(P.Idempotency_key.of_string "loaded-goal-job-episode" |> protocol_ok)
             |> protocol_ok
           in
           let request =
             A.Goal_activation_request.create
               ~goal_id:Goal.goal_id
               ~expected_goal_revision:(G.revision (Goal.goal before))
               ~expected_catalog_revision:
                 (A.Goal_catalog.revision (Option.value_exn before.goal_catalog))
               ~request:headless
             |> protocol_ok
           in
           let service =
             Service.create ~factory:(Daemon.factory daemon) ~authorize:(fun _ current ->
               let result = authorize current in
               Option.iter !cross_deadline ~f:(fun (remaining, deadline) ->
                 Int.incr authorizations;
                 if Int.equal remaining 1
                 then (
                   cross_deadline := None;
                   wall := deadline;
                   crossed_deadline := true;
                   Eio.Fiber.yield ())
                 else cross_deadline := Some (remaining - 1, deadline));
               result)
           in
           let admission =
             Service.start_episode service ~entry ~actor:episode_actor ~session ~request
             |> protocol_ok
             |> Goal.activated
           in
           let active = Goal.await_input env entry counters in
           let queued = one_job active in
           (match queued.status with
            | Queued -> ()
            | _ -> raise_s [%sexp (queued : P.Job.t)]);
           [%test_eq: int] 0 queued.attempt;
           assert (Option.is_some queued.inference_origin);
           let hold () =
             Goal_admitted_guard_tests.command
               daemon
               entry
               ~key:"loaded-goal-job-hold"
               (Hold (Goal.goal_id, Operator_pause))
             |> protocol_ok
             |> ignore
           in
           match scenario with
           | Held_before_claim ->
             hold ();
             let held = state entry in
             let before_frames = Existing.frames env entry in
             assert (G.State.equal (Open (Held Operator_pause)) (G.state (Goal.goal held)));
             assert (
               A.Goal_accounting.equal (Goal.accounting active) (Goal.accounting held));
             assert (
               List.equal P.Moderator_execution.equal (startup active) (startup held));
             let claim =
               Actor.claim_job entry.actor ~job_id:queued.id ~generation:queued.generation
               |> protocol_ok
             in
             assert (Option.is_none claim);
             Actor.reconcile_job_authority
               entry.actor
               ~job_id:queued.id
               ~generation:queued.generation
             |> protocol_ok;
             Existing.assert_same held entry;
             assert (List.equal String.equal before_frames (Existing.frames env entry));
             assert (
               Jsonaf.exactly_equal
                 (P.Job.to_json queued)
                 (P.Job.to_json (one_job (state entry))));
             Owner.with_loaded_runtime entry.runtime (fun selected ->
               assert (phys_equal selected runtime);
               Ok ())
             |> protocol_ok;
             [%test_eq: int] 0 (one_job (state entry)).attempt;
             original_live := false;
             Actor.reconcile_job_authority
               entry.actor
               ~job_id:queued.id
               ~generation:queued.generation
             |> protocol_ok;
             let retired = state entry in
             let job = one_job retired in
             (match job.status, job.delivery with
              | Cancelled, Discarded { reason = Authority_changed; _ } -> ()
              | _ -> failwith "Held Goal hid expired original Job authority");
             [%test_eq: int] 0 job.attempt;
             assert (
               P.Run.Lifecycle.equal
                 (run retired admission).lifecycle
                 (Terminal Interrupted));
             assert (
               A.Goal_accounting.equal (Goal.accounting held) (Goal.accounting retired));
             [%test_eq: int] 1 !(counters.providers)
           | Claimed_then_held ->
             let claimed =
               Actor.claim_job entry.actor ~job_id:queued.id ~generation:queued.generation
               |> protocol_ok
               |> Option.value_exn
             in
             [%test_eq: int] 1 claimed.attempt;
             let deadline_wall = !wall +. 60. in
             let deadline =
               Time_ns.of_span_since_epoch (Time_ns.Span.of_sec deadline_wall)
               |> P.Timestamp.of_time_ns
             in
             let rejection =
               Actor.with_job_execution
                 entry.actor
                 ~job_id:claimed.id
                 ~generation:claimed.generation
                 ~attempt:claimed.attempt
                 ~deadline:(Some deadline)
                 (fun services ->
                    (match Actor.Job_runtime_owner.attribution services.runtime_owner with
                     | Goal -> ()
                     | Ordinary -> failwith "actual Job lost Goal attribution");
                    hold ();
                    let held = state entry in
                    assert (
                      G.State.equal
                        (Open (Held Operator_pause))
                        (G.state (Goal.goal held)));
                    let account = Goal.accounting held in
                    let ledger = Goal_resume_tests.ledger_json held.inference_ledger in
                    let prior_startup = startup held in
                    let rejection =
                      Registry.with_loaded_read
                        (Daemon.registry daemon)
                        held.identity.session_id
                        (fun actual ->
                           assert (phys_equal actual entry);
                           Factory.with_goal_job_runtime
                             (Daemon.factory daemon)
                             entry
                             ~owner:services.runtime_owner
                             (fun selected ->
                                assert (phys_equal selected runtime);
                                assert (
                                  Jsonaf.exactly_equal
                                    (P.Job.to_json claimed)
                                    (P.Job.to_json (one_job (state entry))));
                                assert (Owner.is_loaded entry.runtime);
                                Ok (fresh_denial ~sw entry selected services policy)))
                      |> protocol_ok
                    in
                    let rejection =
                      match rejection with
                      | Owner.Construction_result.Built rejection -> rejection
                      | Callback_completed _ ->
                        failwith "loaded Job unexpectedly completed its callback"
                    in
                    let retained = state entry in
                    assert (A.Goal_accounting.equal account (Goal.accounting retained));
                    assert (
                      Document_schema.Json.equal
                        ledger
                        (Goal_resume_tests.ledger_json retained.inference_ledger));
                    assert (
                      List.equal
                        P.Moderator_execution.equal
                        prior_startup
                        (startup retained));
                    assert (
                      P.Goal_episode.equal
                        (Option.value_exn (G.episode (Goal.goal held)))
                        (Option.value_exn (G.episode (Goal.goal retained))));
                    [%test_eq: int] 1 !(counters.providers);
                    [%test_eq: int] 1 (one_job retained).attempt;
                    (* Owner obtains the resource proof twice: before delegated
                       execution checking, then at final lease entry. Cross only
                       the second actual original authorization, after its early
                       scope check; the first proof must remain timely. *)
                    let before_frames = Existing.frames env entry in
                    let entered = ref false in
                    cross_deadline := Some (2, deadline_wall);
                    let expired =
                      Factory.with_goal_job_runtime
                        (Daemon.factory daemon)
                        entry
                        ~owner:services.runtime_owner
                        (fun _ ->
                           entered := true;
                           Ok ())
                      |> function
                      | Error error -> error
                      | Ok (Owner.Construction_result.Built () | Callback_completed _) ->
                        failwith "expired Job scope entered loaded runtime"
                    in
                    [%test_eq: int] 2 !authorizations;
                    assert !crossed_deadline;
                    assert (not !entered);
                    [%test_eq: P.Error.code] Conflict expired.code;
                    [%test_eq: bool] false expired.retryable;
                    Existing.assert_same retained entry;
                    assert (
                      List.equal String.equal before_frames (Existing.frames env entry));
                    assert (Owner.is_loaded entry.runtime);
                    [%test_eq: int] 1 !(counters.providers);
                    Ok rejection)
               |> protocol_ok
             in
             let terminal =
               Actor.complete_job
                 entry.actor
                 ~job_id:claimed.id
                 ~generation:claimed.generation
                 ~attempt:claimed.attempt
                 (Runtime.Model_failed rejection.message)
               |> protocol_ok
             in
             (match terminal.status with
              | Failed _ -> ()
              | _ -> failwith "fresh denial did not settle Job");
             [%test_eq: int] 1 terminal.attempt;
             let final = state entry in
             assert (Owner.is_loaded entry.runtime);
             assert (P.Id.Run.equal admission.run_id (run final admission).id);
             let admissions =
               A.Run_state.receipts (Option.value_exn final.run_state)
               |> List.filter ~f:(fun receipt ->
                 P.Run_receipt.Kind.equal Admission receipt.kind)
             in
             [%test_eq: int] 1 (List.length admissions);
             let owned =
               List.find_exn (run final admission).terminal_work ~f:(fun terminal ->
                 match terminal.P.Run_work.Terminal.work.key with
                 | Retained (Job { id; attempt }) ->
                   P.Id.Job.equal id claimed.id && Int.equal attempt claimed.attempt
                 | _ -> false)
             in
             (match owned.outcome with
              | Failed -> ()
              | _ -> failwith "actual Goal Job failure was not retained");
             [%test_eq: int] 1 !(counters.providers))))
;;

let%expect_test
    "loaded claimed Goal Job retains resources under Hold without fresh inference"
  =
  exercise Held_before_claim;
  exercise Claimed_then_held;
  print_endline
    "Held queued Job stays unclaimed; expired original authority still retires it";
  print_endline
    "same loaded runtime and claim survive Hold; fresh inference and expired resource \
     borrowing deny";
  [%expect
    {| 
    Held queued Job stays unclaimed; expired original authority still retires it
    same loaded runtime and claim survive Hold; fresh inference and expired resource borrowing deny
    |}]
;;
