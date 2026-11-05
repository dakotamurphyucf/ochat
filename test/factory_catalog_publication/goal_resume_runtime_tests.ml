open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Staging = Goal_resume_tests
module G = P.Goal
module Service = Agent_server.Goal_run_service
module Result = Actor.Goal_resume_result
module Ledger = A.Inference_ledger
module Graph = Agent_server.Graph_tracking

module Surface = struct
  type t =
    | Legacy
    | V1
  [@@deriving sexp_of]
end

module Decision = struct
  type t =
    | Continue
    | Wait
    | Finish
  [@@deriving sexp_of]
end

let resume_context_file = "goal-resume-context.json"
let constructor_recipe = "goal-resume-init.chatmd"
let constructor_input = "543_GOAL_INIT_MODEL"
let initial_input = "543 original runtime input"

module Allocation_snapshot = struct
  type t =
    { state : A.Session_state.t
    ; scope : Transcript.Scope.t
    ; accounting_id : Inference.Observation.Observation_id.t
    ; owner_loaded : bool
    }
end

module Provider_probe = struct
  type t =
    { mutable root_states : A.Session_state.t list
    ; mutable allocations : Allocation_snapshot.t list
    ; mutable awaiting_root : bool
    ; mutable phase : string
    ; mutable last_state : A.Session_state.t option
    ; entered : unit Eio.Promise.t
    ; enter : unit Eio.Promise.u
    ; released : unit Eio.Promise.t
    ; release : unit Eio.Promise.u
    ; mutable release_sent : bool
    }

  let create () =
    let entered, enter = Eio.Promise.create () in
    let released, release = Eio.Promise.create () in
    { root_states = []
    ; allocations = []
    ; awaiting_root = false
    ; phase = "setup"
    ; last_state = None
    ; entered
    ; enter
    ; released
    ; release
    ; release_sent = false
    }
  ;;

  let release t =
    if not t.release_sent
    then (
      t.release_sent <- true;
      Eio.Promise.resolve t.release ())
  ;;
end

let prompt (surface : Surface.t) (decision : Decision.t) =
  let api =
    match surface with
    | Legacy -> ""
    | V1 -> " api=\"extensibility-v1\""
  in
  let model_declaration, constructor =
    match surface, decision with
    | Legacy, Continue ->
      ( {|<tool name="recipe_source" agent="goal-resume-init.chatmd" local/>|}
      , {|let* timer = Schedule.after_ms(600000, `String("cancel during actual Goal Init")) in
  let* cancelled = Schedule.cancel(timer) in
  let* called = Model.call("agent_prompt_v1", `Object([
    {key = "prompt"; value = `String("goal-resume-init.chatmd")},
    {key = "input"; value = `String("543_GOAL_INIT_MODEL")},
    {key = "is_local"; value = `Bool(true)}
  ])) in
  let* checked = (match called with
  | `Ok(value) -> Task.pure(value)
  | `Refused(message) -> Task.fail(message)
  | `Error(message) -> Task.fail(message)) in|}
      )
    | Legacy, (Wait | Finish) | V1, (Continue | Wait | Finish) -> "", ""
  in
  let selected =
    match decision with
    | Continue -> "Task.pure(state + 1)"
    | Finish ->
      "Task.bind(Goal.finish(field(context, \"finish\")), fun ignored -> Task.pure(state \
       + 1))"
    | Wait ->
      {|let* id = Schedule.after_ms(600000, `String("543 actual passive occurrence")) in
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
      Task.bind(Run.wait(wake), fun ignored -> Task.pure(state + 1))|}
  in
  sprintf
    {|<developer>Actual existing Goal runtime continuation.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
%s
<script id="goal_resume" language="chatml" kind="moderator"%s>
let initial_state = 0
let field value key = match Json.get_field(value, key) with
| `Some(found) -> found
| `None -> `Null
let on_event ctx state event = match event with
| `Session_resume ->
  %s
  let* read = Tool.call("read_file", `Object([
    {key = "root"; value = `String("data")},
    {key = "file"; value = `String("goal-resume-context.json")}
  ])) in
  (match read with
  | `Error(message) -> Task.fail(message)
  | `Ok(`String(text)) ->
    let lines = String.split(text, "\n") in
    let context = Json.parse(lines[2]) in
    %s
  | `Ok(_) -> Task.fail("read_file did not return actual text"))
| _ -> Task.pure(state)
</script>|}
    model_declaration
    api
    constructor
    selected
;;

let public daemon =
  Service.create ~factory:(Daemon.factory daemon) ~authorize:(fun actor ->
    Staging.authorize_as (Operator_authorization.principal actor))
;;

let run current admission =
  A.Run_state.find
    (Option.value_exn current.A.Session_state.run_state)
    admission.P.Run_receipt.run_id
  |> Option.value_exn
;;

let assert_episode before after admission =
  let index = Option.value_exn after.A.Session_state.run_state in
  [%test_eq: int] 1 (List.length (A.Run_state.runs index));
  [%test_eq: int]
    1
    (List.count (A.Run_state.receipts index) ~f:(fun receipt ->
       P.Run_receipt.Kind.equal receipt.kind Admission));
  let actual = run after admission in
  assert (P.Id.Principal.equal admission.principal_id actual.principal_id);
  assert (P.Run_source.equal (run before admission).source actual.source);
  let old_episode = G.episode (Goal.goal before) |> Option.value_exn in
  let episode = G.episode (Goal.goal after) |> Option.value_exn in
  assert (P.Goal_episode.equal old_episode episode);
  assert (P.Run_receipt.equal admission (P.Goal_episode.admission episode));
  assert (P.Goal_episode.matches_run episode actual);
  let link = A.Run_state.goal_link index actual.id |> Option.value_exn in
  let old_link =
    A.Run_state.goal_link (Option.value_exn before.run_state) actual.id
    |> Option.value_exn
  in
  assert (A.Run_goal_link.equal old_link link);
  assert (P.Goal_episode.equal episode (A.Run_goal_link.episode link));
  let old_account = Goal.accounting before in
  let account = Goal.accounting after in
  assert (
    A.Goal_accounting.Scope.equal
      (A.Goal_accounting.scope old_account)
      (A.Goal_accounting.scope account));
  List.iter (A.Goal_accounting.bindings old_account) ~f:(fun binding ->
    assert (
      List.exists
        (A.Goal_accounting.bindings account)
        ~f:(A.Goal_accounting.Binding.equal binding)))
;;

let operation_work (run : P.Run.t) =
  run.owned_work
  @ run.relinquished_work
  @ List.map run.terminal_work ~f:(fun terminal -> terminal.P.Run_work.Terminal.work)
  |> List.filter ~f:(fun work ->
    match work.P.Run_work.key with
    | Operation _ -> true
    | Retained _ -> false)
  |> List.dedup_and_sort ~compare:P.Run_work.compare
;;

let callbacks_complete current =
  List.for_all current.A.Session_state.moderator_executions ~f:(fun receipt ->
    match receipt.P.Moderator_execution.status with
    | Completed _ -> true
    | Running -> false
    | Failed _ | Interrupted _ ->
      raise_s
        [%sexp
          "Goal resume callback ended adversely", (receipt : P.Moderator_execution.t)])
;;

let checkpoint entry =
  Actor.with_moderator_checkpoint entry.Registry.actor (fun () -> Actor.state entry.actor)
  |> protocol_ok
;;

let settled_snapshot entry =
  (* Only an installed v1 manager drains native observations. Legacy callbacks
     retain their observation DATA; historical replay must preserve it rather
     than wait for a handler that this runtime does not implement. *)
  let observer =
    if Owner.is_loaded entry.Registry.runtime
    then
      Owner.with_loaded_runtime entry.runtime (fun runtime ->
        Ok
          (Option.bind
             runtime.A.Runtime_builder.moderator_manager
             ~f:Chat_response.Moderator_manager.invocation_observer))
      |> protocol_ok
    else None
  in
  let rec await () =
    let current = checkpoint entry in
    let observed =
      List.for_all current.A.Session_state.invocations ~f:(fun invocation ->
        match invocation.P.Invocation.observation with
        | None -> true
        | Some observation ->
          (match observation.status with
           | Observed -> true
           | Awaiting | Observing ->
             not
               (Option.exists
                  observer
                  ~f:(P.Invocation.equal_observer observation.observer)
                && P.Id.Session.equal
                     invocation.context.session_id
                     current.identity.session_id
                && Int.equal invocation.context.generation current.identity.generation)
           | Observation_failed _ ->
             raise_s
               [%sexp
                 "Goal resume native observation failed", (invocation : P.Invocation.t)]))
    in
    if observed
    then current
    else (
      Eio.Fiber.yield ();
      await ())
  in
  await ()
;;

let await_turns ?providers ?(observe = ignore) env entry counters ~completed =
  let providers = Option.value providers ~default:completed in
  let rec await () =
    let current = checkpoint entry in
    observe current;
    let summary = Ledger.summary current.inference_ledger in
    let turns = P.Inference_query.Summary.turns summary in
    if Int64.(turns.failed > 0L || turns.cancelled > 0L || turns.interrupted > 0L)
    then
      raise_s
        [%sexp
          "Goal continuation ended adversely", (summary : P.Inference_query.Summary.t)];
    List.iter (Ledger.rows current.inference_ledger) ~f:(fun row ->
      match Inference.Observation.Attempt_record.state (Ledger.Row.record row) with
      | Prepared | Running -> ()
      | Interrupted _ -> failwith "Goal continuation inference was interrupted"
      | Terminal terminal ->
        (match Inference.Event.Terminal.outcome terminal with
         | Completed -> ()
         | Refused | Incomplete _ | Failed _ ->
           raise_s
             [%sexp
               "Goal continuation inference failed"
             , (terminal : Inference.Event.Terminal.t)]));
    if !(counters.providers) > providers
    then failwith "Goal continuation replayed its provider";
    if
      Option.is_none current.active_operation
      && Int64.equal turns.completed (Int64.of_int completed)
      && !(counters.providers) = providers
      && callbacks_complete current
    then current
    else (
      Eio.Fiber.yield ();
      await ())
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. await
;;

let started = function
  | Result.Resumed { receipt; continuation = Started operation } -> receipt, operation
  | Resumed { continuation = Waiting _ | Settled _; _ } | Retained _ ->
    failwith "live Goal did not start an actual continuation"
  | Resume_failed { error; _ } | Rejected error | Uncertain error ->
    raise_s [%sexp "Goal resume failed", (error : P.Error.t)]
;;

let startup_receipts current =
  List.filter current.A.Session_state.moderator_executions ~f:(fun receipt ->
    match receipt.P.Moderator_execution.context.phase with
    | Session_start | Session_resume -> true
    | _ -> false)
;;

let assert_started ?(attempts = 2L) before after admission operation =
  [%test_eq: P.Operation.kind] (Turn Administrative) operation.P.Operation.kind;
  assert_episode before after admission;
  assert (G.State.equal (Open Enabled) (G.state (Goal.goal after)));
  let actual = run after admission in
  (match actual.lifecycle with
   | Admitted | Active -> ()
   | Waiting _ | Terminal _ ->
     raise_s [%sexp "Goal continuation lost live Workflow", (actual : P.Run.t)]);
  assert (
    List.exists actual.terminal_work ~f:(fun proof ->
      match proof.P.Run_work.Terminal.work.key, proof.outcome with
      | Operation id, Succeeded -> P.Id.Operation.equal id operation.id
      | _ -> false));
  [%test_eq: int64] 2L (A.Goal_accounting.admitted_turns (Goal.accounting after));
  [%test_eq: int64] attempts (A.Goal_accounting.admitted_attempts (Goal.accounting after));
  [%test_eq: int]
    (Int64.to_int_exn attempts)
    (List.length (A.Goal_accounting.bindings (Goal.accounting after)))
;;

let repeat daemon entry session actor request expected counters env =
  let before = settled_snapshot entry in
  let frames = Existing.frames env entry in
  let counts = !(counters.providers), !(counters.inference_ports) in
  (match
     Service.resume_episode (public daemon) ~entry ~actor ~session ~request |> protocol_ok
   with
   | Retained actual -> assert (A.Goal_command_receipt.equal expected actual)
   | Resumed _ | Resume_failed _ | Rejected _ | Uncertain _ ->
     failwith "same command replay acquired another construction");
  Existing.assert_same before entry;
  [%test_eq: string list] frames (Existing.frames env entry);
  [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports))
;;

let%expect_test
    "loaded Goal continuation reuses the same runtime without lifecycle replay"
  =
  Staging.with_episode (fun env daemon entry runtime counters session admission _ held ->
    let actor = Staging.guarded Staging.resume_principal (ref true) in
    let request = Staging.exact_request held ~key:"goal-loaded-runtime-resume" in
    let receipt, operation =
      Service.resume_episode (public daemon) ~entry ~actor ~session ~request
      |> protocol_ok
      |> started
    in
    Staging.assert_receipt request held receipt;
    let final = await_turns env entry counters ~completed:2 in
    assert_started held final admission operation;
    assert (
      List.equal
        P.Moderator_execution.equal
        (startup_receipts held)
        (startup_receipts final));
    Owner.with_loaded_runtime entry.runtime (fun current ->
      assert (phys_equal runtime current);
      Ok ())
    |> protocol_ok;
    repeat daemon entry session actor request receipt counters env;
    print_endline
      "same Workflow and account receive one Administrative turn; loaded startup is not \
       replayed");
  [%expect
    {| same Workflow and account receive one Administrative turn; loaded startup is not replayed |}]
;;

let start_episode daemon entry old_original session before =
  let headless =
    A.Run_admission_request.Headless.create
      ~session_id:before.A.Session_state.identity.session_id
      ~generation:before.identity.generation
      ~expected_revision:before.counters.revision
      ~mode:Workflow
      ~input:
        (User_submission { kind = Plain_text; text = initial_input; attachments = [] })
      ~key:(P.Idempotency_key.of_string "goal-runtime-original-episode" |> protocol_ok)
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
  Service.start_episode (public daemon) ~entry ~actor:old_original ~session ~request
  |> protocol_ok
  |> Goal.activated
;;

let with_cold_episode surface decision f =
  let counters : counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let selected = ref None in
  let probe = Provider_probe.create () in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs ->
      let transcript =
        `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
      in
      let operation =
        Option.bind !selected ~f:(fun entry -> (state entry).active_operation)
      in
      let root =
        probe.awaiting_root
        && Option.exists operation ~f:(fun operation ->
          P.Operation.equal_kind operation.kind (Turn Administrative))
      in
      if root
      then (
        probe.phase <- "Administrative provider awaiting service return";
        Eio.Promise.resolve probe.enter ();
        Eio.Promise.await probe.released);
      Int.incr counters.providers;
      counters.transcripts := !(counters.transcripts) @ [ transcript ];
      if root
      then
        Option.iter !selected ~f:(fun entry ->
          probe.root_states <- probe.root_states @ [ state entry ]);
      completed_response "543 real Goal continuation result")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Int.incr counters.inference_ports;
          Core.Result.map (base.runtime_inference_ports actor) ~f:(fun upstream ->
            { upstream with
              on_admitted =
                (fun ~scope ~accounting_id ->
                  upstream.on_admitted ~scope ~accounting_id;
                  Option.iter !selected ~f:(fun entry ->
                    probe.allocations
                    <- probe.allocations
                       @ [ Allocation_snapshot.
                             { state = state entry
                             ; scope
                             ; accounting_id
                             ; owner_loaded = Owner.is_loaded entry.runtime
                             }
                         ]))
            }))
    }
  in
  Creation_tests.with_daemon
    ~prompt_text:(prompt surface decision)
    ~qualify_chatml_extensions:true
    ~sources:
      [ resume_context_file, "{}\n"
      ; constructor_recipe, "<developer>543_GOAL_INIT_RECIPE</developer>"
      ]
    ~policy
    (fun env _sw root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
         Source.with_loaded_entry
           daemon
           (fun ~entry ~runtime:initial_runtime ~revision:_ ->
              selected := Some entry;
              let old_live = ref true in
              let old_original = Staging.guarded principal old_live in
              let session, created = Goal.create_goal daemon entry in
              let admission = start_episode daemon entry old_original session created in
              let initial = Goal.await_input env entry counters in
              assert (List.is_empty initial.jobs);
              [%test_eq: int]
                0
                (List.count initial.moderator_executions ~f:(fun receipt ->
                   P.Moderator_execution.equal_phase receipt.context.phase Session_resume));
              (match surface with
               | V1 -> ()
               | Legacy ->
                 (match A.Runtime_builder.runtime_resume initial_runtime with
                  | Error error ->
                    [%test_eq: P.Error.code] Conflict error.code;
                    [%test_eq: bool] false error.retryable
                  | Ok _ -> failwith "initial legacy Session_start claimed a resume ACK"));
              let attachment, _ =
                Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
              in
              protect_cleanup
                ~finally:(fun () -> Actor.detach entry.actor attachment.id |> protocol_ok)
                (fun () ->
                   Goal_admitted_guard_tests.command
                     daemon
                     entry
                     ~key:"goal-runtime-hold"
                     (Hold (Goal.goal_id, Operator_pause))
                   |> protocol_ok
                   |> ignore;
                   let held = state entry in
                   let original_run = run held admission in
                   Actor.stop entry.actor ~attachment_id:attachment.id ~mode:Graceful
                   |> protocol_ok
                   |> ignore;
                   Owner.unload_and_wait entry.runtime |> protocol_ok;
                   assert (not (Owner.is_loaded entry.runtime));
                   let stopped = checkpoint entry in
                   assert (P.Run.equal original_run (run stopped admission));
                   assert (G.equal (Goal.goal held) (Goal.goal stopped));
                   assert (
                     A.Goal_accounting.equal
                       (Goal.accounting held)
                       (Goal.accounting stopped));
                   (match stopped.lifecycle.desired, stopped.lifecycle.observed with
                    | Stopped, Stopped -> ()
                    | _ -> failwith "actual Stop did not join");
                   let evidence =
                     G.Reason.of_string "Actual Session_resume completes this Goal"
                     |> protocol_ok
                   in
                   let finish =
                     P.Goal_achievement.Intent.create
                       ~expected_goal_revision:
                         (Int64.succ (G.revision (Goal.goal stopped)))
                       ~evidence
                       ~action:(Finish { terminal = Completed None; relinquish = [] })
                     |> protocol_ok
                     |> P.Goal_achievement.Intent.to_json
                   in
                   let context =
                     `Object
                       [ "run_id", P.Id.Run.to_json original_run.id
                       ; "source", P.Run_source.to_json original_run.source
                       ; "finish", finish
                       ]
                   in
                   Eio.Path.save
                     ~create:(`Or_truncate 0o600)
                     Eio.Path.(Eio.Stdenv.fs env / root / resume_context_file)
                     (Jsonaf.to_string context ^ "\n");
                   let actor = Staging.guarded Staging.resume_principal (ref true) in
                   let request =
                     Staging.exact_request stopped ~key:"goal-cold-runtime-resume"
                   in
                   probe.awaiting_root <- true;
                   probe.phase <- "resume callback";
                   protect_cleanup
                     ~finally:(fun () -> Provider_probe.release probe)
                     (fun () ->
                        match
                          f
                            env
                            daemon
                            entry
                            initial_runtime
                            counters
                            probe
                            session
                            admission
                            initial
                            stopped
                            actor
                            request
                        with
                        | result -> result
                        | exception exn ->
                          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                          print_s
                            [%sexp
                              "cold runtime fixture failed"
                            , (probe.phase : string)
                            , (!(counters.providers) : int)
                            , (List.length probe.allocations : int)];
                          Option.iter probe.last_state ~f:(fun state ->
                            let callbacks =
                              List.map state.moderator_executions ~f:(fun receipt ->
                                ( receipt.P.Moderator_execution.context.phase
                                , receipt.status ))
                            in
                            print_s
                              [%sexp
                                (Ledger.summary state.inference_ledger
                                 : P.Inference_query.Summary.t)
                              , (state.active_operation : P.Operation.t option)
                              , (callbacks
                                 : (P.Moderator_execution.phase
                                   * P.Moderator_execution.status)
                                     list)]);
                          Exn.raise_with_original_backtrace exn backtrace)))))
;;

let completed_resume current admission =
  let receipt = Goal_cold_tests.completed_resume current in
  let actual = run current admission in
  assert (P.Invocation.equal_observer actual.source.observer receipt.context.source);
  [%test_eq: int] actual.source.generation receipt.context.generation;
  receipt
;;

let assert_native current admission resume =
  let actual = run current admission in
  match
    List.filter current.A.Session_state.invocations ~f:(fun invocation ->
      String.equal invocation.P.Invocation.context.tool_name "read_file")
  with
  | [ invocation ] ->
    assert (
      Option.exists
        invocation.parent_event
        ~f:(P.Id.Moderator_execution.equal resume.P.Moderator_execution.context.id));
    (match invocation.inference_origin with
     | Some (Run id) -> assert (P.Id.Run.equal id actual.id)
     | Some Unbound | None -> failwith "cold resume native lost same-Run origin");
    (match invocation.status with
     | Resolved (Complete _) | Published (Complete _) -> ()
     | _ ->
       raise_s
         [%sexp "cold resume native did not complete", (invocation : P.Invocation.t)])
  | _ -> failwith "cold resume did not execute exactly one actual native read"
;;

let one_actual label = function
  | [ value ] -> value
  | values ->
    failwithf "%s: expected one actual occurrence, found %d" label (List.length values) ()
;;

let constructor_jobs current =
  List.filter current.A.Session_state.jobs ~f:(fun job ->
    P.Job.equal_kind Model_call job.kind
    && Option.equal
         Jsonaf.exactly_equal
         (Jsonaf.member "payload" job.payload |> Option.bind ~f:(Jsonaf.member "input"))
         (Some (`String constructor_input)))
;;

let assert_constructor_model before final admission probe =
  let captured =
    List.filter probe.Provider_probe.allocations ~f:(fun snapshot ->
      List.exists (constructor_jobs snapshot.state) ~f:(fun job ->
        match job.P.Job.status with
        | Running -> true
        | Queued
        | Waiting_permission _
        | Waiting_completion _
        | Succeeded
        | Failed _
        | Cancelled
        | Interrupted _ -> false))
  in
  let snapshot = one_actual "constructor allocation" captured in
  let job = one_actual "running constructor Model" (constructor_jobs snapshot.state) in
  [%test_eq: int] 1 job.attempt;
  assert (not snapshot.owner_loaded);
  assert (Option.is_none snapshot.state.active_operation);
  assert_episode before snapshot.state admission;
  let original_run = run before admission in
  assert (
    Option.equal
      P.Inference_origin.equal
      job.inference_origin
      (Some (Run original_run.id)));
  let work =
    P.Run_work.create
      ~key:(Retained (Job { id = job.id; attempt = job.attempt }))
      ~generation:job.generation
    |> protocol_ok
  in
  assert (List.mem (run snapshot.state admission).owned_work work ~equal:P.Run_work.equal);
  let callback =
    List.filter snapshot.state.moderator_executions ~f:(fun receipt ->
      P.Moderator_execution.equal_phase receipt.context.phase Session_resume)
    |> one_actual "live constructor resume"
  in
  (match callback.status with
   | Running -> ()
   | Completed _ | Failed _ | Interrupted _ ->
     failwith "constructor Model allocation did not occur inside the actual resume");
  assert (P.Invocation.equal_observer original_run.source.observer callback.context.source);
  let row =
    Goal_admitted_guard_tests.row snapshot.state snapshot.scope snapshot.accounting_id
  in
  let handle = Ledger.Row.handle row in
  (match Inference.Observation.Attempt_record.state (Ledger.Row.record row) with
   | Prepared -> ()
   | Running | Terminal _ | Interrupted _ ->
     failwith "Model observer did not record the actual allocation ACK boundary");
  let account = Goal.accounting snapshot.state in
  [%test_eq: int64] 1L (A.Goal_accounting.admitted_turns account);
  [%test_eq: int64] 2L (A.Goal_accounting.admitted_attempts account);
  [%test_eq: int] 2 (List.length (A.Goal_accounting.bindings account));
  assert (
    List.exists (A.Goal_accounting.bindings account) ~f:(fun binding ->
      Ledger.Handle.equal handle (A.Goal_accounting.Binding.handle binding)
      && P.Id.Run.equal original_run.id (A.Goal_accounting.Binding.run_id binding)
      && P.Run_source.equal original_run.source (A.Goal_accounting.Binding.source binding)));
  let completed = one_actual "completed constructor Model" (constructor_jobs final) in
  assert (P.Id.Job.equal job.id completed.id);
  [%test_eq: int] job.attempt completed.attempt;
  (match completed.status, completed.delivery with
   | Succeeded, Not_required -> ()
   | _ -> raise_s [%sexp "constructor Model did not complete", (completed : P.Job.t)]);
  assert (
    Option.equal
      Jsonaf.exactly_equal
      (Option.bind completed.result ~f:(Jsonaf.member "final_text"))
      (Some (`String "543 real Goal continuation result")));
  assert (
    List.exists (run final admission).terminal_work ~f:(fun proof ->
      P.Run_work.equal work proof.P.Run_work.Terminal.work
      && P.Run_work.Terminal.equal_outcome Succeeded proof.outcome));
  let terminal_row =
    Goal_admitted_guard_tests.row final snapshot.scope snapshot.accounting_id
  in
  assert (Ledger.Handle.equal handle (Ledger.Row.handle terminal_row));
  match Inference.Observation.Attempt_record.state (Ledger.Row.record terminal_row) with
  | Terminal terminal ->
    (match Inference.Event.Terminal.outcome terminal with
     | Completed -> ()
     | Refused | Incomplete _ | Failed _ ->
       raise_s
         [%sexp "constructor provider failed", (terminal : Inference.Event.Terminal.t)])
  | Prepared | Running | Interrupted _ -> failwith "constructor inference has no terminal"
;;

let%expect_test "cold Goal continuation runs legacy and v1 Session_resume before its turn"
  =
  List.iter [ Surface.Legacy; V1 ] ~f:(fun surface ->
    with_cold_episode
      surface
      Continue
      (fun
          env
           daemon
           entry
           old_runtime
           counters
           probe
           session
           admission
           initial
           stopped
           actor
           request
         ->
         let constructor_attempts =
           match surface with
           | Legacy -> 1
           | V1 -> 0
         in
         let result =
           Service.resume_episode (public daemon) ~entry ~actor ~session ~request
           |> protocol_ok
         in
         probe.phase <- "resume service returned";
         (match result with
          | Resume_failed { error; _ } | Rejected error | Uncertain error ->
            let jobs =
              Actor.state entry.actor
              |> Core.Result.map ~f:(fun state ->
                List.map state.jobs ~f:(fun job -> job.P.Job.id, job.status, job.attempt))
            in
            raise_s
              [%sexp
                "cold Goal resume failed"
              , (error : P.Error.t)
              , (jobs : ((P.Id.Job.t * P.Job.status * int) list, P.Error.t) Core.Result.t)
              , (!(counters.providers) : int)]
          | Resumed _ | Retained _ -> ());
         let receipt, operation = started result in
         Staging.assert_receipt request stopped receipt;
         (* The public service has returned only after its real source/preparation
              scopes joined. Provider response and factual completion follow that
              boundary, not a test-controlled runtime owner. *)
         Eio.Promise.await probe.entered;
         probe.phase <- "Administrative provider entered";
         [%test_eq: int] (1 + constructor_attempts) !(counters.providers);
         Provider_probe.release probe;
         probe.phase <- "awaiting completed continuation";
         let final =
           await_turns
             ~providers:(2 + constructor_attempts)
             ~observe:(fun state -> probe.last_state <- Some state)
             env
             entry
             counters
             ~completed:2
         in
         probe.phase <- "checking completed continuation";
         assert_started
           ~attempts:(Int64.of_int (2 + constructor_attempts))
           stopped
           final
           admission
           operation;
         (match surface with
          | Legacy ->
            assert_constructor_model stopped final admission probe;
            let timer = one_actual "actual canceled Init timer" final.schedules in
            (match timer.status with
             | Cancelled -> ()
             | Scheduled | Delivering | Delivered | Failed _ ->
               failwith "actual Init timer was not canceled");
            [%test_eq: int] 0 timer.delivery_count
          | V1 -> assert (List.is_empty final.jobs));
         let resume = completed_resume final admission in
         assert_native final admission resume;
         List.iter (startup_receipts initial) ~f:(fun prior ->
           assert (
             List.exists final.moderator_executions ~f:(P.Moderator_execution.equal prior)));
         [%test_eq: int] 2 (List.length (startup_receipts final));
         let first_resumed_effect =
           one_actual "post-CPS root provider" probe.root_states
         in
         assert (
           P.Moderator_execution.equal
             resume
             (completed_resume first_resumed_effect admission));
         assert_native first_resumed_effect admission resume;
         assert_episode stopped first_resumed_effect admission;
         probe.phase <- "checking installed runtime";
         Owner.with_loaded_runtime entry.runtime (fun actual ->
           assert (not (phys_equal old_runtime actual));
           (match surface with
            | V1 -> ()
            | Legacy ->
              let cached = A.Runtime_builder.runtime_resume actual |> protocol_ok in
              A.Runtime_builder.Runtime_resume.check_pending cached |> protocol_ok;
              assert (
                phys_equal actual.worker (A.Runtime_builder.Runtime_resume.worker cached)));
           Ok ())
         |> protocol_ok;
         probe.phase <- "checking historical command replay";
         repeat daemon entry session actor request receipt counters env;
         probe.phase <- "historical replay checked";
         print_s
           [%sexp
             (surface : Surface.t)
           , "one real cold lifecycle precedes same-Run continuation"]));
  [%expect
    {|
    (Legacy "one real cold lifecycle precedes same-Run continuation")
    (V1 "one real cold lifecycle precedes same-Run continuation")
  |}]
;;

let%expect_test
    "cold callback Wait or Goal.finish is not replaced by an Administrative turn"
  =
  List.iter [ Decision.Wait; Finish ] ~f:(fun decision ->
    with_cold_episode
      V1
      decision
      (fun
          env
           daemon
           entry
           _old_runtime
           counters
           _probe
           session
           admission
           _initial
           stopped
           actor
           request
         ->
         let result =
           Service.resume_episode (public daemon) ~entry ~actor ~session ~request
           |> protocol_ok
         in
         let current = settled_snapshot entry in
         assert_episode stopped current admission;
         let resume = completed_resume current admission in
         assert_native current admission resume;
         let receipt =
           match decision, result with
           | Wait, Result.Resumed { receipt; continuation = Waiting wake } ->
             assert (P.Id.Run.equal admission.run_id wake.run_id);
             assert (P.Run_source.equal (run stopped admission).source wake.source);
             (match (run current admission).lifecycle with
              | Waiting actual -> assert (P.Run_wake.equal wake actual)
              | _ -> failwith "callback Wait was not retained");
             (match wake.occurrence, current.schedules with
              | ( Delivered_timer { schedule_id; delivery_count; creator; subscription }
                , [ schedule ] ) ->
                assert (P.Id.Schedule.equal schedule.id schedule_id);
                [%test_eq: int] 1 delivery_count;
                [%test_eq: int] 0 schedule.delivery_count;
                (match schedule.status with
                 | Scheduled -> ()
                 | _ -> failwith "passive wait timer ran early");
                let owner = Option.value_exn schedule.ownership in
                assert (P.Job.equal_launch_owner creator owner.creator);
                assert (
                  P.Job.equal_launch_owner creator (Moderator_event resume.context.id));
                assert (P.Invocation.equal_observer owner.source wake.source.observer);
                assert (Option.is_none subscription);
                let provenance = Option.value_exn schedule.inference_provenance in
                assert (
                  P.Run_source.equal wake.source (P.Schedule_provenance.source provenance));
                (match P.Schedule_provenance.origin provenance with
                 | Some (Run id) -> assert (P.Id.Run.equal admission.run_id id)
                 | Some Unbound | None ->
                   failwith "owned timer lost actual Run provenance")
              | _ -> failwith "Wait did not select its actual new timer");
             assert (G.State.equal (Open Enabled) (G.state (Goal.goal current)));
             receipt
           | Finish, Resumed { receipt; continuation = Settled settlement } ->
             let episode = G.episode (Goal.goal stopped) |> Option.value_exn in
             assert (
               P.Goal_episode.equal episode (P.Goal_episode.Settlement.episode settlement));
             let terminal = P.Goal_episode.Settlement.receipt settlement in
             P.Goal_episode.validate_terminal
               episode
               ~run:(run current admission)
               ~receipt:terminal
             |> protocol_ok;
             (match (run current admission).lifecycle with
              | Terminal (Completed _) -> ()
              | _ -> failwith "Goal finish lacks actual completed Run");
             (match G.state (Goal.goal current) with
              | Closed (Achieved _) -> ()
              | _ -> failwith "Goal.finish did not achieve the actual Goal");
             receipt
           | ( (Wait | Finish)
             , (Resume_failed { error; _ } | Rejected error | Uncertain error) ) ->
             raise_s
               [%sexp
                 "cold decision failed", (decision : Decision.t), (error : P.Error.t)]
           | _ -> failwith "cold decision fabricated a different continuation"
         in
         Staging.assert_receipt request stopped receipt;
         assert (Option.is_none current.active_operation);
         assert (
           List.equal
             P.Run_work.equal
             (operation_work (run stopped admission))
             (operation_work (run current admission)));
         [%test_eq: int] 1 !(counters.providers);
         assert (
           A.Goal_accounting.equal (Goal.accounting stopped) (Goal.accounting current));
         repeat daemon entry session actor request receipt counters env;
         (match decision with
          | Continue | Finish -> ()
          | Wait ->
            let wake =
              match (run current admission).lifecycle with
              | Waiting wake -> wake
              | Admitted | Active | Terminal _ ->
                failwith "passive resume fixture lost its actual issued wake"
            in
            Goal_admitted_guard_tests.command
              daemon
              entry
              ~key:"goal-waiting-resume-hold"
              (Hold (Goal.goal_id, Operator_pause))
            |> protocol_ok
            |> ignore;
            Owner.unload_and_wait entry.runtime |> protocol_ok;
            assert (not (Owner.is_loaded entry.runtime));
            let waiting_held = settled_snapshot entry in
            assert (
              G.State.equal
                (Open (Held Operator_pause))
                (G.state (Goal.goal waiting_held)));
            assert (P.Run.equal (run current admission) (run waiting_held admission));
            (match
               A.Goal_execution_selection.of_run waiting_held (run waiting_held admission)
               |> protocol_ok
             with
             | Suspended_goal { goal_id; hold } ->
               assert (P.Id.Goal.equal goal_id Goal.goal_id);
               assert (G.Hold.equal hold Operator_pause)
             | Eligible -> failwith "Held passive Goal wake was eligible");
            let schedules = List.map current.schedules ~f:P.Schedule.Storage.to_json in
            assert (
              List.equal
                Jsonaf.exactly_equal
                schedules
                (List.map waiting_held.schedules ~f:P.Schedule.Storage.to_json));
            let counts = !(counters.providers), !(counters.inference_ports) in
            let next_request =
              Staging.exact_request waiting_held ~key:"goal-already-waiting-resume"
            in
            let next_receipt =
              match
                Service.resume_episode
                  (public daemon)
                  ~entry
                  ~actor
                  ~session
                  ~request:next_request
                |> protocol_ok
              with
              | Resumed { receipt; continuation = Waiting actual } ->
                assert (P.Run_wake.equal wake actual);
                receipt
              | Resumed { continuation = Started _ | Settled _; _ } | Retained _ ->
                failwith "already-Waiting resume reconstructed or executed a runtime"
              | Resume_failed { error; _ } | Rejected error | Uncertain error ->
                raise_s [%sexp "passive Goal resume failed", (error : P.Error.t)]
            in
            Staging.assert_receipt next_request waiting_held next_receipt;
            assert (not (Owner.is_loaded entry.runtime));
            [%test_eq: int * int]
              counts
              (!(counters.providers), !(counters.inference_ports));
            let resumed_waiting = settled_snapshot entry in
            Staging.assert_existing_episode waiting_held resumed_waiting;
            assert (G.State.equal (Open Enabled) (G.state (Goal.goal resumed_waiting)));
            (match
               A.Goal_execution_selection.of_run
                 resumed_waiting
                 (run resumed_waiting admission)
               |> protocol_ok
             with
             | Eligible -> ()
             | Suspended_goal _ -> failwith "resumed passive Goal wake remained suspended");
            let selected_run = run resumed_waiting admission in
            let stale_run = run stopped admission in
            assert (not (P.Run.equal selected_run stale_run));
            [%test_eq: P.Error.code]
              Conflict
              (A.Goal_execution_selection.of_run resumed_waiting stale_run |> rejected)
                .code;
            assert (
              List.equal
                Jsonaf.exactly_equal
                schedules
                (List.map resumed_waiting.schedules ~f:P.Schedule.Storage.to_json));
            assert (
              Option.exists
                (Staging.historical entry session ~actor request)
                ~f:(A.Goal_command_receipt.equal receipt));
            assert (
              Option.exists
                (Staging.historical entry session ~actor next_request)
                ~f:(A.Goal_command_receipt.equal next_receipt)));
         print_s
           [%sexp
             (decision : Decision.t)
           , "same episode retains its callback decision without another provider or \
              Operation"]));
  [%expect
    {|
    (Wait
     "same episode retains its callback decision without another provider or Operation")
    (Finish
     "same episode retains its callback decision without another provider or Operation")
  |}]
;;

let prepared_identity_probe runtime =
  let context =
    A.Runtime_builder.inference_execution runtime |> Inference_client.Execution.context
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
  match
    Inference_runtime.Context.prepare
      context
      ~preparation_id:"goal-resume-init-escape"
      request
  with
  | Ok prepared -> prepared
  | Error error -> raise_s [%sexp (error : Inference_runtime.Preparation_error.t)]
;;

let%expect_test
    "Goal cold Init retains the old original and cannot expose an unbound graph"
  =
  Staging.with_episode
    (fun _env daemon entry runtime counters session admission old_live held ->
       (* Immutable prepared request DATA comes from the actual loaded runtime
          before its legitimate Stop. Preparing it neither allocates nor submits. *)
       let prepared_request = prepared_identity_probe runtime in
       let attachment, _ =
         Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
       in
       protect_cleanup
         ~finally:(fun () -> Actor.detach entry.actor attachment.id |> protocol_ok)
         (fun () ->
            Actor.stop entry.actor ~attachment_id:attachment.id ~mode:Graceful
            |> protocol_ok
            |> ignore;
            Owner.unload_and_wait entry.runtime |> protocol_ok;
            assert (not (Owner.is_loaded entry.runtime));
            let stopped = checkpoint entry in
            assert (P.Run.equal (run held admission) (run stopped admission));
            let new_original = Staging.guarded Staging.resume_principal (ref true) in
            let request =
              Staging.exact_request stopped ~key:"goal-resume-init-original"
            in
            let admitted = ref 0
            and attempt_bodies = ref 0 in
            let report =
              Staging.stage
                daemon
                entry
                session
                ~actor:new_original
                request
                (fun prepared ->
                   let continuation =
                     match
                       Actor.admit_goal_resume
                         entry.actor
                         ~binding:(Owner.Prepared_goal_continuation.binding prepared)
                       |> protocol_ok
                     with
                     | Ready continuation -> continuation
                     | Retained _ | Waiting _ | Rejected _ | Uncertain _ | Resume_failed _
                       -> failwith "cold Init fixture did not obtain its real Ready owner"
                   in
                   let construction = Actor.Runtime_construction.of_goal continuation in
                   Actor.begin_goal_runtime_construction entry.actor ~construction
                   |> protocol_ok;
                   let expected = state entry in
                   let scope =
                     Actor.begin_runtime_initialization
                       entry.actor
                       ~construction
                       ~purpose:Runtime_construction
                       ~expected
                     |> protocol_ok
                   in
                   let opened_graph = ref None in
                   protect_cleanup
                     ~finally:(fun () ->
                       Option.iter !opened_graph ~f:(fun graph ->
                         Graph.seal graph |> protocol_ok;
                         Graph.finish graph |> protocol_ok))
                     (fun () ->
                        let identity =
                          protect_cleanup
                            ~finally:(fun () ->
                              Actor.end_initialization entry.actor ~scope |> protocol_ok)
                            (fun () ->
                               let borrowed =
                                 Actor.initialization_agent_actor entry.actor ~scope
                                 |> protocol_ok
                                 |> Option.value_exn
                               in
                               let original_run = run stopped admission in
                               assert (
                                 P.Id.Principal.equal
                                   original_run.principal_id
                                   (Operator_authorization.principal borrowed).id);
                               assert (
                                 not
                                   (P.Id.Principal.equal
                                      original_run.principal_id
                                      (Operator_authorization.principal new_original).id));
                               assert (Operator_authorization.is_current borrowed);
                               let graph =
                                 Graph.create_for_runtime_construction
                                   entry.actor
                                   ~construction
                                   ~source:
                                     (Transcript.Source_id.of_string
                                        "goal-resume-init-graph"
                                      |> Core.Result.ok_or_failwith)
                                   ~upstream:
                                     { new_preparation_id =
                                         (fun () -> "goal-resume-init-escape")
                                     ; on_admitted =
                                         (fun ~scope:_ ~accounting_id:_ ->
                                           Int.incr admitted)
                                     ; on_attempt = ignore
                                     ; on_observation = ignore
                                     ; on_completion = ignore
                                     }
                                 |> protocol_ok
                               in
                               opened_graph := Some graph;
                               old_live := false;
                               assert (not (Operator_authorization.is_current borrowed));
                               assert (Operator_authorization.is_current new_original);
                               Graph.identity graph)
                        in
                        (* Init ended before this bare identity is used. Its same
                           immutable graph owner remains for factual cleanup only. *)
                        let before = state entry in
                        let rejection =
                          try
                            identity.with_attempt
                              prepared_request
                              ~relation:Root
                              ~f:(fun ~scope:_ ~accounting_id:_ ~submission_guard:_ ->
                                Int.incr attempt_bodies);
                            failwith "ended Goal Init exposed unbound inference"
                          with
                          | Graph.Rejected error -> error
                        in
                        [%test_eq: P.Error.code] Permission_denied rejection.code;
                        [%test_eq: bool] false rejection.retryable;
                        [%test_eq: int] 0 !admitted;
                        [%test_eq: int] 0 !attempt_bodies;
                        [%test_eq: int] 1 !(counters.providers);
                        Existing.assert_same before entry;
                        let after = state entry in
                        assert (
                          Jsonaf.exactly_equal
                            (Staging.ledger_json before.inference_ledger)
                            (Staging.ledger_json after.inference_ledger));
                        assert (
                          A.Goal_accounting.equal
                            (Goal.accounting before)
                            (Goal.accounting after));
                        Ok (Actor.Goal_continuation.receipt continuation)))
            in
            let receipt =
              match Factory.Goal_resume_source.result report |> protocol_ok with
              | Fresh receipt -> receipt
              | Retained _ -> failwith "Init regression unexpectedly reused a command"
            in
            Staging.assert_receipt request stopped receipt;
            match
              Owner.Scope_outcome.Cleanup.status
                (Factory.Goal_resume_source.cleanup report)
            with
            | Joined -> ()
            | Never_acquired | Failed _ ->
              failwith "real Init/source cleanup did not join"));
  print_endline
    "Init borrows the old Run original; revocation expires it; ended graph identity \
     rejects without an unbound charge and factual graph cleanup joins";
  [%expect
    {| Init borrows the old Run original; revocation expires it; ended graph identity rejects without an unbound charge and factual graph cleanup joins |}]
;;
