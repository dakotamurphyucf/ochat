open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Resume = Goal_resume_runtime_tests
module Staging = Goal_resume_tests
module Manual = Goal_job_runtime_tests
module Source = Deferred_source_support
module Service = Agent_server.Goal_run_service
module G = P.Goal

let context_file = "545-subscription-context.json"

module Gate = struct
  type t =
    { waiting : unit Eio.Promise.t
    ; release : unit Eio.Promise.u
    ; mutable released : bool
    }

  let create () =
    let waiting, release = Eio.Promise.create () in
    { waiting; release; released = false }
  ;;

  let release t =
    if not t.released
    then (
      t.released <- true;
      Eio.Promise.resolve t.release ())
  ;;
end

let watch_response () =
  let open Res.Response_stream in
  let args = "{}" in
  let item arguments status =
    Item.Function_call
      { name = "watch"
      ; arguments
      ; call_id = "545-subscription-call"
      ; _type = "function_call"
      ; id = Some "545-subscription-item"
      ; status
      }
  in
  [ Output_item_added
      { item = item "" None; output_index = 0; type_ = "response.output_item.added" }
  ; Function_call_arguments_done
      { arguments = args
      ; item_id = "545-subscription-item"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ; Output_item_done
      { item = item args (Some "completed")
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let prompt =
  {|<developer>Call watch once, then preserve its exact Goal subscription wait.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="goal_subscription" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = { subscription = ""; invocation = ""; waiting = false; completed_responses = 0 }
let field value key = match Json.get_field(value, key) with
| `Some(found) -> found
| `None -> `Null
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let* subscription = Subscription.create("545 actual subscription", `Some(600000), `No_wake) in
  let* () = Invocation.resolve(p.context.invocation_id,
    `Pending(`Subscription(subscription), `String("545 accepted subscription"))) in
  Task.pure({ subscription = subscription; invocation = p.context.invocation_id; waiting = false; completed_responses = 0 })
| `Turn_end ->
  if state.waiting then
    let* read = Tool.call("read_file", `Object([
      {key = "root"; value = `String("data")},
      {key = "file"; value = `String("545-subscription-context.json")}
    ])) in
    (match read with
    | `Error(message) -> Task.fail(message)
    | `Ok(`String(text)) ->
      let lines = String.split(text, "\n") in
      let context = Json.parse(lines[2]) in
      (match field(context, "finish") with
      | `Null -> Task.pure(state)
      | intent -> Task.bind(Goal.finish(intent), fun ignored -> Task.pure(state)))
    | `Ok(_) -> Task.fail("finish context read did not return actual text")) else
  if state.completed_responses == 0 then
    Task.pure({ state with completed_responses = 1 }) else
  let* read = Tool.call("read_file", `Object([
    {key = "root"; value = `String("data")},
    {key = "file"; value = `String("545-subscription-context.json")}
  ])) in
  (match read with
  | `Error(message) -> Task.fail(message)
  | `Ok(`String(text)) ->
    let lines = String.split(text, "\n") in
    let context = Json.parse(lines[2]) in
    let* completed = Subscription.complete(state.subscription, 0, `String("545 ready")) in
    let* delivery = Notification.publish(
      { key = "545-subscription"; invocation_id = `Some(state.invocation);
        work = `Some(`Subscription(state.subscription)) },
      `Succeeded(`String("545 ready")), `Request_turn) in
    let* current = Notification.get(delivery) in
    let owner = field(field(current, "delivery"), "ownership") in
    let* unrelated = Notification.publish(
      { key = "545-ordinary"; invocation_id = `None; work = `None },
      `Succeeded(`String("545 unrelated notice")), `Request_turn) in
    let wake = `Object([
      {key = "run_id"; value = field(context, "run_id")},
      {key = "source"; value = field(context, "source")},
      {key = "occurrence"; value = `Object([
        {key = "kind"; value = `String("subscription_delivery")},
        {key = "subscription_id"; value = `String(state.subscription)},
        {key = "epoch"; value = field(completed, "epoch")},
        {key = "delivery_id"; value = `String(delivery)},
        {key = "creator"; value = `Object([
          {key = "kind"; value = field(owner, "creator_type")},
          {key = "id"; value = field(owner, "creator_id")}
        ])}
      ])}
    ]) in
    Task.bind(Run.wait(wake), fun ignored -> Task.pure({ state with waiting = true }))
  | `Ok(_) -> Task.fail("read_file did not return actual text"))
| _ -> Task.pure(state)
</script>
<tool name="watch" type="moderator" moderator="goal_subscription" input_schema="any.json" output_schema="any.json" completion_schema="any.json"/>|}
;;

let delivery current correlation =
  Resume.one_actual
    correlation
    (List.filter current.A.Session_state.deliveries ~f:(fun delivery ->
       String.equal delivery.P.Delivery.context.correlation correlation))
;;

let accepted current correlation =
  match (delivery current correlation).wake_disposition with
  | Some (Accepted_wake id) -> id
  | None | Some (Pending_wake | Discarded_wake _) ->
    failwith "notification has no actual accepted Operation"
;;

let pending current =
  let value = delivery current "545-subscription" in
  match value.status, value.wake_disposition with
  | Committed _, Some Pending_wake -> value
  | Pending, _
  | Failed _, _
  | Committed _, (None | Some (Accepted_wake _ | Discarded_wake _)) ->
    let runs = Option.value_map current.run_state ~default:[] ~f:A.Run_state.runs in
    let publisher =
      List.filter current.invocations ~f:(fun invocation ->
        Option.exists value.context.invocation_id ~f:(fun id ->
          P.Id.Invocation.equal id invocation.P.Invocation.context.id))
    in
    raise_s
      [%sexp
        "Held subscription notification lost its actual pending wake"
      , (value : P.Delivery.t)
      , (G.state (Goal.goal current) : G.State.t)
      , (runs : P.Run.t list)
      , (publisher : P.Invocation.t list)
      , (current.active_operation : P.Operation.t option)]
;;

let waiting current admission =
  match (Resume.run current admission).lifecycle with
  | Waiting wake -> wake
  | Admitted | Active | Terminal _ -> failwith "actual Goal subscription Wait is absent"
;;

let assert_operation_owned current admission operation =
  assert (
    Option.exists current.A.Session_state.active_operation ~f:(fun actual ->
      P.Id.Operation.equal actual.id operation.P.Operation.id));
  assert (
    List.exists (Resume.run current admission).owned_work ~f:(fun work ->
      match work.P.Run_work.key with
      | Operation id -> P.Id.Operation.equal id operation.id
      | Retained _ -> false));
  assert (P.Id.Operation.equal operation.id (accepted current "545-subscription"))
;;

let assert_succeeded_proof current admission key =
  let run = Resume.run current admission in
  let work =
    P.Run_work.create ~key ~generation:current.A.Session_state.identity.generation
    |> protocol_ok
  in
  assert (List.mem run.owned_work work ~equal:P.Run_work.equal);
  let proof =
    Resume.one_actual
      "exact owned producer terminal proof"
      (List.filter run.terminal_work ~f:(fun proof ->
         P.Run_work.equal proof.P.Run_work.Terminal.work work))
  in
  [%test_eq: P.Run_work.Terminal.outcome] Succeeded proof.outcome;
  assert (Int64.compare proof.revision run.revision <= 0)
;;

let assert_published_proof current admission correlation =
  let value = delivery current correlation in
  let history_id =
    match value.status with
    | Committed { history_id; _ } -> history_id
    | Pending | Failed _ -> failwith "owned delivery has not published actual history"
  in
  let history =
    Resume.one_actual
      "actual correlated publication"
      (List.filter current.A.Session_state.conversation.canonical_history ~f:(fun entry ->
         P.History.Id.equal entry.P.History.id history_id))
  in
  (match history.provenance with
   | Runtime_notification id -> assert (P.Id.Delivery.equal id value.context.id)
   | Canonical | Moderator_inserted | Moderator_replaced _ | Runtime_authoring _ ->
     failwith "publication history does not retain its actual notification ID");
  assert_succeeded_proof current admission (Retained (Delivery value.context.id))
;;

let%expect_test
    "cold Goal subscription finishes only after correlated publication and its own turn"
  =
  let initial = Gate.create () in
  let resumed = Gate.create () in
  let cache = Gate.create () in
  let cache_entered, cache_enter = Eio.Promise.create () in
  let cache_directory = ref None in
  let cache_hits = ref 0 in
  let entered, enter = Eio.Promise.create () in
  let provider_state = ref None in
  let selected = ref None in
  let phase = ref "initial runtime" in
  let observed = ref None in
  let observe current = observed := Some current in
  let checkpoint entry =
    let current = Resume.checkpoint entry in
    observe current;
    current
  in
  let rec await_signal entry promise =
    match Eio.Promise.peek promise with
    | Some () -> ()
    | None ->
      observe (state entry);
      Eio.Fiber.yield ();
      await_signal entry promise
  in
  let counters : Retry_support.counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr counters.providers;
      match !(counters.providers) with
      | 1 ->
        Eio.Promise.await initial.waiting;
        watch_response ()
      | 2 | 3 -> completed_response "545 actual completed turn"
      | 4 ->
        provider_state := Option.map !selected ~f:state;
        Option.iter !provider_state ~f:observe;
        Eio.Promise.resolve enter ();
        Eio.Promise.await resumed.waiting;
        completed_response "545 actual subscription turn"
      | _ -> failwith "subscription delivery replayed inference")
  in
  let chatml_runtime_policy =
    let defaults = Chat_response.Runtime_semantics.default_policy in
    (* This scenario intentionally admits two independent automatic follow-ups:
       the ordinary notice, then the resumed Goal subscription. *)
    { defaults with budget = { defaults.budget with max_followup_turns = 2 } }
  in
  Creation_tests.with_daemon
    ~chatml_runtime_policy
    ~prompt_text:prompt
    ~qualify_chatml_extensions:true
    ~sources:[ "any.json", "true"; context_file, "{}\n" ]
    ~policy
    ~wrap_env:(fun env ->
      Crash_fault_io.wrap
        env
        ~boundary:Before_rename
        ~matches:(fun path ->
          Option.exists !cache_directory ~f:(fun directory ->
            String.equal directory (Filename.dirname path)
            && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
            && String.is_suffix path ~suffix:".tmp"))
        ~reached:(fun _ ->
          cache_directory := None;
          Int.incr cache_hits;
          Eio.Promise.resolve cache_enter ();
          Eio.Promise.await cache.waiting))
    (fun env _sw root daemon ->
       try
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60. (fun () ->
           Source.with_loaded_entry daemon (fun ~entry ~runtime:old_runtime ~revision:_ ->
             selected := Some entry;
             protect_cleanup
               ~finally:(fun () ->
                 Gate.release initial;
                 Gate.release resumed;
                 Gate.release cache)
               (fun () ->
                  phase := "create Goal";
                  let session, before = Goal.create_goal daemon entry in
                  observe before;
                  cache_directory
                  := Some
                       (Agent_store.Session_store.Handle.responses_directory
                          (Option.value_exn entry.store_handle));
                  phase := "start Goal episode";
                  let admission =
                    Resume.start_episode daemon entry original session before
                  in
                  let actual_run = Resume.run (state entry) admission in
                  Eio.Path.save
                    ~create:(`Or_truncate 0o600)
                    Eio.Path.(Eio.Stdenv.fs env / root / context_file)
                    (Jsonaf.to_string
                       (`Object
                           [ "run_id", P.Id.Run.to_json actual_run.id
                           ; "source", P.Run_source.to_json actual_run.source
                           ])
                     ^ "\n");
                  Gate.release initial;
                  phase := "await initial post-Turn_end cache barrier";
                  await_signal entry cache_entered;
                  [%test_eq: int] 1 !cache_hits;
                  let passive = checkpoint entry in
                  assert (Option.is_some passive.active_operation);
                  [%test_eq: int] 2 !(counters.providers);
                  let wake = waiting passive admission in
                  [%test_eq: int] 1 (List.length passive.subscriptions);
                  (match wake.occurrence with
                   | Subscription_delivery { subscription_id; epoch; delivery_id; _ } ->
                     let subscription = List.hd_exn passive.subscriptions in
                     assert (
                       P.Id.Subscription.equal subscription.context.id subscription_id);
                     [%test_eq: int] subscription.epoch epoch;
                     assert (
                       P.Id.Delivery.equal
                         (delivery passive "545-subscription").context.id
                         delivery_id)
                   | Delivered_timer _ | Job_completion _ ->
                     failwith "actual subscription did not own Wait");
                  [%test_eq: int] 2 (List.length passive.deliveries);
                  List.iter passive.deliveries ~f:(fun value ->
                    match value.P.Delivery.status, value.wake_disposition with
                    | Pending, None -> ()
                    | _ -> failwith "fixture lost the newly pending mixed batch");
                  phase := "Hold exact newly Pending mixed batch";
                  Goal_admitted_guard_tests.command
                    daemon
                    entry
                    ~key:"545-subscription-held"
                    (Hold (Goal.goal_id, Operator_pause))
                  |> protocol_ok
                  |> ignore;
                  let held = checkpoint entry in
                  Gate.release cache;
                  phase := "await ordinary turn while Goal Held";
                  let ordinary =
                    Resume.await_turns
                      ~providers:3
                      ~observe
                      env
                      entry
                      counters
                      ~completed:2
                  in
                  ignore (pending ordinary : P.Delivery.t);
                  let ordinary_operation = accepted ordinary "545-ordinary" in
                  assert (P.Run_wake.equal wake (waiting ordinary admission));
                  Resume.assert_episode held ordinary admission;
                  assert (
                    G.State.equal
                      (Open (Held Operator_pause))
                      (G.state (Goal.goal ordinary)));
                  assert (
                    A.Goal_accounting.equal
                      (Goal.accounting held)
                      (Goal.accounting ordinary));
                  assert (
                    not
                      (List.exists
                         (Resume.operation_work (Resume.run ordinary admission))
                         ~f:(fun work ->
                           match work.P.Run_work.key with
                           | Operation id -> P.Id.Operation.equal id ordinary_operation
                           | Retained _ -> false)));
                  phase := "join retained native observations";
                  let stable = Resume.settled_snapshot entry in
                  assert (
                    not
                      (Factory.drain_idle_moderator (Daemon.factory daemon) entry
                       |> protocol_ok));
                  Existing.assert_same stable entry;
                  [%test_eq: int] 3 !(counters.providers);
                  (* Retire resources while genuinely Held; enabling its exact
                   passive Wait then leaves only the real scheduler as consumer. *)
                  phase := "unload Held runtime";
                  Owner.unload_and_wait entry.runtime |> protocol_ok;
                  assert (not (Owner.is_loaded entry.runtime));
                  let cold = checkpoint entry in
                  assert (P.Run_wake.equal wake (waiting cold admission));
                  let actor = Staging.guarded Staging.resume_principal (ref true) in
                  let request =
                    Staging.exact_request cold ~key:"545-subscription-enable-waiting"
                  in
                  phase := "explicit passive Resume";
                  (match
                     Service.resume_episode
                       (Resume.public daemon)
                       ~entry
                       ~actor
                       ~session
                       ~request
                     |> protocol_ok
                   with
                   | Resumed { continuation = Waiting actual; _ } ->
                     assert (P.Run_wake.equal wake actual)
                   | Resumed { continuation = Started _ | Settled _ | Controlled _; _ }
                   | Retained _ ->
                     failwith "passive public Resume started or reconstructed work"
                   | Resume_failed { error; _ } | Rejected error | Uncertain error ->
                     raise_s [%sexp "passive Resume failed", (error : P.Error.t)]);
                  phase := "await actual cold subscription provider4";
                  await_signal entry entered;
                  let first_effect = Option.value_exn !provider_state in
                  let operation = Option.value_exn first_effect.active_operation in
                  [%test_eq: P.Operation.kind] (Turn Idle_followup) operation.kind;
                  assert_operation_owned first_effect admission operation;
                  let subscription =
                    Resume.one_actual
                      "actual correlated subscription"
                      first_effect.subscriptions
                  in
                  assert_succeeded_proof
                    first_effect
                    admission
                    (Retained (Subscription subscription.context.id));
                  assert_published_proof first_effect admission "545-subscription";
                  assert_published_proof first_effect admission "545-ordinary";
                  assert (
                    not
                      (List.exists
                         (Resume.run first_effect admission).terminal_work
                         ~f:(fun proof ->
                           match proof.P.Run_work.Terminal.work.key with
                           | Operation id -> P.Id.Operation.equal id operation.id
                           | Retained _ -> false)));
                  phase := "join actual cold source while provider4 blocked";
                  let rec await_source_join () =
                    observe (state entry);
                    if not (Owner.source_lane_idle entry.runtime)
                    then (
                      Eio.Fiber.yield ();
                      await_source_join ())
                  in
                  await_source_join ();
                  assert (Owner.source_lane_idle entry.runtime);
                  assert (Owner.is_loaded entry.runtime);
                  let finish =
                    P.Goal_achievement.Intent.create
                      ~expected_goal_revision:(G.revision (Goal.goal first_effect))
                      ~evidence:
                        (G.Reason.of_string
                           "Correlated subscription publication and actual owning turn"
                         |> protocol_ok)
                      ~action:(Finish { terminal = Completed None; relinquish = [] })
                    |> protocol_ok
                  in
                  Eio.Path.save
                    ~create:(`Or_truncate 0o600)
                    Eio.Path.(Eio.Stdenv.fs env / root / context_file)
                    (Jsonaf.to_string
                       (`Object
                           [ "run_id", P.Id.Run.to_json actual_run.id
                           ; "source", P.Run_source.to_json actual_run.source
                           ; "finish", P.Goal_achievement.Intent.to_json finish
                           ])
                     ^ "\n");
                  Gate.release resumed;
                  phase := "await actual subscription final turn";
                  let final =
                    Resume.await_turns
                      ~providers:4
                      ~observe
                      env
                      entry
                      counters
                      ~completed:3
                  in
                  Resume.assert_episode cold final admission;
                  let subscription =
                    Resume.one_actual "actual subscription" final.subscriptions
                  in
                  (match subscription.result with
                   | Some (Succeeded (`String "545 ready")) -> ()
                   | None | Some _ ->
                     failwith "subscription lost its actual first terminal winner");
                  assert (Option.is_some subscription.completed_at);
                  assert_succeeded_proof
                    final
                    admission
                    (Retained (Subscription subscription.context.id));
                  assert_published_proof final admission "545-subscription";
                  assert_published_proof final admission "545-ordinary";
                  assert_succeeded_proof final admission (Operation operation.id);
                  let final_run = Resume.run final admission in
                  List.iter
                    (Resume.run first_effect admission).terminal_work
                    ~f:(fun proof ->
                      assert (
                        List.mem
                          final_run.terminal_work
                          proof
                          ~equal:P.Run_work.Terminal.equal));
                  [%test_eq: int64]
                    (Int64.succ
                       (A.Goal_accounting.admitted_attempts (Goal.accounting cold)))
                    (A.Goal_accounting.admitted_attempts (Goal.accounting first_effect));
                  [%test_eq: int64]
                    (A.Goal_accounting.admitted_attempts (Goal.accounting first_effect))
                    (A.Goal_accounting.admitted_attempts (Goal.accounting final));
                  (match final_run.lifecycle, G.state (Goal.goal final) with
                   | Terminal (Completed None), Closed (Achieved achievement) ->
                     let request =
                       G.achievement_request (Goal.goal final) |> Option.value_exn
                     in
                     assert (
                       P.Goal_achievement.Request.equal
                         request
                         (P.Goal_achievement.request achievement));
                     let terminal =
                       Resume.one_actual
                         "same Run terminal receipt"
                         (List.filter
                            (A.Run_state.receipts (Option.value_exn final.run_state))
                            ~f:(fun receipt ->
                              P.Id.Run.equal receipt.P.Run_receipt.run_id final_run.id
                              && P.Run_receipt.Kind.equal receipt.kind Terminal))
                     in
                     assert (
                       P.Run_receipt.equal
                         terminal
                         (P.Goal_achievement.receipt achievement));
                     assert (
                       P.Run_receipt.equal
                         terminal
                         (P.Goal_episode.Settlement.receipt
                            (G.request_settlement (Goal.goal final) |> Option.value_exn)))
                   | _ ->
                     raise_s
                       [%sexp
                         "subscription Finish did not settle its actual Run"
                       , (final_run : P.Run.t)
                       , (G.state (Goal.goal final) : G.State.t)]);
                  A.Goal_run_evidence.validate
                    ~catalog:final.goal_catalog
                    ~runs:final.run_state
                  |> protocol_ok;
                  assert (
                    P.Id.Operation.equal
                      ordinary_operation
                      (accepted final "545-ordinary"));
                  assert (
                    P.Id.Operation.equal operation.id (accepted final "545-subscription"));
                  [%test_eq: int64]
                    (Int64.succ (A.Goal_accounting.admitted_turns (Goal.accounting cold)))
                    (A.Goal_accounting.admitted_turns (Goal.accounting final));
                  [%test_eq: int] 2 (List.length (Manual.startup final));
                  ignore
                    (Resume.completed_resume final admission : P.Moderator_execution.t);
                  phase := "compare actual replacement runtime";
                  Owner.with_loaded_runtime entry.runtime (fun runtime ->
                    assert (not (phys_equal old_runtime runtime));
                    Ok ())
                  |> protocol_ok;
                  phase := "join retained native observations";
                  let stable = Resume.settled_snapshot entry in
                  phase := "duplicate actual subscription delivery";
                  (match
                     Factory.deliver_goal_wake (Daemon.factory daemon) entry ~wake
                     |> protocol_ok
                   with
                   | No_work -> ()
                   | Suspended_goal _ | Handed_off _ | Controlled _ ->
                     failwith "subscription replayed its accepted wake");
                  Existing.assert_same stable entry;
                  [%test_eq: int] 4 !(counters.providers);
                  print_endline
                    "mixed notice stays isolated while Held; cold subscription \
                     publishes, owns its real turn and achieves the same Goal without \
                     replay")))
       with
       | Eio.Time.Timeout as exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         Exn.protect
           ~finally:(fun () -> Exn.raise_with_original_backtrace exn backtrace)
           ~f:(fun () ->
             print_s
               [%sexp
                 "subscription fixture timeout"
               , (!phase : string)
               , (!(counters.providers) : int)
               , (!cache_hits : int)
               , (Option.is_some (Eio.Promise.peek entered) : bool)];
             Option.iter !observed ~f:(fun current ->
               let callbacks =
                 List.map current.A.Session_state.moderator_executions ~f:(fun receipt ->
                   receipt.P.Moderator_execution.context.phase, receipt.status)
               in
               let goal =
                 Option.bind current.goal_catalog ~f:(fun catalog ->
                   A.Goal_catalog.find catalog Goal.goal_id)
                 |> Option.map ~f:G.state
               in
               print_s
                 [%sexp
                   (current.lifecycle : A.Session_state.Lifecycle.t)
                 , (current.runtime_initialization
                    : A.Session_state.Runtime_initialization.t)
                 , (current.active_operation : P.Operation.t option)
                 , (current.failure : P.Error.t option)
                 , (goal : G.State.t option)
                 , (current.subscriptions : P.Subscription.t list)
                 , (current.deliveries : P.Delivery.t list)
                 , (callbacks
                    : (P.Moderator_execution.phase * P.Moderator_execution.status) list)
                 , (Option.value_map current.run_state ~default:[] ~f:A.Run_state.runs
                    : P.Run.t list)])));
  [%expect
    {| mixed notice stays isolated while Held; cold subscription publishes, owns its real turn and achieves the same Goal without replay |}]
;;
