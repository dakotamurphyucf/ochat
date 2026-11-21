open! Core
open Support
module Cold = Activation_tests
module Host_configuration = Agent_server.Recurrence_execution_configuration
module Planning_configuration = Agent_server.Recurrence_automation_configuration
module Execution = A.Recurrence_execution
module Origin = A.Recurrence_run_origin
module Terminal = A.Recurrence_run_reference

let configured (config : Agent_server.Config.t) session_id =
  let planning =
    Planning_configuration.create
      ~principal_id:(P.Id.Principal.of_string "pri_485_captured_planner" |> protocol_ok)
      ~controllers:[ session_id ]
      ~expires_at:None
      ~zone_directory:None
    |> protocol_ok
  in
  let grant =
    Host_configuration.Controller.create
      ~session_id
      ~existing_targets:[]
      ~captured_children:true
    |> protocol_ok
  in
  let execution =
    Host_configuration.create
      ~principal_id:(P.Id.Principal.of_string "pri_485_captured_executor" |> protocol_ok)
      ~controllers:[ grant ]
      ~expires_at:None
    |> protocol_ok
  in
  { config with
    Agent_server.Config.server =
      { config.server with
        recurrence_automation = Some planning
      ; recurrence_execution = Some execution
      }
  }
;;

let await ?(timeout = 15.) ?(interval = 0.01) env f =
  let clock = Eio.Stdenv.clock env in
  Eio.Time.with_timeout_exn clock timeout (fun () ->
    let rec loop () =
      match f () with
      | Some result -> result
      | None ->
        Eio.Time.sleep clock interval;
        loop ()
    in
    loop ())
;;

let healthy child =
  let current = state child in
  Option.iter current.failure ~f:(fun error -> raise_s [%sexp (error : P.Error.t)]);
  List.iter current.jobs ~f:(fun job ->
    match job.P.Job.status with
    | Failed error -> raise_s [%sexp (error : P.Error.t)]
    | Cancelled | Interrupted _ -> raise_s [%sexp (job : P.Job.t)]
    | Queued | Running | Waiting_permission _ | Waiting_completion _ | Succeeded -> ());
  current
;;

let executions parent =
  Entry.frontier (entry parent) |> protocol_ok |> A.Recurrence_frontier.executions
;;

let actual_execution parent = List.hd_exn (executions parent)

let fixture_config env root child_text =
  let prompt = Filename.concat root "parent.chatmd" in
  let child_prompt = Filename.concat root "captured.chatmd" in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    Eio.Path.(env#fs / prompt)
    {|<developer>Configured captured recurrence controller.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|};
  Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(env#fs / child_prompt) child_text;
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    Eio.Path.(env#fs / root / "activation-recipe.chatmd")
    "<developer>Actual captured scheduler Model.call recipe.</developer>";
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    Eio.Path.(env#fs / root / "activation-native.txt")
    "485 actual allowed captured read";
  let config = T.config root root prompt in
  let configured_prompt = List.hd_exn config.prompts in
  let config =
    { config with
      prompts =
        config.prompts
        @ [ { configured_prompt with id = "cold-activation-target"; path = child_prompt }
          ]
    }
  in
  config, child_prompt
;;

let with_controller_connection daemon f =
  let connection = T.connection daemon principal in
  protect_cleanup
    ~finally:(fun () -> Agent_client.Connection.close connection)
    (fun () ->
       T.initialize connection;
       f connection)
;;

let with_new_controller env sw daemon f =
  with_controller_connection daemon (fun connection ->
    let created, _ = T.create_session ~start_immediately:true connection in
    let parent = loaded daemon created.id in
    start_parent_settled env sw connection parent;
    f parent)
;;

let%expect_test
    "sole daemon driver activates its acknowledged captured Run after controller \
     retention"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = T.temporary_root env in
    protect_cleanup
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / root))
      (fun () ->
         let config, child_prompt = fixture_config env root Cold.cold_prompt in
         let providers = ref 0 in
         let bootstrap_policy =
           T.inference_policy
             ~default_model:"fixture-model"
             ~post_stream:(fun ~sw:_ ~inputs:_ ->
               failwith "captured bootstrap activated inference")
         in
         let parent_id =
           with_daemon
             ~policy:bootstrap_policy
             env
             root
             config
             ~provider_calls:providers
             (fun sw daemon ->
                with_new_controller env sw daemon (fun parent ->
                  let target, _ =
                    Cold.cold_target
                      ~catalog_target:true
                      ~mode:Single_turn
                      env
                      daemon
                      parent
                      ~root_file:child_prompt
                  in
                  with_planner env sw daemon parent (fun () ->
                    ready_nominal parent ~clock:(Eio.Stdenv.clock env) target |> ignore);
                  let frontier =
                    Entry.frontier
                      (Catalog.find
                         (Option.value_exn (state parent).recurrence_catalog)
                         recurrence_id
                       |> Option.value_exn)
                    |> protocol_ok
                  in
                  [%test_eq: int] 1 (List.length (A.Recurrence_frontier.ready frontier));
                  assert (
                    List.is_empty (A.Recurrence_frontier.protected_children frontier));
                  assert (
                    List.is_empty (A.Recurrence_frontier.child_associations frontier));
                  assert (List.is_empty (A.Recurrence_frontier.executions frontier));
                  [%test_eq: int] 0 !providers;
                  (state parent).identity.session_id))
         in
         let requests = ref [] in
         let entered, signal_entered = Eio.Promise.create () in
         let release, signal_release = Eio.Promise.create () in
         let release_provider () =
           if Option.is_none (Eio.Promise.peek release)
           then Eio.Promise.resolve signal_release ()
         in
         let policy =
           T.inference_policy
             ~default_model:"fixture-model"
             ~post_stream:(fun ~sw:_ ~inputs ->
               let transcript =
                 `Array (List.map inputs ~f:Openai.Responses.Item.jsonaf_of_t)
                 |> Jsonaf.to_string
               in
               let labels =
                 List.filter [ "520_START_FIRST"; "520_START_SECOND" ] ~f:(fun label ->
                   String.is_substring transcript ~substring:label)
               in
               requests := labels :: !requests;
               Int.incr providers;
               if Int.equal !providers 1
               then (
                 Eio.Promise.resolve signal_entered ();
                 Eio.Promise.await release);
               if !providers > 2 then failwith "captured startup replayed provider";
               Cold.completed_response "485 actual captured result")
         in
         let config = configured config parent_id in
         let config =
           { config with
             server =
               { config.server with
                 durability =
                   (* This cut supplies its own physical retention reservation. *)
                   { config.server.durability with
                     snapshot_every_ms = Int.max_value
                   ; snapshot_every_events = Int.max_value
                   }
               }
           }
         in
         let daemon_env, cut =
           Held_controller_retention_fixture.sync_boundary env ~controller:parent_id
         in
         with_daemon
           ~policy
           daemon_env
           root
           config
           ~provider_calls:providers
           (fun _sw daemon ->
              let finally () =
                Held_controller_retention_fixture.release_writer cut;
                release_provider ()
              in
              protect_cleanup ~finally (fun () ->
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                  let parent = loaded daemon parent_id in
                  let synced =
                    Held_controller_retention_fixture.prove
                      cut
                      ~clock:(Eio.Stdenv.mono_clock env)
                      ~daemon
                      ~parent
                      ~provider_count:(fun () -> !providers)
                  in
                  Eio.Promise.await entered;
                  let execution = actual_execution parent in
                  let attempt = Execution.current_attempt execution in
                  let origin = Execution.Attempt.origin attempt |> Option.value_exn in
                  assert (Origin.equal synced origin);
                  Origin.validate
                    origin
                    ~intent:(Execution.intent execution)
                    ~attempt:(Execution.Attempt.request attempt)
                  |> protocol_ok;
                  let admission = Origin.admission origin in
                  let child =
                    loaded daemon (P.Session_ref.session_id (Origin.session origin))
                  in
                  let current = healthy child in
                  let index = Option.value_exn current.run_state in
                  let run = A.Run_state.find index admission.run_id |> Option.value_exn in
                  assert (P.Run.Mode.equal run.mode Single_turn);
                  assert (
                    P.Id.Principal.equal
                      run.principal_id
                      (P.Id.Principal.of_string "pri_485_captured_executor" |> protocol_ok));
                  Origin.validate_run origin ~run ~admission |> protocol_ok;
                  assert (
                    Origin.equal
                      origin
                      (A.Run_state.recurrence_origin index run.id |> Option.value_exn));
                  assert (Option.is_none (Execution.Attempt.terminal attempt));
                  let association =
                    A.Recurrence_execution_intent.child (Execution.intent execution)
                    |> Option.value_exn
                  in
                  let reference = A.Recurrence_child_association.reference association in
                  assert (
                    Option.exists
                      current.spec.delegation
                      ~f:(Agent_store.Delegation_store.Reference.equal reference));
                  let record =
                    Agent_store.Delegation_store.resolve
                      (S.delegations (Daemon.store daemon))
                      reference
                    |> store_ok
                  in
                  assert (Agent_store.Delegation_store.equal_stage record.stage Linked);
                  [%test_eq: int] 1 !providers;
                  release_provider ();
                  let final =
                    await env (fun () ->
                      healthy child |> ignore;
                      let execution = actual_execution parent in
                      match
                        Execution.current_attempt execution |> Execution.Attempt.terminal
                      with
                      | None -> None
                      | Some reference -> Some (execution, reference))
                  in
                  let execution, terminal = final in
                  assert (not (Execution.is_pending execution));
                  assert (P.Id.Run.equal admission.run_id (Terminal.run_id terminal));
                  assert (Terminal.Outcome.equal (Terminal.outcome terminal) Completed);
                  let finished = healthy child in
                  let actual_run =
                    A.Run_state.find
                      (Option.value_exn finished.run_state)
                      admission.run_id
                    |> Option.value_exn
                  in
                  Terminal.validate
                    terminal
                    ~origin
                    ~run:actual_run
                    ~receipt:(Terminal.receipt terminal)
                  |> protocol_ok;
                  [%test_eq: int] 1 (List.length (Execution.attempts execution));
                  [%test_eq: int] 2 !providers;
                  [%test_eq: string list list]
                    [ [ "520_START_FIRST" ]; [ "520_START_SECOND" ] ]
                    (List.rev !requests);
                  assert (
                    Agent_server.Session_registry.execution_selected
                      (Daemon.registry daemon)
                      child);
                  let starts =
                    List.filter finished.moderator_executions ~f:(fun receipt ->
                      P.Moderator_execution.equal_phase
                        receipt.context.phase
                        Session_start)
                  in
                  [%test_eq: int] 1 (List.length starts);
                  match (List.hd_exn starts).status with
                  | Completed _ -> ()
                  | _ -> failwith "captured startup did not complete")))));
  print_endline
    "sole timer owns fresh reservation and real linked child; reciprocal Run exists \
     before first Model; retention joins before its only startup, and Single_turn \
     settles once";
  [%expect
    {| sole timer owns fresh reservation and real linked child; reciprocal Run exists before first Model; retention joins before its only startup, and Single_turn settles once |}]
;;

module Goal = P.Goal
module Goal_catalog = A.Goal_catalog
module Totals = A.Goal_policy_totals
module Ledger = A.Inference_ledger
module Record = Inference.Observation.Attempt_record
module Descendants = A.Goal_descendants
module Pending = A.Goal_descendant_pending
module Occurrence = A.Goal_descendant_occurrence
module Delegations = Agent_store.Delegation_store
module Scope = Agent_store.Delegation_goal_origin.Scope
module Binding = A.Recurrence_definition.Goal_binding

let goal_id = P.Id.Goal.of_string "goal_555_captured_owner" |> protocol_ok
let first_marker = "555_CAPTURED_LAST_ATTEMPT"

let bound_prompt =
  Cold.cold_start_prompt ~first_input:first_marker ~after_read:"Task.pure(state + 1)"
;;

let goal current =
  Goal_catalog.find (Option.value_exn current.A.Session_state.goal_catalog) goal_id
  |> Option.value_exn
;;

let budget cap =
  Goal.Budget.create
    ~admitted_turns:None
    ~admitted_attempts:(Some cap)
    ~observed_tokens:None
    ~deadline:None
  |> protocol_ok
;;

let owner daemon parent =
  P.Session_ref.create
    ~server_id:(S.server_id (Daemon.store daemon))
    ~session_id:(state parent).identity.session_id
;;

let goal_command parent session ?expected_goal_revision ?source key mutation =
  let current = state parent in
  let catalog = Option.value current.goal_catalog ~default:Goal_catalog.empty in
  let request =
    A.Goal_command.create
      ~session_id:current.identity.session_id
      ~generation:current.identity.generation
      ~source
      ~expected_catalog_revision:(Goal_catalog.revision catalog)
      ~expected_goal_revision
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
      ~mutation
    |> protocol_ok
  in
  Actor.goal_command parent.actor ~actor:proof ~authorize ~session ~request |> protocol_ok
;;

let goal_totals parent session =
  A.Session_state.goal_policy_context (state parent) ~owner:session
  |> protocol_ok
  |> Totals.capture ~goal_id
  |> protocol_ok
;;

let assert_attempt_availability parent session expected =
  let actual =
    Totals.available
      (goal_totals parent session)
      ~context:Ordinary_existing
      ~goal:(goal (state parent))
      ~dimension:Attempt
    |> protocol_ok
  in
  [%test_pred: A.Goal_admission_policy.Count_availability.t]
    (A.Goal_admission_policy.Count_availability.equal expected)
    actual
;;

let start_controller_goal env daemon parent session =
  let _, source = A.Goal_catalog_source.capture (state parent) |> protocol_ok in
  goal_command
    parent
    session
    ~source
    "555-create-goal"
    (Create
       { id = goal_id
       ; objective =
           Goal.Objective.of_string "Bound the captured Model Job" |> protocol_ok
       ; budget = budget 2L
       })
  |> ignore;
  let before = state parent in
  let request =
    A.Run_admission_request.Headless.create
      ~session_id:before.identity.session_id
      ~generation:before.identity.generation
      ~expected_revision:before.counters.revision
      ~mode:Workflow
      ~input:
        (User_submission
           { kind = Plain_text; text = "555_CONTROLLER_WORKFLOW"; attachments = [] })
      ~key:(P.Idempotency_key.of_string "555-controller-episode" |> protocol_ok)
    |> protocol_ok
  in
  let request =
    A.Goal_activation_request.create
      ~goal_id
      ~expected_goal_revision:(Goal.revision (goal before))
      ~expected_catalog_revision:
        (Goal_catalog.revision (Option.value_exn before.goal_catalog))
      ~request
    |> protocol_ok
  in
  let module Service = Agent_server.Goal_run_service in
  let service =
    Service.create ~factory:(Daemon.factory daemon) ~authorize:(fun _ -> authorize)
  in
  let admission =
    match
      Service.start_episode service ~entry:parent ~actor:proof ~session ~request
      |> protocol_ok
    with
    | Service.Outcome.Attempted attempt ->
      (match Service.Attempt.result attempt |> protocol_ok with
       | Activated admission -> admission
       | _ -> failwith "the controller did not admit its Goal Workflow")
    | Retained _ -> failwith "the fixture did not start a fresh Workflow"
  in
  await env (fun () ->
    let current = healthy parent in
    let totals = goal_totals parent session in
    if
      Option.is_none current.active_operation
      && Int64.equal (Totals.View.committed totals ~dimension:Attempt) 1L
    then (
      let goal = goal current in
      let episode = Goal.episode goal |> Option.value_exn in
      let run =
        A.Run_state.find (Option.value_exn current.run_state) admission.run_id
        |> Option.value_exn
      in
      assert (Goal.State.equal (Goal.state goal) (Open Enabled));
      assert (P.Run.Mode.equal run.mode Workflow);
      assert (P.Goal_episode.matches_run episode run);
      assert (P.Run_receipt.equal admission (P.Goal_episode.admission episode));
      Some episode)
    else None)
;;

let captured_dispatch_watchdog_ms = 180_000

let create_bound_recurrence parent target =
  let period =
    Recurrence_rules.Rule.Period.create_ms 5_000L
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Recurrence_rules.Timing_error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let schedule =
    A.Recurrence_definition.Schedule.Interval
      { rule = Recurrence_rules.Rule.Interval.create ~mode:Fixed_delay ~period
      ; first_due = P.Timestamp.to_time_ns (P.Timestamp.now ())
      }
  in
  let policy =
    let module Policy = A.Recurrence_policy in
    let default = Policy.default in
    (* Grace starts at the nominal, before the controller records execution.
       Keep the accounting scenario eligible through its existing body watchdog. *)
    Policy.create
      ~misfire:(Policy.misfire default)
      ~grace_ms:(Int64.of_int captured_dispatch_watchdog_ms)
      ~max_pending:(Policy.max_pending default)
      ~max_history:(Policy.max_history default)
      ~max_calendar_steps:(Policy.max_calendar_steps default)
    |> protocol_ok
  in
  let goal_owner =
    A.Recurrence_command.Goal_owner_request.create
      ~goal_id
      ~expected_goal_revision:(Goal.revision (goal (state parent)))
    |> protocol_ok
  in
  command_result
    ~goal_owner
    parent
    "555-create-bound-recurrence"
    (Create (recurrence_id, { schedule; target; policy }))
  |> protocol_ok
  |> ignore;
  command parent "555-enable-bound-recurrence" (Enable recurrence_id) |> ignore
;;

let only name = function
  | [ value ] -> value
  | values -> failwithf "Expected one %s, got %d" name (List.length values) ()
;;

let first_model_job current =
  List.filter current.A.Session_state.jobs ~f:(fun job ->
    P.Job.equal_kind Model_call job.kind
    && Option.equal
         String.equal
         (Jsonaf.member "payload" job.payload
          |> Option.bind ~f:(Jsonaf.member "input")
          |> Option.bind ~f:Jsonaf.string)
         (Some first_marker))
  |> only "captured Model Job"
;;

let child_association execution =
  A.Recurrence_execution_intent.child (Execution.intent execution) |> Option.value_exn
;;

let child_run current origin =
  let index = Option.value_exn current.A.Session_state.run_state in
  let run = A.Run_state.find index (Origin.run_id origin) |> Option.value_exn in
  let retained = A.Run_state.recurrence_origin index run.id |> Option.value_exn in
  assert (Origin.equal retained origin);
  Origin.validate_run origin ~run ~admission:(Origin.admission origin) |> protocol_ok;
  run
;;

let assert_goal_capture parent session episode =
  match A.Recurrence_definition.goal_binding (Entry.current (entry parent)) with
  | Unbound -> failwith "the authorized capture was unbound"
  | Binding.Controller_goal binding ->
    assert (P.Session_ref.equal session (Binding.Capture.owner binding));
    assert (P.Id.Goal.equal goal_id (Binding.Capture.goal_id binding));
    assert (P.Goal_episode.equal episode (Binding.Capture.creator_episode binding))
;;

let assert_inherited_scope daemon session episode (child : A.Session_state.t) reference =
  let record =
    Delegations.resolve (S.delegations (Daemon.store daemon)) reference |> store_ok
  in
  assert (Delegations.equal_stage record.stage Linked);
  let origin = Option.value_exn record.admission.goal_origin in
  let captured = Option.value_exn record.admission.captured_recurrence in
  assert (
    Agent_store.Captured_source_kind.equal
      Artifact
      (Agent_store.Delegation_captured_origin.source_kind captured));
  let inherited = Descendants.origin (Option.value_exn child.goal_descendants) in
  assert (Option.equal Agent_store.Delegation_goal_origin.equal (Some origin) inherited);
  let scope = Agent_store.Delegation_goal_origin.scopes origin |> only "inherited Goal" in
  assert (P.Session_ref.equal session (Scope.owner scope));
  assert (P.Id.Goal.equal goal_id (Scope.goal_id scope));
  assert (P.Goal_episode.equal episode (Scope.creator_episode scope));
  let module Anchor = Agent_store.Delegation_goal_origin.Anchor in
  let branch = Scope.branch scope in
  assert (P.Id.Session.equal child.identity.session_id (Anchor.child_session_id branch));
  assert (
    P.Id.Transaction.equal record.admission.transaction_id (Anchor.transaction_id branch));
  scope
;;

let branch parent reference =
  Descendants.branches (Option.value_exn (state parent).goal_descendants) ~goal_id
  |> List.find_exn ~f:(fun branch ->
    Delegations.Reference.equal reference (Descendants.Branch.reference branch))
;;

let model_ack parent child origin reference scope =
  let current = healthy child in
  let run = child_run current origin in
  let job = first_model_job current in
  assert (P.Run.Mode.equal run.mode Single_turn);
  (match job.status with
   | Running -> ()
   | _ -> raise_s [%sexp "provider preceded the Running Model Job", (job : P.Job.t)]);
  assert (Option.equal P.Inference_origin.equal job.inference_origin (Some (Run run.id)));
  [%test_eq: int] 1 job.attempt;
  let work : P.Run_work.Key.t = Retained (Job { id = job.id; attempt = job.attempt }) in
  assert (List.exists run.owned_work ~f:(fun owned -> P.Run_work.Key.equal owned.key work));
  let row = Ledger.rows current.inference_ledger |> only "canonical captured Attempt" in
  let handle = Ledger.Row.handle row in
  assert (P.Id.Session.equal current.identity.session_id (Ledger.Handle.session_id handle));
  (match Record.state (Ledger.Row.record row) with
   | Running -> ()
   | _ -> failwith "provider preceded the captured canonical Running ACK");
  let included =
    Descendants.local_included (Option.value_exn current.goal_descendants)
    |> only "captured child Included acknowledgement"
  in
  let pending = A.Goal_descendant_included.pending included in
  let occurrence = Pending.occurrence pending in
  assert (Scope.equal scope (Pending.scope pending));
  assert (
    P.Session_ref.equal
      (Origin.session origin)
      (Occurrence.Frame.session (Occurrence.frame occurrence)));
  (match Occurrence.kind occurrence with
   | Attempt { ordinal; operation_id; invocation_id } ->
     [%test_eq: int64] (Ledger.Handle.ordinal handle) ordinal;
     assert (
       Option.equal P.Id.Operation.equal operation_id (Ledger.Handle.operation_id handle));
     assert (
       Option.equal
         P.Id.Invocation.equal
         invocation_id
         (Ledger.Handle.invocation_id handle))
   | Host_turn _ -> failwith "captured Model Job was not charged as an Attempt");
  let branch = branch parent reference in
  assert (Option.is_none (Descendants.Branch.pending branch));
  assert (
    Option.equal
      Descendants.Branch.Last.equal
      (Some (Included included))
      (Descendants.Branch.last branch));
  [%test_eq: int64]
    (Pending.next_prefix pending)
    (Descendants.Branch.confirmed_prefix branch);
  [%test_eq: int64] (Pending.nonce pending) (Descendants.Branch.last_nonce branch);
  job.id, handle
;;

let assert_finished child origin terminal ~job_id ~handle =
  let current = state child in
  Option.iter current.failure ~f:(fun error -> raise_s [%sexp (error : P.Error.t)]);
  let run = child_run current origin in
  Terminal.validate terminal ~origin ~run ~receipt:(Terminal.receipt terminal)
  |> protocol_ok;
  assert (Terminal.Outcome.equal (Terminal.outcome terminal) Completed);
  let job = first_model_job current in
  assert (P.Id.Job.equal job.id job_id);
  (match job.status with
   | Succeeded -> ()
   | _ -> raise_s [%sexp "captured Model Job did not settle", (job : P.Job.t)]);
  let row = Ledger.rows current.inference_ledger |> only "retained captured Attempt" in
  assert (Ledger.Handle.equal handle (Ledger.Row.handle row));
  (match Record.state (Ledger.Row.record row) with
   | Terminal terminal ->
     assert (
       Inference.Event.Terminal.equal_outcome
         (Inference.Event.Terminal.outcome terminal)
         Completed)
   | _ -> failwith "captured provider Attempt did not settle");
  let starts =
    List.filter current.moderator_executions ~f:(fun receipt ->
      P.Moderator_execution.equal_phase receipt.context.phase Session_start)
  in
  match (only "captured startup callback" starts).status with
  | Completed _ -> ()
  | _ -> failwith "captured startup did not complete its scoped read"
;;

let assert_one_physical_attempt parent session =
  let view = goal_totals parent session in
  [%test_eq: int64] 2L (Totals.View.committed view ~dimension:Attempt);
  [%test_eq: int64] 0L (Totals.View.pending view ~dimension:Attempt);
  [%test_eq: int64] 1L (A.Goal_usage.attempts (Totals.View.local_usage view));
  [%test_eq: int64] 1L (A.Goal_usage.attempts (Totals.View.physical_usage view));
  [%test_eq: int64] 0L (A.Goal_usage.attempts (Totals.View.causal_usage view));
  Totals.View.total_usage view
;;

let provider_wait_snapshot daemon parent providers =
  let module Intent = A.Recurrence_execution_intent in
  let registry = Daemon.registry daemon in
  let execution = actual_execution parent in
  let intent = Execution.intent execution in
  let attempt = Execution.current_attempt execution in
  let request = Execution.Attempt.request attempt in
  let origin = Option.map (Execution.Attempt.origin attempt) ~f:Origin.run_id in
  let rejection = Execution.Attempt.rejection attempt in
  let uncertainty = Execution.Attempt.uncertainty attempt in
  let terminal = Option.map (Execution.Attempt.terminal attempt) ~f:Terminal.outcome in
  let controller =
    Intent.occurrence intent |> A.Recurrence_identity.Occurrence.definition
  in
  let controller_id =
    A.Recurrence_identity.Definition.controller controller |> P.Session_ref.session_id
  in
  let controller_reserved = Registry.lifecycle_reserved registry controller_id in
  let child_id = P.Session_ref.session_id (Intent.target intent) in
  let target =
    Option.map (Registry.find registry child_id) ~f:(fun handle ->
      let loaded = Agent_server.Runtime_owner.is_loaded handle.runtime in
      let source_idle = Agent_server.Runtime_owner.source_lane_idle handle.runtime in
      let reserved = Registry.lifecycle_reserved registry child_id in
      let current = state handle in
      let run =
        Option.bind current.run_state ~f:(fun index ->
          List.find_map (A.Run_state.runs index) ~f:(fun candidate ->
            let%bind.Option origin = A.Run_state.recurrence_origin index candidate.id in
            if Result.is_ok (Origin.validate origin ~intent ~attempt:request)
            then Some candidate.lifecycle
            else None))
      in
      let model_jobs =
        List.filter_map current.jobs ~f:(fun job ->
          if P.Job.equal_kind job.kind Model_call
          then
            Some
              (match job.status with
               | Queued -> "queued"
               | Running -> "running"
               | Waiting_permission _ -> "waiting for permission"
               | Waiting_completion _ -> "waiting for completion"
               | Succeeded -> "succeeded"
               | Failed _ -> "failed"
               | Cancelled -> "cancelled"
               | Interrupted _ -> "interrupted")
          else None)
      in
      let inference_attempts = List.length (Ledger.rows current.inference_ledger) in
      [%message
        (loaded : bool)
          (source_idle : bool)
          (reserved : bool)
          (run : P.Run.Lifecycle.t option)
          (model_jobs : string list)
          (inference_attempts : int)])
  in
  [%message
    (providers : int)
      (origin : P.Id.Run.t option)
      (rejection : P.Error.code option)
      (uncertainty : P.Error.code option)
      (terminal : Terminal.Outcome.t option)
      (controller_reserved : bool)
      (target : Sexp.t option)]
;;

let%expect_test "captured occurrence spends its original Goal's final Attempt once" =
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.clock env in
    (* Independent watchdogs for the durable progress of this fixture. *)
    let progress_timeout = 60. in
    let await_progress f = await ~timeout:progress_timeout ~interval:0.25 env f in
    let phase name f =
      let started = Eio.Time.now clock in
      match f () with
      | result -> result
      | exception Eio.Time.Timeout ->
        let elapsed_seconds = Eio.Time.now clock -. started in
        raise_s
          [%message
            "captured Goal recurrence timed out" (name : string) (elapsed_seconds : float)]
    in
    Mirage_crypto_rng_unix.use_default ();
    let root = T.temporary_root env in
    protect_cleanup
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / root))
      (fun () ->
         let config, child_prompt = fixture_config env root bound_prompt in
         let providers = ref 0 in
         let entered, signal_entered = Eio.Promise.create () in
         let release, signal_release = Eio.Promise.create () in
         let release_provider () =
           if Option.is_none (Eio.Promise.peek release)
           then Eio.Promise.resolve signal_release ()
         in
         let policy =
           T.inference_policy
             ~default_model:"fixture-model"
             ~post_stream:(fun ~sw:_ ~inputs ->
               Int.incr providers;
               let transcript =
                 `Array (List.map inputs ~f:Openai.Responses.Item.jsonaf_of_t)
                 |> Jsonaf.to_string
               in
               (match !providers with
                | 1 ->
                  assert (
                    String.is_substring transcript ~substring:"555_CONTROLLER_WORKFLOW")
                | 2 ->
                  assert (String.is_substring transcript ~substring:first_marker);
                  Eio.Promise.resolve signal_entered ();
                  Eio.Promise.await release
                | _ ->
                  failwith "captured recurrence replayed or exceeded its provider cap");
               Cold.completed_response "555 permitted Model result")
         in
         let parent_id =
           with_daemon ~policy env root config ~provider_calls:providers (fun sw daemon ->
             with_new_controller env sw daemon (fun parent ->
               [%test_eq: int] 0 !providers;
               (state parent).identity.session_id))
         in
         let config = configured config parent_id in
         let load daemon id =
           Agent_server.Session_registry.load (Daemon.registry daemon) id |> protocol_ok
         in
         (* The named phase deadlines catch stalls. This outer watchdog also bounds
            the combined body, including daemon calls outside those phases. *)
         let with_captured_dispatch =
           let timeout = Float.of_int captured_dispatch_watchdog_ms /. 1_000. in
           with_daemon ~policy ~timeout env root config ~provider_calls:providers
         in
         let recover =
           with_captured_dispatch (fun sw daemon ->
             protect_cleanup ~finally:release_provider (fun () ->
               let parent = load daemon parent_id in
               with_controller_connection daemon (fun connection ->
                 phase "controller startup" (fun () ->
                   start_parent_settled env sw connection parent);
                 let session = owner daemon parent in
                 let episode =
                   phase "controller Goal Workflow" (fun () ->
                     start_controller_goal env daemon parent session)
                 in
                 phase "capture source and create bound recurrence" (fun () ->
                   Eio.Time.with_timeout_exn clock 15. (fun () ->
                     let target, _ =
                       Cold.cold_target
                         ~catalog_target:true
                         ~mode:Single_turn
                         env
                         daemon
                         parent
                         ~root_file:child_prompt
                     in
                     assert_attempt_availability parent session Available;
                     create_bound_recurrence parent target;
                     assert_goal_capture parent session episode));
                 phase "record first controller execution" (fun () ->
                   (* Recording the execution need not publish a durable event. *)
                   await_progress (fun () ->
                     match executions parent with
                     | [] -> None
                     | [ _ ] -> Some ()
                     | _ ->
                       failwith "captured Goal recurrence recorded multiple executions"));
                 phase "first captured Model provider" (fun () ->
                   match
                     Eio.Time.with_timeout_exn clock progress_timeout (fun () ->
                       Eio.Promise.await entered)
                   with
                   | () -> ()
                   | exception Eio.Time.Timeout ->
                     let snapshot =
                       Eio.Time.with_timeout clock 2. (fun () ->
                         Ok (provider_wait_snapshot daemon parent !providers))
                     in
                     (match snapshot with
                      | Ok snapshot ->
                        raise_s
                          [%message
                            "captured Model provider timed out" (snapshot : Sexp.t)]
                      | Error `Timeout ->
                        failwith
                          "captured Model provider timed out and Actor observation did \
                           not return"));
                 let execution = actual_execution parent in
                 let attempt = Execution.current_attempt execution in
                 let origin = Execution.Attempt.origin attempt |> Option.value_exn in
                 assert (Option.is_none (Execution.Attempt.terminal attempt));
                 let child_id = P.Session_ref.session_id (Origin.session origin) in
                 let child = loaded daemon child_id in
                 let reference =
                   A.Recurrence_child_association.reference (child_association execution)
                 in
                 let scope =
                   assert_inherited_scope daemon session episode (state child) reference
                 in
                 let job_id, handle = model_ack parent child origin reference scope in
                 let totals = goal_totals parent session in
                 [%test_eq: int64] 2L (Totals.View.committed totals ~dimension:Attempt);
                 [%test_eq: int64] 0L (Totals.View.pending totals ~dimension:Attempt);
                 release_provider ();
                 let execution, terminal =
                   phase "captured terminal import" (fun () ->
                     await_progress (fun () ->
                       let execution = actual_execution parent in
                       Execution.Attempt.terminal (Execution.current_attempt execution)
                       |> Option.map ~f:(fun terminal -> execution, terminal)))
                 in
                 assert_finished child origin terminal ~job_id ~handle;
                 let usage = assert_one_physical_attempt parent session in
                 [%test_eq: int] 2 !providers;
                 assert_attempt_availability parent session Exhausted;
                 (* Leave available count capacity, so the next real due nominal
                       is denied independently by the original Goal's Hold. *)
                 let change ?source key mutation =
                   goal_command
                     ?source
                     parent
                     session
                     ~expected_goal_revision:(Goal.revision (goal (state parent)))
                     key
                     mutation
                   |> ignore
                 in
                 change "555-operator-hold" (Hold (goal_id, Operator_pause));
                 let _, source =
                   A.Goal_catalog_source.capture (state parent) |> protocol_ok
                 in
                 change
                   ~source
                   "555-available-capacity"
                   (Change_budget (goal_id, budget 3L));
                 assert (
                   Goal.State.equal
                     (Goal.state (goal (state parent)))
                     (Open (Held Operator_pause)));
                 assert_attempt_availability parent session Available;
                 let next =
                   phase "next nominal under held Goal" (fun () ->
                     await_ready_nominal parent ~clock)
                 in
                 let occurrence = A.Recurrence_nominal.key next in
                 assert (
                   not
                     (A.Recurrence_identity.Occurrence.equal
                        occurrence
                        (Origin.occurrence origin)));
                 (match
                    Actor.reserve_recurrence_child
                      parent.actor
                      ~actor:proof
                      ~authorize:authorize_creation
                      ~occurrence
                      ~expected_control_revision:(Entry.control_revision (entry parent))
                  with
                  | Error error when P.Error.equal_code error.code Permission_denied -> ()
                  | Error error -> raise_s [%sexp (error : P.Error.t)]
                  | Ok _ -> failwith "Held Goal admitted the next due captured child");
                 let check_retained daemon parent child =
                   assert_goal_capture parent session episode;
                   let current = actual_execution parent in
                   assert (Execution.equal execution current);
                   assert (not (Execution.is_pending current));
                   [%test_eq: int] 1 (List.length (Execution.attempts current));
                   let frontier = Entry.frontier (entry parent) |> protocol_ok in
                   [%test_eq: int] 1 (List.length (Frontier.executions frontier));
                   assert (
                     List.for_all (Frontier.protected_children frontier) ~f:(fun row ->
                       not
                         (A.Recurrence_identity.Occurrence.equal
                            occurrence
                            (A.Recurrence_child_reservation.occurrence row))));
                   assert (
                     A.Recurrence_child_association.equal
                       (child_association current)
                       (only
                          "captured association"
                          (Frontier.child_associations frontier)));
                   assert (
                     Scope.equal
                       scope
                       (assert_inherited_scope
                          daemon
                          session
                          episode
                          (state child)
                          reference));
                   assert_finished child origin terminal ~job_id ~handle;
                   assert (
                     A.Goal_usage.equal usage (assert_one_physical_attempt parent session));
                   [%test_eq: int] 2 !providers
                 in
                 check_retained daemon parent child;
                 fun daemon ->
                   check_retained daemon (load daemon parent_id) (load daemon child_id))))
         in
         with_daemon ~policy env root config ~provider_calls:providers (fun _sw daemon ->
           recover daemon)));
  print_endline
    "original Goal owns the captured Attempt before provider; one final slot survives \
     import, Hold and cold recovery";
  [%expect
    {| original Goal owns the captured Attempt before provider; one final slot survives import, Hold and cold recovery |}]
;;
