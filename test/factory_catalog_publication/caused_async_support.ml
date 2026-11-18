open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module G = P.Goal
module Birth = Descendant_goal_birth_tests
module Direct = Descendant_turn_adapter_tests
module Model = Delegated_policy_model_tests
module Failure = Delegated_policy_caller_support.Failure
module PC = A.Policy_canonical_usage
module Cause = A.Policy_cause_data
module Book = A.Goal_policy_contributions
module Totals = A.Goal_policy_totals
module Ledger = A.Inference_ledger
module Graph = Agent_server.Graph_tracking
module Manual = Goal_job_runtime_tests

type scenario =
  | Invocation_model
  | Direct_model_retry
  | Model_finish_cleanup
  | Initial_first_drain

let attempt_delta = function
  | Invocation_model -> 3L
  | Direct_model_retry -> 4L
  | Model_finish_cleanup -> 2L
  | Initial_first_drain -> 4L
;;

let marker = "597_CAUSED_DIRECT_ASYNC_CHILD"
let original_file = "597-caused-async-original.txt"

let activate scenario daemon child =
  let session, _ = Goal.create_goal daemon child in
  let before = state child in
  let ceiling = Int64.(Model.committed before + attempt_delta scenario) in
  let budget =
    G.Budget.create
      ~admitted_turns:None
      ~admitted_attempts:(Some ceiling)
      ~observed_tokens:None
      ~deadline:None
    |> protocol_ok
  in
  Goal_admitted_guard_tests.command
    daemon
    child
    ~key:"597-caused-async-cap"
    (Change_budget (Goal.goal_id, budget))
  |> protocol_ok
  |> ignore;
  let before = state child in
  let request =
    A.Run_admission_request.Headless.create
      ~session_id:before.identity.session_id
      ~generation:before.identity.generation
      ~expected_revision:before.counters.revision
      ~mode:Workflow
      ~input:(User_submission { kind = Plain_text; text = marker; attachments = [] })
      ~key:(P.Idempotency_key.of_string "597-caused-async-episode" |> protocol_ok)
    |> protocol_ok
    |> fun request ->
    A.Goal_activation_request.create
      ~goal_id:Goal.goal_id
      ~expected_goal_revision:(G.revision (Goal.goal before))
      ~expected_catalog_revision:
        (A.Goal_catalog.revision (Option.value_exn before.goal_catalog))
      ~request
    |> protocol_ok
  in
  let admission =
    Agent_server.Goal_run_service.create
      ~factory:(Daemon.factory daemon)
      ~authorize:(fun _ -> authorize)
    |> fun service ->
    Agent_server.Goal_run_service.start_episode
      service
      ~entry:child
      ~actor:original
      ~session
      ~request
    |> protocol_ok
    |> Goal.activated
  in
  before, ceiling, admission
;;

type observation =
  { handle : Ledger.Handle.t
  ; cause : Cause.t
  ; native : bool
  ; occurrence : A.Goal_descendant_occurrence.t
  ; stream : PC.Stream.t
  ; prefix : int64
  }

type probe =
  { parent : Registry.entry
  ; child : Registry.entry
  ; before_parent : A.Session_state.t
  ; before_child : A.Session_state.t
  ; job : P.Job.t
  ; admission : P.Run_receipt.t
  ; reference : Agent_store.Delegation_store.Reference.t
  ; scope : Agent_store.Delegation_goal_origin.Scope.t
  ; ceiling : int64
  ; scenario : scenario
  ; mutable observed : observation list
  }

let source_cause current member =
  let _, source = A.Moderator_source_installation.capture current |> protocol_ok in
  let prefix =
    PC.Source.Native_prefix.create
      ~session_id:current.A.Session_state.identity.session_id
      ~generation:current.identity.generation
      ~current_source:(Some source)
      ~events:current.moderator_executions
      ~invocations:current.invocations
      ~jobs:current.jobs
      ~runs:current.run_state
    |> protocol_ok
  in
  let cause =
    PC.Source.cause current.policy_canonical_usage ~prefix ~member
    |> protocol_ok
    |> Option.value_exn
  in
  assert (P.Run_source.equal source (Cause.callee_source cause));
  cause
;;

let assert_completed row =
  match Inference.Observation.Attempt_record.state (Ledger.Row.record row) with
  | Terminal terminal ->
    (match Inference.Event.Terminal.outcome terminal with
     | Completed -> ()
     | Refused | Incomplete _ | Failed _ -> failwith "actual producer H did not complete")
  | Prepared | Running | Interrupted _ -> failwith "actual producer H did not settle"
;;

let row current handle =
  List.find_exn (Ledger.rows current.A.Session_state.inference_ledger) ~f:(fun row ->
    Ledger.Handle.equal handle (Ledger.Row.handle row))
;;

let observe_attempt (t : probe) (expected_job : P.Job.t) =
  let prior = t.observed in
  let total_delta = Int64.of_int (List.length prior + 2) in
  let parent = state t.parent
  and child = state t.child in
  assert (Option.is_some child.active_operation);
  let rows = Model.new_rows t.before_parent parent in
  [%test_eq: int] (List.length prior + 1) (List.length rows);
  let row =
    List.filter rows ~f:(fun row ->
      not
        (List.exists prior ~f:(fun prior ->
           Ledger.Handle.equal prior.handle (Ledger.Row.handle row))))
    |> Model.only "current producer H"
  in
  let handle = Ledger.Row.handle row in
  assert (P.Id.Session.equal parent.identity.session_id (Ledger.Handle.session_id handle));
  assert (Option.is_none (Ledger.Handle.operation_id handle));
  assert (Option.is_none (Ledger.Handle.invocation_id handle));
  let child_row = Model.new_rows t.before_child child |> Model.only "native child H" in
  assert (not (Ledger.Handle.equal handle (Ledger.Row.handle child_row)));
  (match Inference.Observation.Attempt_record.state (Ledger.Row.record row) with
   | Running -> ()
   | Prepared | Terminal _ | Interrupted _ -> failwith "producer provider preceded H ACK");
  let job = List.find_exn parent.jobs ~f:(fun job -> P.Id.Job.equal job.id t.job.id) in
  [%test_eq: P.Job.kind] Async_tool job.kind;
  [%test_eq: int] t.job.attempt job.attempt;
  (match job.status with
   | Running -> ()
   | Queued
   | Waiting_permission _
   | Waiting_completion _
   | Succeeded
   | Failed _
   | Cancelled
   | Interrupted _ -> failwith "direct Async provider lost its claimed Job");
  let accounting = Option.value_exn parent.policy_canonical_usage in
  let binding =
    PC.bindings accounting
    |> List.filter ~f:(fun binding ->
      Int64.equal (PC.Binding.ordinal binding) (Ledger.Handle.ordinal handle))
    |> Model.only "exact Async canonical binding"
  in
  let cause = Model.cause_for accounting binding in
  List.iter prior ~f:(fun prior -> assert (Cause.equal prior.cause cause));
  let allocated =
    List.find_exn parent.jobs ~f:(fun job -> P.Id.Job.equal expected_job.id job.id)
  in
  [%test_eq: P.Job.kind] expected_job.kind allocated.kind;
  [%test_eq: int] expected_job.attempt allocated.attempt;
  (match allocated.status with
   | Running -> ()
   | Queued
   | Waiting_permission _
   | Waiting_completion _
   | Succeeded
   | Failed _
   | Cancelled
   | Interrupted _ -> failwith "provider lost its actual Job");
  let member = PC.Source.Native_prefix.member_of_job allocated |> protocol_ok in
  assert (Cause.equal cause (source_cause parent member));
  assert (P.Session_ref.equal (Cause.caller cause) (G.session (Goal.goal child)));
  assert (P.Session_ref.equal (Cause.callee cause) (G.session (Goal.goal parent)));
  assert (
    Option.equal
      P.Inference_origin.equal
      (Cause.caller_origin cause)
      (Some (Run t.admission.run_id)));
  assert (
    List.equal
      Agent_store.Delegation_store.Reference.equal
      (Cause.path cause)
      [ t.reference ]);
  let receipt =
    List.find_exn parent.moderator_executions ~f:(fun receipt ->
      P.Id.Moderator_execution.equal receipt.context.id (Cause.parent_event cause))
  in
  assert (P.Moderator_execution.equal_phase Pre_tool_call receipt.context.phase);
  assert (Cause.matches_event cause receipt);
  let launch = Option.value_exn job.launch in
  (match P.Job.Birth_parent.view launch.owner with
   | Callback (Moderator_event id) ->
     assert (P.Id.Moderator_execution.equal id receipt.context.id)
   | Callback (Invocation _) | Job_attempt _ -> failwith "Async birth changed its Event");
  let targets = Cause.targets cause in
  [%test_eq: int] 2 (List.length targets);
  let target owner =
    List.filter targets ~f:(fun target ->
      P.Session_ref.equal owner (Cause.Goal_target.owner target))
    |> Model.only "governing Async Goal target"
  in
  let inherited = target (G.session (Goal.goal parent)) in
  (match Cause.Goal_target.view inherited with
   | Inherited scope ->
     assert (Agent_store.Delegation_goal_origin.Scope.equal scope t.scope)
   | Local _ -> failwith "inherited Goal became ambient local DATA");
  let local = target (G.session (Goal.goal child)) in
  (match Cause.Goal_target.view local with
   | Local { goal_id; episode } ->
     assert (P.Id.Goal.equal Goal.goal_id goal_id);
     assert (P.Run_receipt.equal t.admission (P.Goal_episode.admission episode))
   | Inherited _ -> failwith "actual child Goal became an inherited Scope");
  let choices = PC.Binding.choices binding in
  [%test_eq: int] 2 (List.length choices);
  let coverage target =
    List.find_exn choices ~f:(fun choice ->
      Cause.Goal_target.equal target (PC.Choice.target choice))
    |> PC.Choice.coverage
  in
  assert (Cause.Coverage.equal Causal_overlay (coverage local));
  let native =
    List.exists
      (A.Goal_accounting.bindings (Goal.accounting parent))
      ~f:(fun binding ->
        Ledger.Handle.equal handle (A.Goal_accounting.Binding.handle binding))
  in
  (match coverage inherited with
   | Covered_local -> assert native
   | Causal_overlay -> assert (not native)
   | Covered_physical -> failwith "Async H covered itself as a physical child");
  let imported = Model.pending_projection child in
  let head = PC.Projection.head imported |> Option.value_exn in
  (match allocated.kind, PC.Head.work head with
   | Async_tool, Async_job_attempt { id; attempt }
   | Model_call, Model_job_attempt { id; attempt } ->
     assert (P.Id.Job.equal allocated.id id);
     [%test_eq: int] allocated.attempt attempt
   | _, _ -> failwith "producer H was relabelled");
  let occurrence =
    A.Goal_descendants.Canonical.Fact.occurrence (PC.Head.canonical head)
  in
  (match A.Goal_descendant_occurrence.kind occurrence with
   | Attempt { ordinal; _ } -> [%test_eq: int64] (Ledger.Handle.ordinal handle) ordinal
   | Host_turn _ -> failwith "Async import lost actual Attempt H");
  assert (
    P.Session_ref.equal
      (A.Goal_descendant_occurrence.Frame.session
         (A.Goal_descendant_occurrence.frame occurrence))
      (G.session (Goal.goal parent)));
  [%test_eq: int64]
    (Int64.of_int (List.length prior + 1))
    (A.Goal_usage.attempts (Book.Subtotal.usage (Model.subtotal child)));
  assert (List.is_empty (Book.Subtotal.pending (Model.subtotal child)));
  [%test_eq: int64]
    Int64.(Model.committed t.before_child + total_delta)
    (Model.committed child);
  [%test_eq: int64]
    Int64.(Model.committed t.before_parent + total_delta)
    (Model.committed parent);
  let root = Model.total parent in
  [%test_eq: int64] 1L (A.Goal_usage.attempts (Totals.View.physical_usage root));
  [%test_eq: int64]
    Int64.(
      (if native then 0L else 1L)
      + List.fold prior ~init:0L ~f:(fun count prior ->
        count + if prior.native then 0L else 1L))
    (A.Goal_usage.attempts (Totals.View.causal_usage root));
  assert (not (Totals.View.physical_unknown root));
  assert (not (Totals.View.historical_causal_unknown root));
  [%test_eq: int64]
    (Int64.of_int (List.length prior + 1))
    (PC.Projection.admitted_attempts imported);
  let observation =
    { handle
    ; cause
    ; native
    ; occurrence
    ; stream = PC.Projection.stream imported
    ; prefix = PC.Projection.admitted_attempts imported
    }
  in
  t.observed <- prior @ [ observation ];
  observation
;;

type fixture =
  { probe : probe
  ; failure : Failure.t
  ; direct : bool ref
  ; direct_providers : int ref
  ; child_providers : int ref
  ; pause : unit -> unit
  ; resume : unit -> unit
  ; clock_now : unit -> float
  ; release_child : unit -> unit
  }

let parked daemon clock_now =
  match Daemon.For_testing.job_scheduler_wait_deadline daemon with
  | Some deadline -> Float.(deadline > clock_now ())
  | None -> false
;;

let with_claimed_async
      ?(wrap_env = Fn.id)
      ?(at_direct = fun current -> observe_attempt current current.job |> ignore)
      ~scenario
      ~parent_prompt_text
      ~sources
      ~model_marker
      ~at_model
      f
  =
  let failure = Failure.create () in
  let handoff, handoff_u = Eio.Promise.create () in
  let entered, entered_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let resolve promise resolver =
    if not (Eio.Promise.is_resolved promise) then Eio.Promise.resolve resolver ()
  in
  let pause = ref ignore
  and resume = ref ignore in
  let clock_now = ref (fun () -> failwith "controlled polling clock absent") in
  let gate_armed = ref false in
  let direct = ref false in
  let probe = ref None in
  let direct_providers = ref 0
  and child_providers = ref 0 in
  let provider inputs =
    if !direct
    then
      Failure.guard failure (fun () ->
        Int.incr direct_providers;
        let current = Option.value_exn !probe in
        at_direct current;
        completed_response "actual caused Async direct inference")
    else (
      let text = `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string in
      if String.is_substring text ~substring:model_marker
      then (
        let current = Failure.guard failure (fun () -> Option.value_exn !probe) in
        (* The callback guards its oracles, then performs an intentional ordinary
           failure outside that guard when testing actual scheduler retry. *)
        at_model failure current)
      else
        Failure.guard failure (fun () ->
          if String.is_substring text ~substring:marker
          then (
            Int.incr child_providers;
            Eio.Promise.await handoff;
            Direct.function_response
              "read_file"
              (`Object [ "root", `String "data"; "file", `String original_file ]))
          else (
            assert (Option.is_none !probe);
            completed_response "actual Goal child birth completed")))
  in
  Birth.with_native_goal_child
    ~timeout:90.
    ~parent_prompt_text
    ~inherited_tools:[ "read_file", Descendant_goal_resume_tests.parent_reader ]
    ~sources
    ~child_sources:[ "child.chatmd", Model.child_prompt ]
    ~post_creation_stream:provider
    ~wrap_env:(fun env ->
      let env = wrap_env env in
      let env =
        Goal_background_runtime_tests.with_controlled_polling
          env
          ~pause
          ~resume
          ~clock_now
      in
      Crash_fault_io.wrap
        env
        ~boundary:Before_stat
        ~matches:(fun path ->
          !gate_armed && String.is_suffix path ~suffix:("/" ^ original_file))
        ~reached:(fun _ ->
          gate_armed := false;
          resolve entered entered_u;
          Eio.Promise.await release))
    (fun env sw daemon parent child scope reference _counters ->
       protect_cleanup
         ~finally:(fun () ->
           resolve handoff handoff_u;
           resolve release release_u;
           !resume ())
         (fun () ->
            Failure.restore failure (fun () ->
              !pause ();
              Model.within env "park actual scheduler" (fun () ->
                let rec await () =
                  if not (parked daemon !clock_now)
                  then (
                    Eio.Fiber.yield ();
                    await ())
                in
                await ());
              Owner.ensure_loaded_authenticated child.runtime ~actor:original
              |> protocol_ok;
              Direct.start_child daemon child;
              let before_parent = Model.checkpoint parent in
              let before_child, ceiling, admission = activate scenario daemon child in
              gate_armed := true;
              resolve handoff handoff_u;
              Model.within env "actual delegated read after Async birth ACK" (fun () ->
                Eio.Promise.await entered);
              Failure.raise_if_present failure;
              let current = state parent in
              let queued =
                List.filter current.jobs ~f:(fun job ->
                  P.Job.equal_kind Async_tool job.kind
                  && not
                       (List.exists before_parent.jobs ~f:(fun before ->
                          P.Id.Job.equal before.id job.id)))
                |> Model.only "actual selected caused Async Job"
              in
              let claimed =
                Actor.claim_job
                  parent.actor
                  ~job_id:queued.id
                  ~generation:queued.generation
                |> protocol_ok
                |> Option.value_exn
              in
              let subject =
                { parent
                ; child
                ; before_parent
                ; before_child
                ; job = claimed
                ; admission
                ; reference
                ; scope
                ; ceiling
                ; scenario
                ; observed = []
                }
              in
              probe := Some subject;
              f
                env
                sw
                daemon
                { probe = subject
                ; failure
                ; direct
                ; direct_providers
                ; child_providers
                ; pause = !pause
                ; resume = !resume
                ; clock_now = !clock_now
                ; release_child = (fun () -> resolve release release_u)
                })))
;;

let run_direct sw t =
  let subject = t.probe in
  let runtime =
    Owner.with_loaded_runtime subject.parent.runtime (fun runtime -> Ok runtime)
    |> protocol_ok
  in
  let context, request = Manual.prepared runtime in
  let graph =
    Graph.create
      subject.parent.actor
      ~source:
        (Transcript.Source_id.of_string "caused-direct-async" |> Result.ok_or_failwith)
      ~upstream:
        { new_preparation_id = (fun () -> "caused-direct-async-preparation")
        ; on_admitted = (fun ~scope:_ ~accounting_id:_ -> ())
        ; on_attempt = ignore
        ; on_observation = ignore
        ; on_completion = ignore
        }
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () ->
      Graph.seal graph |> protocol_ok;
      Graph.finish graph |> protocol_ok)
    (fun () ->
       Actor.with_job_execution
         subject.parent.actor
         ~job_id:subject.job.id
         ~generation:subject.job.generation
         ~attempt:subject.job.attempt
         ~deadline:None
         (fun execution ->
            let identity =
              Graph.bind_identity graph ~allocate:execution.allocate_inference
            in
            Exn.protect
              ~finally:(fun () -> t.direct := false)
              ~f:(fun () ->
                t.direct := true;
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
                |> Result.map_error ~f:(fun error ->
                  P.Error.invalid_request
                    (Sexp.to_string_hum (Inference_client.Error.sexp_of_t error)))
                |> Result.map ~f:ignore))
       |> protocol_ok);
  [%test_eq: int] 1 !(t.direct_providers);
  let observed = Model.only "direct Async observation" subject.observed in
  let after = state subject.parent in
  let settled =
    Model.new_rows subject.before_parent after |> Model.only "settled Async H"
  in
  assert (Ledger.Handle.equal observed.handle (Ledger.Row.handle settled));
  assert_completed settled
;;

let execute_async daemon t ~check_completion =
  let subject = t.probe in
  let completion =
    Factory.execute_background_job (Daemon.factory daemon) subject.parent subject.job
    |> protocol_ok
  in
  let completion =
    match completion with
    | Owner.Completed completion ->
      check_completion completion;
      completion
    | Pending _ -> failwith "actual Async worker unexpectedly deferred"
  in
  let terminal =
    Actor.complete_background_job
      subject.parent.actor
      ~job_id:subject.job.id
      ~generation:subject.job.generation
      ~attempt:subject.job.attempt
      completion
    |> protocol_ok
  in
  assert (
    Option.equal
      P.Completion.equal
      (P.Job.terminal_completion terminal |> protocol_ok)
      (Some completion));
  match terminal.status with
  | Succeeded -> ()
  | Queued
  | Running
  | Waiting_permission _
  | Waiting_completion _
  | Failed _
  | Cancelled
  | Interrupted _ -> failwith "actual Async native completion was not truthful"
;;

let finish_child env t =
  let subject = t.probe in
  t.release_child ();
  let final_child =
    Model.within env "actual child read and callbacks settle" (fun () ->
      let rec await () =
        Failure.raise_if_present t.failure;
        let current = Model.checkpoint subject.child in
        if Option.is_none current.active_operation
        then current
        else (
          Eio.Fiber.yield ();
          await ())
      in
      await ())
  in
  [%test_eq: int] 1 !(t.direct_providers);
  [%test_eq: int] 1 !(t.child_providers);
  [%test_eq: int64] subject.ceiling (Model.committed final_child);
  assert (List.is_empty (Book.Subtotal.pending (Model.subtotal final_child)));
  let final_parent = state subject.parent in
  [%test_eq: int64]
    Int64.(Model.committed subject.before_parent + attempt_delta subject.scenario)
    (Model.committed final_parent);
  assert (List.is_empty (Book.Subtotal.pending (Model.subtotal final_parent)));
  let rows = Model.new_rows subject.before_parent final_parent in
  [%test_eq: int]
    (Int64.to_int_exn (attempt_delta subject.scenario) - 1)
    (List.length rows);
  [%test_eq: int] (List.length rows) (List.length subject.observed);
  List.iter subject.observed ~f:(fun observed ->
    row final_parent observed.handle |> ignore);
  final_parent
;;
