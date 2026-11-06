open! Core
open Agent_server_test_support
open Retry_support
open Goal_native_test_support
module Create = P.Goal_native_create_request
module Control = P.Goal_native_control_request
module Catalog = A.Goal_catalog

let key text = P.Idempotency_key.of_string text |> protocol_ok
let catalog current = Option.value_exn current.A.Session_state.goal_catalog

let equal_catalog left right =
  Jsonaf.exactly_equal
    (Catalog.to_jsonaf left |> protocol_ok)
    (Catalog.to_jsonaf right |> protocol_ok)
;;

let reason = G.Reason.of_string "actual selected native control" |> protocol_ok

let create_request current ~goal_id ~key =
  Create.create
    ~generation:current.A.Session_state.identity.generation
    ~source:(G.source (Goal.goal current))
    ~goal_id
    ~expected_catalog_revision:(Catalog.revision (catalog current))
    ~objective:(G.Objective.of_string "Created by selected native callback" |> protocol_ok)
    ~budget:G.Budget.unlimited
    ~key
  |> protocol_ok
;;

let control_request current ~key mutation =
  Control.create
    ~generation:current.A.Session_state.identity.generation
    ~goal_id:Goal.goal_id
    ~expected_catalog_revision:(Catalog.revision (catalog current))
    ~expected_goal_revision:(G.revision (Goal.goal current))
    ~key
    ~mutation
  |> protocol_ok
;;

let stage_control (entry : Registry.entry) request admission =
  Actor.stage_native_goal_control entry.actor ~admission ~authorize ~request
;;

let assert_native_denial expected calls =
  match calls with
  | [ invocation ] ->
    (match invocation.P.Invocation.status with
     | Resolved (Fail failure) ->
       [%test_eq: string] (P.Error.code_to_string expected) failure.code
     | _ ->
       raise_s
         [%sexp
           "native control did not return the expected denial"
         , (invocation.status : P.Invocation.status)])
  | _ -> failwith "denial did not reach exactly one actual native invocation"
;;

let assert_no_new_work before after counters counts =
  [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports));
  assert (A.Goal_accounting.equal (Goal.accounting before) (Goal.accounting after));
  assert (
    Jsonaf.exactly_equal
      (Staging.ledger_json before.inference_ledger)
      (Staging.ledger_json after.inference_ledger));
  [%test_eq: int]
    (List.length (A.Run_state.runs (Option.value_exn before.run_state)))
    (List.length (A.Run_state.runs (Option.value_exn after.run_state)))
;;

let%expect_test
    "selected native legacy/v1 Create/control commit atomically without execution"
  =
  List.iter [ false; true ] ~f:(fun v1 ->
    with_callback
      ~v1
      (fun
          _env
           _daemon
           entry
           runtime
           counters
           _session
           _admission
           _old_live
           _held
           _resume
           operation
           ~release_ordinary
           ~resumed_entered
           ~release_resumed:_
           ~reopen:_
         ->
         let before = state entry in
         let counts = !(counters.providers), !(counters.inference_ports) in
         let created_id = P.Id.Goal.of_string "goal_native_created" |> protocol_ok in
         let request =
           create_request before ~goal_id:created_id ~key:(key "native-created")
         in
         let create admission =
           Actor.stage_native_goal_create entry.actor ~admission ~authorize ~request
         in
         let result, calls =
           callback entry runtime ~operation ~mode:"create" ~run:create
         in
         assert (protocol_ok result);
         [%test_eq: int] 1 (List.length calls);
         let current = state entry in
         let created = Catalog.find (catalog current) created_id |> Option.value_exn in
         assert (G.State.equal (G.state created) (Open (Held Source_review)));
         assert (Option.is_none (G.episode created));
         assert (P.Id.Principal.equal principal.id (G.creator created));
         let accounting =
           A.Goal_accounts.find (Option.value_exn current.goal_accounts) created_id
           |> Option.value_exn
           |> fun document ->
           A.Goal_accounting_document.resolve
             document
             ~goal:created
             ~ledger:current.inference_ledger
           |> protocol_ok
         in
         [%test_eq: int64] 0L (A.Goal_accounting.admitted_turns accounting);
         [%test_eq: int64] 0L (A.Goal_accounting.admitted_attempts accounting);
         assert_no_new_work before current counters counts;
         (* Same frozen request/key is retained DATA, not a new account or mutation. *)
         let retained_catalog = catalog current in
         let result, _ = callback entry runtime ~operation ~mode:"create" ~run:create in
         assert (protocol_ok result);
         assert (equal_catalog retained_catalog (catalog (state entry)));
         (* The Progress request anticipates precisely the first staged Hold CAS;
         no current Get is impersonated and no request is silently rebased. *)
         let before_control = state entry in
         let hold =
           control_request before_control ~key:(key "native-self-hold") (Hold reason)
         in
         let edit =
           P.Goal_edit.create
             ~generation:before_control.identity.generation
             ~source:(G.source (Goal.goal before_control))
             ~goal_id:Goal.goal_id
             ~expected_catalog_revision:
               Int64.(succ (Catalog.revision (catalog before_control)))
             ~expected_goal_revision:Int64.(succ (G.revision (Goal.goal before_control)))
             ~key:(key "native-after-hold-progress")
             ~mutation:
               (Record_progress
                  (G.Progress.of_string "progress after selected Hold" |> protocol_ok))
           |> protocol_ok
         in
         let result, calls =
           callback
             entry
             runtime
             ~operation
             ~mode:"hold-progress"
             ~additional:
               [ ( Admission.Effect.Progress
                 , fun admission ->
                     Actor.stage_native_goal_edit entry.actor ~admission ~authorize ~edit
                 )
               ]
             ~run:(stage_control entry hold)
         in
         assert (protocol_ok result);
         [%test_eq: int] 2 (List.length calls);
         let current = state entry in
         [%test_eq: int64]
           Int64.(G.revision (Goal.goal before_control) + 2L)
           (G.revision (Goal.goal current));
         assert (
           G.State.equal (G.state (Goal.goal current)) (Open (Held (Needs_input reason))));
         assert (
           String.equal
             "progress after selected Hold"
             (G.Progress.to_string (G.progress (Goal.goal current))));
         let stale =
           control_request current ~key:(key "native-stale-control") (Hold reason)
         in
         let same = control_request current ~key:(key "native-same-Hold") (Hold reason) in
         let result, _ =
           callback entry runtime ~operation ~mode:"hold" ~run:(stage_control entry same)
         in
         assert (protocol_ok result);
         [%test_eq: int64]
           Int64.(succ (G.revision (Goal.goal current)))
           (G.revision (Goal.goal (state entry)));
         let retained = catalog (state entry) in
         let result, calls =
           callback
             entry
             runtime
             ~operation
             ~mode:"hold-denied-catch"
             ~run:(stage_control entry stale)
         in
         assert (protocol_ok result);
         assert_native_denial Conflict calls;
         assert (equal_catalog retained (catalog (state entry)));
         let policy_denied =
           control_request (state entry) ~key:(key "native-policy-denied") (Hold reason)
         in
         let result, calls =
           callback
             entry
             runtime
             ~operation
             ~mode:"hold-denied-catch"
             ~run:(fun admission ->
               Actor.stage_native_goal_control
                 entry.actor
                 ~admission
                 ~authorize:(fun _ _ ->
                   Error
                     (P.Error.create
                        Permission_denied
                        ~message:"actual host denies native Goal control"
                        ~retryable:false
                        ()))
                 ~request:policy_denied)
         in
         assert (protocol_ok result);
         assert_native_denial Permission_denied calls;
         assert (equal_catalog retained (catalog (state entry)));
         let result, _ =
           callback entry runtime ~operation ~mode:"hold" ~run:(stage_control entry hold)
         in
         assert (protocol_ok result);
         assert (equal_catalog retained (catalog (state entry)));
         let denied =
           control_request (state entry) ~key:(key "wrong-selected-effect") (Hold reason)
         in
         let result, calls =
           callback
             entry
             runtime
             ~operation
             ~mode:"hold-wrong-effect-catch"
             ~run:(stage_control entry denied)
         in
         assert (protocol_ok result);
         assert_native_denial Permission_denied calls;
         assert (equal_catalog retained (catalog (state entry)));
         let abandon =
           control_request (state entry) ~key:(key "native-abandon") (Abandon reason)
         in
         let original_run = Goal_native_resume_tests.goal_run (state entry) _admission in
         let result, _ =
           callback
             entry
             runtime
             ~operation
             ~mode:"abandon"
             ~run:(stage_control entry abandon)
         in
         assert (protocol_ok result);
         let current = state entry in
         assert (G.State.equal (G.state (Goal.goal current)) (Closed (Abandoned reason)));
         assert (
           P.Run.equal original_run (Goal_native_resume_tests.goal_run current _admission));
         assert_no_new_work before current counters counts;
         let rollback_id = P.Id.Goal.of_string "goal_native_rolled_back" |> protocol_ok in
         let rollback =
           create_request
             (state entry)
             ~goal_id:rollback_id
             ~key:(key "native-create-rollback")
         in
         let result, _ =
           callback
             entry
             runtime
             ~operation
             ~mode:"create-rollback"
             ~run:(fun admission ->
               Actor.stage_native_goal_create
                 entry.actor
                 ~admission
                 ~authorize
                 ~request:rollback)
         in
         assert (Result.is_error result);
         assert (Option.is_none (Catalog.find (catalog (state entry)) rollback_id));
         assert (
           Option.is_none
             (A.Goal_accounts.find
                (Option.value_exn (state entry).goal_accounts)
                rollback_id));
         assert (Option.is_none (Eio.Promise.peek resumed_entered));
         release_ordinary ();
         ignore (await_quiet entry : A.Session_state.t)));
  print_endline
    "actual legacy/v1 selected Create; rollback; Hold->Progress; same-valued/retained \
     control; stale/policy/scope denial; Abandon without Run cancellation";
  [%expect
    {| actual legacy/v1 selected Create; rollback; Hold->Progress; same-valued/retained control; stale/policy/scope denial; Abandon without Run cancellation |}]
;;

let%expect_test
    "own Goal native Hold rejects contradictory Continue/turn in the same checkpoint"
  =
  List.iter
    [ false, "hold-turn", false; true, "hold-continue", false; true, "hold", true ]
    ~f:(fun (v1, mode, succeeds) ->
      let ordinal = ref 0 in
      let entered, enter = Eio.Promise.create () in
      let released, release = Eio.Promise.create () in
      let release_provider () =
        if Option.is_none (Eio.Promise.peek released) then Eio.Promise.resolve release ()
      in
      let post_stream _inputs =
        Int.incr ordinal;
        if Int.equal !ordinal 2
        then (
          Eio.Promise.resolve enter ();
          Eio.Promise.await released);
        completed_response "actual admitted Goal provider"
      in
      Staging.with_episode
        ~prompt_text:(prompt ~v1)
        ~post_stream
        (fun env daemon entry runtime counters session admission _old_live _held ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
             protect_cleanup ~finally:release_provider (fun () ->
               let held = set_passive daemon entry in
               let request =
                 Staging.exact_request held ~key:"native-control-own-resume"
               in
               let service =
                 Agent_server.Goal_run_service.create
                   ~factory:(Daemon.factory daemon)
                   ~authorize:(fun actor ->
                     Staging.authorize_as (Operator_authorization.principal actor))
               in
               let result =
                 Agent_server.Goal_run_service.resume_episode
                   service
                   ~entry
                   ~actor:original
                   ~session
                   ~request
                 |> protocol_ok
               in
               let operation =
                 match result with
                 | Resumed { continuation = Started operation; _ } -> operation
                 | Resumed { continuation = Waiting _ | Settled _ | Controlled _; _ }
                 | Retained _ ->
                   failwith "actual loaded Goal did not start its same-Run Operation"
                 | Resume_failed { error; _ } | Rejected error | Uncertain error ->
                   raise_s [%sexp (error : P.Error.t)]
               in
               Eio.Promise.await entered;
               let before = state entry in
               let old_run = Goal_native_resume_tests.goal_run before admission in
               assert (
                 List.exists old_run.owned_work ~f:(fun work ->
                   match work.P.Run_work.key with
                   | Operation id -> P.Id.Operation.equal id operation.id
                   | Retained _ -> false));
               let account = Goal.accounting before in
               let counts = !(counters.providers), !(counters.inference_ports) in
               let request =
                 control_request before ~key:(key "native-own-Hold") (Hold reason)
               in
               let result, calls =
                 callback
                   entry
                   runtime
                   ~operation
                   ~mode
                   ~run:(stage_control entry request)
               in
               [%test_eq: int] 1 (List.length calls);
               if succeeds
               then assert (protocol_ok result)
               else assert (Result.is_error result);
               let current = state entry in
               if succeeds
               then (
                 assert (
                   G.State.equal
                     (G.state (Goal.goal current))
                     (Open (Held (Needs_input reason))));
                 [%test_eq: int64]
                   Int64.(succ (G.revision (Goal.goal before)))
                   (G.revision (Goal.goal current)))
               else (
                 (match result with
                  | Error error ->
                    assert (
                      String.is_substring
                        error.P.Error.message
                        ~substring:
                          "native Goal control conflicts with its own new execution \
                           request")
                  | Ok _ -> assert false);
                 (* Failed callback settlement is a separate actual transition:
                    it can hold the associated Goal for review. No native control
                    receipt or mutation may have survived its rejected checkpoint. *)
                 assert (
                   List.equal
                     A.Goal_command_receipt.equal
                     (Catalog.receipts (catalog before))
                     (Catalog.receipts (catalog current)));
                 let failures =
                   List.filter current.moderator_executions ~f:(fun receipt ->
                     Option.equal
                       P.Id.Operation.equal
                       receipt.P.Moderator_execution.context.operation_id
                       (Some operation.id)
                     && P.Moderator_execution.equal_phase receipt.context.phase Turn_end
                     && not
                          (List.exists before.moderator_executions ~f:(fun old ->
                             P.Id.Moderator_execution.equal
                               old.context.id
                               receipt.context.id)))
                 in
                 (match failures with
                  | [ { status = Failed failure; context; _ } ] ->
                    [%test_eq: string] "event.handler_failed" failure.code;
                    assert (
                      P.Invocation.equal_observer context.source old_run.source.observer)
                  | _ ->
                    raise_s
                      [%sexp
                        "exact failed callback absent"
                      , (failures : P.Moderator_execution.t list)]);
                 let failed = Goal_native_resume_tests.goal_run current admission in
                 assert (
                   Option.exists current.active_operation ~f:(fun active ->
                     P.Id.Operation.equal active.id operation.id));
                 assert (Option.is_none (Eio.Promise.peek released));
                 if v1
                 then (
                   assert (P.Run.Lifecycle.equal (Terminal (Failed None)) failed.lifecycle);
                   [%test_eq: int64] Int64.(succ old_run.revision) failed.revision;
                   assert (
                     List.equal P.Run_work.equal old_run.owned_work failed.owned_work);
                   List.iter old_run.terminal_work ~f:(fun proof ->
                     assert (
                       List.mem
                         failed.terminal_work
                         proof
                         ~equal:P.Run_work.Terminal.equal));
                   assert (
                     List.exists failed.terminal_work ~f:(fun proof ->
                       match proof.work.key, proof.outcome with
                       | Operation id, Unconfirmed ->
                         P.Id.Operation.equal id operation.id
                         && Int64.equal proof.revision failed.revision
                       | _ -> false));
                   assert (
                     List.exists
                       (A.Run_state.receipts (Option.value_exn current.run_state))
                       ~f:(fun receipt ->
                         P.Run_receipt.Kind.equal receipt.P.Run_receipt.kind Terminal
                         && P.Id.Run.equal receipt.run_id failed.id
                         && Int64.equal receipt.run_revision failed.revision));
                   let expected =
                     G.hold (Goal.goal before) Source_review |> protocol_ok
                   in
                   assert (G.equal expected (Goal.goal current)))
                 else (
                   assert (P.Run.equal old_run failed);
                   assert (G.equal (Goal.goal before) (Goal.goal current))));
               assert (
                 not
                   (List.exists
                      (A.Run_state.intents (Option.value_exn current.run_state))
                      ~f:(fun intent ->
                        P.Id.Run.equal intent.A.Run_intent.receipt.run_id admission.run_id
                        &&
                        match intent.action with
                        | Continue -> true
                        | Wait _ | Finish _ -> false)));
               assert (A.Goal_accounting.equal account (Goal.accounting current));
               [%test_eq: int * int]
                 counts
                 (!(counters.providers), !(counters.inference_ports));
               release_provider ();
               let joined = await_quiet entry in
               assert (Option.is_none joined.active_operation);
               [%test_eq: int] 2 !ordinal;
               assert (A.Goal_accounting.equal account (Goal.accounting joined));
               let events =
                 match
                   A.Durable_event_log.replay
                     entry.durable_events
                     ~after_sequence:before.counters.event_sequence
                     ~through_sequence:joined.counters.event_sequence
                 with
                 | Available events -> events
                 | Snapshot_required -> failwith "actual operation receipt window lost"
               in
               let terminal =
                 List.filter_map events ~f:(fun event ->
                   match
                     P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
                     |> protocol_ok
                   with
                   | (Operation_completed actual | Operation_failed actual)
                     when P.Id.Operation.equal actual.id operation.id -> Some actual
                   | _ -> None)
               in
               let terminal =
                 match terminal with
                 | [ terminal ] -> terminal
                 | _ ->
                   raise_s
                     [%sexp
                       "one actual terminal Operation required"
                     , (terminal : P.Operation.t list)]
               in
               let outcome =
                 match terminal.state with
                 | Completed -> P.Run_work.Terminal.Succeeded
                 | Failed _ -> Failed
                 | _ ->
                   raise_s
                     [%sexp
                       "admitted provider did not finish", (terminal : P.Operation.t)]
               in
               let after_run = Goal_native_resume_tests.goal_run joined admission in
               if v1 && not succeeds
               then (
                 assert (
                   P.Run.equal
                     (Goal_native_resume_tests.goal_run current admission)
                     after_run);
                 assert (G.equal (Goal.goal current) (Goal.goal joined)))
               else (
                 assert (
                   List.exists after_run.terminal_work ~f:(fun proof ->
                     match proof.work.key with
                     | Operation id ->
                       P.Id.Operation.equal id operation.id
                       && P.Run_work.Terminal.equal_outcome proof.outcome outcome
                     | _ -> false));
                 if succeeds || P.Run_work.Terminal.equal_outcome outcome Succeeded
                 then assert (G.equal (Goal.goal current) (Goal.goal joined))
                 else (
                   assert (
                     P.Run.Lifecycle.equal (Terminal (Failed None)) after_run.lifecycle);
                   let expected =
                     G.hold (Goal.goal before) Source_review |> protocol_ok
                   in
                   assert (G.equal expected (Goal.goal joined))))))));
  print_endline
    "actual own-Run native control; contradictory new execution rejected atomically; \
     acknowledged provider still joins";
  [%expect
    {| actual own-Run native control; contradictory new execution rejected atomically; acknowledged provider still joins |}]
;;
