open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Goal = Goal_service_tests
module Direct = Descendant_turn_adapter_tests
module Existing = Source_preparation_tests
module GD = A.Goal_descendants
module L = A.Inference_ledger
module G = P.Goal
module Goal_service = Agent_server.Goal_run_service
module Recurrence_service = Agent_server.Recurrence_run_service

module Consumer = struct
  type t =
    | Goal
    | Recurrence

  let input = function
    | Goal -> "572 actual held Goal input"
    | Recurrence -> "539 actual retry input"
  ;;
end

let within env phase f =
  try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. f with
  | Eio.Time.Timeout -> failwith ("held inherited input timed out: " ^ phase)
;;

let run current receipt =
  let index = Option.value_exn current.A.Session_state.run_state in
  let selected =
    A.Run_state.find index receipt.P.Run_receipt.run_id |> Option.value_exn
  in
  assert (List.exists (A.Run_state.receipts index) ~f:(P.Run_receipt.equal receipt));
  index, selected
;;

let has_operation (work : P.Run_work.t) (operation : P.Operation.t) =
  Int.equal work.generation operation.generation
  &&
  match work.key with
  | Operation id -> P.Id.Operation.equal id operation.id
  | Retained _ -> false
;;

let assert_goal current index selected receipt handle =
  let link = A.Run_state.goal_link index selected.P.Run.id |> Option.value_exn in
  let episode = G.episode (Goal.goal current) |> Option.value_exn in
  assert (P.Goal_episode.equal episode (A.Run_goal_link.episode link));
  assert (P.Run_receipt.equal receipt (P.Goal_episode.admission episode));
  let account = Goal.accounting current in
  [%test_eq: int64] 1L (A.Goal_accounting.admitted_turns account);
  [%test_eq: int64] 1L (A.Goal_accounting.admitted_attempts account);
  let binding =
    A.Goal_accounting.bindings account
    |> List.find_exn ~f:(fun binding ->
      L.Handle.equal (A.Goal_accounting.Binding.handle binding) handle)
  in
  assert (P.Id.Run.equal (A.Goal_accounting.Binding.run_id binding) selected.id)
;;

module Probe = struct
  type accepted =
    { included : A.Goal_descendant_included.t
    ; handle : L.Handle.t
    ; operation : P.Operation.t
    ; admission : P.Run_receipt.t
    }

  type t =
    { consumer : Consumer.t
    ; parent : Registry.entry
    ; child : Registry.entry
    ; scope : Agent_store.Delegation_goal_origin.Scope.t
    ; reference : Agent_store.Delegation_store.Reference.t
    ; accounting : Birth.Provider_probe.t
    ; mutable calls : int
    ; mutable accepted : accepted option
    ; mutable failure : (exn * Stdlib.Printexc.raw_backtrace) option
    }

  let create ~consumer ~parent ~child ~scope ~reference =
    let parent_attempts_before =
      A.Goal_accounting.admitted_attempts (Goal.accounting (state parent))
    in
    [%test_eq: int64] 2L parent_attempts_before;
    { consumer
    ; parent
    ; child
    ; scope
    ; reference
    ; accounting =
        Birth.Provider_probe.create
          ~parent
          ~child
          ~scope
          ~reference
          ~parent_attempts_before
    ; calls = 0
    ; accepted = None
    ; failure = None
    }
  ;;

  let raise_failure t =
    Option.iter t.failure ~f:(fun (exn, backtrace) ->
      Exn.raise_with_original_backtrace exn backtrace)
  ;;

  let check t =
    assert (Option.is_none t.accepted);
    let current = state t.child in
    let included =
      Direct.confirmed t.parent t.child t.scope t.reference ~turns:1L ~attempts:1L
    in
    [%test_eq: int64]
      2L
      (GD.Branch.confirmed_prefix (Direct.branch t.parent t.scope t.reference));
    let occurrence =
      A.Goal_descendant_included.pending included |> A.Goal_descendant_pending.occurrence
    in
    let ordinal, operation_id, invocation_id =
      match A.Goal_descendant_occurrence.kind occurrence with
      | Attempt { ordinal; operation_id; invocation_id } ->
        ordinal, operation_id, invocation_id
      | Host_turn _ -> failwith "provider preceded canonical inherited Attempt"
    in
    let operation = Option.value_exn current.active_operation in
    assert (Option.equal P.Id.Operation.equal operation_id (Some operation.id));
    assert (Option.is_none invocation_id);
    assert (List.is_empty current.jobs);
    let row = L.find current.inference_ledger ~ordinal |> Option.value_exn in
    let handle = L.Row.handle row in
    assert (P.Id.Session.equal (L.Handle.session_id handle) current.identity.session_id);
    [%test_eq: int] current.identity.generation (L.Handle.generation handle);
    assert (Option.equal P.Id.Operation.equal operation_id (L.Handle.operation_id handle));
    assert (
      Option.equal P.Id.Invocation.equal invocation_id (L.Handle.invocation_id handle));
    (match Inference.Observation.Attempt_record.state (L.Row.record row) with
     | Running -> ()
     | Prepared | Terminal _ | Interrupted _ ->
       failwith "provider preceded actual Running ACK");
    let index = Option.value_exn current.run_state in
    let selected =
      A.Run_state.runs index
      |> List.filter ~f:(fun selected ->
        List.exists selected.P.Run.owned_work ~f:(fun work ->
          has_operation work operation))
      |> function
      | [ selected ] -> selected
      | [] | _ :: _ -> failwith "actual input Operation did not have one Run owner"
    in
    let admission =
      A.Run_state.receipts index
      |> List.find_exn ~f:(fun receipt ->
        P.Id.Run.equal receipt.P.Run_receipt.run_id selected.id
        && P.Run_receipt.Kind.equal receipt.kind Admission)
    in
    (match t.consumer with
     | Goal -> assert_goal current index selected admission handle
     | Recurrence ->
       assert (Option.is_none current.goal_catalog);
       assert (Option.is_none current.goal_accounts);
       let origin = A.Run_state.recurrence_origin index selected.id |> Option.value_exn in
       A.Recurrence_run_origin.validate_run origin ~run:selected ~admission |> protocol_ok);
    (* Reuse only the accounting oracle; before_submit deliberately assumes no
       local Goal and therefore is not a valid held-Goal provider check. *)
    Birth.Provider_probe.checked_root_snapshot t.accounting (state t.parent) included
    |> ignore;
    t.accepted <- Some { included; handle; operation; admission }
  ;;

  let before_submit t =
    t.calls <- t.calls + 1;
    try check t with
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      t.failure <- Some (exn, backtrace);
      Exn.raise_with_original_backtrace exn backtrace
  ;;

  let completed t receipt =
    raise_failure t;
    [%test_eq: int] 1 t.calls;
    let accepted = Option.value_exn t.accepted in
    assert (P.Run_receipt.equal receipt accepted.admission);
    let current = state t.child in
    let index, selected = run current receipt in
    let work =
      List.exists selected.owned_work ~f:(fun work ->
        has_operation work accepted.operation)
    in
    let terminal =
      List.filter selected.terminal_work ~f:(fun evidence ->
        has_operation evidence.P.Run_work.Terminal.work accepted.operation)
    in
    (* Run settlement retains the owned inventory and appends exact terminal
       evidence; lifecycle classification does not delete the Operation. *)
    assert work;
    (match terminal with
     | [ evidence ] -> [%test_eq: P.Run_work.Terminal.outcome] Succeeded evidence.outcome
     | [] | _ :: _ ->
       failwith "actual completed input lacked exact terminal Operation evidence");
    assert (
      Option.is_some
        (L.find_turn_handle
           current.inference_ledger
           ~operation_id:accepted.operation.id
           ~generation:accepted.operation.generation));
    let row =
      L.find current.inference_ledger ~ordinal:(L.Handle.ordinal accepted.handle)
      |> Option.value_exn
    in
    assert (L.Handle.equal accepted.handle (L.Row.handle row));
    (match Inference.Observation.Attempt_record.state (L.Row.record row) with
     | Terminal terminal ->
       assert (
         Inference.Event.Terminal.equal_outcome
           (Inference.Event.Terminal.outcome terminal)
           Completed)
     | Prepared | Running | Interrupted _ ->
       failwith "actual provider Attempt did not complete");
    (match t.consumer with
     | Goal -> assert_goal current index selected receipt accepted.handle
     | Recurrence ->
       let origin = A.Run_state.recurrence_origin index selected.id |> Option.value_exn in
       A.Recurrence_run_origin.validate_run origin ~run:selected ~admission:receipt
       |> protocol_ok);
    let included =
      Direct.confirmed t.parent t.child t.scope t.reference ~turns:1L ~attempts:1L
    in
    assert (A.Goal_descendant_included.equal included accepted.included);
    Birth.Provider_probe.checked_root_snapshot t.accounting (state t.parent) included
    |> ignore;
    [%test_eq: int]
      1
      (List.count current.conversation.canonical_history ~f:(fun entry ->
         String.is_substring
           (Jsonaf.to_string (P.History.entry_to_json entry))
           ~substring:(Consumer.input t.consumer)))
  ;;
end

let await_completed_input env child probe =
  within env "actual input worker completion" (fun () ->
    let rec await () =
      Probe.raise_failure probe;
      let current =
        Actor.with_moderator_checkpoint child.Registry.actor (fun () ->
          Actor.state child.actor)
        |> protocol_ok
      in
      let summary = L.summary current.inference_ledger in
      let turns = P.Inference_query.Summary.turns summary in
      if Int64.(turns.failed > 0L || turns.cancelled > 0L || turns.interrupted > 0L)
      then
        raise_s
          [%sexp "actual held input Turn failed", (summary : P.Inference_query.Summary.t)];
      if Option.is_none current.active_operation && Int64.equal turns.completed 1L
      then ()
      else (
        Eio.Fiber.yield ();
        await ())
    in
    await ())
;;

let goal_call daemon child =
  let session, before = Goal.create_goal daemon child in
  let request =
    A.Run_admission_request.Headless.create
      ~session_id:before.identity.session_id
      ~generation:before.identity.generation
      ~expected_revision:before.counters.revision
      ~mode:Workflow
      ~input:
        (User_submission
           { kind = Plain_text; text = Consumer.input Goal; attachments = [] })
      ~key:(P.Idempotency_key.of_string "572-held-goal" |> protocol_ok)
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
  let service =
    Goal_service.create ~factory:(Daemon.factory daemon) ~authorize:(fun _ -> authorize)
  in
  fun () ->
    Goal_service.start_episode service ~entry:child ~actor:original ~session ~request
;;

let successful consumer =
  let probe = ref None in
  let budget =
    G.Budget.create
      ~admitted_turns:None
      ~admitted_attempts:(Some 3L)
      ~observed_tokens:None
      ~deadline:None
    |> protocol_ok
  in
  Birth.with_native_goal_child
    ~timeout:120.
    ~budget
    ~post_creation_stream:(fun _ ->
      Option.iter !probe ~f:Probe.before_submit;
      completed_response "572 actual held provider completed")
    (fun env sw daemon parent child scope reference counters ->
       Actor.with_moderator_checkpoint parent.actor (fun () -> Ok ()) |> protocol_ok;
       within env "actual scripted child startup" (fun () ->
         Direct.start_child daemon child);
       let initial = Direct.marks child in
       [%test_eq: int64] 0L initial.turn;
       [%test_eq: int64] 0L initial.attempt;
       [%test_eq: int] 2 !(counters.providers);
       let checked = Probe.create ~consumer ~parent ~child ~scope ~reference in
       probe := Some checked;
       let assert_replay call check =
         Actor.with_moderator_checkpoint child.actor (fun () -> Ok ()) |> protocol_ok;
         Actor.with_moderator_checkpoint parent.actor (fun () -> Ok ()) |> protocol_ok;
         let before_child = state child in
         let before_parent = state parent in
         let child_frames = Existing.frames env child in
         let parent_frames = Existing.frames env parent in
         within env "retained public service replay" call |> check;
         Probe.raise_failure checked;
         Existing.assert_same before_child child;
         Existing.assert_same before_parent parent;
         [%test_eq: string list] child_frames (Existing.frames env child);
         [%test_eq: string list] parent_frames (Existing.frames env parent);
         [%test_eq: int] 1 checked.calls;
         [%test_eq: int] 3 !(counters.providers)
       in
       let receipt =
         match consumer with
         | Goal ->
           let call = goal_call daemon child in
           let result = within env "Goal held input activation" call in
           Probe.raise_failure checked;
           let receipt = result |> protocol_ok |> Goal.activated in
           await_completed_input env child checked;
           assert_replay call (fun result ->
             match result |> protocol_ok with
             | Goal_service.Outcome.Retained known ->
               assert (P.Run_receipt.equal receipt known)
             | Attempted _ -> failwith "actual Goal replay reran held input");
           receipt
         | Recurrence ->
           with_planner env sw daemon parent (fun () ->
             let occurrence = create_occurrence daemon parent child in
             let service = Recurrence_service_tests.service daemon in
             let call () =
               Recurrence_service.activate
                 service
                 ~controller:parent
                 ~controller_actor:original
                 ~target:child
                 ~target_actor:original
                 ~intent:occurrence.intent
                 ~attempt:occurrence.previous
             in
             let result = within env "recurrence held input activation" call in
             Probe.raise_failure checked;
             let receipt = result |> protocol_ok |> Recurrence_service_tests.activated in
             await_completed_input env child checked;
             let index, selected = run (state child) receipt in
             (match selected.lifecycle with
              | Terminal (Completed _) -> ()
              | Admitted | Active | Waiting _ | Terminal _ ->
                failwith "actual Single_turn recurrence input was not terminal");
             let origin =
               A.Run_state.recurrence_origin index selected.id |> Option.value_exn
             in
             A.Recurrence_run_origin.validate
               origin
               ~intent:occurrence.intent
               ~attempt:occurrence.previous
             |> protocol_ok;
             let imported =
               match
                 Recurrence_service.reconcile
                   service
                   ~controller:parent
                   ~controller_actor:original
                   ~target:child
                   ~target_actor:original
                   ~intent:occurrence.intent
                   ~attempt:occurrence.previous
                 |> protocol_ok
               with
               | Retained known ->
                 assert (P.Run_receipt.equal receipt known.admission);
                 known.reconciliation
                 |> protocol_ok
                 |> Recurrence_service_tests.imported_row parent occurrence ~changed:true
               | Absent -> failwith "actual held recurrence admission disappeared"
             in
             assert (
               Option.is_some
                 (Execution.Attempt.terminal (Execution.current_attempt imported)));
             assert_replay call (fun result ->
               let retained =
                 result
                 |> protocol_ok
                 |> Recurrence_service_tests.retained parent occurrence receipt
               in
               assert (Execution.equal imported retained));
             receipt)
       in
       Probe.completed checked receipt;
       [%test_eq: int] 3 !(counters.providers))
;;

let%expect_test "actual Goal held input confirms inherited Turn and replays its receipt" =
  successful Goal;
  print_endline
    "Goal held Turn and Attempt confirmed before provider; original receipt replayed once";
  [%expect
    {| Goal held Turn and Attempt confirmed before provider; original receipt replayed once |}]
;;

let%expect_test
    "actual recurrence held input confirms inherited Turn and replays its receipt"
  =
  successful Recurrence;
  print_endline
    "Recurrence held Turn and Attempt confirmed before provider; original receipt \
     replayed once";
  [%expect
    {| Recurrence held Turn and Attempt confirmed before provider; original receipt replayed once |}]
;;

let%expect_test
    "held recurrence controller retires during inherited Reserve without a child writer"
  =
  let module History = Descendant_history_adapter_tests in
  let module Barrier = History.Root_barrier in
  let module Origin = A.Recurrence_run_origin in
  let module O = Agent_store.Delegation_goal_origin in
  let module D = Agent_store.Delegation_store in
  let barrier = Barrier.create () in
  let armed = ref false in
  let provider_calls = ref 0 in
  Birth.with_native_goal_child
    ~timeout:120.
    ~wrap_env:(Barrier.wrap barrier)
    ~post_creation_stream:(fun _ ->
      if !armed then Int.incr provider_calls;
      completed_response "572 controller-retirement unexpected provider")
    (fun env sw daemon parent child scope reference counters ->
       Actor.with_moderator_checkpoint parent.actor (fun () -> Ok ()) |> protocol_ok;
       within env "actual scripted child startup" (fun () ->
         Direct.start_child daemon child);
       with_planner env sw daemon parent (fun () ->
         let occurrence = create_occurrence daemon parent child in
         let initial = Direct.marks child in
         [%test_eq: int64] 0L initial.turn;
         [%test_eq: int64] 0L initial.attempt;
         let before = state child in
         let source = A.Recurrence_source.installed before |> protocol_ok in
         let accounting = Option.value_exn before.goal_descendants in
         let before_ledger = History.ledger_document before.inference_ledger in
         let root = Direct.branch parent scope reference in
         [%test_eq: int64] 0L (GD.Branch.confirmed_prefix root);
         assert (Option.is_none (GD.Branch.pending root));
         let before_nonce = GD.Branch.last_nonce root in
         let root_account = Goal.accounting (state parent) in
         let root_turns = A.Goal_accounting.admitted_turns root_account in
         let root_attempts = A.Goal_accounting.admitted_attempts root_account in
         let providers_before = !(counters.providers) in
         let controller_id = (state parent).identity.session_id in
         let revoked = ref false in
         let denials = ref 0 in
         let authorize_controller _ (current : A.Session_state.t) =
           let open Result.Let_syntax in
           let%bind () = authorize current in
           if !revoked && P.Id.Session.equal current.identity.session_id controller_id
           then (
             Int.incr denials;
             Error
               (P.Error.create
                  Permission_denied
                  ~message:"572 controller host authorization retired"
                  ~retryable:false
                  ()))
           else Ok ()
         in
         let service =
           Recurrence_service_tests.service ~authorize:authorize_controller daemon
         in
         let pending = ref None in
         protect_cleanup
           ~finally:(fun () ->
             Barrier.release barrier;
             Option.iter !pending ~f:(fun pending ->
               within env "joined controller-retirement cleanup" (fun () ->
                 Eio.Promise.await_exn pending)
               |> ignore))
           (fun () ->
              armed := true;
              barrier.target
              <- Some (controller_id, scope, Barrier.Kind.Reserve (GD.session accounting));
              let activation =
                Eio.Fiber.fork_promise ~sw (fun () ->
                  Recurrence_service.activate
                    service
                    ~controller:parent
                    ~controller_actor:original
                    ~target:child
                    ~target_actor:original
                    ~intent:occurrence.intent
                    ~attempt:occurrence.previous)
              in
              pending := Some activation;
              within env "actual held recurrence ancestor Reserve" (fun () ->
                Eio.Promise.await barrier.entered);
              (* Only the parent writer is parked. Read the child's actual known
                 Held admission; no parent mailbox request can run here. *)
              let waiting = state child in
              let index = Option.value_exn waiting.run_state in
              let selected, origin =
                A.Run_state.runs index
                |> List.filter_map ~f:(fun selected ->
                  Option.map
                    (A.Run_state.recurrence_origin index selected.P.Run.id)
                    ~f:(fun origin -> selected, origin))
                |> function
                | [ selected ] -> selected
                | [] | _ :: _ ->
                  failwith "Reserve did not retain one actual recurrence Run"
              in
              Origin.validate
                origin
                ~intent:occurrence.intent
                ~attempt:occurrence.previous
              |> protocol_ok;
              let admission = Origin.admission origin in
              Origin.validate_run origin ~run:selected ~admission |> protocol_ok;
              assert (
                List.exists
                  (A.Run_state.receipts index)
                  ~f:(P.Run_receipt.equal admission));
              [%test_eq: string]
                before_ledger
                (History.ledger_document waiting.inference_ledger);
              assert (Option.is_none waiting.active_operation);
              authorize_controller original waiting |> protocol_ok;
              revoked := true;
              (* The same actual target remains authorized; only the controller's
                 service host policy changes, not either source or original. *)
              authorize_controller original waiting |> protocol_ok;
              assert (Operator_authorization.is_current original);
              [%test_eq: int] 0 !denials;
              Barrier.release barrier;
              let attempted =
                within env "controller-retired held response" (fun () ->
                  Eio.Promise.await_exn activation)
                |> protocol_ok
                |> function
                | Recurrence_service.Outcome.Attempted attempted -> attempted
                | Retained _ -> failwith "fresh held activation returned retained data"
              in
              (match Recurrence_service.Attempt.result attempted |> protocol_ok with
               | Activation_failed { admission = known; error } ->
                 assert (P.Run_receipt.equal admission known);
                 [%test_eq: P.Error.code] Permission_denied error.code
               | Activated _ | Retained _ | Admission_uncertain _ ->
                 failwith "controller retirement did not preserve known admission failure");
              assert (!denials > 0);
              let root, closed =
                within env "exact held no-writer close" (fun () ->
                  let rec await () =
                    let actual = Direct.branch parent scope reference in
                    match GD.Branch.pending actual, GD.Branch.last actual with
                    | None, Some (No_writer_closed closed) -> actual, closed
                    | (Some _ | None), (None | Some (Included _ | No_writer_closed _)) ->
                      Eio.Fiber.yield ();
                      await ()
                  in
                  await ())
              in
              [%test_eq: int64] 0L (GD.Branch.confirmed_prefix root);
              [%test_eq: int64] (Int64.succ before_nonce) (GD.Branch.last_nonce root);
              [%test_eq: int64]
                (GD.Branch.last_nonce root)
                (A.Goal_descendant_pending.nonce closed);
              assert (O.Scope.equal scope (A.Goal_descendant_pending.scope closed));
              assert (
                D.Reference.equal
                  reference
                  (A.Goal_descendant_pending.branch_reference closed));
              assert (
                A.Goal_descendant_occurrence.equal
                  (Option.value_exn barrier.occurrence)
                  (A.Goal_descendant_pending.occurrence closed));
              let after = state child in
              authorize_controller original after |> protocol_ok;
              assert (
                P.Run_source.equal
                  source
                  (A.Recurrence_source.installed after |> protocol_ok));
              assert (
                Option.equal
                  D.Reference.equal
                  before.spec.delegation
                  after.spec.delegation);
              assert (
                List.equal
                  P.History.equal_entry
                  before.conversation.canonical_history
                  after.conversation.canonical_history);
              [%test_eq: string]
                before_ledger
                (History.ledger_document after.inference_ledger);
              assert (
                List.equal
                  A.Goal_descendant_included.equal
                  (GD.local_included accounting)
                  (GD.local_included (Option.value_exn after.goal_descendants)));
              assert (
                Option.equal
                  O.equal
                  (GD.origin accounting)
                  (GD.origin (Option.value_exn after.goal_descendants)));
              assert (Option.is_none after.active_operation);
              let retained_index, retained_run = run after admission in
              let retained_origin =
                A.Run_state.recurrence_origin retained_index retained_run.id
                |> Option.value_exn
              in
              assert (Origin.equal origin retained_origin);
              Origin.validate_run retained_origin ~run:retained_run ~admission
              |> protocol_ok;
              let root_after = Goal.accounting (state parent) in
              [%test_eq: int64] root_turns (A.Goal_accounting.admitted_turns root_after);
              [%test_eq: int64]
                root_attempts
                (A.Goal_accounting.admitted_attempts root_after);
              [%test_eq: int] 0 !provider_calls;
              [%test_eq: int] providers_before !(counters.providers))));
  print_endline
    "controller-only retirement preserves Held admission; no child Turn or provider; \
     exact reservation closed";
  [%expect
    {| controller-only retirement preserves Held admission; no child Turn or provider; exact reservation closed |}]
;;
