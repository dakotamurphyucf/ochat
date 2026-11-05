open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module G = P.Goal
module C = A.Goal_catalog
module I = P.Invocation
module E = P.Moderator_execution
module T = A.Goal_control_transition

(* Local immutable state laws, using the existing actual-source admission model.
   The synthesized callback/action DTOs below are not physical execution owners,
   and the Wait DTO does not claim a ready delivery or permission to execute. *)
let command catalog (run : P.Run.t) key mutation =
  A.Goal_command.create
    ~session_id:(P.Session_ref.session_id run.session)
    ~generation:run.source.generation
    ~source:
      (if A.Goal_command.Mutation.needs_source mutation then Some run.source else None)
    ~expected_catalog_revision:(C.revision catalog)
    ~expected_goal_revision:
      (Option.map
         (C.find catalog (A.Goal_command.Mutation.goal_id mutation))
         ~f:G.revision)
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    ~mutation
  |> protocol_ok
  |> fun request ->
  A.Goal_command_transition.prepare
    catalog
    ~request
    ~principal_id:run.principal_id
    ~session:run.session
    ~generation:run.source.generation
    ~current_source:(Some run.source)
    ~now:timestamp
  |> Goal_catalog_tests.committed
  |> fst
;;

let handler (run : P.Run.t) name origin requests =
  let prototype = invocation_fixture () in
  I.create
    { prototype.context with
      id = P.Id.Invocation.of_string ("inv_goal_control_" ^ name) |> protocol_ok
    ; session_id = P.Session_ref.session_id run.session
    ; generation = run.source.generation
    }
  |> protocol_ok
  |> fun invocation ->
  I.capture_inference_origin invocation origin
  |> protocol_ok
  |> I.dispatch
  |> protocol_ok
  |> fun invocation ->
  I.resolve
    invocation
    ~session_id:(P.Session_ref.session_id run.session)
    ~generation:run.source.generation
    (Complete (`String "immutable result"))
  |> protocol_ok
  |> fun invocation -> I.record_handler_intent invocation ~requests |> protocol_ok
;;

let event_id name =
  P.Id.Moderator_execution.of_string ("mex_goal_control_" ^ name) |> protocol_ok
;;

let with_event_evidence (run : P.Run.t) =
  let work =
    List.map [ "owned"; "stop" ] ~f:(fun name ->
      P.Run_work.create
        ~key:(Retained (Moderator_execution (event_id name)))
        ~generation:run.source.generation
      |> protocol_ok)
  in
  let proofs =
    List.map work ~f:(fun work ->
      P.Run_work.Terminal.create ~work ~outcome:Succeeded ~revision:run.revision
      |> protocol_ok)
  in
  P.Run.create
    ~id:run.id
    ~session:run.session
    ~principal_id:run.principal_id
    ~source:run.source
    ~mode:run.mode
    ~lifecycle:run.lifecycle
    ~revision:run.revision
    ~owned_work:(run.owned_work @ work)
    ~relinquished_work:run.relinquished_work
    ~terminal_work:(run.terminal_work @ proofs)
    ~created_at:run.created_at
    ~updated_at:run.updated_at
  |> protocol_ok
;;

let event (run : P.Run.t) name requests =
  E.create
    { id = event_id name
    ; session_id = P.Session_ref.session_id run.session
    ; generation = run.source.generation
    ; source = run.source.observer
    ; operation_id = None
    ; job = None
    ; phase = Internal_event
    ; event = `Null
    ; checkpoint_sha256 = String.make 64 'b'
    ; created_at = timestamp
    }
  |> protocol_ok
  |> E.complete ~checkpoint_sha256:(String.make 64 'c') ~requests
  |> protocol_ok
;;

let%expect_test
    "Goal Hold and Stop retire future execution without rewriting admitted evidence"
  =
  Goal_run_tests.with_model (fun ~basis:_ ~held:_ ~admission:_ ~intent:_ ~delta:_ state ->
    let index = Option.value_exn state.A.Session_state.run_state in
    let admitted = List.hd_exn (A.Run_state.runs index) in
    let wake =
      P.Run_wake.create
        ~run_id:admitted.id
        ~source:admitted.source
        ~occurrence:
          (Job_completion
             { job_id = P.Id.Job.of_string "job_control_local_wake" |> protocol_ok
             ; attempt = 1
             })
      |> protocol_ok
    in
    List.iter
      [ P.Run_action.Continue
      ; Wait wake
      ; Finish { terminal = Completed None; relinquish = [] }
      ]
      ~f:(fun action ->
        let lifecycle =
          match action with
          | Wait wake -> P.Run.Lifecycle.Waiting wake
          | Continue | Finish _ -> Active
        in
        let run = Goal_run_tests.rebuild admitted ~lifecycle |> with_event_evidence in
        let receipt =
          Goal_run_tests.receipt
            state
            run
            Action
            (P.Run_action.to_json action)
            "control-action"
        in
        let intent =
          A.Run_intent.create
            ~receipt
            ~execution_id:
              (P.Id.Moderator_execution.of_string "mex_control_action" |> protocol_ok)
            ~action
          |> protocol_ok
        in
        let index =
          A.Run_state.commit index ~run ~receipt ~intent:(Some intent) |> protocol_ok
        in
        let requests : I.follow_up =
          { request_turn = true; request_compaction = true; end_session = None }
        in
        let stop_requests = { requests with end_session = Some "truthful stop" } in
        let owned = handler run "owned" (Run run.id) requests in
        let unrelated = handler run "unrelated" Unbound requests in
        let stop = handler run "stop" (Run run.id) stop_requests in
        let owned_event = event run "owned" requests in
        let unrelated_event = event run "unrelated" requests in
        let stop_event = event run "stop" stop_requests in
        let before =
          { state with
            run_state = Some index
          ; invocations = [ owned; unrelated; stop ]
          ; moderator_executions = [ owned_event; unrelated_event; stop_event ]
          }
        in
        A.Session_state.validate before |> protocol_ok;
        let catalog = Option.value_exn before.goal_catalog in
        let goal = Goal_run_tests.current_goal before in
        let planned =
          command
            catalog
            run
            "control-hold"
            (Hold
               ( G.id goal
               , Needs_input (G.Reason.of_string "retained hold reason" |> protocol_ok) ))
        in
        let delta =
          T.prepare_hold before ~goal_id:(G.id goal) ~delta:(Goal_catalog_changed planned)
          |> protocol_ok
        in
        let after = A.Session_delta.apply before delta |> protocol_ok in
        A.Session_state.validate after |> protocol_ok;
        assert (
          Option.equal
            Jsonaf.exactly_equal
            (Option.map before.goal_accounts ~f:(fun accounts ->
               A.Goal_accounts.to_jsonaf accounts |> protocol_ok))
            (Option.map after.goal_accounts ~f:(fun accounts ->
               A.Goal_accounts.to_jsonaf accounts |> protocol_ok)));
        let after_goal = Goal_run_tests.current_goal after in
        [%test_eq: int64] (Int64.succ (G.revision goal)) (G.revision after_goal);
        [%test_eq: int64]
          (C.revision planned)
          (C.revision (Option.value_exn after.goal_catalog));
        assert (
          List.equal
            A.Goal_command_receipt.equal
            (C.receipts planned)
            (C.receipts (Option.value_exn after.goal_catalog)));
        let after_index = Option.value_exn after.run_state in
        assert (P.Run.equal run (A.Run_state.find after_index run.id |> Option.value_exn));
        assert (
          List.equal
            P.Run_receipt.equal
            (A.Run_state.receipts index)
            (A.Run_state.receipts after_index));
        assert (
          A.Run_intent.Disposition.equal
            (match action with
             | Continue -> Retired
             | Wait _ | Finish _ -> Pending)
            (List.hd_exn (A.Run_state.intents after_index)).disposition);
        let changed =
          List.find_exn after.invocations ~f:(fun invocation ->
            P.Id.Invocation.equal invocation.context.id owned.context.id)
        in
        assert (I.equal_status owned.status changed.status);
        (match changed.handler_intent with
         | Some { follow_up = Discarded_follow_up _; _ } -> ()
         | _ -> failwith "owned future handler survived Hold");
        List.iter [ unrelated; stop ] ~f:(fun expected ->
          assert (
            I.equal
              expected
              (List.find_exn after.invocations ~f:(fun invocation ->
                 P.Id.Invocation.equal expected.context.id invocation.context.id))));
        let changed =
          List.find_exn after.moderator_executions ~f:(fun event ->
            P.Id.Moderator_execution.equal event.context.id owned_event.context.id)
        in
        assert (E.equal_status owned_event.status changed.status);
        (match changed.intent with
         | Some (Discarded _) -> ()
         | _ -> failwith "owned future event survived Hold");
        List.iter [ unrelated_event; stop_event ] ~f:(fun expected ->
          assert (
            E.equal
              expected
              (List.find_exn after.moderator_executions ~f:(fun event ->
                 P.Id.Moderator_execution.equal expected.context.id event.context.id))));
        assert (Option.is_none (T.prepare_stop after |> protocol_ok));
        let stopped =
          A.Session_delta.apply
            before
            (T.prepare_stop before |> protocol_ok |> Option.value_exn)
          |> protocol_ok
        in
        [%test_eq: int64]
          (Int64.succ (G.revision goal))
          (G.revision (Goal_run_tests.current_goal stopped));
        assert (
          List.equal
            A.Goal_command_receipt.equal
            (C.receipts catalog)
            (C.receipts (Option.value_exn stopped.goal_catalog)));
        print_endline
          (match action with
           | Continue -> "Continue"
           | Wait _ -> "Wait"
           | Finish _ -> "Finish")));
  [%expect
    {|
Continue
Wait
Finish
|}]
;;

let%expect_test "Stop safety transition does not need a user command receipt slot" =
  Goal_run_tests.with_model (fun ~basis:_ ~held:_ ~admission:_ ~intent:_ ~delta:_ state ->
    let run = state.run_state |> Option.value_exn |> A.Run_state.runs |> List.hd_exn in
    let initial = Option.value_exn state.goal_catalog in
    let filled =
      List.init (256 - List.length (C.receipts initial)) ~f:Fn.id
      |> List.fold ~init:initial ~f:(fun catalog i ->
        command
          catalog
          run
          ("control-capacity-" ^ Int.to_string i)
          (Record_progress
             ( Goal_run_tests.goal_id
             , G.Progress.of_string (Int.to_string i) |> protocol_ok )))
    in
    let before = { state with goal_catalog = Some filled } in
    let after =
      A.Session_delta.apply
        before
        (T.prepare_stop before |> protocol_ok |> Option.value_exn)
      |> protocol_ok
    in
    let held = Goal_run_tests.current_goal after in
    [%test_eq: int] 256 (List.length (C.receipts (Option.value_exn after.goal_catalog)));
    assert (
      List.equal
        A.Goal_command_receipt.equal
        (C.receipts filled)
        (C.receipts (Option.value_exn after.goal_catalog)));
    assert (G.State.equal (Open (Held Operator_pause)) (G.state held));
    [%test_eq: int64]
      (Int64.succ (G.revision (Goal_run_tests.current_goal before)))
      (G.revision held);
    assert (Option.is_none (T.prepare_stop after |> protocol_ok));
    print_endline "all 256 receipts retained; one Goal revision; repeat is inert");
  [%expect {| all 256 receipts retained; one Goal revision; repeat is inert |}]
;;
