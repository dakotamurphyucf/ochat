open! Core
open Fixtures
module P = Agent_protocol

let correlated t (view : P.Run_query.View.t) =
  let original = admission t in
  assert (P.Id.Run.equal view.run.id original.run_id);
  assert (P.Id.Principal.equal view.run.principal_id original.principal_id);
  assert (P.Run_source.equal view.run.source original.source);
  assert (Option.exists view.admission_receipt ~f:(P.Run_receipt.equal original))
;;

let terminal (view : P.Run_query.View.t) =
  match view.run.lifecycle with
  | Terminal _ -> true
  | Admitted | Active | Waiting _ -> false
;;

let%expect_test "inert successful single-turn startup completes without fabricated work" =
  with_run ~mode:Single_turn ~startup:"Task.pure(state)" ~f:(fun t ->
    let view = await t ~f:terminal in
    correlated t view;
    assert (P.Run.Lifecycle.equal view.run.lifecycle (Terminal (Completed None)));
    assert (List.is_empty view.run.owned_work);
    assert (List.is_empty view.run.terminal_work);
    assert (Option.is_none view.pending_action);
    let receipt = Option.value_exn view.terminal_receipt in
    assert (P.Run_receipt.Kind.equal receipt.kind Terminal);
    assert (P.Id.Run.equal receipt.run_id view.run.id);
    assert (P.Run_source.equal receipt.source view.run.source);
    let current = snapshot t in
    assert (Option.is_none current.session.active_operation);
    [%test_eq: int] (requests t) 0;
    print_endline "completed startup; original admission; zero work; zero provider calls");
  [%expect {| completed startup; original admission; zero work; zero provider calls |}]
;;

let%expect_test "same authored continuation has explicit single-turn or workflow lifetime"
  =
  List.iter [ P.Run.Mode.Single_turn; Workflow ] ~f:(fun mode ->
    with_run
      ~mode
      ~startup:"Task.bind(Run.continue(), fun ignored -> Task.pure(state))"
      ~f:(fun t ->
        let view =
          await t ~f:(fun view ->
            List.exists view.run.terminal_work ~f:(fun proof ->
              match proof.work.key with
              | Operation _ -> P.Run_work.Terminal.equal_outcome proof.outcome Succeeded
              | Retained _ -> false))
        in
        correlated t view;
        [%test_eq: int] (List.length view.run.owned_work) 1;
        [%test_eq: int] (requests t) 1;
        (match mode, view.run.lifecycle with
         | Single_turn, Terminal (Completed None) ->
           assert (Option.is_some view.terminal_receipt)
         | Workflow, Active -> assert (Option.is_none view.terminal_receipt)
         | _ -> failwith "mode did not retain its actual operation settlement rule");
        let replay = query t in
        assert (P.Run.equal replay.run view.run);
        match mode with
        | Single_turn -> print_endline "Single_turn: completed after owned operation"
        | Workflow -> print_endline "Workflow: active after owned operation"));
  [%expect
    {|
    Single_turn: completed after owned operation
    Workflow: active after owned operation
    |}]
;;

let%test_unit "single-turn no-work completion preserves actual session-end disposition" =
  with_run
    ~mode:Single_turn
    ~startup:
      "Task.bind(Runtime.end_session(\"startup ended\"), fun ignored -> Task.pure(state))"
    ~f:(fun t ->
      let view =
        await t ~f:(fun view ->
          let current = snapshot t in
          terminal view
          &&
          match current.session.desired_state, current.session.observed_state with
          | Stopped, Stopped -> true
          | _ -> false)
      in
      correlated t view;
      assert (P.Run.Lifecycle.equal view.run.lifecycle (Terminal (Completed None)));
      let current = snapshot t in
      assert current.halted;
      [%test_eq: int] (requests t) 0)
;;
