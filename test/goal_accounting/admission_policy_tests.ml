open! Core
open Fixture
module Policy = Agent_session.Goal_admission_policy

let assert_decision actual expected =
  [%test_pred: Policy.decision] (Policy.equal_decision expected) actual
;;

let check goal accounting current dimension =
  Policy.check goal ~accounting ~ledger:current ~now:timestamp ~dimension |> protocol
;;

let%expect_test "fresh count gates use independent pre-admission lifetime counters" =
  (* These are pure accounting model admissions, not runtime permissions. *)
  List.iter [ Policy.Attempt; Host_turn ] ~f:(fun dimension ->
    List.iter [ 0L; 1L; 2L ] ~f:(fun ceiling ->
      let before = make_ledger () in
      let goal, episode = goal () in
      let goal =
        G.change_budget
          goal
          (G.Budget.create
             ~admitted_turns:
               (if Policy.equal_dimension dimension Host_turn then Some ceiling else None)
             ~admitted_attempts:
               (if Policy.equal_dimension dimension Attempt then Some ceiling else None)
             ~observed_tokens:None
             ~deadline:None
           |> protocol)
        |> protocol
        |> G.resume
        |> protocol
      in
      let accounting = A.create goal ~ledger:before |> account in
      let expected count =
        if Int64.(count < ceiling)
        then Policy.Allow
        else
          Policy.Hold
            (Budget_gate
               (match dimension with
                | Attempt -> Admitted_attempts
                | Host_turn -> Admitted_turns))
      in
      assert_decision (check goal accounting before dimension) (expected 0L);
      let after, accounting =
        match dimension with
        | Attempt ->
          let after, accounting, _, _ = admitted accounting goal episode before in
          after, accounting
        | Host_turn ->
          let operation = operation 1 in
          let after, handle, tracking = L.admit_turn before operation |> ledger in
          ( after
          , step
              accounting
              goal
              before
              after
              (Some (A.Admission.Turn { episode; operation; handle; tracking })) )
      in
      assert_decision (check goal accounting after dimension) (expected 1L)));
  print_endline "zero denies; last fresh allocation is allowed; dimensions stay separate";
  [%expect {| zero denies; last fresh allocation is allowed; dimensions stay separate |}]
;;

let%expect_test "deadline and selected token component remain honest independent gates" =
  let before = make_ledger () in
  let goal, episode = goal () in
  let goal = G.resume goal |> protocol in
  let accounting = A.create goal ~ledger:before |> account in
  let current, accounting, handle, _ = admitted accounting goal episode before in
  assert_decision (check goal accounting current Attempt) Allow;
  List.iter components ~f:(fun selected ->
    let token_stop =
      G.Budget.Token_stop.create ~component:selected ~ceiling:5L |> protocol
    in
    let with_stop =
      G.change_budget
        goal
        (G.Budget.create
           ~admitted_turns:None
           ~admitted_attempts:None
           ~observed_tokens:(Some token_stop)
           ~deadline:None
         |> protocol)
      |> protocol
    in
    assert_decision
      (check with_stop accounting current Attempt)
      (Hold (Usage_uncertain selected));
    let views =
      List.map components ~f:(fun component ->
        ( component
        , O.Count.Actual (if O.Usage.Component.equal component selected then 4L else 100L)
        ))
    in
    let below = put current handle 0L views in
    assert_decision (check with_stop accounting below Attempt) Allow;
    let reached = put below handle 1L (actual_views 5L) in
    assert_decision
      (check with_stop accounting reached Attempt)
      (Hold (Budget_gate Observed_tokens)));
  let expired =
    G.change_budget
      goal
      (G.Budget.create
         ~admitted_turns:None
         ~admitted_attempts:None
         ~observed_tokens:None
         ~deadline:(Some timestamp)
       |> protocol)
    |> protocol
  in
  assert_decision (check expired accounting current Attempt) (Hold Deadline_reached);
  let held = G.hold expired Deadline_reached |> protocol in
  let restored = G.to_json held |> G.of_json |> protocol in
  assert (G.equal held restored);
  print_endline
    "no-token policy allows unknown usage; selected counts and deadline persist";
  [%expect
    {| no-token policy allows unknown usage; selected counts and deadline persist |}]
;;
