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

let%expect_test
    "scoped fresh policy separates capacity deferral and combined observations"
  =
  (* Pure policy algebra only: these projections are not runtime authority. *)
  let before = make_ledger () in
  let goal, episode = goal () in
  let goal = G.resume goal |> protocol in
  let accounting = A.create goal ~ledger:before |> account in
  let current, accounting, handle, _ = admitted accounting goal episode before in
  let current = put current handle 0L (actual_views 3L) in
  let descendants_usage =
    U.add_record
      U.empty
      (L.Row.record
         (L.find current ~ordinal:(L.Handle.ordinal handle) |> Option.value_exn))
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (U.Counter.sexp_of_error error))
    |> Result.ok_or_failwith
  in
  let selected = List.hd_exn components in
  let with_stop =
    G.change_budget
      goal
      (G.Budget.create
         ~admitted_turns:None
         ~admitted_attempts:None
         ~observed_tokens:
           (Some (G.Budget.Token_stop.create ~component:selected ~ceiling:5L |> protocol))
         ~deadline:None
       |> protocol)
    |> protocol
  in
  let check goal usage coverage_unknown availability =
    Policy.check_scoped
      goal
      ~accounting
      ~ledger:current
      ~descendants:Policy.Descendants.{ usage; coverage_unknown }
      ~availability
      ~now:timestamp
      ~dimension:Attempt
    |> protocol
  in
  (* This readonly algebra deliberately supplies a second 3-token partition;
     the actual State adapter, not this model, authenticates disjoint provenance. *)
  (match check with_stop descendants_usage false Available with
   | Decided (Hold (Budget_gate Observed_tokens)) -> ()
   | _ -> failwith "combined known subtotal did not stop fresh work");
  (match check with_stop U.empty true Available with
   | Decided (Hold (Usage_uncertain component)) ->
     assert (O.Usage.Component.equal component selected)
   | _ -> failwith "selected incomplete coverage did not hold");
  (match check goal U.empty true Available with
   | Decided Allow -> ()
   | _ -> failwith "no-token policy fabricated an uncertainty hold");
  List.iter [ Policy.Count_availability.Reserved; Unknown ] ~f:(fun availability ->
    match check goal U.empty false availability with
    | Deferred reason ->
      (match availability, reason with
       | Reserved, Reserved | Unknown, Unknown -> ()
       | _ -> failwith "capacity reason changed")
    | Decided _ -> failwith "capacity deferral became a durable Hold");
  (match check goal U.empty true Exhausted with
   | Decided (Hold (Budget_gate Admitted_attempts)) -> ()
   | _ -> failwith "known exhausted lower bound did not hold");
  print_endline
    "combined subtotal stops; selected uncertainty holds; pending stays deferred";
  [%expect
    {| combined subtotal stops; selected uncertainty holds; pending stays deferred |}]
;;

let%expect_test "legacy policy and empty-complete scoped policy preserve gate ordering" =
  let before = make_ledger () in
  let goal, episode = goal () in
  let goal = G.resume goal |> protocol in
  let accounting = A.create goal ~ledger:before |> account in
  let current, accounting, handle, _ = admitted accounting goal episode before in
  let current = put current handle 0L (actual_views 3L) in
  let descendants = Policy.Descendants.{ usage = U.empty; coverage_unknown = false } in
  let compare_result left right =
    match left, right with
    | Ok left, Ok right -> assert_decision left right
    | Error left, Error right ->
      assert (Sexp.equal (P.Error.sexp_of_t left) (P.Error.sexp_of_t right))
    | Ok _, Error _ | Error _, Ok _ -> failwith "legacy policy outcome changed"
  in
  let scoped goal ledger availability dimension =
    Policy.check_scoped
      goal
      ~accounting
      ~ledger
      ~descendants
      ~availability
      ~now:timestamp
      ~dimension
    |> Result.map ~f:(function
      | Decided decision -> decision
      | Deferred _ -> failwith "complete empty projection unexpectedly deferred")
  in
  let compare_activation goal ledger =
    compare_result
      (Policy.check_activation goal ~accounting ~ledger ~now:timestamp)
      (Policy.check_scoped_activation
         goal
         ~accounting
         ~ledger
         ~descendants
         ~now:timestamp)
  in
  let budget ?(counts = false) ?(tokens = false) ?(expired = false) () =
    G.Budget.create
      ~admitted_turns:(if counts then Some 1L else None)
      ~admitted_attempts:(if counts then Some 1L else None)
      ~observed_tokens:
        (if tokens
         then Some (G.Budget.Token_stop.create ~component:Input ~ceiling:3L |> protocol)
         else None)
      ~deadline:(Option.some_if expired timestamp)
    |> protocol
  in
  let verify goal availability expected_activation expected_attempt expected_turn =
    compare_activation goal current;
    assert_decision
      (Policy.check_activation goal ~accounting ~ledger:current ~now:timestamp |> protocol)
      expected_activation;
    List.iter
      [ Policy.Attempt, availability, expected_attempt
      ; Host_turn, Policy.Count_availability.Available, expected_turn
      ]
      ~f:(fun (dimension, availability, expected) ->
        let old =
          Policy.check goal ~accounting ~ledger:current ~now:timestamp ~dimension
        in
        compare_result old (scoped goal current availability dimension);
        assert_decision (old |> protocol) expected)
  in
  verify goal Available Allow Allow Allow;
  let counts = G.change_budget goal (budget ~counts:true ()) |> protocol in
  verify counts Exhausted Allow (Hold (Budget_gate Admitted_attempts)) Allow;
  let tokens = G.change_budget goal (budget ~counts:true ~tokens:true ()) |> protocol in
  verify
    tokens
    Exhausted
    (Hold (Budget_gate Observed_tokens))
    (Hold (Budget_gate Admitted_attempts))
    (Hold (Budget_gate Observed_tokens));
  let expired =
    G.change_budget goal (budget ~counts:true ~tokens:true ~expired:true ()) |> protocol
  in
  verify
    expired
    Exhausted
    (Hold Deadline_reached)
    (Hold Deadline_reached)
    (Hold Deadline_reached);
  let held = G.hold expired Operator_pause |> protocol in
  verify held Exhausted (Hold Operator_pause) (Hold Operator_pause) (Hold Operator_pause);
  let closed =
    G.abandon goal (G.Reason.of_string "model closed" |> protocol) |> protocol
  in
  compare_activation closed current;
  assert (
    Result.is_error
      (Policy.check_activation closed ~accounting ~ledger:current ~now:timestamp));
  let wrong, _ = Fixture.goal ~suffix:"wrong-scope" () in
  List.iter
    [ wrong, current; held, before; closed, before ]
    ~f:(fun (goal, ledger) ->
      let old = Policy.check_activation goal ~accounting ~ledger ~now:timestamp in
      assert (Result.is_error old);
      compare_activation goal ledger;
      compare_result
        (Policy.check goal ~accounting ~ledger ~now:timestamp ~dimension:Attempt)
        (scoped goal ledger Available Attempt));
  print_endline
    "account integrity first; control/deadline/count/token precedence unchanged";
  [%expect
    {| account integrity first; control/deadline/count/token precedence unchanged |}]
;;
