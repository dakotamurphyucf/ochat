open! Core
open Fixture

let%expect_test "actual snapshots replace; sealed corrections stop at retirement" =
  let goal, episode = goal () in
  let l0 = make_ledger ~attempts:1 () in
  let a0 = A.create goal ~ledger:l0 |> account in
  let l1, a1, h1, _ = admitted a0 goal episode l0 in
  let l2 = put l1 h1 0L (actual_views 7L) in
  let l3 =
    L.observe
      l2
      h1
      (usage
         ~inclusions:
           [ { subset = Cached_input; included_in = Input }
           ; { subset = Reasoning_output; included_in = Output }
           ]
         h1
         ~revision:1L
         (actual_views 11L))
    |> ledger
    |> fst
  in
  let l4, disposition =
    L.observe l3 h1 (usage h1 ~revision:0L (actual_views 99L)) |> ledger
  in
  assert (L.equal_observation_disposition disposition Stale);
  assert (U.Metric.equal (metric a1 l3 Input) (metric a1 l4 Input));
  assert (U.Sum.equal_sum (U.Metric.actual (metric a1 l4 Input)) (Tokens 11L));
  let l5 = L.set_state l4 h1 interrupted |> ledger in
  let l6, a2, _, _ = admitted a1 goal episode l5 in
  assert (Int64.equal (A.settled_tracked_attempts a2) 1L);
  assert (List.length (A.bindings a2) = 1);
  assert (Result.is_error (A.usage a1 ~ledger:l6));
  let l7, disposition =
    L.observe l6 h1 (usage h1 ~revision:2L (actual_views 1L)) |> ledger
  in
  assert (L.equal_observation_disposition disposition Ignored_retired);
  assert (U.Sum.equal_sum (U.Metric.actual (metric a2 l7 Input)) (Tokens 11L));
  assert (
    Int64.equal (U.Unknown.count (U.Metric.unknown (metric a2 l7 Input)) Not_submitted) 1L);
  assert (Int64.equal (A.admitted_attempts a2) 2L);
  assert (Int64.equal (A.admitted_turns a2) 0L);
  assert (
    Result.is_error
      (A.plan
         a2
         ~goal
         ~before:l5
         ~after:l6
         ~admission:
           (Some
              (Attempt
                 { episode
                 ; handle = L.rows l6 |> List.hd_exn |> L.Row.handle
                 ; tracking = Tracked
                 }))));
  print_endline
    "7→11 replaces; stale adds zero; final11 sealed before retirement; late correction \
     ignored; attempts2 turns0";
  [%expect
    {| 7→11 replaces; stale adds zero; final11 sealed before retirement; late correction ignored; attempts2 turns0 |}]
;;

let%expect_test "unknown, exact zero, estimates and untracked remain independent" =
  let goal, episode = goal () in
  let l0 = make_ledger ~attempts:2 () in
  let a0 = A.create goal ~ledger:l0 |> account in
  let l1, a1, h1, _ = admitted a0 goal episode l0 in
  assert (
    Int64.equal (U.Unknown.count (U.Metric.unknown (metric a1 l1 Input)) Not_submitted) 1L);
  let l1 = L.set_state l1 h1 Running |> ledger in
  assert (
    Int64.equal (U.Unknown.count (U.Metric.unknown (metric a1 l1 Input)) Not_reported) 1L);
  let l1 = L.set_state l1 h1 interrupted |> ledger in
  assert (
    Int64.equal (U.Unknown.count (U.Metric.unknown (metric a1 l1 Input)) Interrupted) 1L);
  let estimator method_ version = O.Estimator.create ~method_ ~version |> observed in
  let views =
    [ O.Usage.Component.Input, O.Count.Actual 0L
    ; Output, Estimated { tokens = 3L; estimator = estimator Utf8_heuristic 1 }
    ; Reported_total, Unknown Not_reported
    ; Cached_input, Unknown Explicit_null
    ; Cache_write_input, Unknown Before_tracking
    ; Reasoning_output, Unknown Interrupted
    ]
  in
  let l2 = put l1 h1 0L views in
  assert (Int64.equal (U.Metric.actual_attempts (metric a1 l2 Input)) 1L);
  assert (
    Int64.equal
      (U.Unknown.count (U.Metric.unknown (metric a1 l2 Reported_total)) Not_reported)
      1L);
  let l3, a2, h2, _ = admitted a1 goal episode l2 in
  let l4 =
    put
      l3
      h2
      0L
      (List.map views ~f:(fun (component, value) ->
         ( component
         , match component with
           | Output ->
             O.Count.Estimated
               { tokens = 4L; estimator = estimator O200k_serialized_history 2 }
           | Input | Reported_total | Cached_input | Cache_write_input | Reasoning_output
             -> value )))
  in
  assert (U.Metric.mixed_estimators (metric a2 l4 Output));
  let l5 = L.set_state l4 h2 Running |> ledger in
  let l6, a3, h3, _ = admitted a2 goal episode l5 in
  let l7, a4, h4, tracking = admitted a3 goal episode l6 in
  assert (L.equal_tracking tracking (Untracked Attempt_count));
  assert (Int64.equal (A.untracked_attempts a4) 1L);
  assert (U.Metric.mixed_estimators (metric a4 l7 Output));
  List.iter components ~f:(fun component ->
    assert (Int64.equal (U.Metric.untracked_attempts (metric a4 l7 component)) 1L));
  let l8, disposition =
    L.observe l7 h4 (usage h4 ~revision:0L (actual_views 100L)) |> ledger
  in
  assert (L.equal_observation_disposition disposition Ignored_untracked);
  assert (U.equal (A.usage a4 ~ledger:l7 |> account) (A.usage a4 ~ledger:l8 |> account));
  ignore (h3 : L.Handle.t);
  print_endline
    "actual zero differs from absence; independent components; mixed estimates survive \
     sealing; untracked is never zero";
  [%expect
    {| actual zero differs from absence; independent components; mixed estimates survive sealing; untracked is never zero |}]
;;

let%expect_test "checked lifetime counters and live versus sealed token overflow" =
  let module C = Agent_session.Goal_accounting_counter in
  let checked result =
    Result.map_error result ~f:(fun error -> Sexp.to_string_hum (C.sexp_of_error error))
    |> Result.ok_or_failwith
  in
  let maximum = C.of_int64 Int64.max_value |> checked in
  assert (Result.is_error (C.of_int64 (-1L)));
  assert (C.equal (C.add maximum C.zero |> checked) maximum);
  assert (Result.is_error (C.succ maximum));
  assert (Result.is_error (C.add maximum (C.of_int64 1L |> checked)));
  let goal, episode = goal () in
  let l0 = make_ledger ~attempts:2 () in
  let a0 = A.create goal ~ledger:l0 |> account in
  let l1, a1, h1, _ = admitted a0 goal episode l0 in
  let l1 = put l1 h1 0L (actual_views Int64.max_value) in
  let l2, a2, h2, _ = admitted a1 goal episode l1 in
  let l2 = put l2 h2 0L (actual_views 1L) in
  assert (U.Sum.equal_sum (U.Metric.actual (metric a2 l2 Input)) Overflow);
  let l3 = put l2 h1 1L (actual_views 0L) in
  assert (U.Sum.equal_sum (U.Metric.actual (metric a2 l3 Input)) (Tokens 1L));
  let l0 = make_ledger ~attempts:1 () in
  let a0 = A.create goal ~ledger:l0 |> account in
  let l1, a1, h1, _ = admitted a0 goal episode l0 in
  let l1 =
    L.set_state (put l1 h1 0L (actual_views Int64.max_value)) h1 interrupted |> ledger
  in
  let l2, a2, h2, _ = admitted a1 goal episode l1 in
  let l2 = L.set_state (put l2 h2 0L (actual_views 1L)) h2 interrupted |> ledger in
  let l3, a3, _, _ = admitted a2 goal episode l2 in
  assert (U.Sum.equal_sum (U.Metric.actual (metric a3 l3 Input)) Overflow);
  let l4 = put l3 h1 1L (actual_views 0L) in
  assert (U.Sum.equal_sum (U.Metric.actual (metric a3 l4 Input)) Overflow);
  print_endline
    "counters reject exhaustion; live replacement recovers exact sum; sealed overflow \
     cannot be corrected after retirement";
  [%expect
    {| counters reject exhaustion; live replacement recovers exact sum; sealed overflow cannot be corrected after retirement |}]
;;

let%expect_test "actual host turn admission and closed partitions preserve goal scope" =
  let first, first_episode = goal () in
  let second, second_episode = goal ~suffix:"two" () in
  let l0 = make_ledger ~attempts:1 ~turns:1 () in
  let a0 = A.create first ~ledger:l0 |> account in
  let op1 = operation 1 in
  let l1, turn, tracking = L.admit_turn l0 op1 |> ledger in
  let a1 =
    step
      a0
      first
      l0
      l1
      (Some (Turn { episode = first_episode; operation = op1; handle = turn; tracking }))
  in
  let l2, turn, tracking = L.admit_turn l1 op1 |> ledger in
  let duplicate =
    A.plan
      a1
      ~goal:first
      ~before:l1
      ~after:l2
      ~admission:
        (Some (Turn { episode = first_episode; operation = op1; handle = turn; tracking }))
    |> account
  in
  assert (
    A.Plan.equal_admission_disposition
      (A.Plan.admission_disposition duplicate)
      Already_retained);
  assert (A.equal a1 (A.Plan.candidate duplicate));
  let op2 = operation 2 in
  let l3, handle, tracking = L.admit_turn l2 op2 |> ledger in
  assert (L.equal_tracking tracking (Untracked Turn_count));
  let a2 =
    step
      a1
      first
      l2
      l3
      (Some (Turn { episode = first_episode; operation = op2; handle; tracking }))
  in
  assert (Int64.equal (A.admitted_turns a2) 2L);
  assert (Int64.equal (A.untracked_turns a2) 1L);
  let l4, a3, h1, _ = admitted a2 first first_episode l3 in
  let terminal =
    Inference.Event.Terminal.create
      ~scope:(L.Handle.scope h1)
      ~delivery:Definitely_not_submitted
      ~outcome:(Failed (Authentication Missing))
    |> Result.ok_or_failwith
  in
  let l5 = L.set_state l4 h1 (Terminal terminal) |> ledger in
  assert (
    Int64.equal (U.Unknown.count (U.Metric.unknown (metric a3 l5 Input)) Not_submitted) 1L);
  let l5 = put l5 h1 0L (actual_views 5L) in
  let closed = G.abandon first (G.Reason.of_string "Stopped" |> protocol) |> protocol in
  let b0 = A.create second ~ledger:l5 |> account in
  assert (Int64.equal (U.attempts (A.usage b0 ~ledger:l5 |> account)) 0L);
  let l6, h2, tracking = admit l5 in
  let a4 = step a3 closed l5 l6 None in
  let b1 =
    step
      b0
      second
      l5
      l6
      (Some (Attempt { episode = second_episode; handle = h2; tracking }))
  in
  assert (
    Int64.equal (A.admitted_attempts a4) 1L && Int64.equal (A.admitted_attempts b1) 1L);
  assert (List.is_empty (A.bindings a4));
  assert (U.Sum.equal_sum (U.Metric.actual (metric a4 l6 Input)) (Tokens 5L));
  assert (Result.is_error (A.plan a4 ~goal:second ~before:l6 ~after:l6 ~admission:None));
  assert (
    Result.is_error
      (A.plan
         b0
         ~goal:second
         ~before:l5
         ~after:l6
         ~admission:(Some (Attempt { episode = first_episode; handle = h2; tracking }))));
  let start = make_ledger ~attempts:2 () in
  let limited = A.create ~max_bindings:1 second ~ledger:start |> account in
  let next, limited, _, _ = admitted limited second second_episode start in
  let final, handle, tracking = admit next in
  assert (
    Result.is_error
      (A.plan
         limited
         ~goal:second
         ~before:next
         ~after:final
         ~admission:(Some (Attempt { episode = second_episode; handle; tracking }))));
  let l6 = L.set_state l6 h2 interrupted |> ledger in
  let terminal_op = { op1 with state = Completed } in
  let l6 = L.finish_turn l6 turn terminal_op |> ledger in
  let l7 = L.with_generation l6 ~generation:1 |> ledger in
  let adopted = G.adopt_source second (source ~generation:1 ~epoch:2L ()) |> protocol in
  let b2 = step b1 adopted l6 l7 None in
  assert (A.equal b1 b2);
  assert (Int64.equal (A.admitted_attempts b2) 1L);
  let same_generation = G.adopt_source second (source ~epoch:2L ()) |> protocol in
  List.iteri
    [ second, l7; adopted, l7; same_generation, l6 ]
    ~f:(fun index (current_goal, before) ->
      let after, handle, tracking = admit before in
      assert (
        Result.is_error
          (A.plan
             b2
             ~goal:current_goal
             ~before
             ~after
             ~admission:(Some (Attempt { episode = second_episode; handle; tracking }))));
      let operation = { (operation (3 + index)) with generation = L.generation before } in
      let after, handle, tracking = L.admit_turn before operation |> ledger in
      assert (
        Result.is_error
          (A.plan
             b2
             ~goal:current_goal
             ~before
             ~after
             ~admission:
               (Some (Turn { episode = second_episode; operation; handle; tracking }))));
      let tokens = Int64.of_int (8 + index) in
      let corrected = put before h2 0L (actual_views tokens) in
      let same = step b2 current_goal before corrected None in
      assert (A.equal b2 same);
      assert (
        U.Sum.equal_sum (U.Metric.actual (metric same corrected Input)) (Tokens tokens));
      let after, _, _ = admit corrected in
      let sealed = step same current_goal corrected after None in
      assert (List.is_empty (A.bindings sealed));
      assert (
        U.Sum.equal_sum (U.Metric.actual (metric sealed after Input)) (Tokens tokens)));
  print_endline
    "turn duplicates do not recount; untracked host turns counted; closed old partition \
     seals independently; identity/generation preserved; binding limit gates; stale \
     source admissions reject while old corrections seal";
  [%expect
    {| turn duplicates do not recount; untracked host turns counted; closed old partition seals independently; identity/generation preserved; binding limit gates; stale source admissions reject while old corrections seal |}]
;;
