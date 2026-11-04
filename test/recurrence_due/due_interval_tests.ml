open! Core
open Due_support

let%expect_test "interval planning agrees with enumerated slots and preserves one cursor" =
  let cases = ref 0 in
  List.iter [ A.Recurrence_policy.Skip; Coalesce; Catch_up 3 ] ~f:(fun misfire ->
    List.iter [ 0; 1000; 5000 ] ~f:(fun grace ->
      List.iter [ 0; 1; 999; 1000; 1001; 2999; 8000 ] ~f:(fun milliseconds ->
        let initial =
          definition "oracle" ~policy:(policy ~misfire ~grace_ms:(Int64.of_int grace) ())
          |> enabled
        in
        let now = snapshot ~wall:(at_ms milliseconds) ~elapsed_ms:milliseconds in
        let next, phase, changed = step initial ~phase:None now in
        assert changed;
        (* Independent tiny oracle enumerates each nominal, never the production range arithmetic. *)
        let due =
          List.range 0 10 |> List.filter ~f:(fun slot -> slot * 1000 <= milliseconds)
        in
        let eligible =
          List.filter due ~f:(fun slot -> milliseconds - (slot * 1000) <= grace)
        in
        let expected =
          match misfire with
          | Skip | Coalesce -> Option.to_list (List.last eligible)
          | Catch_up maximum -> List.take eligible maximum
        in
        [%test_eq: int64 list] (slots next) (List.map expected ~f:Int64.of_int);
        let summary = H.summary (frontier next) in
        [%test_eq: int64] summary.consumed (Int64.of_int (List.length due));
        [%test_eq: int64]
          summary.omitted
          (Int64.of_int (List.length due - List.length expected));
        let restored = restore next in
        assert (D.Json.equal (entry_json restored) (entry_json next));
        let _, _, changed = step restored ~phase now in
        assert (not changed);
        Int.incr cases)));
  List.iter [ A.Recurrence_policy.Skip; Coalesce; Catch_up 3 ] ~f:(fun misfire ->
    let initial =
      definition "sequential" ~policy:(policy ~misfire ~grace_ms:60000L ()) |> enabled
    in
    let first, phase, _ =
      step initial ~phase:None (snapshot ~wall:(at_ms 0) ~elapsed_ms:0)
    in
    let next, _, _ = step first ~phase (snapshot ~wall:(at_ms 10000) ~elapsed_ms:10000) in
    let expected, omitted, withdrawn =
      match misfire with
      | Skip -> [ 0L ], 10L, 0L
      | Coalesce -> [ 10L ], 9L, 1L
      | Catch_up _ -> [ 0L; 1L; 2L ], 8L, 0L
    in
    [%test_eq: int64 list] (slots next) expected;
    let summary = H.summary (frontier next) in
    [%test_eq: int64] summary.consumed 11L;
    [%test_eq: int64] summary.omitted omitted;
    [%test_eq: int64] summary.withdrawn withdrawn);
  let large = definition "large" |> enabled in
  let next, _, _ =
    step large ~phase:None (snapshot ~wall:(at_ms 1000000000) ~elapsed_ms:0)
  in
  [%test_eq: int64] (H.summary (frontier next)).consumed 1000001L;
  [%test_eq: int64 list] (slots next) [ 1000000L ];
  printf
    "%d enumerated cases; a million missed slots stay bounded; unchanged ticks are inert\n"
    !cases;
  [%expect
    {| 63 enumerated cases; a million missed slots stay bounded; unchanged ticks are inert |}]
;;

let%expect_test
    "live interval phase ignores wall jumps; restart and missed delay are explicit"
  =
  let initial = definition "clock" |> enabled in
  let armed, phase, _ =
    step initial ~phase:None (snapshot ~wall:(at_ms (-1000)) ~elapsed_ms:0)
  in
  let forward, phase, changed =
    step armed ~phase (snapshot ~wall:(at_ms 1000000000) ~elapsed_ms:500)
  in
  assert (not changed);
  [%test_eq: int64 list] (slots forward) [];
  let due, _, _ =
    step forward ~phase (snapshot ~wall:(at_ms (-1000000)) ~elapsed_ms:1000)
  in
  [%test_eq: int64 list] (slots due) [ 0L ];
  (* Independent elapsed subtraction oracle for the selected row, after the
     cursor advances. A corrected wall cannot extend its live dispatch grace. *)
  let selected_plan =
    match prepare forward ~phase (snapshot ~wall:(at_ms (-1000000)) ~elapsed_ms:1000) with
    | Plan.Candidate plan -> plan
    | Unchanged _ -> assert false
  in
  let anchor = List.hd_exn (Plan.selection_grace selected_plan) in
  let later = snapshot ~wall:(at_ms (-2000000)) ~elapsed_ms:1600 in
  [%test_eq: int64]
    (A.Recurrence_selection_grace.lateness_ns anchor ~snapshot:later |> protocol_ok)
    600_000_000L;
  assert (
    Result.is_error
      (A.Recurrence_selection_grace.lateness_ns
         anchor
         ~snapshot:(snapshot ~wall:(at_ms 0) ~elapsed_ms:999)));
  let overflowing =
    A.Recurrence_selection_grace.create
      ~occurrence:(A.Recurrence_selection_grace.occurrence anchor)
      ~snapshot:(snapshot ~wall:(at_ms 0) ~elapsed_ms:0)
      ~lateness_ns:Int64.max_value
    |> protocol_ok
  in
  assert (
    Result.is_error
      (A.Recurrence_selection_grace.lateness_ns
         overflowing
         ~snapshot:(snapshot ~wall:(at_ms 0) ~elapsed_ms:1)));
  let restarted, _, _ =
    step armed ~phase:None (snapshot ~wall:(at_ms 1000000000) ~elapsed_ms:0)
  in
  [%test_eq: int64 list] (slots restarted) [ 1000000L ];
  let rule =
    R.Rule.Interval.create
      ~mode:Fixed_delay
      ~period:(R.Rule.Period.create_ms 1000L |> timing)
  in
  let delay =
    definition
      "delay"
      ~schedule:(Interval { rule; first_due = at_ms 0 })
      ~policy:(policy ~misfire:Coalesce ~grace_ms:0L ())
    |> enabled
  in
  let now = snapshot ~wall:(at_ms 1) ~elapsed_ms:0 in
  let missed, _, changed = step delay ~phase:None now in
  assert changed;
  assert (
    Option.equal H.Issue.equal (H.issue (frontier missed)) (Some Initial_delay_missed));
  [%test_eq: int64] (H.summary (frontier missed)).consumed 0L;
  assert (A.Recurrence_cursor.equal (E.cursor delay) (E.cursor missed));
  let _, _, changed = step missed ~phase:None now in
  assert (not changed);
  (* A target completion can precede a newly opened controller's elapsed origin.
     Literal elapsed subtraction is independent of phase/clock constructors;
     wall rollback must not retime this live receipt-derived deadline. *)
  let next =
    R.Cursor.Interval.create ~first_due:(at_ms 0)
    |> R.Cursor.Interval.after_completion ~rule ~completed_at:(at_ms 0)
    |> timing
  in
  List.iter [ -2_000_000_000L; 0L; 500_000_000L ] ~f:(fun completion_elapsed_ns ->
    let phase =
      Phase.from_completion
        (E.current delay)
        ~cursor:next
        ~completed_at:(at_ms 0)
        ~completion_elapsed_ns
      |> protocol_ok
    in
    List.iter [ 0; 999; 1000; 1600; 4000 ] ~f:(fun elapsed_ms ->
      let elapsed_ns = Int64.(of_int elapsed_ms * 1_000_000L) in
      let current =
        Phase.Snapshot.create ~wall:(at_ms (-1000000)) ~elapsed_ns |> protocol_ok
      in
      let since_completion = Int64.(elapsed_ns - completion_elapsed_ns) in
      if Int64.(since_completion < 0L)
      then (
        assert (Result.is_error (Phase.due_count phase ~snapshot:current));
        assert (Result.is_error (Phase.lateness_ns phase ~snapshot:current)))
      else (
        let expected_late =
          if Int64.(since_completion < 1_000_000_000L)
          then None
          else Some Int64.(since_completion - 1_000_000_000L)
        in
        [%test_eq: int64 option]
          (Phase.lateness_ns phase ~snapshot:current |> protocol_ok)
          expected_late;
        [%test_eq: int64]
          (Phase.due_count phase ~snapshot:current |> protocol_ok)
          (if Option.is_some expected_late then 1L else 0L))));
  let overflow =
    Phase.from_completion
      (E.current delay)
      ~cursor:next
      ~completed_at:(at_ms 0)
      ~completion_elapsed_ns:Int64.min_value
    |> protocol_ok
  in
  assert (Result.is_error (Phase.lateness_ns overflow ~snapshot:now));
  assert (
    Result.is_error
      (Phase.from_completion
         (E.current delay)
         ~cursor:(R.Cursor.Interval.create ~first_due:(at_ms 2000))
         ~completed_at:(at_ms 0)
         ~completion_elapsed_ns:0L));
  assert (
    Result.is_error
      (Phase.from_completion
         (E.current initial)
         ~cursor:next
         ~completed_at:(at_ms 0)
         ~completion_elapsed_ns:0L));
  print_endline
    "live elapsed phase; honest restart catch-up; missed initial delay never invents \
     completion";
  [%expect
    {| live elapsed phase; honest restart catch-up; missed initial delay never invents completion |}]
;;
