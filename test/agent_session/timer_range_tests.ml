open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module Setup = Subscription_transaction_tests
module Timers = Schedule_transaction_tests
module Clock = Extension_clock_tests

let stamp ns =
  Int63.of_int64_exn ns |> Time_ns.of_int63_ns_since_epoch |> P.Timestamp.of_time_ns
;;

let create actor owner delay_ms =
  A.create_script_schedule
    actor
    ~owner
    ~source:Setup.source
    ~delay_ms
    ~payload:(`String "range")
    ~misfire:Deliver_once_immediately
;;

let%expect_test "wide timer policies preserve exact live and recovered durations" =
  List.iter [ `Short; `Across_epoch; `At_boundary ] ~f:(fun mode ->
    let wall_now = ref timestamp
    and elapsed = ref Mtime.min_stamp in
    let delay_ms, created_at =
      match mode with
      | `Short -> 1000, timestamp
      | `Across_epoch -> 8_000_000_000_000, stamp (-4_000_000_000_000_000_000L)
      | `At_boundary -> 1, stamp Int64.(Int63.to_int64 Int63.max_value - 1_000_000L)
    in
    with_actor
      ~now:(fun () -> !wall_now)
      ~monotonic_now:(fun () -> !elapsed)
      ~schedule_limits:
        { Agent_session.Staged_schedules.default_limits with
          max_delay_ms = Int.max_value
        }
      (fun env sw actor _writer backend ->
         A.change_moderator actor (Some (Setup.encode Setup.before))
         |> protocol_ok
         |> ignore;
         let parent = add_claimed_job actor in
         Timers.with_event ~deadline:None actor parent (fun owner commit ->
           wall_now := created_at;
           let receipt, timer = create actor owner delay_ms |> protocol_ok in
           [%test_eq: int64]
             Int64.(of_int delay_ms * 1_000_000L)
             (P.Timestamp.diff_ns timer.next_due_at timer.created_at);
           let decoded = P.Schedule.of_json (P.Schedule.to_json timer) |> protocol_ok in
           assert (P.Timestamp.equal decoded.next_due_at timer.next_due_at);
           A.select_schedule_mutations
             actor
             ~owner
             ~source:Setup.source
             ~receipts:[ receipt ]
           |> protocol_ok;
           (* Keep the existing parent's absolute deadline valid at checkpoint. *)
           wall_now := timestamp;
           Timers.save commit)
         |> protocol_ok
         |> ignore;
         let state = A.state actor |> protocol_ok in
         assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
         let duration_ns = Int64.(of_int delay_ms * 1_000_000L) in
         elapsed := Mtime.of_uint64_ns Int64.(duration_ns - 1L);
         assert (List.is_empty (snd (A.due_schedules actor |> protocol_ok)));
         elapsed := Mtime.of_uint64_ns duration_ns;
         [%test_eq: int] 1 (List.length (snd (A.due_schedules actor |> protocol_ok)));
         A.shutdown actor;
         wall_now := created_at;
         elapsed := Mtime.min_stamp;
         Clock.reopen ~env ~sw ~initial:state ~wall_now ~elapsed (fun restored ->
           elapsed := Mtime.of_uint64_ns Int64.(duration_ns - 1L);
           assert (List.is_empty (snd (A.due_schedules restored |> protocol_ok)));
           elapsed := Mtime.of_uint64_ns duration_ns;
           let ready = snd (A.due_schedules restored |> protocol_ok) in
           [%test_eq: int] 1 (List.length ready);
           print_s
             [%sexp
               (mode : [ `Short | `Across_epoch | `At_boundary ])
             , ("live/recovered exact" : string)])));
  [%expect
    {|
    (Short "live/recovered exact")
    (Across_epoch "live/recovered exact")
    (At_boundary "live/recovered exact")
    |}]
;;

let%expect_test
    "overflowing requests and forged out-of-policy staging leave no reservations"
  =
  List.iter [ `Wrapped_duration; `Endpoint_overflow; `Fractional_excess ] ~f:(fun mode ->
    let wall_now = ref timestamp in
    with_actor
      ~now:(fun () -> !wall_now)
      ~monotonic_now:(fun () -> Mtime.min_stamp)
      ~schedule_limits:
        { Agent_session.Staged_schedules.default_limits with
          max_delay_ms =
            (match mode with
             | `Fractional_excess -> 1000
             | _ -> Int.max_value)
        ; max_per_source = 1
        }
      (fun _env _sw actor _writer backend ->
         A.change_moderator actor (Some (Setup.encode Setup.before))
         |> protocol_ok
         |> ignore;
         let parent = add_claimed_job actor in
         Timers.with_event ~deadline:None actor parent (fun owner commit ->
           let result =
             match mode with
             | `Wrapped_duration ->
               create actor owner 9_223_372_037_854 |> Result.map ~f:ignore
             | `Endpoint_overflow ->
               wall_now := stamp Int64.(Int63.to_int64 Int63.max_value - 500_000L);
               create actor owner 1 |> Result.map ~f:ignore
             | `Fractional_excess ->
               let receipt, timer = create actor owner 1000 |> protocol_ok in
               A.abort_schedule_mutation actor ~owner ~receipt |> protocol_ok;
               let next =
                 { timer with
                   next_due_at =
                     P.Timestamp.to_time_ns timer.next_due_at
                     |> Fn.flip Time_ns.add (Time_ns.Span.of_int_ns 1)
                     |> P.Timestamp.of_time_ns
                 }
               in
               A.stage_schedule_mutation
                 actor
                 ~owner
                 ~source:Setup.source
                 ~previous:None
                 ~next
               |> Result.map ~f:ignore
           in
           (match result with
            | Ok () -> failwith "invalid timer accepted"
            | Error error ->
              print_s
                [%sexp
                  (mode : [ `Wrapped_duration | `Endpoint_overflow | `Fractional_excess ])
                , (error.code : P.Error.code)]);
           assert (List.is_empty (Agent_session.Memory_backend.state backend).schedules);
           wall_now := timestamp;
           let receipt, _ = create actor owner 1000 |> protocol_ok in
           A.select_schedule_mutations
             actor
             ~owner
             ~source:Setup.source
             ~receipts:[ receipt ]
           |> protocol_ok;
           Timers.save commit)
         |> protocol_ok
         |> ignore;
         let state = A.state actor |> protocol_ok in
         [%test_eq: int] 1 (List.length state.schedules);
         assert_same_session_snapshot state (Agent_session.Memory_backend.state backend)));
  [%expect
    {|
    (Wrapped_duration Invalid_request)
    (Endpoint_overflow Invalid_request)
    (Fractional_excess Permission_denied)
    |}]
;;
