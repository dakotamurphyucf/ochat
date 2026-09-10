open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module Setup = Subscription_transaction_tests
module Timers = Schedule_transaction_tests

let wall ms =
  Time_ns.add (P.Timestamp.to_time_ns timestamp) (Time_ns.Span.of_int_ms ms)
  |> P.Timestamp.of_time_ns
;;

let mono ms = Mtime.of_uint64_ns Int64.(of_int ms * 1_000_000L)

let create actor owner =
  A.create_script_schedule
    actor
    ~owner
    ~source:Setup.source
    ~delay_ms:1000
    ~payload:(`String "elapsed")
    ~misfire:Deliver_once_immediately
  |> protocol_ok
;;

let%expect_test
    "owned timers keep creation-time elapsed anchors across delayed saves, rollback and \
     wall jumps"
  =
  List.iter [ `Discarded; `Rejected ] ~f:(fun mode ->
    let wall_now = ref timestamp
    and elapsed = ref (mono 0) in
    let reject_save = ref false in
    with_actor
      ~now:(fun () -> !wall_now)
      ~monotonic_now:(fun () -> !elapsed)
      ~reject_save:(fun _ -> !reject_save)
      (fun _env _sw actor _writer backend ->
         A.change_moderator actor (Some (Setup.encode Setup.before))
         |> protocol_ok
         |> ignore;
         let parent = add_claimed_job actor in
         let retained = ref None in
         Timers.with_event actor parent (fun owner commit ->
           let receipt, timer = create actor owner in
           retained := Some timer;
           elapsed := mono 250;
           wall_now := wall 10000;
           assert (List.is_empty (snd (A.due_schedules actor |> protocol_ok)));
           A.select_schedule_mutations
             actor
             ~owner
             ~source:Setup.source
             ~receipts:[ receipt ]
           |> protocol_ok;
           Timers.save commit)
         |> protocol_ok
         |> ignore;
         let timer = Option.value_exn !retained in
         assert (List.is_empty (snd (A.due_schedules actor |> protocol_ok)));
         assert (
           Option.is_none
             (A.claim_schedule actor ~schedule_id:timer.id ~generation:0 |> protocol_ok));
         let result =
           Timers.with_event ~snapshot:Setup.after actor parent (fun owner commit ->
             let receipt, _ = create actor owner in
             wall_now := wall (-3600000);
             elapsed := mono 500;
             (match mode with
              | `Discarded ->
                A.abort_schedule_mutation actor ~owner ~receipt |> protocol_ok
              | `Rejected ->
                A.select_schedule_mutations
                  actor
                  ~owner
                  ~source:Setup.source
                  ~receipts:[ receipt ]
                |> protocol_ok;
                reject_save := true);
             let result = Timers.save commit in
             reject_save := false;
             result)
         in
         (match mode, result with
          | `Discarded, Ok _ | `Rejected, Error _ -> ()
          | _ -> failwith "unexpected save disposition");
         [%test_eq: int] 1 (List.length (A.state actor |> protocol_ok).schedules);
         elapsed := mono 999;
         assert (List.is_empty (snd (A.due_schedules actor |> protocol_ok)));
         elapsed := mono 1000;
         let ready = snd (A.due_schedules actor |> protocol_ok) in
         assert (
           List.equal
             P.Id.Schedule.equal
             (List.map ready ~f:(fun timer -> timer.P.Schedule.id))
             [ timer.id ]);
         let claimed =
           A.claim_schedule actor ~schedule_id:timer.id ~generation:0
           |> protocol_ok
           |> Option.value_exn
         in
         let frame =
           Chat_response.Schedule_delivery.capture claimed
           |> Result.bind ~f:Session.Snapshot.of_value
           |> Result.ok_or_failwith
         in
         let after = { Setup.after with queued_internal_events = [ frame ] } in
         let delivered =
           A.complete_schedule
             actor
             ~expected:Setup.after
             ~expected_schedule:claimed
             ~schedule_id:timer.id
             ~generation:0
             ~moderator_snapshot:(Some (Setup.encode after))
           |> protocol_ok
         in
         assert (
           P.Timestamp.equal
             (Option.value_exn delivered.last_delivery_at)
             timer.created_at);
         assert (List.is_empty (snd (A.due_schedules actor |> protocol_ok)));
         let state = A.state actor |> protocol_ok in
         assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
         let restored =
           Agent_session.Session_persistence.restore_snapshot
             (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
           |> store_ok
         in
         assert_same_session_snapshot state restored;
         print_s
           [%sexp
             (mode : [ `Discarded | `Rejected ]), (delivered.status : P.Schedule.status)]));
  [%expect
    {|
    (Discarded Delivered)
    (Rejected Delivered)
    |}]
;;

let reopen ~env ~sw ~initial ~wall_now ~elapsed f =
  let initial =
    Agent_session.Session_persistence.restore_snapshot
      (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t initial))
    |> store_ok
  in
  let backend =
    Agent_session.Memory_backend.create ~event_capacity:128 ~initial_state:initial
  in
  let actor =
    A.create
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~mailbox_capacity:32
      ~compaction_env:None
      ~initial_state:initial
      ~operation_worker:None
      ~persistence:(Agent_session.Memory_backend.persistence backend)
      ~services:
        { now = (fun () -> !wall_now)
        ; monotonic_now = (fun () -> !elapsed)
        ; create_attachment_id = P.Id.Attachment.create
        ; create_reclaim_token = (fun () -> "elapsed-reopen")
        ; state_committed = (fun _ _ -> ())
        ; job_results = None
        ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
        ; schedule_limits = Agent_session.Staged_schedules.default_limits
        }
  in
  Exn.protect ~finally:(fun () -> A.shutdown actor) ~f:(fun () -> f actor)
;;

let%expect_test
    "recovery derives elapsed duration once from persisted wall due time and accepts a \
     new monotonic epoch"
  =
  List.iter [ `Future; `Overdue ] ~f:(fun mode ->
    let wall_now = ref timestamp
    and elapsed = ref (mono 100000) in
    with_actor
      ~now:(fun () -> !wall_now)
      ~monotonic_now:(fun () -> !elapsed)
      (fun env sw actor _writer _backend ->
         A.change_moderator actor (Some (Setup.encode Setup.before))
         |> protocol_ok
         |> ignore;
         let parent = add_claimed_job actor in
         Timers.with_event actor parent (fun owner commit ->
           let receipt, _ = create actor owner in
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
         A.shutdown actor;
         (wall_now
          := match mode with
             | `Future -> wall 400
             | `Overdue -> wall 2000);
         elapsed := mono 0;
         reopen ~env ~sw ~initial:state ~wall_now ~elapsed (fun restored ->
           (match mode with
            | `Future ->
              assert (List.is_empty (snd (A.due_schedules restored |> protocol_ok)));
              wall_now := wall 3600000;
              elapsed := mono 599;
              assert (List.is_empty (snd (A.due_schedules restored |> protocol_ok)));
              wall_now := wall (-3600000);
              elapsed := mono 600
            | `Overdue -> ());
           let ready = snd (A.due_schedules restored |> protocol_ok) in
           [%test_eq: int] 1 (List.length ready);
           let timer = List.hd_exn ready in
           let claimed =
             A.claim_schedule restored ~schedule_id:timer.id ~generation:0
             |> protocol_ok
             |> Option.value_exn
           in
           print_s
             [%sexp (mode : [ `Future | `Overdue ]), (claimed.status : P.Schedule.status)])));
  [%expect
    {|
    (Future Delivering)
    (Overdue Delivering)
    |}]
;;
