open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module S = P.Subscription
module Setup = Subscription_transaction_tests
module Clock = Extension_clock_tests
module Timers = Schedule_transaction_tests

let admit ?(before_create = ignore) actor parent after_create =
  let captured = ref None in
  A.with_job_execution
    actor
    ~job_id:parent.J.id
    ~generation:0
    ~attempt:parent.attempt
    ~deadline:(Some deadline)
    (fun services ->
       services.execute ~invocation:(root parent) (fun ~dispatched:root ->
         let invocation =
           I.create
             { root.context with
               id = P.Id.Invocation.create ()
             ; parent_job = None
             ; parent_invocation = Some root.context.id
             ; tool_name = "counter"
             }
           |> protocol_ok
         in
         services.moderator_execute ~invocation (fun ~dispatched ~commit ->
           let owner = J.Invocation dispatched.context.id in
           before_create ();
           let receipt, subscription =
             A.create_script_subscription
               actor
               ~owner
               ~source:Setup.source
               ~kind:"elapsed"
               ~lifetime_ms:1000
               ~wake:No_wake
               ~completion_schema:(Some (`Object [ "type", `String "string" ]))
             |> protocol_ok
           in
           captured := Some subscription;
           after_create ();
           A.select_subscription_mutations
             actor
             ~owner
             ~source:Setup.source
             ~receipts:[ receipt ]
           |> protocol_ok;
           let resolved =
             I.resolve
               dispatched
               ~session_id
               ~generation:0
               (Pending (Subscription subscription.context.id, `String "accepted"))
             |> protocol_ok
           in
           commit ~resolved ~snapshot:Setup.after)
         |> Result.map ~f:(fun () -> I.Complete `Null)))
  |> protocol_ok
  |> ignore;
  Option.value_exn !captured
;;

let%expect_test
    "subscription finish and expiry share elapsed time across wall jumps, discarded \
     results and rejected saves"
  =
  List.iter
    [ `Forward; `Backward; `Finish_expired; `Sweep_expired; `Rejected; `Boundary ]
    ~f:(fun mode ->
      let wall_now = ref timestamp
      and elapsed = ref (Clock.mono 0) in
      let crossing = ref false
      and samples = ref 0 in
      let reject_save = ref false in
      with_actor
        ~now:(fun () -> !wall_now)
        ~monotonic_now:(fun () ->
          match !crossing with
          | false -> !elapsed
          | true ->
            incr samples;
            Clock.mono
              (match !samples with
               | 1 -> 999
               | _ -> 1000))
        ~reject_save:(fun _ -> !reject_save)
        (fun _env _sw actor _writer backend ->
           A.change_moderator actor (Some (Setup.encode Setup.before))
           |> protocol_ok
           |> ignore;
           let parent = add_claimed_job actor in
           let sub =
             admit actor parent (fun () ->
               wall_now := Clock.wall 10000;
               elapsed := Clock.mono 250;
               [%test_eq: int] 0 (A.expire_subscriptions actor |> protocol_ok))
           in
           [%test_eq: int] 0 (A.expire_subscriptions actor |> protocol_ok);
           let result =
             Timers.with_event ~snapshot:Setup.after actor parent (fun owner commit ->
               let discarded, next =
                 A.finish_script_subscription
                   actor
                   ~owner
                   ~source:Setup.source
                   ~id:sub.context.id
                   ~expected_epoch:0
                   (Succeeded (`String "discarded"))
                 |> protocol_ok
               in
               assert (
                 Option.equal
                   P.Completion.equal
                   next.result
                   (Some (Succeeded (`String "discarded"))));
               A.abort_subscription_mutation actor ~owner ~receipt:discarded
               |> protocol_ok;
               let retained =
                 A.read_script_subscription
                   actor
                   ~owner
                   ~source:Setup.source
                   ~id:sub.context.id
                 |> protocol_ok
               in
               assert (S.equal retained sub);
               (match mode with
                | `Forward -> elapsed := Clock.mono 500
                | `Boundary -> crossing := true
                | `Backward | `Rejected ->
                  wall_now := Clock.wall (-3600000);
                  elapsed := Clock.mono 500
                | `Finish_expired | `Sweep_expired ->
                  wall_now := Clock.wall (-3600000);
                  elapsed := Clock.mono 1000);
               (match mode with
                | `Sweep_expired ->
                  [%test_eq: int] 1 (A.expire_subscriptions actor |> protocol_ok)
                | _ -> ());
               let receipt, next =
                 A.finish_script_subscription
                   actor
                   ~owner
                   ~source:Setup.source
                   ~id:sub.context.id
                   ~expected_epoch:0
                   (Succeeded (`String "winner"))
                 |> protocol_ok
               in
               (match mode with
                | `Boundary ->
                  crossing := false;
                  elapsed := Clock.mono 1000
                | _ -> ());
               (match mode with
                | `Finish_expired | `Sweep_expired ->
                  assert (Option.equal P.Completion.equal next.result (Some Expired));
                  assert (
                    P.Timestamp.equal
                      (Option.value_exn next.completed_at)
                      sub.context.deadline)
                | `Backward | `Rejected ->
                  assert (
                    P.Timestamp.equal
                      (Option.value_exn next.completed_at)
                      sub.context.created_at)
                | `Forward | `Boundary -> ());
               A.select_subscription_mutations
                 actor
                 ~owner
                 ~source:Setup.source
                 ~receipts:[ receipt ]
               |> protocol_ok;
               (match mode with
                | `Rejected -> reject_save := true
                | _ -> ());
               let result = Timers.save commit in
               reject_save := false;
               result)
           in
           (match mode, result with
            | `Rejected, Error _ ->
              let retained = List.hd_exn (A.state actor |> protocol_ok).subscriptions in
              assert (S.equal retained sub);
              elapsed := Clock.mono 1000;
              [%test_eq: int] 1 (A.expire_subscriptions actor |> protocol_ok)
            | (`Forward | `Backward | `Finish_expired | `Sweep_expired | `Boundary), Ok _
              -> ()
            | _ -> failwith "unexpected subscription checkpoint result");
           let state = A.state actor |> protocol_ok in
           let final = List.hd_exn state.subscriptions in
           [%test_eq: int] 0 (A.expire_subscriptions actor |> protocol_ok);
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           let restored =
             Agent_session.Session_persistence.restore_snapshot
               (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
             |> store_ok
           in
           assert_same_session_snapshot state restored;
           print_s
             [%sexp
               (mode
                : [ `Forward
                  | `Backward
                  | `Finish_expired
                  | `Sweep_expired
                  | `Rejected
                  | `Boundary
                  ])
             , (final.result : P.Completion.t option)]));
  [%expect
    {|
    (Forward ((Succeeded (String winner))))
    (Backward ((Succeeded (String winner))))
    (Finish_expired (Expired))
    (Sweep_expired (Expired))
    (Rejected (Expired))
    (Boundary ((Succeeded (String winner))))
    |}]
;;

let%expect_test
    "queued bound timer admission uses the subscription elapsed deadline before the \
     expiry sweep"
  =
  List.iter [ `Before; `Due ] ~f:(fun mode ->
    let wall_now = ref timestamp
    and elapsed = ref (Clock.mono 0) in
    with_actor
      ~now:(fun () -> !wall_now)
      ~monotonic_now:(fun () -> !elapsed)
      (fun _env _sw actor _writer _backend ->
         A.change_moderator actor (Some (Setup.encode Setup.before))
         |> protocol_ok
         |> ignore;
         let parent = add_claimed_job actor in
         let sub = admit actor parent ignore in
         let timer = ref None in
         Timers.with_event ~snapshot:Setup.after actor parent (fun owner commit ->
           let creation, value = Timers.create actor owner |> protocol_ok in
           let armed =
             S.arm sub ~expected_epoch:0 ~timer_id:(Some value.id) ~job_id:None
             |> protocol_ok
           in
           let sub_receipt =
             A.stage_subscription_mutation
               actor
               ~owner
               ~source:Setup.source
               ~previous:(Some sub)
               ~next:armed
             |> protocol_ok
           in
           let ownership = Option.value_exn value.ownership in
           let bound =
             { value with
               ownership =
                 Some { ownership with subscription = Some (sub.context.id, armed.epoch) }
             }
           in
           let binding =
             A.stage_schedule_mutation
               actor
               ~owner
               ~source:Setup.source
               ~previous:(Some value)
               ~next:bound
             |> protocol_ok
           in
           timer := Some bound;
           A.select_subscription_mutations
             actor
             ~owner
             ~source:Setup.source
             ~receipts:[ sub_receipt ]
           |> protocol_ok;
           A.select_schedule_mutations
             actor
             ~owner
             ~source:Setup.source
             ~receipts:[ creation; binding ]
           |> protocol_ok;
           Timers.save commit)
         |> protocol_ok
         |> ignore;
         let timer = Option.value_exn !timer in
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
         let queued = { Setup.after with queued_internal_events = [ frame ] } in
         A.complete_schedule
           actor
           ~expected:Setup.after
           ~expected_schedule:claimed
           ~schedule_id:timer.id
           ~generation:0
           ~moderator_snapshot:(Some (Setup.encode queued))
         |> protocol_ok
         |> ignore;
         (match mode with
          | `Before ->
            wall_now := Clock.wall 10000;
            elapsed := Clock.mono 999
          | `Due ->
            wall_now := Clock.wall (-3600000);
            elapsed := Clock.mono 1000);
         let reason = ref None in
         A.with_current_idle_queued_moderator_event_tools
           actor
           ~snapshot:(fun () -> Ok queued)
           (fun ~executing:_ ~retirement_reason ~event:_ ~execute:_ ~commit ->
              reason := retirement_reason;
              commit
                ~snapshot:Setup.after
                ~requests:
                  I.
                    { request_turn = false
                    ; request_compaction = false
                    ; end_session = None
                    })
         |> protocol_ok
         |> ignore;
         assert (
           Option.is_none
             (List.hd_exn (A.state actor |> protocol_ok).subscriptions).result);
         print_s [%sexp (mode : [ `Before | `Due ]), (!reason : string option)]));
  [%expect
    {|
    (Before ())
    (Due (timer.stale_subscription))
    |}]
;;

let%expect_test
    "subscription recovery establishes a fresh elapsed epoch and preserves the remaining \
     lifetime"
  =
  let wall_now = ref timestamp
  and elapsed = ref (Clock.mono 100000) in
  with_actor
    ~now:(fun () -> !wall_now)
    ~monotonic_now:(fun () -> !elapsed)
    (fun env sw actor _writer _backend ->
       A.change_moderator actor (Some (Setup.encode Setup.before))
       |> protocol_ok
       |> ignore;
       let parent = add_claimed_job actor in
       let sub = admit actor parent ignore in
       let state = A.state actor |> protocol_ok in
       A.shutdown actor;
       wall_now := Clock.wall 400;
       elapsed := Clock.mono 0;
       Clock.reopen ~env ~sw ~initial:state ~wall_now ~elapsed (fun restored ->
         wall_now := Clock.wall 10000;
         elapsed := Clock.mono 599;
         [%test_eq: int] 0 (A.expire_subscriptions restored |> protocol_ok);
         wall_now := Clock.wall (-3600000);
         elapsed := Clock.mono 600;
         [%test_eq: int] 1 (A.expire_subscriptions restored |> protocol_ok);
         let terminal = List.hd_exn (A.state restored |> protocol_ok).subscriptions in
         assert (
           P.Timestamp.equal (Option.value_exn terminal.completed_at) sub.context.deadline);
         print_s [%sexp (terminal.result : P.Completion.t option)]));
  [%expect {| (Expired) |}]
;;

let%expect_test
    "elapsed subscription lifetime does not widen an inherited absolute job deadline"
  =
  List.iter [ `Absolute; `Cancel_rollback ] ~f:(fun mode ->
    let wall_now = ref timestamp in
    with_actor
      ~now:(fun () -> !wall_now)
      ~monotonic_now:(fun () -> Clock.mono 0)
      (fun _env _sw actor _writer _backend ->
         let parent, _, _ = Subscription_dependency_tests.prepare actor in
         (wall_now
          := match mode with
             | `Absolute -> Clock.wall 2000
             | `Cancel_rollback -> Clock.wall (-3600000));
         [%test_eq: int] 0 (A.expire_subscriptions actor |> protocol_ok);
         let parent =
           (match mode with
            | `Absolute ->
              A.refresh_background_job
                actor
                ~job_id:parent.id
                ~generation:0
                ~attempt:parent.attempt
            | `Cancel_rollback -> A.cancel_job_internal actor ~job_id:parent.id)
           |> protocol_ok
         in
         let completion =
           J.terminal_completion parent |> protocol_ok |> Option.value_exn
         in
         let sub = List.hd_exn (A.state actor |> protocol_ok).subscriptions in
         print_s
           [%sexp
             (mode : [ `Absolute | `Cancel_rollback ])
           , (completion : P.Completion.t)
           , (sub.result : P.Completion.t option)]));
  [%expect
    {|
    (Absolute Expired ((Cancelled "owning job stopped waiting")))
    (Cancel_rollback (Cancelled "job cancelled")
     ((Cancelled "owning job stopped waiting")))
    |}]
;;

let%expect_test
    "clock rollback after invocation admission does not reject a legitimately owned new \
     subscription"
  =
  let wall_now = ref timestamp
  and elapsed = ref (Clock.mono 0) in
  with_actor
    ~now:(fun () -> !wall_now)
    ~monotonic_now:(fun () -> !elapsed)
    (fun _env _sw actor _writer _backend ->
       A.change_moderator actor (Some (Setup.encode Setup.before))
       |> protocol_ok
       |> ignore;
       let parent = add_claimed_job actor in
       let sub =
         admit
           ~before_create:(fun () -> wall_now := Clock.wall (-3600000))
           actor
           parent
           ignore
       in
       assert (P.Timestamp.equal sub.context.created_at !wall_now);
       elapsed := Clock.mono 999;
       [%test_eq: int] 0 (A.expire_subscriptions actor |> protocol_ok);
       elapsed := Clock.mono 1000;
       [%test_eq: int] 1 (A.expire_subscriptions actor |> protocol_ok);
       print_s
         [%sexp
           ((List.hd_exn (A.state actor |> protocol_ok).subscriptions).result
            : P.Completion.t option)]);
  [%expect {| (Expired) |}]
;;
