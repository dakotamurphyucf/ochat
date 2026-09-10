open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module S = P.Subscription
module Setup = Subscription_transaction_tests

let at seconds = Subscription_dependency_tests.advance seconds
let commit = Subscription_dependency_tests.commit

let schedule () : P.Schedule.t =
  { id = P.Id.Schedule.create ()
  ; session_id
  ; generation = 0
  ; payload = `Null
  ; created_at = timestamp
  ; next_due_at = timestamp
  ; misfire = Deliver_once_immediately
  ; status = Scheduled
  ; delivery_count = 0
  ; last_delivery_at = None
  ; delivery_cancellation = None
  ; ownership = None
  }
;;

let admit actor ~deadline ~timer ~job ~completed =
  let invocation = invocation_fixture () in
  let invocation =
    I.create { invocation.context with id = P.Id.Invocation.create () } |> protocol_ok
  in
  let dispatched = I.dispatch invocation |> protocol_ok in
  let initial = Setup.make_subscription dispatched in
  let initial = S.create { initial.context with deadline } |> protocol_ok in
  let armed =
    S.arm initial ~expected_epoch:0 ~timer_id:timer ~job_id:job |> protocol_ok
  in
  let resolved =
    I.resolve
      dispatched
      ~session_id
      ~generation:0
      (Pending (Subscription initial.context.id, `String "accepted"))
    |> protocol_ok
  in
  let terminal =
    match completed with
    | false -> []
    | true -> [ Setup.finish armed (Succeeded (`String "already ready")) ]
  in
  commit
    actor
    ([ A.Extension_change.Invocation invocation
     ; Invocation dispatched
     ; Subscription initial
     ; Subscription armed
     ; Invocation resolved
     ]
     @ List.map terminal ~f:(fun value -> A.Extension_change.Subscription value))
  |> protocol_ok
  |> ignore;
  armed
;;

let%expect_test
    "expiry atomically retires linked scheduled or claimed timers while preserving \
     unrelated work"
  =
  List.iter [ `Scheduled; `Delivering ] ~f:(fun mode ->
    let now = ref timestamp in
    let reject_save = ref false in
    with_actor
      ~now:(fun () -> !now)
      ~reject_save:(fun _ -> !reject_save)
      (fun _env _sw actor _writer backend ->
         let watched = add_claimed_job actor in
         let timer = A.add_schedule actor (schedule ()) |> protocol_ok in
         let unrelated = A.add_schedule actor (schedule ()) |> protocol_ok in
         let due =
           admit
             actor
             ~deadline:(at 1)
             ~timer:(Some timer.id)
             ~job:(Some watched.id)
             ~completed:false
         in
         let future =
           admit actor ~deadline:(at 10) ~timer:None ~job:None ~completed:false
         in
         let winner =
           admit actor ~deadline:(at 1) ~timer:None ~job:None ~completed:true
         in
         (match mode with
          | `Scheduled -> ()
          | `Delivering ->
            A.claim_schedule actor ~schedule_id:timer.id ~generation:0
            |> protocol_ok
            |> Option.value_exn
            |> ignore);
         let before = Agent_session.Memory_backend.state backend in
         [%test_eq: int] 0 (A.expire_subscriptions actor |> protocol_ok);
         now := at (-1);
         [%test_eq: int] 0 (A.expire_subscriptions actor |> protocol_ok);
         now := at 1;
         reject_save := true;
         assert (Result.is_error (A.expire_subscriptions actor));
         let after_rejection = Agent_session.Memory_backend.state backend in
         assert (
           Sexp.equal
             (Agent_session.Session_state.sexp_of_t before)
             (Agent_session.Session_state.sexp_of_t after_rejection));
         reject_save := false;
         [%test_eq: int] 1 (A.expire_subscriptions actor |> protocol_ok);
         let state = Agent_session.Memory_backend.state backend in
         let lookup id =
           List.find_exn state.subscriptions ~f:(fun value ->
             P.Id.Subscription.equal value.context.id id)
         in
         let expired = lookup due.context.id in
         assert (Option.equal P.Completion.equal expired.result (Some Expired));
         [%test_eq: int] (due.epoch + 1) expired.epoch;
         assert (Option.is_none expired.timer_id && Option.is_none expired.job_id);
         assert (S.equal future (lookup future.context.id));
         assert (
           Option.equal
             P.Completion.equal
             (lookup winner.context.id).result
             (Some (Succeeded (`String "already ready"))));
         assert (
           Jsonaf.exactly_equal (J.to_json watched) (J.to_json (List.hd_exn state.jobs)));
         let find_schedule id =
           List.find_exn state.schedules ~f:(fun schedule ->
             P.Id.Schedule.equal id schedule.id)
         in
         assert (
           Jsonaf.exactly_equal
             (P.Schedule.to_json unrelated)
             (P.Schedule.to_json (find_schedule unrelated.id)));
         (match (find_schedule timer.id).status with
          | Cancelled -> ()
          | _ -> failwith "expiry left timer active");
         assert (
           Option.is_none
             (A.claim_schedule actor ~schedule_id:timer.id ~generation:0 |> protocol_ok));
         assert (
           Result.is_error
             (A.complete_schedule
                actor
                ~schedule_id:timer.id
                ~generation:0
                ~moderator_snapshot:None));
         [%test_eq: int] 0 (A.expire_subscriptions actor |> protocol_ok);
         [%test_eq: int64]
           state.counters.revision
           (Agent_session.Memory_backend.state backend).counters.revision;
         let restored =
           Agent_session.Session_persistence.restore_snapshot
             (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
           |> store_ok
         in
         Agent_session.Session_state.validate restored |> protocol_ok;
         print_s
           [%sexp
             (mode : [ `Scheduled | `Delivering ])
           , (expired.result : P.Completion.t option)]));
  [%expect
    {|
    (Scheduled (Expired))
    (Delivering (Expired))
    |}]
;;

let%expect_test "expiry and moderator completion choose the first persisted winner" =
  List.iter [ `Expiry_first; `Completion_first ] ~f:(fun mode ->
    let now = ref timestamp in
    with_actor
      ~now:(fun () -> !now)
      (fun _env _sw actor _writer backend ->
         A.change_moderator actor (Some (Setup.encode Setup.before))
         |> protocol_ok
         |> ignore;
         let subscription =
           admit actor ~deadline:(at 1) ~timer:None ~job:None ~completed:false
         in
         let parent = add_claimed_job actor in
         let event =
           Chat_response.Moderation.Event.Pre_tool_call
             { id = "expiry-race"
             ; name = "fixture"
             ; args = `Null
             ; kind = Function
             ; payload_text = "null"
             ; meta = `Null
             }
         in
         let result =
           A.with_job_execution
             actor
             ~job_id:parent.id
             ~generation:0
             ~attempt:parent.attempt
             ~deadline:(Some deadline)
             (fun services ->
                services.claim_event
                  ~event
                  ~snapshot:(fun () -> Ok Setup.before)
                  (fun ~executing ~retirement_reason:_ ~event:_ ~execute:_ ~commit:save ->
                     let owner = J.Moderator_event executing.context.id in
                     let completed =
                       Setup.finish subscription (Succeeded (`String "won"))
                     in
                     let receipt =
                       A.stage_subscription_mutation
                         actor
                         ~owner
                         ~source:Setup.source
                         ~previous:(Some subscription)
                         ~next:completed
                       |> protocol_ok
                     in
                     A.select_subscription_mutations
                       actor
                       ~owner
                       ~source:Setup.source
                       ~receipts:[ receipt ]
                     |> protocol_ok;
                     (match mode with
                      | `Expiry_first ->
                        now := at 1;
                        [%test_eq: int] 1 (A.expire_subscriptions actor |> protocol_ok)
                      | `Completion_first -> ());
                     save
                       ~snapshot:Setup.after
                       ~requests:
                         { request_turn = false
                         ; request_compaction = false
                         ; end_session = None
                         }))
         in
         (match mode, result with
          | `Expiry_first, Error { code = Conflict; _ } | `Completion_first, Ok _ -> ()
          | _ -> failwith "unexpected competing checkpoint result");
         now := at 2;
         [%test_eq: int] 0 (A.expire_subscriptions actor |> protocol_ok);
         let state = Agent_session.Memory_backend.state backend in
         let expected_snapshot =
           match mode with
           | `Expiry_first -> Setup.before
           | `Completion_first -> Setup.after
         in
         assert (
           Option.exists
             state.moderator
             ~f:(Jsonaf.exactly_equal (Setup.encode expected_snapshot)));
         print_s
           [%sexp
             (mode : [ `Expiry_first | `Completion_first ])
           , ((List.hd_exn state.subscriptions).result : P.Completion.t option)]));
  [%expect
    {|
    (Expiry_first (Expired))
    (Completion_first ((Succeeded (String won))))
    |}]
;;

let%expect_test
    "expiry can retire historical subscriptions without granting new-generation authority"
  =
  let now = ref timestamp in
  with_actor
    ~now:(fun () -> !now)
    (fun _env _sw actor writer backend ->
       let subscription =
         admit actor ~deadline:(at 1) ~timer:None ~job:None ~completed:false
       in
       let state = Agent_session.Memory_backend.state backend in
       let module Delta = Agent_session.Session_delta in
       let older = Delta.apply state (Reset_generation 1) |> protocol_ok in
       let expired =
         S.finish subscription ~expected_epoch:subscription.epoch ~now:(at 1) Expired
         |> protocol_ok
         |> fst
       in
       assert (Result.is_error (Delta.apply older (Subscription_changed expired)));
       let delta = Delta.Subscription_expired expired in
       let replayed =
         Delta.t_of_sexp (Delta.sexp_of_t delta) |> Delta.apply older |> protocol_ok
       in
       Agent_session.Session_state.validate replayed |> protocol_ok;
       assert (Result.is_error (Delta.apply { older with subscriptions = [] } delta));
       assert (Result.is_error (Delta.apply older (Subscription_expired subscription)));
       let success = Setup.finish subscription (Succeeded (`String "forged")) in
       assert (Result.is_error (Delta.apply older (Subscription_expired success)));
       print_s
         [%sexp
           (replayed.identity.generation : int)
         , ((List.hd_exn replayed.subscriptions).context.generation : int)
         , ((List.hd_exn replayed.subscriptions).result : P.Completion.t option)];
       A.stop actor ~attachment_id:writer.id ~mode:Graceful |> protocol_ok |> ignore;
       now := at 1;
       [%test_eq: int] 1 (A.expire_subscriptions actor |> protocol_ok);
       let stopped = Agent_session.Memory_backend.state backend in
       print_s
         [%sexp
           (stopped.lifecycle.observed : P.Session.observed_state)
         , ((List.hd_exn stopped.subscriptions).result : P.Completion.t option)]);
  [%expect
    {|
    (1 0 (Expired))
    (Stopped (Expired))
    |}]
;;
