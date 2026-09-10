open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module M = Chat_response.Moderator_manager
module B = Agent_session.Runtime_builder
module Q = Agent_session.Queued_moderator_event
module D = Chat_response.Schedule_delivery

let requests : I.follow_up =
  { request_turn = false; request_compaction = false; end_session = None }
;;

let%expect_test
    "timer provenance survives delivery; duplicate retirement is atomic and leaves \
     ordinary JSON alone"
  =
  let reject_save = ref false in
  with_actor
    ~reject_save:(fun _ -> !reject_save)
    (fun env _sw actor _writer backend ->
       let create_manager ?snapshot () =
         handoff_definition
           env
           ?snapshot
           ~declare_tool:false
           ~events:
             {| | `Internal_event(payload) ->
             let ignored = state[0] <- state[0] + 1 in Task.pure(state)
             | _ -> Task.pure(state) |}
       in
       let manager, _, _ = create_manager () in
       let snapshot () = M.identity_snapshot manager |> Result.ok_or_failwith in
       A.change_moderator actor (Some (B.encode_moderator_snapshot (snapshot ())))
       |> protocol_ok
       |> ignore;
       let timers = ref [] in
       A.with_current_moderator_event
         actor
         ~operation_id:None
         ~event:Session_start
         ~snapshot:(fun () -> Ok (snapshot ()))
         (fun ~executing ~retirement_reason ~event:_ ~execute:_ ~commit ->
            assert (Option.is_none retirement_reason);
            let owner = J.Moderator_event executing.context.id in
            let source = executing.context.source in
            let create () =
              A.create_script_schedule
                actor
                ~owner
                ~source
                ~delay_ms:0
                ~payload:
                  (`Object
                      [ "large", `Number "9007199254740993"; "exponent", `Number "1e2" ])
                ~misfire:Deliver_once_immediately
              |> protocol_ok
            in
            let first = create () in
            let second = create () in
            let staged = [ first; second ] in
            timers := List.map staged ~f:snd;
            A.select_schedule_mutations
              actor
              ~owner
              ~source
              ~receipts:(List.map staged ~f:fst)
            |> protocol_ok;
            commit ~snapshot:(snapshot ()) ~requests)
       |> protocol_ok
       |> ignore;
       let deliver timer =
         let timer =
           A.claim_schedule actor ~schedule_id:timer.P.Schedule.id ~generation:0
           |> protocol_ok
           |> Option.value_exn
         in
         let event = D.capture timer |> Result.ok_or_failwith in
         let before = snapshot () in
         let unchanged = A.state actor |> protocol_ok in
         assert (
           Result.is_error
             (A.complete_schedule
                actor
                ~expected:before
                ~expected_schedule:timer
                ~schedule_id:timer.id
                ~generation:0
                ~moderator_snapshot:(Some (B.encode_moderator_snapshot before))));
         assert_same_session_snapshot unchanged (A.state actor |> protocol_ok);
         M.enqueue_internal_event_entries
           manager
           ~event
           ~prepare:(fun ~before ~snapshot ->
             A.complete_schedule
               actor
               ~expected:before
               ~expected_schedule:timer
               ~schedule_id:timer.id
               ~generation:0
               ~moderator_snapshot:(Some (B.encode_moderator_snapshot snapshot))
             |> Result.map ~f:ignore
             |> Result.map_error ~f:(fun error -> error.P.Error.message))
         |> Result.ok_or_failwith
         |> ignore;
         timer, event
       in
       let enqueue event =
         M.enqueue_internal_event_entries
           manager
           ~event
           ~prepare:(fun ~before:_ ~snapshot ->
             A.change_moderator actor (Some (B.encode_moderator_snapshot snapshot))
             |> Result.map ~f:ignore
             |> Result.map_error ~f:(fun error -> error.P.Error.message))
         |> Result.ok_or_failwith
         |> ignore
       in
       let first, frame = deliver (List.nth_exn !timers 0) in
       enqueue frame;
       let _second, _ = deliver (List.nth_exn !timers 1) in
       (* An unretained timer cannot borrow authority from another timer with the
          same JSON. These host injections simulate duplicate/stale queue recovery. *)
       enqueue
         (D.capture { first with id = P.Id.Schedule.create () } |> Result.ok_or_failwith);
       enqueue
         (Chatml.Chatml_lang.VVariant
            ( "Internal_event"
            , [ Chatml.Chatml_value_codec.jsonaf_to_value
                  (`Object [ "__Ochat_timer_delivery_v1", P.Schedule.to_json first ])
              ] ));
       let restored =
         let module S = Session.Moderator_state.Identity_snapshot in
         snapshot ()
         |> S.sexp_of_t
         |> Sexp.to_string_mach
         |> Sexp.of_string
         |> S.t_of_sexp
       in
       let manager, _, _ = create_manager ~snapshot:restored () in
       let snapshot () = M.identity_snapshot manager |> Result.ok_or_failwith in
       let run () =
         Agent_session.Moderator_event.run_queued_idle
           ~claim:(A.with_current_idle_queued_moderator_event_tools actor)
           ~manager
           ~history:(fun () -> [])
           ~available_tools:[]
           ~session_meta:`Null
           ~now:(fun () -> timestamp)
           ()
       in
       run () |> protocol_ok |> ignore;
       let before = A.state actor |> protocol_ok in
       let before_manager = snapshot () in
       reject_save := true;
       assert (Result.is_error (run ()));
       reject_save := false;
       assert_same_session_snapshot before (A.state actor |> protocol_ok);
       assert (
         Sexp.equal
           (Session.Moderator_state.Identity_snapshot.sexp_of_t before_manager)
           (Session.Moderator_state.Identity_snapshot.sexp_of_t (snapshot ())));
       List.iter [ 1; 2; 3; 4 ] ~f:(fun _ -> run () |> protocol_ok |> ignore);
       assert (Option.is_none (run () |> protocol_ok));
       let final = A.state actor |> protocol_ok in
       let restored =
         Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t final))
         |> store_ok
       in
       assert_same_session_snapshot final restored;
       assert_same_session_snapshot final (Agent_session.Memory_backend.state backend);
       let receipts =
         List.filter final.moderator_executions ~f:(fun receipt ->
           P.Moderator_execution.equal_phase receipt.context.phase Internal_event)
       in
       let retired =
         List.filter_map receipts ~f:(fun receipt ->
           Option.map receipt.retirement ~f:(fun retired -> retired.reason))
         |> List.sort ~compare:String.compare
       in
       print_s
         [%sexp
           ((snapshot ()).current_state : Session.Snapshot.t)
         , (List.length receipts : int)
         , (retired : string list)
         , (List.length (snapshot ()).queued_internal_events : int)]);
  [%expect {| ((Array ((Int 3))) 5 (timer.duplicate_delivery timer.stale_delivery) 0) |}]
;;

let%expect_test
    "queued timers cannot run after subscription expiry, completion or rearming"
  =
  List.iter [ `Expire; `Complete; `Rearm ] ~f:(fun mode ->
    let now = ref timestamp in
    with_actor
      ~now:(fun () -> !now)
      (fun env _sw actor _writer _backend ->
         let manager, _, _ =
           handoff_definition
             env
             ~declare_tool:false
             ~events:
               {| | `Internal_event(payload) -> Task.fail("stale timer ran")
                     | _ -> Task.pure(state) |}
         in
         let snapshot () = M.identity_snapshot manager |> Result.ok_or_failwith in
         let source = Option.value_exn (M.invocation_observer manager) in
         A.change_moderator actor (Some (B.encode_moderator_snapshot (snapshot ())))
         |> protocol_ok
         |> ignore;
         let invocation = invocation_fixture () in
         let dispatched = I.dispatch invocation |> protocol_ok in
         let template = Subscription_transaction_tests.make_subscription dispatched in
         let subscription =
           P.Subscription.create
             { template.context with
               source = Some source
             ; deadline = Subscription_dependency_tests.advance 1
             }
           |> protocol_ok
         in
         let resolved =
           I.resolve
             dispatched
             ~session_id
             ~generation:0
             (Pending (Subscription subscription.context.id, `String "accepted"))
           |> protocol_ok
         in
         Subscription_dependency_tests.commit
           actor
           [ Invocation invocation
           ; Invocation dispatched
           ; Subscription subscription
           ; Invocation resolved
           ]
         |> protocol_ok
         |> ignore;
         let armed = ref subscription in
         let timer = ref None in
         A.with_current_moderator_event
           actor
           ~operation_id:None
           ~event:Session_start
           ~snapshot:(fun () -> Ok (snapshot ()))
           (fun ~executing ~retirement_reason:_ ~event:_ ~execute:_ ~commit ->
              let owner = J.Moderator_event executing.context.id in
              let creation, created =
                A.create_script_schedule
                  actor
                  ~owner
                  ~source
                  ~delay_ms:0
                  ~payload:`Null
                  ~misfire:Deliver_once_immediately
                |> protocol_ok
              in
              let bound =
                P.Subscription.arm
                  subscription
                  ~expected_epoch:0
                  ~timer_id:(Some created.id)
                  ~job_id:None
                |> protocol_ok
              in
              armed := bound;
              let sub_receipt =
                A.stage_subscription_mutation
                  actor
                  ~owner
                  ~source
                  ~previous:(Some subscription)
                  ~next:bound
                |> protocol_ok
              in
              let ownership = Option.value_exn created.ownership in
              let linked =
                { created with
                  ownership =
                    Some
                      { ownership with
                        subscription = Some (bound.context.id, bound.epoch)
                      }
                }
              in
              timer := Some linked;
              let linkage =
                A.stage_schedule_mutation
                  actor
                  ~owner
                  ~source
                  ~previous:(Some created)
                  ~next:linked
                |> protocol_ok
              in
              A.select_subscription_mutations
                actor
                ~owner
                ~source
                ~receipts:[ sub_receipt ]
              |> protocol_ok;
              A.select_schedule_mutations
                actor
                ~owner
                ~source
                ~receipts:[ creation; linkage ]
              |> protocol_ok;
              commit ~snapshot:(snapshot ()) ~requests)
         |> protocol_ok
         |> ignore;
         let timer = Option.value_exn !timer in
         let claimed =
           A.claim_schedule actor ~schedule_id:timer.id ~generation:0
           |> protocol_ok
           |> Option.value_exn
         in
         let event = D.capture claimed |> Result.ok_or_failwith in
         let captured = Session.Snapshot.of_value event |> Result.ok_or_failwith in
         M.enqueue_internal_event_entries
           manager
           ~event
           ~prepare:(fun ~before ~snapshot ->
             A.complete_schedule
               actor
               ~expected:before
               ~expected_schedule:claimed
               ~schedule_id:timer.id
               ~generation:0
               ~moderator_snapshot:(Some (B.encode_moderator_snapshot snapshot))
             |> Result.map ~f:ignore
             |> Result.map_error ~f:(fun error -> error.P.Error.message))
         |> Result.ok_or_failwith
         |> ignore;
         let queued = A.state actor |> protocol_ok in
         assert (
           Option.is_none
             (Q.timer_retirement_reason
                ~state:queued
                ~observer:source
                ~event:captured
                ~now:!now
              |> protocol_ok));
         assert (
           Option.equal
             String.equal
             (Some "timer.stale_delivery")
             (Q.timer_retirement_reason
                ~state:queued
                ~observer:{ source with source_sha256 = String.make 64 'f' }
                ~event:captured
                ~now:!now
              |> protocol_ok));
         (match mode with
          | `Expire ->
            now := subscription.context.deadline;
            (* Even before the independent sweep runs, admission respects deadline. *)
            assert (
              Option.equal
                String.equal
                (Some "timer.stale_subscription")
                (Q.timer_retirement_reason
                   ~state:queued
                   ~observer:source
                   ~event:captured
                   ~now:!now
                 |> protocol_ok));
            [%test_eq: int] 1 (A.expire_subscriptions actor |> protocol_ok)
          | `Complete | `Rearm ->
            A.with_current_moderator_event
              actor
              ~operation_id:None
              ~event:Session_resume
              ~snapshot:(fun () -> Ok (snapshot ()))
              (fun ~executing ~retirement_reason:_ ~event:_ ~execute:_ ~commit ->
                 let next =
                   match mode with
                   | `Rearm ->
                     P.Subscription.arm
                       !armed
                       ~expected_epoch:1
                       ~timer_id:None
                       ~job_id:None
                     |> protocol_ok
                   | `Complete ->
                     P.Subscription.finish
                       !armed
                       ~expected_epoch:1
                       ~now:!now
                       (Succeeded (`String "ready"))
                     |> protocol_ok
                     |> fst
                   | `Expire -> assert false
                 in
                 let owner = J.Moderator_event executing.context.id in
                 let receipt =
                   A.stage_subscription_mutation
                     actor
                     ~owner
                     ~source
                     ~previous:(Some !armed)
                     ~next
                   |> protocol_ok
                 in
                 A.select_subscription_mutations
                   actor
                   ~owner
                   ~source
                   ~receipts:[ receipt ]
                 |> protocol_ok;
                 commit ~snapshot:(snapshot ()) ~requests)
            |> protocol_ok
            |> ignore);
         Agent_session.Moderator_event.run_queued_idle
           ~claim:(A.with_current_idle_queued_moderator_event_tools actor)
           ~manager
           ~history:(fun () -> [])
           ~available_tools:[]
           ~session_meta:`Null
           ~now:(fun () -> !now)
           ()
         |> protocol_ok
         |> ignore;
         let state = A.state actor |> protocol_ok in
         let receipt =
           List.find_exn state.moderator_executions ~f:(fun receipt ->
             P.Moderator_execution.equal_phase receipt.context.phase Internal_event)
         in
         print_s
           [%sexp
             (mode : [ `Expire | `Complete | `Rearm ])
           , ((Option.value_exn receipt.retirement).reason : string)
           , (List.length (snapshot ()).queued_internal_events : int)]));
  [%expect
    {|
    (Expire timer.stale_subscription 0)
    (Complete timer.stale_subscription 0)
    (Rearm timer.stale_subscription 0)
    |}]
;;

let%expect_test
    "foreground timer retirement retains operation ownership without invoking the \
     moderator"
  =
  with_handoff_actor
    ~make_worker:(fun env _ ->
      let manager, _, _ =
        handoff_definition
          env
          ~declare_tool:false
          ~events:
            {| | `Internal_event(payload) -> Task.fail("orphan timer ran")
                   | _ -> Task.pure(state) |}
      in
      let source = Option.value_exn (M.invocation_observer manager) in
      let orphan =
        { (Subscription_expiry_tests.schedule ()) with
          status = P.Schedule.Delivering
        ; ownership =
            Some
              { source
              ; creator = Invocation (P.Id.Invocation.create ())
              ; subscription = None
              }
        }
      in
      let frame = D.capture orphan |> Result.ok_or_failwith in
      M.enqueue_internal_event_entries
        manager
        ~event:frame
        ~prepare:(fun ~before:_ ~snapshot:_ -> Ok ())
      |> Result.ok_or_failwith
      |> ignore;
      let snapshot () = M.identity_snapshot manager |> Result.ok_or_failwith in
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        caps.commit_moderator (Some (B.encode_moderator_snapshot (snapshot ())))
        |> protocol_ok;
        Agent_session.Moderator_event.run_queued_idle
          ~claim:caps.with_queued_moderator_event
          ~manager
          ~history:(fun () -> [])
          ~available_tools:[]
          ~session_meta:`Null
          ~now:(fun () -> timestamp)
          ()
        |> protocol_ok
        |> ignore;
        Completed
          { final_history = input.history
          ; moderator_snapshot = Some (B.encode_moderator_snapshot (snapshot ()))
          ; runtime_requests = []
          }))
    (fun _env actor _writer _backend ->
       let state = await_idle actor in
       let receipt = List.hd_exn state.moderator_executions in
       print_s
         [%sexp
           (Option.is_some receipt.context.operation_id : bool)
         , ((Option.value_exn receipt.retirement).reason : string)
         , (Option.is_none state.failure : bool)]);
  [%expect {| (true timer.stale_delivery true) |}]
;;
