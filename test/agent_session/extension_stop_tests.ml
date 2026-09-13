open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module S = P.Schedule
module M = Chat_response.Moderator_manager
module B = Agent_session.Runtime_builder

let requests : I.follow_up =
  { request_turn = false; request_compaction = false; end_session = None }
;;

let admit actor source ~completed =
  let invocation = invocation_fixture () in
  let invocation =
    I.create { invocation.context with id = P.Id.Invocation.create () } |> protocol_ok
  in
  let dispatched = I.dispatch invocation |> protocol_ok in
  let template = Subscription_transaction_tests.make_subscription dispatched in
  let subscription =
    P.Subscription.create { template.context with source = Some source } |> protocol_ok
  in
  let resolved =
    I.resolve
      dispatched
      ~session_id
      ~generation:0
      (Pending (Subscription subscription.context.id, `String "accepted"))
    |> protocol_ok
  in
  let changes =
    [ A.Extension_change.Invocation invocation
    ; Invocation dispatched
    ; Subscription subscription
    ; Invocation resolved
    ]
  in
  let winner =
    match completed with
    | false -> subscription
    | true ->
      P.Subscription.finish
        subscription
        ~expected_epoch:0
        ~now:timestamp
        (Succeeded (`String "winner"))
      |> protocol_ok
      |> fst
  in
  Subscription_dependency_tests.commit
    actor
    (changes
     @
     match completed with
     | true -> [ Subscription winner ]
     | false -> [])
  |> protocol_ok
  |> ignore;
  winner
;;

let%expect_test
    "cancel stop atomically cancels owned work and queued callbacks without rewriting \
     completed deliveries"
  =
  List.iter
    [ `Cancel; `Rejected; `Graceful; `Escalated; `Clock_rollback; `Borrowed ]
    ~f:(fun mode ->
      let reject_save = ref false in
      let now = ref timestamp in
      with_actor
        ~reject_save:(fun _ -> !reject_save)
        ~now:(fun () -> !now)
        (fun env _sw actor writer backend ->
           let manager, _, _ =
             handoff_definition
               env
               ~declare_tool:false
               ~events:
                 {| | `Internal_event(payload) ->
             let ignored = state[0] <- state[0] + 1 in Task.pure(state)
             | _ -> Task.pure(state) |}
           in
           let snapshot () = M.identity_snapshot manager |> Result.ok_or_failwith in
           let source = Option.value_exn (M.invocation_observer manager) in
           A.change_moderator actor (Some (B.encode_moderator_snapshot (snapshot ())))
           |> protocol_ok
           |> ignore;
           let subscription = admit actor source ~completed:false in
           let winner = admit actor source ~completed:true in
           let legacy =
             A.add_schedule actor (Subscription_expiry_tests.schedule ()) |> protocol_ok
           in
           let timers = ref [] in
           A.with_current_moderator_event
             actor
             ~operation_id:None
             ~event:Session_start
             ~snapshot:(fun () -> Ok (snapshot ()))
             (fun ~executing ~retirement_reason:_ ~event:_ ~execute:_ ~commit ->
                let owner = J.Moderator_event executing.context.id in
                let create () =
                  A.create_script_schedule
                    actor
                    ~owner
                    ~source
                    ~delay_ms:0
                    ~payload:`Null
                    ~misfire:Deliver_once_immediately
                  |> protocol_ok
                in
                let creation, linked = create () in
                let armed =
                  P.Subscription.arm
                    subscription
                    ~expected_epoch:0
                    ~timer_id:(Some linked.id)
                    ~job_id:None
                  |> protocol_ok
                in
                let sub_receipt =
                  A.stage_subscription_mutation
                    actor
                    ~owner
                    ~source
                    ~previous:(Some subscription)
                    ~next:armed
                  |> protocol_ok
                in
                let ownership = Option.value_exn linked.ownership in
                let linked =
                  { linked with
                    ownership =
                      Some
                        { ownership with
                          subscription = Some (armed.context.id, armed.epoch)
                        }
                  }
                in
                let predecessor =
                  A.read_script_schedule actor ~owner ~source ~id:linked.id |> protocol_ok
                in
                let binding =
                  A.stage_schedule_mutation
                    actor
                    ~owner
                    ~source
                    ~previous:(Some predecessor)
                    ~next:linked
                  |> protocol_ok
                in
                let queued_receipt, queued = create () in
                let handled_receipt, handled = create () in
                timers := [ linked; queued; handled ];
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
                  ~receipts:[ creation; binding; queued_receipt; handled_receipt ]
                |> protocol_ok;
                commit ~snapshot:(snapshot ()) ~requests)
           |> protocol_ok
           |> ignore;
           let linked = List.nth_exn !timers 0 in
           let queued = List.nth_exn !timers 1 in
           let handled = List.nth_exn !timers 2 in
           let deliver timer =
             let claimed =
               A.claim_schedule actor ~schedule_id:timer.S.id ~generation:0
               |> protocol_ok
               |> Option.value_exn
             in
             let event =
               Chat_response.Schedule_delivery.capture claimed |> Result.ok_or_failwith
             in
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
             |> ignore
           in
           let run_result () =
             Agent_session.Moderator_event.run_queued_idle
               ~claim:(A.with_current_idle_queued_moderator_event_tools actor)
               ~manager
               ~history:(fun () -> [])
               ~available_tools:[]
               ~session_meta:`Null
               ~now:(fun () -> !now)
               ()
           in
           let run () = run_result () |> protocol_ok in
           deliver handled;
           run () |> ignore;
           deliver queued;
           let stop mode = A.stop actor ~attachment_id:writer.id ~mode in
           (match mode with
            | `Escalated -> stop Graceful |> protocol_ok |> ignore
            | `Clock_rollback -> now := Subscription_dependency_tests.advance (-1)
            | _ -> ());
           let before = A.state actor |> protocol_ok in
           let before_queue = snapshot () in
           (match mode with
            | `Rejected ->
              reject_save := true;
              assert (Result.is_error (stop Cancel));
              reject_save := false;
              assert_same_session_snapshot before (A.state actor |> protocol_ok)
            | _ -> ());
           let stop_mode =
             match mode with
             | `Graceful -> P.Session.Graceful
             | _ -> Cancel
           in
           (match mode with
            | `Borrowed ->
              let result =
                try
                  A.with_current_idle_queued_moderator_event_tools
                    actor
                    ~snapshot:(fun () -> Ok (snapshot ()))
                    (fun ~executing:_
                      ~retirement_reason:_
                      ~event:_
                      ~execute:_
                      ~commit:_ ->
                       stop Cancel |> protocol_ok |> ignore;
                       Error (handoff_error "callback stopped"))
                with
                | Eio.Cancel.Cancelled _ -> Error (handoff_error "callback cancelled")
              in
              assert (Result.is_error result)
            | _ -> stop stop_mode |> protocol_ok |> ignore);
           let stopped = A.state actor |> protocol_ok in
           assert (
             Sexp.equal
               (Session.Moderator_state.Identity_snapshot.sexp_of_t before_queue)
               (Session.Moderator_state.Identity_snapshot.sexp_of_t (snapshot ())));
           stop stop_mode |> protocol_ok |> ignore;
           [%test_eq: int64]
             stopped.counters.revision
             (A.state actor |> protocol_ok).counters.revision;
           let find id =
             List.find_exn stopped.schedules ~f:(fun timer ->
               P.Id.Schedule.equal timer.id id)
           in
           let sub =
             List.find_exn stopped.subscriptions ~f:(fun item ->
               P.Id.Subscription.equal item.context.id subscription.context.id)
           in
           assert (List.exists stopped.subscriptions ~f:(P.Subscription.equal winner));
           assert (Jsonaf.exactly_equal (S.to_json legacy) (S.to_json (find legacy.id)));
           let before_handled =
             List.find_exn before.schedules ~f:(fun timer ->
               P.Id.Schedule.equal timer.id handled.id)
           in
           assert (
             Jsonaf.exactly_equal (S.to_json before_handled) (S.to_json (find handled.id)));
           let queued = find queued.id in
           assert (
             Int.equal queued.delivery_count 1 && Option.is_some queued.last_delivery_at);
           let restored =
             Agent_session.Session_persistence.restore_snapshot
               (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t stopped))
             |> store_ok
           in
           assert_same_session_snapshot stopped restored;
           assert_same_session_snapshot
             stopped
             (Agent_session.Memory_backend.state backend);
           assert (
             Jsonaf.exactly_equal
               (S.to_json queued)
               (S.to_json (S.of_json (S.to_json queued) |> protocol_ok)));
           A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
           (match mode with
            | `Borrowed ->
              assert (Result.is_error (run_result ()));
              assert (
                List.exists stopped.moderator_executions ~f:(fun receipt ->
                  match receipt.status, receipt.retirement with
                  | Interrupted _, None -> true
                  | _ -> false))
            | _ ->
              run () |> ignore;
              assert (Option.is_none (run ())));
           print_s
             [%sexp
               (mode
                : [ `Cancel
                  | `Rejected
                  | `Graceful
                  | `Escalated
                  | `Clock_rollback
                  | `Borrowed
                  ])
             , (sub.result : P.Completion.t option)
             , ((find linked.id).status : S.status)
             , (queued.delivery_cancellation : string option)
             , ((snapshot ()).current_state : Session.Snapshot.t)]));
  [%expect
    {|
    (Cancel ((Cancelled "session stopped")) Cancelled ("session stopped")
     (Array ((Int 1))))
    (Rejected ((Cancelled "session stopped")) Cancelled ("session stopped")
     (Array ((Int 1))))
    (Graceful () Scheduled () (Array ((Int 2))))
    (Escalated ((Cancelled "session stopped")) Cancelled ("session stopped")
     (Array ((Int 1))))
    (Clock_rollback ((Cancelled "session stopped")) Cancelled ("session stopped")
     (Array ((Int 1))))
    (Borrowed ((Cancelled "session stopped")) Cancelled () (Array ((Int 1))))
    |}]
;;

let%expect_test
    "delivery cancellation cannot be dropped by codec downgrade or rewritten after commit"
  =
  let delivered =
    { (Subscription_expiry_tests.schedule ()) with
      status = S.Delivered
    ; delivery_count = 1
    ; last_delivery_at = Some timestamp
    ; ownership =
        Some
          { source = Subscription_transaction_tests.source
          ; creator = Invocation (P.Id.Invocation.create ())
          ; subscription = None
          }
    }
  in
  let cancelled = { delivered with delivery_cancellation = Some "session stopped" } in
  S.validate_transition ~previous:(Some delivered) cancelled |> protocol_ok;
  let json = S.to_json cancelled in
  let fields =
    match json with
    | `Object fields -> fields
    | _ -> assert false
  in
  assert (Jsonaf.exactly_equal json (S.to_json (S.of_json json |> protocol_ok)));
  reject
    "missing disposition"
    (S.of_json
       (`Object (List.Assoc.remove fields ~equal:String.equal "delivery_cancellation")));
  reject
    "version downgrade"
    (S.of_json
       (`Object (List.Assoc.add fields ~equal:String.equal "schema_version" (`Number "2"))));
  reject
    "discard cancellation"
    (S.validate_transition ~previous:(Some cancelled) delivered);
  reject
    "replace cancellation"
    (S.validate_transition
       ~previous:(Some cancelled)
       { cancelled with delivery_cancellation = Some "other reason" });
  reject
    "rewrite delivered payload"
    (S.validate_transition
       ~previous:(Some delivered)
       { cancelled with payload = `String "replacement" });
  reject
    "cancel before enqueue"
    (S.validate_transition
       ~previous:
         (Some
            { delivered with
              status = Scheduled
            ; delivery_count = 0
            ; last_delivery_at = None
            })
       cancelled);
  [%expect
    {|
    ("missing disposition" Invalid_request)
    ("version downgrade" Invalid_request)
    ("discard cancellation" Conflict)
    ("replace cancellation" Conflict)
    ("rewrite delivered payload" Conflict)
    ("cancel before enqueue" Conflict)
    |}]
;;

let%expect_test
    "host cancellation replays for historical subscriptions without admitting work or \
     forging success"
  =
  with_actor (fun _env _sw actor _writer _backend ->
    let subscription =
      admit actor Subscription_transaction_tests.source ~completed:false
    in
    let module Delta = Agent_session.Session_delta in
    let state = A.state actor |> protocol_ok in
    let historical = Delta.apply state (Reset_generation 1) |> protocol_ok in
    let plan =
      Agent_session.Extension_stop.prepare ~state:historical ~mode:Cancel ~now:timestamp
      |> protocol_ok
    in
    let delta = Delta.Batch (Agent_session.Extension_stop.deltas plan) in
    let restored =
      Delta.apply historical (Delta.t_of_sexp (Delta.sexp_of_t delta)) |> protocol_ok
    in
    Agent_session.Session_state.validate restored |> protocol_ok;
    let cancelled = List.hd_exn restored.subscriptions in
    assert (Result.is_error (Delta.apply historical (Subscription_changed cancelled)));
    assert (Result.is_error (Delta.apply { historical with subscriptions = [] } delta));
    let success =
      P.Subscription.finish
        subscription
        ~expected_epoch:0
        ~now:timestamp
        (Succeeded (`String "forged"))
      |> protocol_ok
      |> fst
    in
    assert (Result.is_error (Delta.apply historical (Subscription_cancelled success)));
    assert_same_session_snapshot restored (Delta.apply restored delta |> protocol_ok);
    print_s
      [%sexp
        (restored.identity.generation : int)
      , (cancelled.context.generation : int)
      , (cancelled.result : P.Completion.t option)]);
  [%expect {| (1 0 ((Cancelled "session stopped"))) |}]
;;
