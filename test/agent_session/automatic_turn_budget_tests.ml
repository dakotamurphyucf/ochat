open Core
open Fixtures
module P = Agent_protocol
module A = Agent_session.Session_actor
module B = Agent_session.Automatic_turn_budget
module R = Chat_response.Runtime_semantics
module Setup = Subscription_transaction_tests

let budget state =
  Option.value_exn state.Agent_session.Session_state.automatic_turn_budget
;;

let policy =
  { R.default_policy with
    budget =
      { R.default_budget_policy with
        max_followup_turns = 1
      ; turn_rate_limit = Some { max_turns = 2; window_ms = 1_000 }
      }
  }
;;

let%expect_test
    "automatic turn accounting commits with admission and survives suppression, user \
     turns and restore"
  =
  let now = ref timestamp in
  let reject = ref false in
  let runs = ref 0 in
  let first_admission = ref None in
  let reject_pause = ref false in
  let saved_pause = ref None in
  Job_fixtures.with_actor
    ~now:(fun () -> !now)
    ~reject_save:(fun next ->
      match next.Agent_session.Session_transition.delta with
      | Automatic_turn_pauses_changed _ ->
        saved_pause := Some next;
        !reject_pause
      | _ ->
        (match !reject, next.Agent_session.Session_transition.state.active_operation with
         | true, Some _ -> true
         | false, Some { state = Starting; _ } ->
           if Option.is_none !first_admission then first_admission := Some next;
           false
         | _ -> false))
    (fun _ _ actor writer backend ->
       A.enable_automatic_turn_budget actor policy |> protocol_ok;
       A.set_operation_worker
         actor
         (Some
            (Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
               Int.incr runs;
               let state = A.state actor |> protocol_ok in
               Completed
                 { final_history = input.history
                 ; moderator_snapshot = state.moderator
                 ; runtime_requests = []
                 })))
       |> protocol_ok;
       let request () =
         A.change_moderator actor (Some (Setup.encode Setup.before))
         |> protocol_ok
         |> ignore;
         let parent = Job_fixtures.add_claimed_job actor in
         let event_id = ref None in
         Schedule_transaction_tests.with_event actor parent (fun owner commit ->
           (match owner with
            | Moderator_event id -> event_id := Some id
            | _ -> assert false);
           commit
             ~snapshot:Setup.after
             ~requests:
               P.Invocation.
                 { request_turn = true; request_compaction = false; end_session = None })
         |> protocol_ok
         |> ignore;
         Job_fixtures.complete actor parent |> protocol_ok |> ignore;
         Option.value_exn !event_id
       in
       let first = request () in
       let before = A.state actor |> protocol_ok in
       reject := true;
       assert (Result.is_error (A.apply_moderator_follow_up actor));
       reject := false;
       assert_same_session_snapshot before (A.state actor |> protocol_ok);
       assert_same_session_snapshot before (Agent_session.Memory_backend.state backend);
       [%test_eq: int] 0 !runs;
       assert (A.apply_moderator_follow_up actor |> protocol_ok);
       let state = await_idle actor in
       [%test_eq: int] 1 !runs;
       [%test_eq: int] 1 (budget state).followup_turns;
       let admission = Option.value_exn !first_admission in
       let transaction =
         Agent_store.Transaction.create
           ~session_id
           ~generation:0
           ~transaction_sequence:admission.state.counters.transaction_sequence
           ~previous_transaction_hash:None
           ~session_revision:admission.state.counters.revision
           ~first_event_sequence:
             (Option.map (List.hd admission.events) ~f:(fun event ->
                event.P.Event.Durable.sequence))
           ~last_event_sequence:
             (Option.map (List.last admission.events) ~f:(fun event ->
                event.P.Event.Durable.sequence))
           ~accepted_at_ns:
             (P.Timestamp.to_time_ns !now
              |> Time_ns.to_int63_ns_since_epoch
              |> Int63.to_int64)
           ~command_audit:None
           ~delta:
             (Sexp.to_string_mach (Agent_session.Session_delta.sexp_of_t admission.delta))
           ~durable_events:
             (List.map admission.events ~f:(fun event ->
                Sexp.to_string_mach (P.Event.Durable.sexp_of_t event)))
         |> store_ok
         |> Agent_store.Transaction.encode
         |> Agent_store.Transaction.decode
         |> store_ok
       in
       let replayed =
         Agent_session.Session_persistence.apply_transaction before transaction
         |> store_ok
       in
       assert_same_session_snapshot admission.state replayed;
       let before_pause = A.state actor |> protocol_ok in
       let pauses = [ R.Pause_followup_turns; Pause_internal_event_drains ] in
       reject_pause := true;
       assert (Result.is_error (A.set_automatic_turn_pauses actor pauses));
       assert_same_session_snapshot before_pause (A.state actor |> protocol_ok);
       assert_same_session_snapshot
         before_pause
         (Agent_session.Memory_backend.state backend);
       reject_pause := false;
       A.set_automatic_turn_pauses actor pauses |> protocol_ok;
       let paused = A.state actor |> protocol_ok in
       [%test_eq: int] 1 (budget paused).followup_turns;
       [%test_eq: int64 list] (budget before_pause).started_ms (budget paused).started_ms;
       let pause_delta = (Option.value_exn !saved_pause).delta in
       let restored_delta =
         Agent_session.Session_delta.sexp_of_t pause_delta
         |> Sexp.to_string_mach
         |> Sexp.of_string
         |> Agent_session.Session_delta.t_of_sexp
       in
       let replayed_pause =
         Agent_session.Session_delta.apply before_pause restored_delta |> protocol_ok
       in
       assert (B.equal (budget paused) (budget replayed_pause));
       let restored_pause =
         Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t paused)
         |> Agent_session.Session_persistence.restore_snapshot
         |> store_ok
       in
       assert (B.equal (budget paused) (budget restored_pause));
       A.set_automatic_turn_pauses actor (List.rev pauses @ pauses) |> protocol_ok;
       assert_same_session_snapshot paused (A.state actor |> protocol_ok);
       A.set_automatic_turn_pauses actor [] |> protocol_ok;
       assert (B.equal (budget before_pause) (budget (A.state actor |> protocol_ok)));
       let admitted =
         List.find_exn state.moderator_executions ~f:(fun event ->
           P.Id.Moderator_execution.equal event.context.id first)
       in
       (match admitted.intent with
        | Some Applied -> ()
        | _ -> failwith "first request not admitted");
       let denied () =
         let id = request () in
         let previous = !runs in
         assert (A.apply_moderator_follow_up actor |> protocol_ok);
         let state = A.state actor |> protocol_ok in
         [%test_eq: int] previous !runs;
         assert (Option.is_none state.active_operation);
         let event =
           List.find_exn state.moderator_executions ~f:(fun event ->
             P.Id.Moderator_execution.equal event.context.id id)
         in
         (match event.intent with
          | Some (Discarded _) -> ()
          | _ -> failwith "suppressed intent not discarded");
         assert (not (A.apply_moderator_follow_up actor |> protocol_ok));
         let restored =
           Agent_session.Session_persistence.restore_snapshot
             (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
           |> store_ok
         in
         assert (B.equal (budget state) (budget restored));
         let repeated =
           Agent_session.Session_delta.apply
             restored
             (Automatic_turn_budget_enabled policy)
           |> protocol_ok
         in
         assert (B.equal (budget restored) (budget repeated));
         match B.decide (budget restored) ~now:!now with
         | Suppress_automatic_turn { notice_key; _ } -> print_endline notice_key
         | Allow_automatic_turn -> failwith "restored budget allowed suppressed request"
       in
       denied ();
       A.enable_automatic_turn_budget actor policy |> protocol_ok;
       let before = A.state actor |> protocol_ok in
       A.stop actor ~attachment_id:writer.id ~mode:Cancel |> protocol_ok |> ignore;
       let stopped = A.state actor |> protocol_ok in
       assert (
         Result.is_error
           (A.commit_administration
              actor
              ~command_audit:None
              ~attachment_id:writer.id
              ~expected_revision:stopped.counters.revision
              ~kind:Upgrade
              { stopped with automatic_turn_budget = None }));
       assert_same_session_snapshot stopped (A.state actor |> protocol_ok);
       A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
       assert (B.equal (budget before) (budget (A.state actor |> protocol_ok)));
       let before = A.state actor |> protocol_ok in
       assert (Result.is_error (A.enable_automatic_turn_budget actor R.default_policy));
       assert_same_session_snapshot before (A.state actor |> protocol_ok);
       let user sequence =
         let id =
           History_entry.Id.create ~namespace:"budget-user" ~sequence
           |> Result.ok_or_failwith
         in
         let entry =
           Agent_session.History_codec.user_text ~id "continue"
           |> Agent_session.History_codec.to_protocol
         in
         A.submit_message actor ~attachment_id:writer.id entry |> protocol_ok |> ignore;
         let state = await_idle actor in
         [%test_eq: int] 0 (budget state).followup_turns
       in
       let deferred_id =
         History_entry.Id.create ~namespace:"budget-deferred" ~sequence:0
         |> Result.ok_or_failwith
       in
       let deferred =
         Agent_session.History_codec.user_text ~id:deferred_id "queued user input"
         |> Agent_session.History_codec.to_protocol
       in
       A.defer_history actor ~attachment_id:writer.id [ deferred ]
       |> protocol_ok
       |> ignore;
       ignore (request () : P.Id.Moderator_execution.t);
       assert (A.apply_moderator_follow_up actor |> protocol_ok);
       let state = await_idle actor in
       [%test_eq: int] 0 (budget state).followup_turns;
       assert (List.is_empty state.conversation.deferred_user_entries);
       assert (
         List.exists state.conversation.canonical_history ~f:(fun entry ->
           History_entry.Id.equal entry.id deferred_id));
       ignore (request () : P.Id.Moderator_execution.t);
       assert (A.apply_moderator_follow_up actor |> protocol_ok);
       let state = await_idle actor in
       [%test_eq: int] 2 (List.length (budget state).started_ms);
       user 1;
       now := P.Timestamp.add_ms timestamp 1_000 |> protocol_ok;
       denied ();
       now := P.Timestamp.add_ms timestamp 1_001 |> protocol_ok;
       ignore (request () : P.Id.Moderator_execution.t);
       assert (A.apply_moderator_follow_up actor |> protocol_ok);
       let state = await_idle actor in
       [%test_eq: int] 5 !runs;
       [%test_eq: int] 1 (budget state).followup_turns;
       [%test_eq: int] 1 (List.length (budget state).started_ms);
       assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
       print_endline
         "save rollback, once-only admission, restored limits and user/rate separation \
          passed");
  [%expect
    {|
    budget:max-followup-turns
    budget:turn-rate-limit
    save rollback, once-only admission, restored limits and user/rate separation passed
    |}]
;;

let%expect_test "shared host policy keeps precedence and does not wrap the rate window" =
  let module Policy = Chat_response.Automatic_turn_policy in
  List.iter
    [ ( "pause"
      , { policy with
          budget = { policy.budget with pause_conditions = [ Pause_followup_turns ] }
        }
      , 1_000L )
    ; "rate", policy, 1_000L
    ; "count", policy, 2_001L
    ; ( "wide-window"
      , { policy with
          budget =
            { policy.budget with
              turn_rate_limit = Some { max_turns = 1; window_ms = Int.max_value }
            }
        }
      , Int64.min_value )
    ]
    ~f:(fun (label, policy, now_ms) ->
      match
        Policy.decide
          ~policy
          ~followup_turns_started_since_user_submit:1
          ~started_followup_turn_timestamps_ms:[ 0L; 1_000L ]
          ~now_ms
      with
      | Allow_automatic_turn -> failwith "policy bypassed"
      | Suppress_automatic_turn { notice_key; _ } ->
        print_s [%sexp (label : string), (notice_key : string)]);
  [%expect
    {|
    (pause budget:pause-followup-turns)
    (rate budget:turn-rate-limit)
    (count budget:max-followup-turns)
    (wide-window budget:turn-rate-limit)
    |}]
;;
