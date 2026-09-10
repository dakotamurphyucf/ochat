open Core
open Fixtures
module P = Agent_protocol
module A = Agent_session.Session_actor
module D = P.Delivery
module N = Agent_session.Notification_delivery
module State = Agent_session.Session_state
module Setup = Subscription_transaction_tests

type mode =
  | Fresh
  | Recovered
  | Revoked
  | Suppressed
  | Stopped
  | Quiet
[@@deriving sexp_of]

let initial mode registry state =
  let source = Setup.source in
  let event =
    P.Moderator_execution.create
      { id = P.Id.Moderator_execution.create ()
      ; session_id
      ; generation = 0
      ; source
      ; operation_id = None
      ; job = None
      ; phase = Internal_event
      ; event = `Null
      ; checkpoint_sha256 = String.make 64 'a'
      ; created_at = timestamp
      }
    |> protocol_ok
  in
  let finished =
    P.Moderator_execution.complete
      event
      ~checkpoint_sha256:(String.make 64 'b')
      ~requests:{ request_turn = false; request_compaction = false; end_session = None }
    |> protocol_ok
  in
  let pins = Chat_response.Background_request.capability_pins registry |> protocol_ok in
  let values =
    List.map [ "one"; "two" ] ~f:(fun text ->
      D.create
        ~disclosure_pins:pins
        { id = P.Id.Delivery.create ()
        ; session_id
        ; generation = 0
        ; invocation_id = None
        ; work = None
        ; correlation = text
        ; source = Moderator
        ; completion = Succeeded (`String text)
        ; wake =
            (match mode with
             | Quiet -> No_wake
             | _ -> Request_turn)
        ; created_at = timestamp
        ; ownership = Some { source; creator = Moderator_event event.context.id }
        }
      |> protocol_ok)
  in
  let policy =
    match mode with
    | Suppressed ->
      { Chat_response.Runtime_semantics.default_policy with
        budget =
          { Chat_response.Runtime_semantics.default_budget_policy with
            max_followup_turns = 0
          }
      }
    | _ -> Chat_response.Runtime_semantics.default_policy
  in
  let state =
    { state with
      State.lifecycle = { desired = Running; observed = Idle }
    ; moderator = Some (Setup.encode Setup.after)
    }
  in
  let publications =
    match mode with
    | Recovered | Revoked ->
      List.mapi values ~f:(fun sequence value ->
        let id =
          History_entry.Id.create ~namespace:"before-crash" ~sequence
          |> Result.ok_or_failwith
        in
        let entry = Agent_session.Notification_history.create ~id value |> protocol_ok in
        let value =
          D.commit ~track_wake:true value ~history_id:id ~now:timestamp |> protocol_ok
        in
        Agent_session.Session_delta.Delivery_committed (value, entry))
    | _ -> []
  in
  let state =
    Agent_session.Session_transition.apply
      ~now:timestamp
      state
      ~payloads:[]
      ~delta:
        Agent_session.Session_delta.(
          Batch
            ([ Automatic_turn_budget_enabled policy
             ; Moderator_execution_changed event
             ; Moderator_execution_changed finished
             ]
             @ List.map values ~f:(fun value -> Delivery_changed value)
             @ publications))
    |> protocol_ok
    |> fun transition -> transition.Agent_session.Session_transition.state
  in
  let state = { state with lifecycle = { desired = Stopped; observed = Stopped } } in
  Agent_session.Session_persistence.restore_snapshot
    (Sexp.to_string_mach (State.sexp_of_t state))
  |> store_ok
;;

let%expect_test
    "idle notification save retries and restored wake decisions preserve original \
     history identity"
  =
  List.iter [ Fresh; Recovered; Revoked; Suppressed; Stopped; Quiet ] ~f:(fun mode ->
    let registry = Notification_disclosure_tests.registry [ "read_file" ] (ref 0) in
    let current =
      match mode with
      | Revoked ->
        Notification_disclosure_tests.registry
          ~resources:"revoked"
          [ "read_file" ]
          (ref 0)
      | _ -> registry
    in
    let reject = ref false in
    let operations = ref [] in
    Job_fixtures.with_actor
      ~prepare_state:(initial mode registry)
      ~reject_save:(fun _ -> !reject)
      (fun _ _ actor writer backend ->
         A.set_operation_worker
           actor
           (Some
              (Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
                 operations := input.operation.id :: !operations;
                 let state = A.state actor |> protocol_ok in
                 Completed
                   { final_history = input.history
                   ; moderator_snapshot = state.moderator
                   ; runtime_requests = []
                   })))
         |> protocol_ok;
         let prepare () =
           N.prepare_idle
             ~state:(A.state actor |> protocol_ok)
             ~source:Setup.source
             ~current_capabilities:current
             ~policy:Chat_response.One_off_request.default_policy
             ~max_count:64
           |> protocol_ok
         in
         let original = A.state actor |> protocol_ok in
         assert (N.has_idle_work original);
         let plan = prepare () in
         A.reserve_history_block actor ~count:1 |> protocol_ok |> ignore;
         (match A.deliver_idle_notifications actor plan with
          | Error { code = Conflict; _ } -> ()
          | _ -> failwith "accepted stale idle proposal");
         (match mode with
          | Stopped ->
            A.stop actor ~attachment_id:writer.id ~mode:Cancel |> protocol_ok |> ignore;
            let stopped = A.state actor |> protocol_ok in
            assert (not (A.deliver_idle_notifications actor (prepare ()) |> protocol_ok));
            assert_same_session_snapshot stopped (A.state actor |> protocol_ok);
            A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore
          | _ -> ());
         let before = A.state actor |> protocol_ok in
         let plan = prepare () in
         reject := true;
         assert (Result.is_error (A.deliver_idle_notifications actor plan));
         reject := false;
         assert_same_session_snapshot before (A.state actor |> protocol_ok);
         assert_same_session_snapshot before (Agent_session.Memory_backend.state backend);
         assert (List.is_empty !operations);
         assert (A.deliver_idle_notifications actor plan |> protocol_ok);
         let state = await_idle actor in
         assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
         [%test_eq: int]
           2
           (List.count state.conversation.canonical_history ~f:(fun entry ->
              match entry.P.History.provenance with
              | Runtime_notification _ -> true
              | _ -> false));
         (match mode with
          | Recovered | Revoked ->
            assert (
              List.equal
                P.History.equal_entry
                original.conversation.canonical_history
                state.conversation.canonical_history)
          | _ -> ());
         List.iter state.deliveries ~f:(fun value ->
           match mode, value.status, value.wake_disposition with
           | (Fresh | Recovered | Stopped), Committed _, Some (Accepted_wake id) ->
             assert (P.Id.Operation.equal id (List.hd_exn !operations))
           | (Revoked | Suppressed), Committed _, Some (Discarded_wake _) -> ()
           | Quiet, Committed _, None -> ()
           | _ -> failwith "invalid idle disposition");
         [%test_eq: int]
           (match mode with
            | Fresh | Recovered | Stopped -> 1
            | _ -> 0)
           (List.length !operations);
         assert (not (N.has_idle_work state));
         assert (not (A.deliver_idle_notifications actor (prepare ()) |> protocol_ok));
         assert_same_session_snapshot state (A.state actor |> protocol_ok);
         print_s
           [%sexp
             (mode : mode)
           , (List.length !operations : int)
           , "two original frames; no repeated wake"]));
  [%expect
    {|
    (Fresh 1 "two original frames; no repeated wake")
    (Recovered 1 "two original frames; no repeated wake")
    (Revoked 0 "two original frames; no repeated wake")
    (Suppressed 0 "two original frames; no repeated wake")
    (Stopped 1 "two original frames; no repeated wake")
    (Quiet 0 "two original frames; no repeated wake")
    |}]
;;
