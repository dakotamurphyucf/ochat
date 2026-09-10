open Core
open Fixtures
module P = Agent_protocol
module A = Agent_session.Session_actor
module N = Agent_session.Notification_delivery
module State = Agent_session.Session_state
module Setup = Notification_idle_tests

let frames state =
  List.filter state.State.conversation.canonical_history ~f:(fun entry ->
    match entry.P.History.provenance with
    | Runtime_notification _ -> true
    | _ -> false)
;;

let%expect_test
    "compaction retains pending publications and saved wakes without reinserting \
     archived frames"
  =
  List.iter [ Setup.Fresh; Recovered ] ~f:(fun mode ->
    let registry = Notification_disclosure_tests.registry [ "read_file" ] (ref 0) in
    let runs = ref 0 in
    Job_fixtures.with_actor
      ~prepare_state:(Setup.initial mode registry)
      (fun _ _ actor writer backend ->
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
         let prepare () =
           N.prepare_idle
             ~state:(A.state actor |> protocol_ok)
             ~source:Subscription_transaction_tests.source
             ~current_capabilities:registry
             ~policy:Chat_response.One_off_request.default_policy
             ~max_count:64
           |> protocol_ok
         in
         let before = A.state actor |> protocol_ok in
         let old_plan = prepare () in
         A.compact
           actor
           ~attachment_id:writer.id
           ~expected_revision:(Some before.counters.revision)
         |> protocol_ok
         |> ignore;
         let compacted = await_idle actor in
         [%test_eq: int] 1 compacted.conversation.compaction_generation;
         [%test_eq: int] 1 (List.length compacted.conversation.compaction_archives);
         [%test_eq: int] 0 (List.length (frames compacted));
         assert (List.equal P.Delivery.equal before.deliveries compacted.deliveries);
         assert (
           Agent_session.Automatic_turn_budget.equal
             (Option.value_exn before.automatic_turn_budget)
             (Option.value_exn compacted.automatic_turn_budget));
         (match A.deliver_idle_notifications actor old_plan with
          | Error { code = Conflict; _ } -> ()
          | _ -> failwith "compaction did not invalidate old notification proposal");
         assert_same_session_snapshot compacted (A.state actor |> protocol_ok);
         let restored =
           State.sexp_of_t compacted
           |> Sexp.to_string_mach
           |> Agent_session.Session_persistence.restore_snapshot
           |> store_ok
         in
         assert (List.equal P.Delivery.equal compacted.deliveries restored.deliveries);
         assert (A.deliver_idle_notifications actor (prepare ()) |> protocol_ok);
         let final = await_idle actor in
         [%test_eq: int] 1 !runs;
         let expected =
           match mode with
           | Setup.Fresh -> 2
           | _ -> 0
         in
         [%test_eq: int] expected (List.length (frames final));
         assert (
           List.for_all compacted.conversation.canonical_history ~f:(fun entry ->
             List.mem
               final.conversation.canonical_history
               entry
               ~equal:P.History.equal_entry));
         assert (
           List.for_all final.deliveries ~f:(fun value ->
             match value.wake_disposition with
             | Some (Accepted_wake _) -> true
             | _ -> false));
         assert (not (N.has_idle_work final));
         assert (not (A.deliver_idle_notifications actor (prepare ()) |> protocol_ok));
         assert_same_session_snapshot final (Agent_session.Memory_backend.state backend);
         print_s
           [%sexp (expected : int), "new frames; one wake; archived data not reinserted"]));
  [%expect
    {|
    (2 "new frames; one wake; archived data not reinserted")
    (0 "new frames; one wake; archived data not reinserted")
    |}]
;;
