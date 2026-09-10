open Core
open Fixtures
module P = Agent_protocol
module A = Agent_session.Session_actor
module O = Agent_session.Operation_worker
module N = Agent_session.Notification_delivery
module S = Agent_session.Script_notification_service
module Setup = Subscription_transaction_tests

let%expect_test
    "notification consumer rejects stale and failed saves, deduplicates input and \
     settles only admitted wakes"
  =
  List.iter [ `Accepted; `Native; `Discarded; `Revoked ] ~f:(fun mode ->
    let ready, signal_ready = Eio.Promise.create () in
    let finish, signal_finish = Eio.Promise.create () in
    let reject_save = ref false in
    with_handoff_actor
      ~reject:(fun next ->
        !reject_save
        && List.exists
             next.Agent_session.Session_transition.state.deliveries
             ~f:(fun value ->
               match value.status with
               | Committed _ | Failed _ -> true
               | Pending -> false))
      ~make_worker:(fun _ actor_ready ->
        O.create ~run:(fun ~sw:_ ~input capabilities ->
          Eio.Promise.resolve signal_ready (input, capabilities);
          Eio.Promise.await finish;
          (match mode with
           | `Accepted -> capabilities.admit_moderator_turn () |> protocol_ok
           | `Native -> capabilities.admit_notification_turn () |> protocol_ok
           | `Discarded | `Revoked -> ());
          let actor = Eio.Promise.await actor_ready in
          let state = A.state actor |> protocol_ok in
          Completed
            { final_history =
                Agent_session.History_codec.all_of_protocol
                  state.conversation.canonical_history
                |> protocol_ok
            ; moderator_snapshot = state.moderator
            ; runtime_requests = []
            }))
      (fun _ actor _ backend ->
         let input, capabilities = Eio.Promise.await ready in
         A.change_moderator actor (Some (Setup.encode Setup.before))
         |> protocol_ok
         |> ignore;
         capabilities.O.Capabilities.manage_moderator_follow_up ~observer:Setup.source
         |> protocol_ok;
         let registry = native_registry (ref 0) ~raises:false in
         capabilities.with_moderator_event
           ~event:Chat_response.Moderation.Event.Turn_end
           ~snapshot:(fun () -> Ok Setup.before)
           (fun ~executing ~retirement_reason:_ ~event:_ ~execute:_ ~commit ->
              S.with_scope
                (Notification_access_tests.notices actor)
                ~owner:(Moderator_event executing.context.id)
                ~source:Setup.source
                ~selected:registry
                ~jobs:None
                ~error:P.Error.invalid_request
                (fun scope ->
                   let open Result.Let_syntax in
                   let tx = S.moderator_transaction scope in
                   let error result =
                     Result.map_error result ~f:P.Error.invalid_request
                   in
                   let%bind receipt, _ =
                     tx.handlers.publish
                       ~correlation:{ key = "ready"; invocation_id = None; work = None }
                       ~completion:(Succeeded (`String "saved result"))
                       ~wake:Request_turn
                     |> error
                   in
                   let%bind ack = tx.prepare [ receipt ] |> error in
                   let%map () = Schedule_transaction_tests.save commit in
                   ack ()))
         |> protocol_ok
         |> ignore;
         let current_capabilities =
           match mode with
           | `Accepted | `Native | `Discarded -> registry
           | `Revoked ->
             Chat_response.Tool_capability.select registry ~names:[]
             |> Notification_disclosure_tests.cap
         in
         let prepare () =
           N.prepare
             ~state:(A.state actor |> protocol_ok)
             ~source:Setup.source
             ~current_capabilities
             ~policy:Chat_response.One_off_request.default_policy
             ~max_count:64
           |> protocol_ok
         in
         let plan = prepare () in
         A.reserve_history_block actor ~count:1 |> protocol_ok |> ignore;
         (match A.consume_notifications actor ~operation_id:input.operation.id plan with
          | Error { code = Conflict; _ } -> ()
          | _ -> failwith "stale proposal was accepted");
         let before = A.state actor |> protocol_ok in
         let plan = prepare () in
         reject_save := true;
         assert (
           Result.is_error
             (A.consume_notifications actor ~operation_id:input.operation.id plan));
         reject_save := false;
         assert_same_session_snapshot before (A.state actor |> protocol_ok);
         assert_same_session_snapshot before (Agent_session.Memory_backend.state backend);
         let batch =
           A.consume_notifications actor ~operation_id:input.operation.id plan
           |> protocol_ok
         in
         let expected =
           match mode with
           | `Revoked -> 0
           | `Accepted | `Native | `Discarded -> 1
         in
         [%test_eq: int] expected (List.length batch.entries);
         [%test_eq: bool] (expected > 0) batch.request_turn;
         assert (not batch.user_input);
         let repeat =
           A.consume_notifications actor ~operation_id:input.operation.id (prepare ())
           |> protocol_ok
         in
         assert (List.is_empty repeat.entries);
         assert (not repeat.request_turn);
         (match mode with
          | `Accepted | `Native ->
            let before_admission = A.state actor |> protocol_ok in
            reject_save := true;
            let admission =
              match mode with
              | `Accepted -> capabilities.admit_moderator_turn ()
              | _ -> capabilities.admit_notification_turn ()
            in
            assert (Result.is_error admission);
            reject_save := false;
            assert_same_session_snapshot before_admission (A.state actor |> protocol_ok);
            assert_same_session_snapshot
              before_admission
              (Agent_session.Memory_backend.state backend)
          | `Discarded | `Revoked -> ());
         Eio.Promise.resolve signal_finish ();
         let state = await_idle actor in
         assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
         let notices =
           List.filter state.conversation.canonical_history ~f:(fun entry ->
             match entry.P.History.provenance with
             | Runtime_notification _ -> true
             | _ -> false)
         in
         [%test_eq: int] expected (List.length notices);
         let delivery = List.hd_exn state.deliveries in
         (match mode, delivery.status, delivery.wake_disposition with
          | (`Accepted | `Native), Committed _, Some (Accepted_wake id) ->
            assert (P.Id.Operation.equal id input.operation.id)
          | `Discarded, Committed _, Some (Discarded_wake _) -> ()
          | `Revoked, Failed failure, None ->
            [%test_eq: string] "notification.disclosure_denied" failure.code;
            assert (Jsonaf.exactly_equal failure.details `Null);
            assert (not (String.is_substring failure.message ~substring:"saved result"))
          | _ -> failwith "wake disposition did not match admission");
         let restored =
           Agent_session.Session_persistence.restore_snapshot
             (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
           |> store_ok
         in
         assert (List.equal P.Delivery.equal state.deliveries restored.deliveries);
         print_s
           [%sexp
             (mode : [ `Accepted | `Native | `Discarded | `Revoked ])
           , (List.length notices : int)]));
  [%expect
    {|
    (Accepted 1)
    (Native 1)
    (Discarded 1)
    (Revoked 0)
    |}]
;;
