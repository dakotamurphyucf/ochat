open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module Admission = Notification_admission_tests

let%expect_test
    "foreground notification wakes bind actual provider admission and coalesce with tool \
     continuation"
  =
  List.iter [ `Immediate; `Late ] ~f:(fun mode ->
    let sources =
      Admission.sources
        (match mode with
         | `Immediate -> Immediate
         | `Late -> Turn_end)
      |> List.map ~f:(fun (path, source) ->
        path, String.substr_replace_all source ~pattern:"`No_wake" ~with_:"`Request_turn")
    in
    let expected =
      match mode with
      | `Immediate -> 2
      | `Late -> 3
    in
    let saw_data = ref false in
    with_daemon
      ~sources
      ~expect_moderator:true
      ~expected_requests:expected
      ~initial_requests:expected
      ~calls:[ "watch", "watch", `Null ]
      ~inspect_request:(fun request inputs ->
        if Int.equal request expected
        then (
          let matches =
            List.count inputs ~f:(function
              | Openai.Responses.Item.Input_message
                  { role = User; content = Text { text; _ } :: _; _ } ->
                String.is_prefix text ~prefix:"Ochat runtime notification."
              | _ -> false)
          in
          [%test_eq: int] 1 matches;
          saw_data := true))
      ~settle:(fun _ entry ->
        let state = A.state entry.Agent_server.Session_registry.actor |> protocol_ok in
        let delivery = List.hd_exn state.deliveries in
        let accepted =
          match delivery.wake_disposition with
          | Some (Accepted_wake id) -> id
          | disposition ->
            raise_s
              [%sexp
                "wake was not admitted"
              , (disposition : P.Delivery.wake_disposition option)]
        in
        let events =
          match
            Agent_session.Durable_event_log.replay
              entry.durable_events
              ~after_sequence:0L
              ~through_sequence:Int64.max_value
          with
          | Available events -> events
          | Snapshot_required -> failwith "operation audit missing"
        in
        let completed =
          List.filter_map events ~f:(fun event ->
            match
              P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
              |> protocol_ok
            with
            | Operation_completed operation -> Some operation
            | _ -> None)
        in
        [%test_eq: int] 1 (List.length completed);
        assert (P.Id.Operation.equal accepted (List.hd_exn completed).id))
      (fun state ->
         assert !saw_data;
         [%test_eq: int] 1 (List.length state.deliveries);
         let restored =
           Agent_session.Session_persistence.restore_snapshot
             (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
           |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
           |> protocol_ok
         in
         assert (List.equal P.Delivery.equal restored.deliveries state.deliveries);
         print_s
           [%sexp
             (mode : [ `Immediate | `Late ])
           , (expected : int)
           , ("accepted by the original operation" : string)]));
  [%expect
    {|
    (Immediate 2 "accepted by the original operation")
    (Late 3 "accepted by the original operation")
    |}]
;;
