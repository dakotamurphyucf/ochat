open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module N = Agent_session.Notification_history

let%expect_test
    "daemon runtime reload retains notification provenance and sends supported provider \
     data"
  =
  let rendered = ref None in
  let seen = ref false in
  with_daemon
    ~sources:(Subscription_tests.sources Immediate)
    ~expect_moderator:true
    ~expected_requests:3
    ~calls:[ "watch-call", "watch", `Null ]
    ~inspect_request:(fun request inputs ->
      match request, !rendered with
      | 3, Some entry ->
        let matches =
          List.filter inputs ~f:(fun item ->
            Jsonaf.exactly_equal
              (Openai.Responses.Item.jsonaf_of_t item)
              entry.P.History.payload)
        in
        [%test_eq: int] 1 (List.length matches);
        (match List.hd_exn matches with
         | Input_message { role = User; _ } -> seen := true
         | _ -> failwith "unsupported notification provider role")
      | _ -> ())
    ~after_turn:(fun env handle registry_entry ->
      let actor = registry_entry.Agent_server.Session_registry.actor in
      let state = A.state actor |> protocol_ok in
      let sub = List.hd_exn state.subscriptions in
      let delivery =
        P.Delivery.create
          { id = P.Id.Delivery.create ()
          ; session_id = state.identity.session_id
          ; generation = state.identity.generation
          ; invocation_id = Some sub.context.invocation_id
          ; work = Some (Subscription sub.context.id)
          ; correlation = "subscription-ready"
          ; source = Moderator
          ; completion = Option.value_exn sub.result
          ; wake = No_wake
          ; created_at = P.Timestamp.now ()
          }
        |> protocol_ok
      in
      let id =
        History_entry.Id.create ~namespace:"notification-composition" ~sequence:0
        |> Result.ok_or_failwith
      in
      let entry = N.create ~id delivery |> protocol_ok in
      rendered := Some entry;
      let committed =
        P.Delivery.commit delivery ~history_id:id ~now:(P.Timestamp.now ()) |> protocol_ok
      in
      A.commit_extensions
        actor
        ~generation:state.identity.generation
        ~expected_revision:state.counters.revision
        [ Delivery delivery; Publish (committed, entry) ]
      |> protocol_ok
      |> ignore;
      H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
      Agent_server.Runtime_owner.unload registry_entry.runtime |> protocol_ok;
      H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
      H.send_message
        handle
        { kind = Plain_text; text = "Summarize the result."; attachments = [] }
      |> protocol_ok
      |> ignore;
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
        let rec wait () =
          let current = A.state actor |> protocol_ok in
          match current.active_operation, !seen with
          | None, true ->
            let actual =
              List.find_exn current.conversation.canonical_history ~f:(fun item ->
                History_entry.Id.equal item.id id)
            in
            assert (P.History.equal_entry entry actual);
            let effective =
              (Agent_session.Session_state.snapshot ~now:(P.Timestamp.now ()) current)
                .effective_history
              |> Option.value_exn
            in
            let projected =
              List.find_exn effective.entries ~f:(fun item ->
                History_entry.Id.equal item.id id)
            in
            assert (P.History.equal_entry entry projected)
          | _ ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
            wait ()
        in
        wait ()))
    ~settle:(fun _ _ -> ())
    (fun state ->
       [%test_eq: int] 1 (List.length state.deliveries);
       assert !seen;
       match (List.hd_exn state.deliveries).status with
       | Committed _ ->
         print_endline
           "one framed notification survived runtime reload and the next provider turn"
       | _ -> failwith "notification delivery lost its commit");
  [%expect
    {| one framed notification survived runtime reload and the next provider turn |}]
;;
