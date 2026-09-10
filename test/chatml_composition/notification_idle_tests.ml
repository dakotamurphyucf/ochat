open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module Background = Background_fixtures

type mode =
  | Quiet
  | Wake
  | Denied
  | Stopped
[@@deriving sexp_of]

let sources mode =
  let wake =
    match mode with
    | Quiet -> "`No_wake"
    | Wake | Denied | Stopped -> "`Request_turn"
  in
  [ ( "agent.chatmd"
    , Background.native_agent
      ^ {|
<script id="publisher" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = ""
let on_event ctx state event = match event with
| `Tool_invoked(p) -> (match p.context.tool_name with
  | "watch" ->
    let* () = Invocation.resolve(p.context.invocation_id, `Complete(`String("ready"))) in
    Task.pure(p.context.invocation_id)
  | "finish" ->
    let reference = { key = "finished"; invocation_id = `Some(state); work = `None } in
    let* first = Notification.publish(reference, `Succeeded(`String("one")), |}
      ^ wake
      ^ {|
    ) in
    let* second = Notification.publish(reference, `Succeeded(`String("two")), |}
      ^ wake
      ^ {|
    ) in
    let* () = Invocation.resolve(p.context.invocation_id, `Complete(`String("published"))) in
    Task.pure(state)
  | _ -> Task.fail("unknown tool"))
| _ -> Task.pure(state)
</script>
<tool name="watch" type="moderator" moderator="publisher" input_schema="any.json" output_schema="string.json"/>
<tool name="finish" type="moderator" moderator="publisher" input_schema="any.json" output_schema="string.json"/>
|}
    )
  ; "any.json", "true"
  ; "string.json", {|{"type":"string"}|}
  ]
;;

let read entry = A.state entry.Agent_server.Session_registry.actor |> protocol_ok
let wait = Background_shell_tests.wait

let%expect_test
    "idle daemon notifications coalesce, keep quiet data and obey budget and stopped \
     lifecycle"
  =
  List.iter [ Quiet; Wake; Denied; Stopped ] ~f:(fun mode ->
    let seen = ref 0 in
    let final_frames = ref 0 in
    let blocked, signal_blocked = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let expected =
      match mode with
      | Quiet | Wake -> 3
      | Denied | Stopped -> 4
    in
    with_daemon
      ~sources:(sources mode)
      ~expect_moderator:true
      ~calls:[ "watch", "watch", `Null ]
      ~expected_requests:expected
      ~inspect_request:(fun number inputs ->
        seen := number;
        (match mode, number with
         | Stopped, 3 ->
           Eio.Promise.resolve signal_blocked ();
           Eio.Promise.await never
         | _ -> ());
        if number = expected
        then
          final_frames
          := List.count inputs ~f:(function
               | Openai.Responses.Item.Input_message
                   { role = User; content = Text { text; _ } :: _; _ } ->
                 String.is_prefix text ~prefix:"Ochat runtime notification."
               | _ -> false))
      ~after_turn:(fun env handle entry ->
        let idle () = wait env (fun () -> Option.is_none (read entry).active_operation) in
        (match mode with
         | Denied ->
           A.claim_idle_moderator entry.actor |> protocol_ok |> Option.value_exn |> ignore;
           A.complete_idle_moderator
             entry.actor
             { moderator_snapshot = (read entry).moderator
             ; runtime_requests = [ Request_turn ]
             ; notifications = []
             ; remaining_events = false
             }
           |> protocol_ok;
           idle ();
           [%test_eq: int] 3 !seen
         | Stopped ->
           H.send_message
             handle
             { kind = Plain_text; text = "Hold a foreground request."; attachments = [] }
           |> protocol_ok
           |> ignore;
           Eio.Promise.await blocked
         | Quiet | Wake -> ());
        let capabilities = Background_subscription_tests.capabilities entry in
        let request = Background.tool capabilities "finish" `Null in
        let job =
          Background.submit entry (Chat_response.Background_request.to_json request)
        in
        wait env (fun () ->
          let state = read entry in
          let job =
            List.find_exn state.jobs ~f:(fun value -> P.Id.Job.equal value.id job.id)
          in
          match P.Job.terminal_completion job |> protocol_ok with
          | Some (Succeeded (`String "published")) -> true
          | None -> false
          | Some completion -> raise_s [%sexp (completion : P.Completion.t)]);
        (match mode with
         | Stopped ->
           [%test_eq: int] 2 (List.length (read entry).deliveries);
           assert (
             List.for_all (read entry).deliveries ~f:(fun value ->
               match value.status with
               | Pending -> true
               | _ -> false));
           H.stop handle ~mode:Cancel |> protocol_ok |> ignore;
           Eio.Time.sleep (Eio.Stdenv.clock env) 0.03;
           [%test_eq: int] 3 !seen;
           assert (
             List.for_all (read entry).deliveries ~f:(fun value ->
               match value.status with
               | Pending -> true
               | _ -> false));
           H.start handle ~queue_if_limited:false |> protocol_ok |> ignore
         | _ -> ());
        wait env (fun () ->
          let state = read entry in
          List.length state.deliveries = 2
          && List.for_all state.deliveries ~f:(fun value ->
            match value.status with
            | Committed _ -> true
            | _ -> false)
          && Option.is_none state.active_operation);
        match mode with
        | Quiet | Denied ->
          [%test_eq: int]
            (match mode with
             | Quiet -> 2
             | _ -> 3)
            !seen;
          H.send_message
            handle
            { kind = Plain_text; text = "Read the retained results."; attachments = [] }
          |> protocol_ok
          |> ignore;
          idle ()
        | Wake | Stopped -> ())
      ~settle:(fun _ _ -> ())
      (fun state ->
         [%test_eq: int] 2 !final_frames;
         [%test_eq: int]
           2
           (List.count state.conversation.canonical_history ~f:(fun entry ->
              match entry.P.History.provenance with
              | Runtime_notification _ -> true
              | _ -> false));
         (match
            mode, List.map state.deliveries ~f:(fun value -> value.wake_disposition)
          with
          | Quiet, [ None; None ] -> ()
          | Denied, [ Some (Discarded_wake _); Some (Discarded_wake _) ] -> ()
          | (Wake | Stopped), [ Some (Accepted_wake first); Some (Accepted_wake second) ]
            -> assert (P.Id.Operation.equal first second)
          | _ -> failwith "unexpected idle wake dispositions");
         let restored =
           Agent_session.Session_persistence.restore_snapshot
             (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
           |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
           |> protocol_ok
         in
         assert (List.equal P.Delivery.equal state.deliveries restored.deliveries);
         print_s
           [%sexp (mode : mode), (expected : int), "two frames, one shared wake decision"]));
  [%expect
    {|
    (Quiet 3 "two frames, one shared wake decision")
    (Wake 3 "two frames, one shared wake decision")
    (Denied 4 "two frames, one shared wake decision")
    (Stopped 4 "two frames, one shared wake decision")
    |}]
;;
