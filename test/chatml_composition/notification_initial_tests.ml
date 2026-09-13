open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol

let sources =
  [ ( "agent.chatmd"
    , Background_fixtures.native_agent
      ^ {|
<script id="publisher" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let* () = Invocation.resolve(p.context.invocation_id, `Complete(`String("ready"))) in
  Task.pure(state)
| `Item_appended(item) -> Task.pure(state + 1)
| _ -> Task.pure(state)
</script>
<tool name="watch" type="moderator" moderator="publisher" input_schema="any.json" output_schema="string.json"/>
|}
    )
  ; "any.json", "true"
  ; "string.json", {|{"type":"string"}|}
  ]
;;

let appended state =
  let fields =
    P.Json_codec.fields (Option.value_exn state.Agent_session.Session_state.moderator)
    |> protocol_ok
  in
  let encoded =
    P.Json_codec.required_as fields "identity_snapshot_sexp" P.Json_codec.string
    |> protocol_ok
  in
  let snapshot =
    Session.Moderator_state.Identity_snapshot.t_of_sexp (Sexp.of_string encoded)
  in
  match snapshot.current_state with
  | Int count -> count
  | _ -> failwith "invalid fixture counter"
;;

let%expect_test
    "a user winning the idle race satisfies pending and restored wakes before its first \
     provider call"
  =
  List.iter [ `Pending; `Committed ] ~f:(fun mode ->
    let operation = ref None in
    let frames = ref 0 in
    let callbacks = ref 0 in
    with_daemon
      ~sources
      ~expect_moderator:true
      ~calls:[ "watch", "watch", `Null ]
      ~expected_requests:3
      ~inspect_request:(fun number inputs ->
        if number = 3
        then
          frames
          := List.count inputs ~f:(function
               | Openai.Responses.Item.Input_message
                   { role = User; content = Text { text; _ } :: _; _ } ->
                 String.is_prefix text ~prefix:"Ochat runtime notification."
               | _ -> false))
      ~after_turn:(fun env _ entry ->
        Agent_server.Runtime_owner.For_testing.with_loaded_runtime
          entry.runtime
          (fun () ->
             let actor = entry.Agent_server.Session_registry.actor in
             let writer, _ =
               A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
             in
             let state = A.state actor |> protocol_ok in
             let before = appended state in
             let source =
               Agent_session.Runtime_builder.moderator_snapshot_observer state.moderator
               |> protocol_ok
               |> Option.value_exn
             in
             let creator = model_invocation state "watch" in
             let delivery =
               P.Delivery.create
                 ~disclosure_pins:[]
                 { id = P.Id.Delivery.create ()
                 ; session_id = state.identity.session_id
                 ; generation = state.identity.generation
                 ; invocation_id = Some creator.context.id
                 ; work = None
                 ; correlation = "user-first"
                 ; source = Moderator
                 ; completion = Succeeded (`String "saved result")
                 ; wake = Request_turn
                 ; created_at = P.Timestamp.now ()
                 ; ownership = Some { source; creator = Invocation creator.context.id }
                 }
               |> protocol_ok
             in
             let changes =
               match mode with
               | `Pending -> [ A.Extension_change.Delivery delivery ]
               | `Committed ->
                 let id =
                   History_entry.Id.create
                     ~namespace:"preexisting-notification"
                     ~sequence:0
                   |> Result.ok_or_failwith
                 in
                 let frame =
                   Agent_session.Notification_history.create ~id delivery |> protocol_ok
                 in
                 let saved =
                   P.Delivery.commit
                     ~track_wake:true
                     delivery
                     ~history_id:id
                     ~now:(P.Timestamp.now ())
                   |> protocol_ok
                 in
                 [ A.Extension_change.Delivery delivery; Publish (saved, frame) ]
             in
             A.commit_extensions
               actor
               ~generation:state.identity.generation
               ~expected_revision:state.counters.revision
               changes
             |> protocol_ok
             |> ignore;
             let id =
               History_entry.Id.create ~namespace:"user-before-idle" ~sequence:0
               |> Result.ok_or_failwith
             in
             let user =
               Agent_session.History_codec.user_text ~id "Use the saved result."
               |> Agent_session.History_codec.to_protocol
             in
             let submitted =
               A.submit_message actor ~attachment_id:writer.id user |> protocol_ok
             in
             operation := submitted.operation_id;
             Background_shell_tests.wait env (fun () ->
               Option.is_none (A.state actor |> protocol_ok).active_operation);
             let state = A.state actor |> protocol_ok in
             callbacks := appended state - before;
             [%test_eq: int]
               0
               (Option.value_exn state.automatic_turn_budget).followup_turns;
             Ok ())
        |> protocol_ok)
      ~settle:(fun _ _ -> ())
      (fun state ->
         [%test_eq: int] 1 !frames;
         [%test_eq: int]
           (match mode with
            | `Pending -> 2
            | `Committed -> 1)
           !callbacks;
         [%test_eq: int] 1 (List.length state.deliveries);
         (match (List.hd_exn state.deliveries).wake_disposition with
          | Some (Accepted_wake id) ->
            assert (P.Id.Operation.equal id (Option.value_exn !operation))
          | _ -> failwith "user admission did not satisfy saved wake");
         assert (not (Agent_session.Notification_delivery.has_idle_work state));
         print_s
           [%sexp
             (mode : [ `Pending | `Committed ])
           , (!callbacks : int)
           , "one user provider call; no extra wake"]));
  [%expect
    {|
    (Pending 2 "one user provider call; no extra wake")
    (Committed 1 "one user provider call; no extra wake")
    |}]
;;
