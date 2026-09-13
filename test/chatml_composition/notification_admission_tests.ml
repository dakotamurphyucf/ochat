open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol

type mode =
  | Immediate
  | Queued
  | Observation
  | Turn_end
  | Invalid_ack
  | Nested
[@@deriving sexp_of]

let sources mode =
  let later =
    match mode with
    | Queued ->
      "| `Internal_event(payload) -> publish(state.subscription, state.invocation)\n"
    | Observation ->
      "| `Tool_observed(p) -> publish(state.subscription, state.invocation)\n"
    | Turn_end ->
      "| `Turn_end -> (match state.finished with | true -> Task.pure(state) | false -> \
       publish(state.subscription, state.invocation))\n"
    | _ -> ""
  in
  let work =
    match mode with
    | Immediate | Invalid_ack | Nested ->
      "let* state = publish(id, p.context.invocation_id) in\n"
    | Queued -> "let* () = Runtime.emit(`String(\"publish\")) in\n"
    | Observation ->
      {|let* result = Tool.call("read_file", `Object([
        { key = "root"; value = `String("reports") },
        { key = "file"; value = `String("report-a.json") }])) in|}
      ^ "\n"
    | Turn_end -> ""
  in
  let acknowledgement =
    match mode with
    | Invalid_ack -> "`Null"
    | _ -> "`String(\"accepted\")"
  in
  let agent =
    Background_fixtures.native_agent
    ^ {|
<script id="publisher" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = { subscription = ""; invocation = ""; delivery = ""; finished = true }
let publish subscription invocation =
  let* result = Subscription.complete(subscription, 0, `String("ready")) in
  let reference = { key = "ready"; invocation_id = `Some(invocation); work = `Some(`Subscription(subscription)) } in
  let* () = Task.catch(
    (let* discarded = Notification.publish(reference, `Succeeded(`String("ready")), `No_wake) in Task.fail("discard")),
    fun error -> Task.pure(())) in
  let* rejected = Task.catch(
    (let* bad = Notification.publish(reference, `Succeeded(`String("forged")), `No_wake) in Task.pure(false)),
    fun error -> Task.pure(true)) in
  match rejected with
  | false -> Task.fail("accepted a different work result")
  | true ->
    let* delivery = Notification.publish(reference, `Succeeded(`String("ready")), `No_wake) in
    let* duplicate_rejected = Task.catch(
      (let* duplicate = Notification.publish(reference, `Succeeded(`String("ready")), `No_wake) in Task.pure(false)),
      fun error -> Task.pure(true)) in
    (match duplicate_rejected with
     | false -> Task.fail("admitted two delivery owners")
     | true ->
       let* view = Notification.get(delivery) in
       Task.pure({ subscription = subscription; invocation = invocation; delivery = delivery; finished = true }))
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let* id = Subscription.create("watch", `None, `No_wake) in
  let state = { subscription = id; invocation = p.context.invocation_id; delivery = ""; finished = false } in
|}
    ^ work
    ^ "let* () = Invocation.resolve(p.context.invocation_id, `Pending(`Subscription(id), "
    ^ acknowledgement
    ^ ")) in\nTask.pure(state)\n"
    ^ later
    ^ {|
| _ -> Task.pure(state)
</script>
<tool name="watch" type="moderator" moderator="publisher" input_schema="input.json" output_schema="accepted.json" completion_schema="completion.json"/>
|}
    ^
    match mode with
    | Nested -> "<tool name=\"run_chatml\"/>"
    | _ -> ""
  in
  [ "agent.chatmd", agent
  ; "input.json", "true"
  ; "accepted.json", {|{"const":"accepted"}|}
  ; "completion.json", {|{"type":"string"}|}
  ]
;;

let%expect_test
    "daemon ChatML publishes owned intents with handler state and preserves invalid-ack \
     rollback"
  =
  List.iter
    [ Immediate; Queued; Observation; Turn_end; Invalid_ack; Nested ]
    ~f:(fun mode ->
      let name, input =
        match mode with
        | Nested ->
          ( "run_chatml"
          , `Object
              [ ( "source"
                , `String
                    {|let main input = let* result = Tool.call("watch", input) in
            match result with | `Ok(value) -> Task.pure(value) | `Error(code) -> Task.fail(code)|}
                )
              ; "input", `Null
              ; "tools", `Array [ `String "watch" ]
              ] )
        | _ -> "watch", `Null
      in
      with_daemon
        ~sources:(sources mode)
        ~expect_moderator:true
        ~calls:[ "publish", name, input ]
        ~settle:(fun env entry ->
          match mode with
          | Invalid_ack -> ()
          | _ ->
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
              let rec wait () =
                let state =
                  A.state entry.Agent_server.Session_registry.actor |> protocol_ok
                in
                (match mode with
                 | Nested when List.is_empty state.deliveries ->
                   raise_s
                     [%sexp
                       "nested publication missing"
                     , (List.map state.invocations ~f:(fun invocation ->
                          invocation.P.Invocation.context.tool_name, invocation.status)
                        : (string * P.Invocation.status) list)]
                 | _ -> ());
                if List.is_empty state.deliveries
                then (
                  Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                  wait ())
              in
              wait ()))
        (fun state ->
           match mode, state.deliveries with
           | Invalid_ack, [] ->
             assert (List.is_empty state.subscriptions);
             print_endline "Invalid_ack: all staged work discarded"
           | (Immediate | Queued | Observation | Turn_end | Nested), [ delivery ] ->
             let subscription = List.hd_exn state.subscriptions in
             assert (
               Option.equal
                 P.Id.Invocation.equal
                 delivery.context.invocation_id
                 (Some subscription.context.invocation_id));
             assert (
               P.Completion.equal
                 delivery.context.completion
                 (Succeeded (`String "ready")));
             (match delivery.status with
              | Committed _ -> ()
              | status ->
                raise_s
                  [%sexp "notification was not delivered", (status : P.Delivery.status)]);
             let ownership = Option.value_exn delivery.context.ownership in
             assert (Option.is_some delivery.disclosure_pins);
             assert (String.equal ownership.source.script_id "publisher");
             assert (
               Option.equal
                 P.Invocation.equal_observer
                 subscription.context.source
                 (Some ownership.source));
             let notifications =
               List.filter state.conversation.canonical_history ~f:(fun entry ->
                 match entry.P.History.provenance with
                 | Runtime_notification _ -> true
                 | _ -> false)
             in
             [%test_eq: int] 1 (List.length notifications);
             let notification = List.hd_exn notifications in
             Agent_session.Notification_history.validate ~delivery notification
             |> protocol_ok;
             let index id =
               List.findi_exn state.conversation.canonical_history ~f:(fun _ entry ->
                 History_entry.Id.equal entry.id id)
               |> fst
             in
             List.iter state.invocations ~f:(fun invocation ->
               match invocation.context.origin, invocation.output_entry_id with
               | Model, Some id -> assert (index id < index notification.id)
               | _ -> ());
             let restored =
               Agent_session.Session_persistence.restore_snapshot
                 (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
               |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
               |> protocol_ok
             in
             assert (
               Jsonaf.exactly_equal
                 (P.Delivery.to_json delivery)
                 (P.Delivery.to_json (List.hd_exn restored.deliveries)));
             print_s [%sexp (mode : mode), ("Committed" : string)]
           | _ -> failwith "unexpected notification admission result"));
  [%expect
    {|
    (Immediate Committed)
    (Queued Committed)
    (Observation Committed)
    (Turn_end Committed)
    Invalid_ack: all staged work discarded
    (Nested Committed)
    |}]
;;
