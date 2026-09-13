open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol

type trigger =
  | Immediate
  | Queued
  | Observation
  | Turn_end
[@@deriving sexp_of]

let agent trigger =
  let finish =
    {|let* result = Subscription.complete(state.id, 0, `String("ready")) in
      let* repeated = Subscription.cancel(state.id, 0, "too late") in
      Task.pure({ id = state.id; finished = true })|}
  in
  let later =
    match trigger with
    | Immediate -> ""
    | Queued -> "| `Internal_event(payload) -> " ^ finish
    | Observation -> "| `Tool_observed(p) -> " ^ finish
    | Turn_end ->
      "| `Turn_end -> (match state with\n"
      ^ "| { id = \"\"; _ } -> Task.pure(state)\n"
      ^ "| { finished = true; _ } -> Task.pure(state)\n| _ -> "
      ^ finish
      ^ ")"
  in
  let work =
    match trigger with
    | Immediate -> {|let* result = Subscription.complete(id, 0, `String("ready")) in|}
    | Queued -> {|let* () = Runtime.emit(`String("complete")) in|}
    | Observation ->
      {|let* result = Tool.call("read_file", `Object([
        { key = "root"; value = `String("reports") },
        { key = "file"; value = `String("report-a.json") }
      ])) in|}
    | Turn_end -> ""
  in
  Background_fixtures.native_agent
  ^ {|<script id="watcher" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = { id = ""; finished = false }
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let* () = Task.catch(
    (let* discarded = Subscription.create("discard", `None, `No_wake) in Task.fail("undo")),
    fun error -> Task.pure(())) in
  let* id = Subscription.create("watch", `None, `No_wake) in
  let* () = Task.catch(
    (let* invalid = Subscription.complete(id, 0, `Null) in Task.fail("invalid schema accepted")),
    fun error -> Task.pure(())) in
|}
  ^ work
  ^ "\n"
  ^ {|let* () = Invocation.resolve(p.context.invocation_id, `Pending(`Subscription(id), `String("accepted"))) in
  Task.pure({ id = id; finished = false })
|}
  ^ later
  ^ {|
| _ -> Task.pure(state)
</script>
<tool name="watch" type="moderator" moderator="watcher" input_schema="input.json"
 output_schema="accepted.json" completion_schema="completion.json"/>
|}
;;

let sources trigger =
  [ "agent.chatmd", agent trigger
  ; "input.json", "true"
  ; "accepted.json", {|{"const":"accepted"}|}
  ; "completion.json", {|{"type":"string"}|}
  ]
;;

let await_subscription env entry =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
    let rec wait () =
      let state = A.state entry.Agent_server.Session_registry.actor |> protocol_ok in
      match state.subscriptions with
      | [ value ] when Option.is_some value.result -> ()
      | _ ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
        wait ()
    in
    wait ())
;;

let%expect_test
    "daemon moderator subscriptions commit through handlers, queued events, observations \
     and ordinary hooks"
  =
  List.iter [ Immediate; Queued; Observation; Turn_end ] ~f:(fun trigger ->
    with_daemon
      ~expect_moderator:true
      ~sources:(sources trigger)
      ~calls:[ "watch-call", "watch", `Null ]
      ~settle:await_subscription
      (fun state ->
         let invocation = model_invocation state "watch-call" in
         let subscription = List.hd_exn state.subscriptions in
         [%test_eq: int] 1 (List.length state.subscriptions);
         assert (
           P.Id.Invocation.equal subscription.context.invocation_id invocation.context.id);
         (match outcome invocation with
          | Pending (Subscription id, `String "accepted") ->
            assert (P.Id.Subscription.equal id subscription.context.id)
          | other -> raise_s [%sexp (other : I.outcome)]);
         assert (
           Option.equal
             P.Completion.equal
             (Some (Succeeded (`String "ready")))
             subscription.result);
         assert (
           Option.exists subscription.context.source ~f:(fun source ->
             String.equal source.script_id "watcher"));
         assert (Option.is_some subscription.context.completion_schema);
         [%test_eq: int] 1 subscription.epoch;
         let originals =
           List.filter state.invocations ~f:(fun inv ->
             String.equal inv.context.tool_name "watch")
         in
         [%test_eq: int] 1 (List.length originals);
         print_s
           [%sexp (trigger : trigger), (subscription.result : P.Completion.t option)]));
  [%expect
    {|
    (Immediate ((Succeeded (String ready))))
    (Queued ((Succeeded (String ready))))
    (Observation ((Succeeded (String ready))))
    (Turn_end ((Succeeded (String ready))))
    |}]
;;

let%expect_test
    "nested moderator subscriptions retain their own invocation and invalid \
     acknowledgements leave no work"
  =
  List.iter [ `Nested; `Invalid_ack ] ~f:(fun mode ->
    let sources =
      sources Immediate
      |> List.map ~f:(fun (name, text) ->
        match name, mode with
        | "agent.chatmd", `Nested -> name, text ^ "<tool name=\"run_chatml\"/>"
        | "agent.chatmd", `Invalid_ack ->
          ( name
          , String.substr_replace_all text ~pattern:"`String(\"accepted\")" ~with_:"`Null"
          )
        | _ -> name, text)
    in
    let name, input =
      match mode with
      | `Invalid_ack -> "watch", `Null
      | `Nested ->
        ( "run_chatml"
        , `Object
            [ ( "source"
              , `String
                  {|let main input =
              let* result = Tool.call("watch", input) in
              match result with
              | `Ok(value) -> Task.pure(value)
              | `Error(code) -> Task.fail(code)|}
              )
            ; "input", `Null
            ; "tools", `Array [ `String "watch" ]
            ] )
    in
    with_daemon
      ~expect_moderator:true
      ~sources
      ~calls:[ "call", name, input ]
      (fun state ->
         match mode, result state "call", state.subscriptions with
         | `Invalid_ack, Fail _, [] ->
           print_endline "invalid acknowledgement discarded every subscription"
         | `Nested, Complete (`String "accepted"), [ subscription ] ->
           let creator =
             List.find_exn state.invocations ~f:(fun invocation ->
               P.Id.Invocation.equal
                 invocation.context.id
                 subscription.context.invocation_id)
           in
           assert (String.equal creator.context.tool_name "watch");
           assert (Option.is_some creator.context.parent_invocation);
           (match creator.status with
            | Resolved (Pending (Subscription id, `String "accepted")) ->
              assert (P.Id.Subscription.equal id subscription.context.id)
            | _ -> failwith "nested acknowledgement lost its originating invocation");
           assert (
             Option.equal
               P.Completion.equal
               (Some (Succeeded (`String "ready")))
               subscription.result);
           print_endline
             "nested moderator owns its completed subscription; caller receives \
              acknowledgement"
         | _ -> raise_s [%sexp (result state "call" : I.outcome)]));
  [%expect
    {|
    nested moderator owns its completed subscription; caller receives acknowledgement
    invalid acknowledgement discarded every subscription
    |}]
;;
