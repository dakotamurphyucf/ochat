open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module R = Chat_response.Runtime_semantics

type mode =
  | Limited
  | Paused
  | Zero
[@@deriving sexp_of]

let sources =
  [ ( "agent.chatmd"
    , Background_fixtures.native_agent
      ^ {|
<script id="drainer" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = { events = 0; observations = 0 }
let on_event ctx state event = match event with
| `Tool_invoked(p) -> (match p.context.tool_name with
  | "queue" ->
    let* result = Tool.call("read_file", `Object([
      { key = "root"; value = `String("reports") },
      { key = "file"; value = `String("report-a.json") }])) in
    let* () = Runtime.emit(`String("one")) in
    let* () = Runtime.emit(`String("two")) in
    let* () = Runtime.emit(`String("three")) in
    let* () = Invocation.resolve(p.context.invocation_id, `Complete(`String("queued"))) in
    Task.pure(state)
  | _ ->
    let* () = Invocation.resolve(p.context.invocation_id, `Complete(`String("ready"))) in
    Task.pure(state))
| `Tool_observed(p) -> Task.pure({ events = state.events; observations = state.observations + 1 })
| `Internal_event(p) ->
  let* () = match state.events == 0 with
    | true -> Runtime.request_turn()
    | false -> Task.pure(()) in
  Task.pure({ events = state.events + 1; observations = state.observations })
| _ -> Task.pure(state)
</script>
<tool name="watch" type="moderator" moderator="drainer" input_schema="any.json" output_schema="string.json"/>
<tool name="queue" type="moderator" moderator="drainer" input_schema="any.json" output_schema="string.json"/>
|}
    )
  ; "any.json", "true"
  ; "string.json", {|{"type":"string"}|}
  ]
;;

let snapshot state =
  let fields =
    P.Json_codec.fields (Option.value_exn state.Agent_session.Session_state.moderator)
    |> protocol_ok
  in
  let encoded =
    P.Json_codec.required_as fields "identity_snapshot_sexp" P.Json_codec.string
    |> protocol_ok
  in
  Session.Moderator_state.Identity_snapshot.t_of_sexp (Sexp.of_string encoded)
;;

let events state =
  match (snapshot state).current_state with
  | Record fields ->
    (match List.Assoc.find_exn fields "events" ~equal:String.equal with
     | Int count -> count
     | _ -> assert false)
  | _ -> assert false
;;

let native state =
  List.find_exn state.Agent_session.Session_state.invocations ~f:(fun value ->
    String.equal value.context.tool_name "read_file")
;;

let%expect_test
    "qualified daemon pause and zero budgets retain idle work, resume preserves limits \
     and drains are bounded"
  =
  List.iter [ Limited; Paused; Zero ] ~f:(fun mode ->
    let policy =
      { R.default_policy with
        budget =
          { R.default_budget_policy with
            max_internal_event_drain =
              (match mode with
               | Zero -> 0
               | _ -> 1)
          ; pause_conditions =
              (match mode with
               | Paused -> [ Pause_internal_event_drains ]
               | _ -> [])
          }
      }
    in
    let entry_ref = ref None in
    let idle_at_provider = ref None in
    with_daemon
      ~runtime_policy:policy
      ~sources
      ~expect_moderator:true
      ~calls:[ "watch", "watch", `Null ]
      ~expected_requests:
        (match mode with
         | Zero -> 2
         | _ -> 3)
      ~inspect_request:(fun number _ ->
        if number = 3
        then (
          let state =
            A.state (Option.value_exn !entry_ref).Agent_server.Session_registry.actor
            |> protocol_ok
          in
          let count =
            List.count state.moderator_executions ~f:(fun event ->
              match
                ( event.P.Moderator_execution.context.phase
                , event.context.operation_id
                , event.status )
              with
              | Internal_event, None, Completed _ -> true
              | _ -> false)
          in
          [%test_eq: int] 1 count;
          idle_at_provider := Some count))
      ~after_turn:(fun env _ entry ->
        entry_ref := Some entry;
        let read () = A.state entry.actor |> protocol_ok in
        assert (
          List.exists (read ()).moderator_executions ~f:(fun event ->
            match event.context.phase, event.status with
            | Session_start, Completed _ -> true
            | _ -> false));
        let caps = Background_subscription_tests.capabilities entry in
        let request = Background_fixtures.tool caps "queue" `Null in
        let job =
          Background_fixtures.submit
            entry
            (Chat_response.Background_request.to_json request)
        in
        Background_shell_tests.wait env (fun () ->
          let job =
            List.find_exn (read ()).jobs ~f:(fun value -> P.Id.Job.equal value.id job.id)
          in
          match P.Job.terminal_completion job |> protocol_ok with
          | None -> false
          | Some (Succeeded (`String "queued")) -> true
          | Some completion -> raise_s [%sexp (completion : P.Completion.t)]);
        (match mode with
         | Limited -> ()
         | Paused | Zero ->
           let state = read () in
           [%test_eq: int] 0 (events state);
           [%test_eq: int] 3 (List.length (snapshot state).queued_internal_events);
           (match (native state).observation with
            | Some { status = Awaiting; _ } -> ()
            | _ -> failwith "paused observation was consumed");
           let before = Option.value_exn state.automatic_turn_budget in
           Agent_server.Runtime_owner.unload entry.runtime |> protocol_ok;
           Agent_server.Runtime_owner.ensure_loaded entry.runtime |> protocol_ok;
           Background_shell_tests.wait env (fun () ->
             List.exists (read ()).moderator_executions ~f:(fun event ->
               match event.context.phase, event.status with
               | Session_resume, Completed _ -> true
               | _ -> false));
           let state = read () in
           assert (
             Agent_session.Automatic_turn_budget.equal
               before
               (Option.value_exn state.automatic_turn_budget));
           [%test_eq: int] 3 (List.length (snapshot state).queued_internal_events);
           Agent_server.Runtime_owner.For_testing.with_loaded_runtime
             entry.runtime
             (fun () ->
                A.set_automatic_turn_pauses entry.actor [] |> protocol_ok;
                let after = Option.value_exn (read ()).automatic_turn_budget in
                [%test_eq: int] before.followup_turns after.followup_turns;
                [%test_eq: int64 list] before.started_ms after.started_ms;
                Ok ())
           |> protocol_ok);
        match mode with
        | Zero -> ()
        | Limited | Paused ->
          Background_shell_tests.wait env (fun () ->
            let state = read () in
            events state = 3
            && Option.is_none state.active_operation
            && Option.is_some !idle_at_provider))
      ~settle:(fun _ _ -> ())
      (fun state ->
         let count, remaining =
           match mode with
           | Zero -> 0, 3
           | _ -> 3, 0
         in
         [%test_eq: int] count (events state);
         [%test_eq: int] remaining (List.length (snapshot state).queued_internal_events);
         let budget = Option.value_exn state.automatic_turn_budget in
         [%test_eq: int]
           policy.budget.max_internal_event_drain
           budget.policy.budget.max_internal_event_drain;
         assert (Option.is_none state.failure);
         print_s
           [%sexp
             (mode : mode)
           , (count : int)
           , (remaining : int)
           , (!idle_at_provider : int option)]));
  [%expect
    {|
    (Limited 3 0 (1))
    (Paused 3 0 (1))
    (Zero 0 3 ())
    |}]
;;
