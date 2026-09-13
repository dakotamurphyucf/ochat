open Core
open Fixtures
open Agent_server_test_support
module J = Agent_protocol.Job

let%expect_test "moderator Pending acknowledges its job through direct and nested calls" =
  List.iter [ `Direct; `Nested; `Invalid_ack ] ~f:(fun mode ->
    let acknowledgement =
      match mode with
      | `Invalid_ack -> "`Null"
      | _ -> "`String(\"accepted\")"
    in
    let sources =
      [ "any.json", "true"
      ; "string.json", {|{"type":"string"}|}
      ; ( "agent.chatmd"
        , {|<developer>Inspect the reports.</developer>
<tool name="read_file"><read id="reports" path="${workspace}/reports"/></tool>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event = fun ctx state event -> match event with
| `Tool_invoked(p) -> Task.bind(Job.start_tool("read_file", `Object([
    {key="root"; value=`String("reports")},
    {key="file"; value=`String("report-a.json")}
  ])), fun id -> Task.bind(Invocation.resolve(p.context.invocation_id,
    `Pending(`Job(id), |}
          ^ acknowledgement
          ^ {|)), fun ignored -> Task.pure(state + 1)))
| _ -> Task.pure(state)
</script>
<tool name="counter" type="moderator" moderator="owner" input_schema="any.json" output_schema="string.json"/>
<script id="wrapper" language="chatml" kind="tool">
let run ctx input = Task.bind(Tool.call("counter", input), fun result -> match result with
| `Ok(ack) -> Task.pure(`Complete(ack))
| `Error(code) -> Task.fail(code))
</script>
<tool name="wrapper" type="chatml" script="wrapper" entrypoint="run" input_schema="any.json" output_schema="string.json"><uses tool="counter"/></tool>|}
        )
      ]
    in
    let name =
      match mode with
      | `Nested -> "wrapper"
      | _ -> "counter"
    in
    with_daemon
      ~expect_moderator:true
      ~sources
      ~settle:Job_launch_tests.settle
      ~calls:[ "call", name, `Object [] ]
      (fun state ->
         let outcome = result state "call" in
         match mode, outcome with
         | `Invalid_ack, Fail _ ->
           [%test_eq: int] 0 (List.length state.jobs);
           print_endline "invalid acknowledgement: no job published"
         | ( (`Direct | `Nested)
           , (Pending (_, `String "accepted") | Complete (`String "accepted")) ) ->
           (match mode, outcome with
            | `Direct, Pending _ | `Nested, Complete _ -> ()
            | _ -> failwith "caller received the wrong acknowledgement envelope");
           [%test_eq: int] 1 (List.length state.jobs);
           let job = List.hd_exn state.jobs in
           let invocation =
             List.find_exn state.invocations ~f:(fun invocation ->
               String.equal invocation.context.tool_name "counter")
           in
           (match invocation.status with
            | Published (Pending (Job id, _)) | Resolved (Pending (Job id, _)) ->
              assert (Agent_protocol.Id.Job.equal id job.id)
            | status -> raise_s [%sexp (status : I.status)]);
           assert (
             J.equal_launch_owner
               (Option.value_exn job.launch).owner
               (Invocation invocation.context.id));
           (match J.terminal_completion job |> protocol_ok with
            | Some (Succeeded (`String text)) ->
              assert (String.is_substring text ~substring:(String.strip report_a))
            | completion ->
              raise_s [%sexp (completion : Agent_protocol.Completion.t option)]);
           print_s
             [%sexp (mode : [ `Direct | `Nested | `Invalid_ack ]), "owned job completed"]
         | _, outcome -> raise_s [%sexp (outcome : I.outcome)]));
  [%expect
    {|
    (Direct "owned job completed")
    (Nested "owned job completed")
    invalid acknowledgement: no job published
    |}]
;;
