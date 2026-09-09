open Core
open Fixtures
open Agent_server_test_support
module J = Agent_protocol.Job
module Completion = Agent_protocol.Completion

let%expect_test "nested standalone Pending is an acknowledgement backed by its owned job" =
  let declaration =
    {|<script id="nested" language="chatml" kind="tool" src="nested.chatml"/>
<tool name="nested" type="chatml" script="nested" entrypoint="run" input_schema="any.json" output_schema="string.json"><uses tool="direct"/></tool>|}
  in
  let source =
    {|let run ctx input = Task.bind(Tool.call("direct", input), fun result ->
    match result with
    | `Ok(acknowledgement) -> Task.pure(`Complete(acknowledgement))
    | `Error(code) -> Task.pure(`Fail({code=code; message="nested failed"; retryable=false; details=`Null})))|}
  in
  let sources =
    ("nested.chatml", source)
    :: List.map Job_launch_tests.sources ~f:(fun (name, source) ->
      ( name
      , if String.equal name "agent.chatmd" then source ^ "\n" ^ declaration else source ))
  in
  with_daemon
    ~sources
    ~settle:Job_launch_tests.settle
    ~calls:[ "nested", "nested", Job_launch_tests.read ]
    (fun state ->
       (match result state "nested" with
        | Complete (`String "accepted") -> ()
        | outcome -> raise_s [%sexp (outcome : I.outcome)]);
       let job = List.hd_exn state.jobs in
       [%test_eq: int] 1 (List.length state.jobs);
       let nested =
         List.find_exn state.invocations ~f:(fun invocation ->
           String.equal invocation.context.tool_name "direct")
       in
       (match nested.status with
        | Resolved (Pending (Job id, `String "accepted")) ->
          assert (Agent_protocol.Id.Job.equal job.id id)
        | status -> raise_s [%sexp (status : I.status)]);
       let launch = Option.value_exn job.launch in
       assert (J.equal_launch_owner launch.owner (Invocation nested.context.id));
       [%test_eq: int] 0 launch.nested_depth;
       assert (Option.is_some nested.context.parent_invocation);
       assert (Option.is_none nested.output_entry_id);
       (match J.terminal_completion job |> protocol_ok with
        | Some (Succeeded (`String value)) ->
          assert (String.is_substring value ~substring:(String.strip report_a))
        | completion -> raise_s [%sexp (completion : Completion.t option)]);
       print_endline
         "parent received acknowledgement; nested invocation owns one executed job");
  [%expect {| parent received acknowledgement; nested invocation owns one executed job |}]
;;
