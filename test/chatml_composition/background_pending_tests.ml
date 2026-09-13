open Core
open Agent_server_test_support
open Background_fixtures

let source handler =
  let body =
    {|Task.bind(Job.start_tool("read_file", `Object([
    {key="root"; value=`String("reports")},
    {key="file"; value=`String("second.txt")}
  ])), fun id -> |}
  in
  native_agent
  ^
  match handler with
  | `Standalone ->
    {|<script id="pending" language="chatml" kind="tool">
let run ctx input = |}
    ^ body
    ^ {|Task.pure(`Pending(`Job(id), `String("initial acknowledgement"))))
</script>
<tool name="pending" type="chatml" script="pending" entrypoint="run" input_schema="any.json" output_schema="string.json"><uses tool="read_file"/></tool>|}
  | `Moderator ->
    {|<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event = fun ctx state event -> match event with
| `Tool_invoked(p) -> |}
    ^ body
    ^ {|Task.bind(Invocation.resolve(p.context.invocation_id,
    `Pending(`Job(id), `String("initial acknowledgement"))), fun ignored -> Task.pure(state + 1)))
| _ -> Task.pure(state)
</script>
<tool name="pending" type="moderator" moderator="owner" input_schema="any.json" output_schema="string.json"/>|}
;;

let%expect_test "background Pending waits for the owned job's real completion" =
  List.iter [ `Standalone; `Moderator ] ~f:(fun handler ->
    with_background_daemon
      ~agent:(source handler)
      ~sources:[ "any.json", "true"; "string.json", {|{"type":"string"}|} ]
      (fun env client entry capabilities ->
         let parent =
           submit entry (B.to_json (tool capabilities "pending" (`Object [])))
         in
         let _, completion = await env client parent in
         let state = A.state entry.actor |> protocol_ok in
         [%test_eq: int] 2 (List.length state.jobs);
         let child =
           List.find_exn state.jobs ~f:(fun job ->
             not (Agent_protocol.Id.Job.equal job.id parent.id))
         in
         let _, child_completion = await env client child in
         assert (Completion.equal completion child_completion);
         (match completion with
          | Succeeded (`String text) ->
            assert (String.is_substring text ~substring:"second report")
          | completion -> raise_s [%sexp (completion : Completion.t)]);
         let invocation =
           List.find_exn state.invocations ~f:(fun invocation ->
             String.equal invocation.context.tool_name "pending")
         in
         (match invocation.status with
          | Resolved (Pending (Job id, `String "initial acknowledgement")) ->
            assert (Agent_protocol.Id.Job.equal child.id id)
          | status -> raise_s [%sexp (status : Agent_protocol.Invocation.status)]);
         print_s
           [%sexp (handler : [ `Standalone | `Moderator ]), "returned final file result"]));
  [%expect
    {|
    (Standalone "returned final file result")
    (Moderator "returned final file result")
    |}]
;;

let%expect_test "eventual results retain the parent's completion schema and output budget"
  =
  List.iter [ `Standalone; `Moderator ] ~f:(fun handler ->
    List.iter [ `Schema; `Output_limit ] ~f:(fun mode ->
      let agent =
        match mode with
        | `Schema ->
          String.substr_replace_all
            (source handler)
            ~pattern:{|output_schema="string.json"|}
            ~with_:{|output_schema="string.json" completion_schema="completion.json"|}
        | `Output_limit -> source handler
      in
      with_background_daemon
        ~agent
        ~sources:
          [ "any.json", "true"
          ; "string.json", {|{"type":"string"}|}
          ; "completion.json", {|{"type":"integer"}|}
          ]
        (fun env client entry capabilities ->
           let policy = Chat_response.One_off_request.default_policy in
           let policy =
             match mode with
             | `Schema -> policy
             | `Output_limit -> { policy with max_output_bytes = 65 }
           in
           let reference =
             C.find capabilities ~name:"pending"
             |> Result.map_error ~f:(fun error -> error.C.message)
             |> Result.ok_or_failwith
             |> C.reference
           in
           let request =
             B.tool ~capabilities ~reference ~input:(`Object []) ~policy |> protocol_ok
           in
           let parent = submit entry (B.to_json request) in
           let _, completion = await env client parent in
           (match completion with
            | Failed { code = "background.invalid_completion"; details = `Null; _ } -> ()
            | completion -> raise_s [%sexp (completion : Completion.t)]);
           let state = A.state entry.actor |> protocol_ok in
           [%test_eq: int] 2 (List.length state.jobs);
           let child =
             List.find_exn state.jobs ~f:(fun job ->
               not (Agent_protocol.Id.Job.equal job.id parent.id))
           in
           let _, child_completion = await env client child in
           (match child_completion with
            | Succeeded _ -> ()
            | _ -> failwith "child did not complete its actual work");
           print_s
             [%sexp
               (handler : [ `Standalone | `Moderator ])
             , (mode : [ `Schema | `Output_limit ])
             , "invalid eventual value withheld"])));
  [%expect
    {|
    (Standalone Schema "invalid eventual value withheld")
    (Standalone Output_limit "invalid eventual value withheld")
    (Moderator Schema "invalid eventual value withheld")
    (Moderator Output_limit "invalid eventual value withheld")
    |}]
;;
