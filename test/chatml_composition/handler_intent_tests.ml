open Core
open Agent_server_test_support
open Background_fixtures

let%expect_test
    "background moderator end request is committed with its result and applied after job \
     completion"
  =
  let agent =
    native_agent
    ^ {|
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event = fun ctx state event -> match event with
| `Tool_invoked(p) -> Task.bind(Runtime.end_session("background work finished"),
    fun ignored -> Task.bind(Invocation.resolve(p.context.invocation_id,
      `Complete(`String("finished"))), fun ignored -> Task.pure(state + 1)))
| _ -> Task.pure(state)
</script>
<tool name="finish" type="moderator" moderator="owner" input_schema="input.json" output_schema="output.json"/>|}
  in
  with_background_daemon
    ~agent
    ~sources:[ "input.json", {|{"type":"object"}|}; "output.json", {|{"type":"string"}|} ]
    (fun env client entry capabilities ->
       let job = submit entry (B.to_json (tool capabilities "finish" (`Object []))) in
       let _, completion = await env client job in
       (match completion with
        | Succeeded _ -> ()
        | _ -> raise_s [%sexp (completion : Completion.t)]);
       print_s [%sexp (completion : Completion.t)];
       let rec wait_stopped () =
         let state = A.state entry.actor |> protocol_ok in
         match state.lifecycle.desired with
         | Stopped -> state
         | Running ->
           Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
           wait_stopped ()
       in
       let state = wait_stopped () in
       let handler =
         List.find_exn state.invocations ~f:(fun invocation ->
           String.equal invocation.context.tool_name "finish")
       in
       print_s
         [%sexp
           (handler.handler_intent : Agent_protocol.Invocation.handler_intent option)];
       assert state.halted;
       let restored =
         Agent_protocol.Invocation.of_json (Agent_protocol.Invocation.to_json handler)
         |> protocol_ok
       in
       assert (Agent_protocol.Invocation.equal handler restored));
  [%expect
    {|
    (Succeeded (String finished))
    (((follow_up
       (Applied_follow_up
        ((request_turn false) (request_compaction false)
         (end_session ("background work finished")))))))
    |}]
;;

let%expect_test
    "a persisted background handler request admits one model turn and does not replay \
     after restart"
  =
  let agent =
    native_agent
    ^ {|
<developer>Continue when background work requests a turn.</developer>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event = fun ctx state event -> match event with
| `Tool_invoked(p) -> Task.bind(Runtime.request_turn(),
    fun ignored -> Task.bind(Invocation.resolve(p.context.invocation_id,
      `Complete(`String("ready"))), fun ignored -> Task.pure(state + 1)))
| _ -> Task.pure(state)
</script>
<tool name="resume" type="moderator" moderator="owner" input_schema="input.json" output_schema="output.json"/>|}
  in
  let provider_calls = ref 0 in
  with_background_daemon
    ~agent
    ~sources:[ "input.json", {|{"type":"object"}|}; "output.json", {|{"type":"string"}|} ]
    ~expected_model_calls:1
    ~model_post_stream:(fun ~sw:_ ~inputs:_ ->
      incr provider_calls;
      Stdlib.Seq.empty)
    (fun env client entry capabilities ->
       let job = submit entry (B.to_json (tool capabilities "resume" (`Object []))) in
       let _, completion = await env client job in
       (match completion with
        | Succeeded _ -> ()
        | _ -> raise_s [%sexp (completion : Completion.t)]);
       print_s [%sexp (completion : Completion.t)];
       let rec wait_idle () =
         let state = A.state entry.actor |> protocol_ok in
         match !provider_calls, state.active_operation with
         | 1, None -> state
         | _ ->
           Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
           wait_idle ()
       in
       let state = wait_idle () in
       let invocation =
         List.find_exn state.invocations ~f:(fun invocation ->
           String.equal invocation.context.tool_name "resume")
       in
       print_s
         [%sexp
           (invocation.handler_intent : Agent_protocol.Invocation.handler_intent option)]);
  [%expect
    {|
    (Succeeded (String ready))
    (((follow_up
       (Applied_follow_up
        ((request_turn true) (request_compaction false) (end_session ()))))))
    |}]
;;
