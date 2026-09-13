open Core
open Fixtures
open Job_fixtures
module Managed = Chat_response.Managed_tool_registry

let%expect_test "job progress cannot reveal a managed target's private native dependency" =
  with_actor (fun env sw actor _writer _backend ->
    let started, started_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let finished, finished_u = Eio.Promise.create () in
    let calls = ref 0 in
    let base = native_registry calls ~raises:false in
    let native =
      C.find base ~name:"read_file"
      |> Background_execution_tests.cap
      |> C.native_implementation
      |> Option.value_exn
    in
    let implementation =
      { native with
        run_with_progress =
          (fun ~invocation input ->
            assert (not (Ochat_function.Invocation.is_observed invocation));
            Ochat_function.Invocation.emit
              invocation
              { channel = `Stdout; update = Append "PRIVATE-NATIVE-PROGRESS" };
            Eio.Promise.resolve started_u ();
            Eio.Promise.await release;
            native.run_with_progress ~invocation input)
      }
    in
    let base =
      C.create
        ~owner:"private-progress"
        ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "private resources")
        [ Chatmd_shell_spec.Source_ref.digest "private runner", implementation ]
      |> Background_execution_tests.cap
    in
    let dir = Eio.Stdenv.cwd env in
    let loader =
      Source_loader.captured_filesystem ~root:dir ~sources:[ "schema.json", "true" ]
    in
    let elements =
      Prompt.Chat_markdown.parse_chat_inputs
        ~dir
        ~source_loader:loader
        {|
<tool name="read_file"/>
<script id="summary" language="chatml" kind="tool">
let run ctx input = Task.bind(Tool.call("read_file", `Object([])), fun ignored -> Task.pure(`Complete(`String("public summary"))))
</script>
<tool name="summary" type="chatml" script="summary" entrypoint="run" input_schema="schema.json" output_schema="schema.json"><uses tool="read_file"/></tool>|}
    in
    let definition =
      Managed.prepare ~env ~owner:"progress-authority" ~capabilities:base elements
      |> Result.map_error ~f:(fun errors ->
        String.concat
          ~sep:"; "
          (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
      |> Result.ok_or_failwith
    in
    let registry = Managed.capabilities definition in
    let reference =
      C.find registry ~name:"summary" |> Background_execution_tests.cap |> C.reference
    in
    let request =
      Chat_response.Background_request.tool
        ~capabilities:registry
        ~reference
        ~input:(`Object [])
        ~policy:Chat_response.One_off_request.default_policy
      |> protocol_ok
    in
    let parent =
      add_claimed_job actor ~payload:(Chat_response.Background_request.to_json request)
    in
    let tools =
      Background_execution_tests.tools (fun () -> registry)
      |> fun tools ->
      Agent_session.Script_tool_calls.with_progress
        tools
        ~emit:(fun invocation progress ->
          A.publish_job_progress actor ~invocation_id:invocation.context.id progress)
      |> fun tools ->
      Agent_session.Script_tool_calls.with_managed_tools
        tools
        ~env
        ~definition
        ~execution_limits:Agent_session.Standalone_tool_dispatch.declared_execution_limits
    in
    Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve
        finished_u
        (Background_execution_tests.run env actor parent request tools));
    Eio.Promise.await started;
    let view = A.read_job actor ~job_id:parent.id |> protocol_ok in
    assert (Option.is_none view.progress);
    (* A normal authorized command remains available while the native work waits. *)
    A.state actor |> protocol_ok |> ignore;
    Eio.Promise.resolve release_u ();
    let result = Eio.Promise.await finished |> protocol_ok in
    (match result.resolved.status with
     | Resolved (Complete (`String "public summary")) -> ()
     | status -> raise_s [%sexp (status : I.status)]);
    [%test_eq: int] 1 !calls;
    print_endline "private progress withheld; admitted target returned its public summary");
  [%expect {| private progress withheld; admitted target returned its public summary |}]
;;
