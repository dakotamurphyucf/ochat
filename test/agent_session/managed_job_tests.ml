open Core
open Fixtures
open Job_fixtures
module Calls = Agent_session.Script_tool_calls
module Managed = Chat_response.Managed_tool_registry
module Jobs = Agent_session.Script_job_service
module Capacity = Agent_server.Job_capacity

let%expect_test "nested managed launches survive only successful common output validation"
  =
  List.iter [ `Accepted; `Disclosure; `Schema; `Foreign ] ~f:(fun mode ->
    with_actor (fun env _sw actor _writer _backend ->
      let calls = ref 0 in
      let base = Fixtures.native_registry calls ~raises:false in
      let dir = Eio.Stdenv.cwd env in
      let source_loader =
        Source_loader.captured_filesystem
          ~root:dir
          ~sources:
            [ "input.json", {|{"type":"object"}|}; "output.json", {|{"type":"string"}|} ]
      in
      let elements =
        Prompt.Chat_markdown.parse_chat_inputs
          ~dir
          ~source_loader
          {|<tool name="read_file"/>
<script id="leaf" language="chatml" kind="tool">
let run ctx input = Task.bind(Job.start_tool("read_file", input), fun id ->
  Task.pure(`Pending(`Job(id), `String("accepted"))))
</script>
<tool name="leaf" type="chatml" script="leaf" entrypoint="run" input_schema="input.json" output_schema="output.json"><uses tool="read_file"/></tool>|}
      in
      let definition =
        Managed.prepare ~env ~owner:"managed-jobs" ~capabilities:base elements
        |> Result.map_error ~f:(fun errors ->
          List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
          |> String.concat ~sep:"; ")
        |> Result.ok_or_failwith
      in
      let registry = Managed.capabilities definition in
      let capacity =
        Capacity.create
          ~limits:
            { daemon_total = 1
            ; per_principal = 1
            ; per_prompt = 1
            ; per_workspace = 1
            ; per_session = 1
            ; per_kind = 1
            ; max_nested_depth = 2
            }
      in
      let quota job = Background_admission_tests.key actor job in
      let host : Jobs.host =
        { stage =
            (fun owner request ->
              let open Result.Let_syntax in
              let%bind job = A.prepare_background_job_launch actor ~owner request in
              let%bind reservation = Capacity.reserve_job capacity (quota job) ~job in
              let reservation = Option.value_exn reservation in
              let%map () =
                A.stage_background_job
                  actor
                  ~job
                  ~capacity:
                    { publish = (fun () -> Capacity.publish reservation)
                    ; abort = (fun () -> Capacity.abort reservation)
                    }
              in
              job)
        ; select = (fun owner ids -> A.select_background_jobs actor ~owner ~ids)
        ; abort = (fun owner id -> A.abort_background_job actor ~owner ~id |> protocol_ok)
        ; get = (fun owner id -> A.read_script_job actor ~owner ~id)
        ; materialize =
            (fun owner expected -> A.read_script_job_result actor ~owner ~expected)
        ; cancel = (fun owner id -> A.cancel_script_job actor ~owner ~id)
        }
      in
      let jobs =
        Jobs.create
          ~env
          ~policy:Chat_response.One_off_request.default_policy
          ~current_capabilities:(fun () -> registry)
          ~host
      in
      let reached = ref 0 in
      let tools =
        Calls.create
          ~registry:(fun () -> registry)
          ~moderator_names:String.Set.empty
          ~now:Agent_protocol.Timestamp.now
          ~is_halted:(fun () -> false)
          ~requires_active_moderator:(fun _ -> false)
          ~authorize:(fun _ _ -> Ok ())
          ~prepare_output:(function
            | Text text ->
              let outcome = I.outcome_of_json (Jsonaf.of_string text) |> protocol_ok in
              (match outcome with
               | Pending (work, _) ->
                 incr reached;
                 (match mode with
                  | `Accepted -> Ok (`String text)
                  | `Disclosure ->
                    Error
                      (Agent_protocol.Error.invalid_request
                         "injected disclosure rejection")
                  | `Schema ->
                    Ok
                      (`String
                          (Jsonaf.to_string (I.outcome_to_json (Pending (work, `Null)))))
                  | `Foreign ->
                    Ok
                      (`String
                          (Jsonaf.to_string
                             (I.outcome_to_json
                                (Pending
                                   ( Job (Agent_protocol.Id.Job.create ())
                                   , `String "accepted" ))))))
               | _ -> Ok (`String text))
            | _ -> failwith "unexpected output kind")
          ~defer_observation:(fun _ -> Ok ())
        |> fun tools ->
        Calls.with_job_service tools jobs
        |> fun tools ->
        Calls.with_managed_tools
          tools
          ~env
          ~definition
          ~execution_limits:
            Agent_session.Standalone_tool_dispatch.declared_execution_limits
      in
      let prepared =
        Chat_response.One_off_script.prepare_in_domain
          ~env
          ~capabilities:registry
          ~tools:[ "leaf" ]
          ~source:
            {|let main input = Task.bind(Tool.call("leaf", input), fun result ->
          match result with | `Ok(value) -> Task.pure(value) | `Error(code) -> Task.pure(`String(code)))|}
          ()
        |> Result.map_error ~f:(fun errors ->
          List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
          |> String.concat ~sep:"; ")
        |> Result.ok_or_failwith
      in
      let parent = add_claimed_job actor in
      let root = root parent in
      let root =
        I.create { root.context with capability_fingerprint = C.fingerprint registry }
        |> protocol_ok
      in
      let result =
        with_job actor parent (fun ~job:_ ~execute ->
          execute ~invocation:root (fun ~dispatched ->
            N.with_dispatched_scope
              ~execute
              ~selected:registry
              ~invocation:dispatched
              (fun () ->
                 let open Result.Let_syntax in
                 let%bind borrowed = N.borrow () in
                 let%bind result =
                   Agent_session.One_off_execution.run
                     ~env
                     ~prepared
                     ~borrowed
                     ~script_tools:tools
                     ~input:(`Object [])
                     ~limits:
                       (Chat_response.One_off_request.script_limits_for
                          Chat_response.One_off_request.default_policy)
                     ~max_nested_calls:100
                     ~now:Agent_protocol.Timestamp.now
                     ~moderate_tool:(fun _ _ -> Ok None)
                     ~prepare_outcome:(fun _ -> Ok ())
                     ()
                 in
                 match result.resolved.status with
                 | Resolved outcome -> Ok outcome
                 | _ -> failwith "one-off did not resolve")))
        |> protocol_ok
      in
      [%test_eq: int] 1 !reached;
      [%test_eq: int] 0 !calls;
      let state = A.state actor |> protocol_ok in
      let launched = List.filter state.jobs ~f:(fun job -> Option.is_some job.launch) in
      let accepted =
        match mode with
        | `Accepted -> true
        | _ -> false
      in
      [%test_eq: int] (if accepted then 1 else 0) (List.length launched);
      let probe = Background_scheduler_tests.new_job (`Object []) in
      let available = Capacity.try_acquire capacity (quota probe) |> protocol_ok in
      [%test_eq: bool] (not accepted) (Option.is_some available);
      Option.iter available ~f:Capacity.release;
      Capacity.close_session capacity ~session_id;
      let output =
        match result.status with
        | Resolved (Complete (`String value)) -> value
        | other -> raise_s [%sexp (other : I.status)]
      in
      print_s
        [%sexp
          (mode : [ `Accepted | `Disclosure | `Schema | `Foreign ]), (output : string)]));
  [%expect
    {|
    (Accepted accepted)
    (Disclosure invocation.disclosure_rejected)
    (Schema invocation.invalid_output)
    (Foreign invocation.invalid_output)
    |}]
;;
