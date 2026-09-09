open Core
open Fixtures
open Job_fixtures
module B = Chat_response.Background_request
module P = Chat_response.One_off_request
module S = Chat_response.One_off_script
module X = Agent_session.Background_execution
module M = Chat_response.Moderation

let cap result =
  Result.map_error result ~f:(fun error -> error.C.message) |> Result.ok_or_failwith
;;

let policy = P.default_policy

let tools ?(authorize = fun _ _ -> Ok ()) registry =
  Agent_session.Script_tool_calls.create
    ~registry
    ~moderator_names:String.Set.empty
    ~now:(fun () -> timestamp)
    ~is_halted:(fun () -> false)
    ~requires_active_moderator:(fun _ -> false)
    ~authorize
    ~prepare_output:(function
      | Openai.Responses.Tool_output.Output.Text "private output" ->
        Ok (`String "disclosed")
      | Text text -> Ok (`String text)
      | _ -> Error (handoff_error "unexpected fixture output"))
    ~defer_observation:(fun _ -> Ok ())
;;

let capture_tool registry policy =
  let reference = C.find registry ~name:"read_file" |> cap |> C.reference in
  B.tool ~capabilities:registry ~reference ~input:(`Object []) ~policy |> protocol_ok
;;

let run ?(moderate_tool = fun _ _ -> Ok None) env actor job request script_tools =
  with_job actor job (fun ~job ~execute ->
    X.run
      ~env
      ~job
      ~deadline
      ~execute
      ~request
      ~policy
      ~script_tools
      ~now:(fun () -> timestamp)
      ~moderate_tool
      ~prepare_outcome:(fun outcome ->
        I.validate_outcome outcome
        |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message))
      ())
;;

let show_result = function
  | Ok (result : X.result) -> print_s [%sexp (result.resolved.status : I.status)]
  | Error (error : Agent_protocol.Error.t) ->
    print_s [%sexp (error.code : Agent_protocol.Error.code)]
;;

let%expect_test "background adapter preserves disclosed outcomes and policy boundaries" =
  List.iter
    [ `Success
    ; `Structured_failure
    ; `Pre_reject
    ; `Denied
    ; `Stale
    ; `Wrong_intent
    ; `Call_limit
    ; `Output_limit
    ]
    ~f:(fun mode ->
      with_actor (fun env _sw actor _writer backend ->
        let calls = ref 0 in
        let base = native_registry calls ~raises:false in
        let registry =
          match mode with
          | `Structured_failure ->
            let native =
              C.find base ~name:"read_file"
              |> cap
              |> C.native_implementation
              |> Option.value_exn
            in
            let error =
              I.Fail
                { code = "fixture.retry_later"
                ; message = "Public diagnostic"
                ; retryable = true
                ; details = `Object [ "attempts", `Number "2" ]
                }
            in
            let native =
              { native with
                run_with_progress =
                  (fun ~invocation input ->
                    native.run_with_progress ~invocation input |> ignore;
                    Openai.Responses.Tool_output.Output.Text
                      (Jsonaf.to_string (I.outcome_to_json error)))
              }
            in
            C.create
              ~owner:"background-fixture"
              ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
              ~result_contracts:[ "read_file", Invocation_v1 ]
              [ Chatmd_shell_spec.Source_ref.digest "structured-v1", native ]
            |> cap
          | _ -> base
        in
        let effective =
          match mode with
          | `Call_limit ->
            { policy with execution = { policy.execution with max_calls = 0 } }
          | `Output_limit -> { policy with max_output_bytes = 8 }
          | _ -> policy
        in
        let request = capture_tool registry effective in
        let payload =
          match mode with
          | `Wrong_intent -> `Null
          | _ -> B.to_json request
        in
        let job = add_claimed_job ~payload actor in
        let current =
          match mode with
          | `Stale ->
            native_registry (ref 0) ~raises:false
            |> fun registry -> C.select registry ~names:[] |> cap
          | _ -> registry
        in
        let script_tools =
          tools
            (fun () -> current)
            ~authorize:(fun _ _ ->
              match mode with
              | `Denied ->
                Error
                  (Agent_protocol.Error.create
                     Permission_denied
                     ~message:"denied"
                     ~retryable:false
                     ())
              | _ -> Ok ())
        in
        let moderate_tool _ _ =
          match mode with
          | `Pre_reject ->
            Ok (Some { M.Outcome.empty with tool_moderation = Some (Reject "rejected") })
          | _ -> Ok None
        in
        print_s
          [%sexp
            (mode
             : [ `Success
               | `Structured_failure
               | `Pre_reject
               | `Denied
               | `Stale
               | `Wrong_intent
               | `Call_limit
               | `Output_limit
               ])];
        show_result (run ~moderate_tool env actor job request script_tools);
        let state = Agent_session.Memory_backend.state backend in
        print_s
          [%sexp
            { calls = (!calls : int)
            ; invocations = (List.length state.invocations : int)
            ; history = (List.length state.conversation.canonical_history : int)
            }]));
  [%expect
    {|
    Success
    (Resolved (Complete (String disclosed)))
    ((calls 1) (invocations 2) (history 0))
    Structured_failure
    (Resolved
     (Fail
      ((code fixture.retry_later) (message "Public diagnostic") (retryable true)
       (details (Object ((attempts (Number 2))))))))
    ((calls 1) (invocations 2) (history 0))
    Pre_reject
    (Resolved
     (Fail
      ((code invocation.pre_tool_rejected)
       (message "Pre-tool moderation rejected the call.") (retryable false)
       (details Null))))
    ((calls 0) (invocations 2) (history 0))
    Denied
    (Resolved
     (Fail
      ((code invocation.permission_denied)
       (message "Tool execution was not authorized.") (retryable false)
       (details Null))))
    ((calls 0) (invocations 2) (history 0))
    Stale
    Invalid_request
    ((calls 0) (invocations 0) (history 0))
    Wrong_intent
    Invalid_request
    ((calls 0) (invocations 0) (history 0))
    Call_limit
    Resource_limit
    ((calls 0) (invocations 1) (history 0))
    Output_limit
    Invalid_request
    ((calls 1) (invocations 2) (history 0))
    |}]
;;

let%expect_test
    "reconstructed background scripts use fresh state, one real depth slot and retained \
     requests"
  =
  with_actor (fun env _sw actor _writer backend ->
    let calls = ref 0 in
    let registry =
      native_registry calls ~raises:false ~on_call:(fun () ->
        Chat_response.Runtime_request_scope.emit [ Request_turn; Request_turn ]
        |> Result.ok_or_failwith)
    in
    let source =
      {|let count = [0.0]
let main input =
  let ignored = count[0] <- count[0] +. 1.0 in
  Task.bind(Tool.call("read_file", input), fun result ->
    match result with
    | `Ok(value) -> Task.pure(`Object([
        { key = "count"; value = `Number(count[0]) },
        { key = "read"; value = value }
      ]))
    | `Error(code) -> Task.fail(code))|}
    in
    let prepared =
      S.prepare_in_domain ~env ~capabilities:registry ~tools:[ "read_file" ] ~source ()
      |> Result.map_error ~f:(fun errors ->
        [%sexp (errors : Chatmd_shell_spec.Diagnostic.t list)] |> Sexp.to_string_hum)
      |> Result.ok_or_failwith
    in
    let effective =
      { policy with
        execution = { policy.execution with max_invocation_depth = 1; max_calls = 1 }
      }
    in
    let captured =
      B.script ~prepared ~input:(`Object []) ~policy:effective |> protocol_ok
    in
    let request =
      B.to_json captured
      |> Jsonaf.to_string
      |> Jsonaf.of_string
      |> B.of_json ~policy
      |> protocol_ok
    in
    List.iter [ 1; 2 ] ~f:(fun _ ->
      let job = add_claimed_job ~payload:(B.to_json request) actor in
      let result =
        run env actor job request (tools (fun () -> registry)) |> protocol_ok
      in
      print_s
        [%sexp
          (result.resolved.status : I.status)
        , (result.runtime_requests : M.Runtime_request.t list)];
      [%test_eq: string]
        (B.fingerprint request)
        result.resolved.context.implementation_revision);
    let state = Agent_session.Memory_backend.state backend in
    print_s
      [%sexp
        { calls = (!calls : int)
        ; invocations = (List.length state.invocations : int)
        ; model_operation = (Option.is_some state.active_operation : bool)
        ; history = (List.length state.conversation.canonical_history : int)
        }]);
  [%expect
    {|
    ((Resolved
      (Complete (Object ((count (Number 1)) (read (String disclosed))))))
     (Request_turn))
    ((Resolved
      (Complete (Object ((count (Number 1)) (read (String disclosed))))))
     (Request_turn))
    ((calls 2) (invocations 6) (model_operation false) (history 0))
    |}]
;;

let%expect_test "background execution cancels an admitted native wait" =
  with_actor (fun env sw actor _writer backend ->
    let entered, entered_u = Eio.Promise.create () in
    let finished, finished_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let calls = ref 0 in
    let registry =
      native_registry calls ~raises:false ~on_call:(fun () ->
        Eio.Promise.resolve entered_u ();
        Eio.Promise.await never)
    in
    let request = capture_tool registry policy in
    let job = add_claimed_job ~payload:(B.to_json request) actor in
    Eio.Fiber.fork ~sw (fun () ->
      let cancelled =
        try
          run env actor job request (tools (fun () -> registry)) |> ignore;
          false
        with
        | Eio.Cancel.Cancelled _ -> true
      in
      Eio.Promise.resolve finished_u cancelled);
    Eio.Promise.await entered;
    A.cancel_job_internal actor ~job_id:job.id |> protocol_ok |> ignore;
    [%test_eq: bool] true (Eio.Promise.await finished);
    let state = Agent_session.Memory_backend.state backend in
    print_s
      [%sexp
        (!calls : int)
      , (List.map state.invocations ~f:(fun i -> i.I.status) : I.status list)]);
  [%expect
    {|
    (1
     ((Resolved (Cancelled "background job cancelled"))
      (Resolved (Cancelled "background job cancelled"))))
    |}]
;;

let%expect_test "host budget wrappers preserve an already entered ancestor depth ceiling" =
  with_actor (fun env _sw actor _writer _backend ->
    let registry = native_registry (ref 0) ~raises:false in
    let prepared =
      S.prepare_in_domain
        ~env
        ~capabilities:registry
        ~tools:[]
        ~source:"let main input = Task.pure(input)"
        ()
      |> Result.map_error ~f:(fun _ -> "fixture did not compile")
      |> Result.ok_or_failwith
    in
    let request = B.script ~prepared ~input:`Null ~policy |> protocol_ok in
    let job = add_claimed_job ~payload:(B.to_json request) actor in
    let result =
      Chatml_execution.with_control
        ~env
        ~policy:(Bounded { policy.execution with max_invocation_depth = 1 })
        (fun _ -> run env actor job request (tools (fun () -> registry)))
    in
    match result with
    | Ok result -> show_result result
    | Error error -> print_s [%sexp (error : Chatml_execution.error)]);
  [%expect {| Resource_limit |}]
;;

let%expect_test
    "background managed tools restore private dependencies without widening caller \
     authority"
  =
  with_actor (fun env _sw actor _writer _backend ->
    let module Managed = Chat_response.Managed_tool_registry in
    let calls = ref 0 in
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
<script id="leaf-code" language="chatml" kind="tool">
let run ctx input = Task.bind(Tool.call("read_file", input), fun result ->
  match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(code) -> Task.pure(`Fail({ code = code; message = "leaf failed"; retryable = false; details = `Null })))
</script>
<tool name="leaf" type="chatml" script="leaf-code" entrypoint="run" input_schema="input.json" output_schema="output.json"><uses tool="read_file"/></tool>|}
    in
    let prepare () =
      Managed.prepare
        ~env
        ~owner:"background-managed"
        ~capabilities:(native_registry calls ~raises:false)
        elements
      |> Result.map_error ~f:(fun diagnostics ->
        String.concat
          ~sep:"; "
          (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string))
      |> Result.ok_or_failwith
    in
    let first = prepare () in
    let reference =
      C.find (Managed.capabilities first) ~name:"leaf" |> cap |> C.reference
    in
    let effective =
      { policy with
        execution = { policy.execution with max_invocation_depth = 1; max_calls = 2 }
      }
    in
    let request =
      B.tool
        ~capabilities:(Managed.capabilities first)
        ~reference
        ~input:(`Object [])
        ~policy:effective
      |> protocol_ok
      |> B.to_json
      |> Jsonaf.to_string
      |> Jsonaf.of_string
      |> B.of_json ~policy
      |> protocol_ok
    in
    let current = prepare () in
    let registry = Managed.capabilities current in
    let script_tools =
      Agent_session.Script_tool_calls.with_managed_tools
        (tools (fun () -> registry))
        ~env
        ~definition:current
        ~execution_limits:Agent_session.Standalone_tool_dispatch.declared_execution_limits
    in
    let job = add_claimed_job ~payload:(B.to_json request) actor in
    let result = run env actor job request script_tools |> protocol_ok in
    let caller = C.select registry ~names:[ "leaf" ] |> cap in
    [%test_eq: string]
      (C.fingerprint caller)
      result.resolved.context.capability_fingerprint;
    print_s
      [%sexp
        (result.resolved.status : I.status)
      , (!calls : int)
      , (List.map (C.references caller) ~f:(fun reference -> reference.name)
         : string list)]);
  [%expect {| ((Resolved (Complete (String disclosed))) 1 (leaf)) |}]
;;
