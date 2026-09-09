open Core
open Fixtures
module T = Agent_session.Run_chatml_tool
module Calls = Agent_session.Script_tool_calls
module C = Chat_response.Tool_capability
module Managed = Chat_response.Managed_tool_registry
module EC = Chat_response.Extension_compiler
module I = Agent_protocol.Invocation
module A = Agent_session.Session_actor
module D = Chat_response.In_memory_stream.Tool_dispatch

let capability result =
  result |> Result.map_error ~f:(fun error -> error.C.message) |> Result.ok_or_failwith
;;

let%expect_test
    "managed descendants use private dependencies without widening their caller"
  =
  List.iter
    [ `Success; `Denied; `Revoked; `Invalid_output; `Disclosure; `Pre_reject; `Depth ]
    ~f:(fun mode ->
      let calls = ref 0 in
      let finished = ref false in
      let summary = ref `Null in
      with_handoff_actor
        ~make_worker:(fun env actor_ready ->
          let services = ref None in
          let registration =
            T.registration
              ~env
              ~policy:Chat_response.One_off_request.default_policy
              ~services:(fun () -> Result.of_option !services ~error:"not installed")
          in
          let reader =
            native_registry calls ~raises:false
            |> fun registry ->
            C.find registry ~name:"read_file"
            |> capability
            |> C.native_implementation
            |> Option.value_exn
          in
          let base =
            C.create
              ~result_contracts:[ T.name, registration.result_contract ]
              ~owner:"managed-nesting"
              ~resource_fingerprint:
                (Chatmd_shell_spec.Source_ref.digest "managed resources")
              [ registration.implementation_revision, registration.implementation
              ; Chatmd_shell_spec.Source_ref.digest "reader", reader
              ]
            |> capability
          in
          let dir = Eio.Stdenv.cwd env in
          let schema =
            match mode with
            | `Invalid_output -> "false"
            | _ -> {|{"type":"string"}|}
          in
          let source_loader =
            Source_loader.captured_filesystem
              ~root:dir
              ~sources:[ "input.json", {|{"type":"object"}|}; "output.json", schema ]
          in
          let elements =
            Prompt.Chat_markdown.parse_chat_inputs
              ~dir
              ~source_loader
              {|<tool name="read_file"/><tool name="run_chatml"/>
<script id="leaf-code" language="chatml" kind="tool">
let run ctx input = Task.bind(Tool.call("read_file", input), fun result ->
  match result with | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(code) -> Task.pure(`Fail({code = code; message = "leaf failed"; retryable = false; details = `Null})))
</script>
<script id="root-code" language="chatml" kind="tool">
let count = [0]
let run ctx input =
  let ignored = count[0] <- count[0] + 1 in
  Task.bind(Tool.call("leaf", input), fun result ->
    match result with
    | `Ok(`String(value)) -> Task.pure(`Complete(`String(to_string(count[0]) ++ ":" ++ value)))
    | `Ok(_) -> Task.fail("unexpected value")
    | `Error(code) -> Task.pure(`Fail({code = code; message = "root failed"; retryable = false; details = `Null})))
</script>
<tool name="leaf" type="chatml" script="leaf-code" entrypoint="run" input_schema="input.json" output_schema="output.json"><uses tool="read_file"/></tool>
<tool name="root" type="chatml" script="root-code" entrypoint="run" input_schema="input.json" output_schema="output.json"><uses tool="leaf"/></tool>|}
          in
          let managed =
            Managed.prepare ~env ~owner:"managed-nesting" ~capabilities:base elements
            |> Result.map_error ~f:(fun errors ->
              String.concat
                ~sep:"; "
                (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
            |> Result.ok_or_failwith
          in
          let registry = ref (Managed.capabilities managed) in
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let script_tools =
              Calls.create
                ~registry:(fun () -> !registry)
                ~moderator_names:String.Set.empty
                ~now:Agent_protocol.Timestamp.now
                ~is_halted:(fun () -> false)
                ~requires_active_moderator:(fun _ -> false)
                ~authorize:(fun invocation _ ->
                  match invocation.I.context.tool_name, mode with
                  | "leaf", `Denied ->
                    Error (Agent_protocol.Error.invalid_request "denied leaf")
                  | "leaf", `Revoked ->
                    registry
                    := C.select !registry ~names:[ "run_chatml"; "root"; "leaf" ]
                       |> capability;
                    Ok ()
                  | _ -> Ok ())
                ~prepare_output:(function
                  | Text text ->
                    (match mode with
                     | `Disclosure
                       when String.is_prefix text ~prefix:{|{"type":"complete"|}
                            && String.is_substring text ~substring:"private output" ->
                       Ok
                         (`String (Jsonaf.to_string (I.outcome_to_json (Complete `Null))))
                     | _ -> Ok (`String text))
                  | _ -> assert false)
                ~defer_observation:(fun _ -> failwith "unexpected observer")
              |> fun calls ->
              Calls.with_managed_tools
                calls
                ~env
                ~definition:managed
                ~execution_limits:(fun prepared ->
                  let limits =
                    Agent_session.Standalone_tool_dispatch.declared_execution_limits
                      prepared
                  in
                  match mode with
                  | `Depth -> { limits with max_invocation_depth = 1 }
                  | _ -> limits)
            in
            services
            := Some
                 T.
                   { script_tools
                   ; observer = None
                   ; now = Agent_protocol.Timestamp.now
                   ; moderate_tool =
                       (fun _ call ->
                         match mode, call.Chat_response.Moderation.Tool_call.name with
                         | `Pre_reject, "leaf" ->
                           Ok
                             (Some
                                { Chat_response.Moderation.Outcome.empty with
                                  tool_moderation =
                                    Some (Reject "leaf denied by moderator")
                                })
                         | _ -> Ok None)
                   ; prepare_outcome =
                       (fun outcome ->
                         I.validate_outcome outcome
                         |> Result.map_error ~f:(fun error ->
                           error.Agent_protocol.Error.message))
                   };
            let source =
              {|let main input = Task.bind(Tool.call("root", input), fun first ->
  match first with
  | `Error(code) -> Task.pure(`String(code))
  | `Ok(value) -> Task.bind(Tool.call("root", input), fun second ->
    Task.bind(Tool.call("read_file", input), fun private_call ->
      match second with
      | `Ok(other) -> (match private_call with
        | `Error(code) -> Task.pure(`Array([value, other, `String(code)]))
        | _ -> Task.fail("private dependency leaked"))
      | _ -> Task.fail("second call failed"))))|}
            in
            let payload =
              Jsonaf.to_string
                (`Object
                    [ "source", `String source
                    ; "input", `Object []
                    ; "tools", `Array [ `String "root" ]
                    ])
            in
            let dispatch = Calls.native_dispatch script_tools ~input ~capabilities:caps in
            let call_id = "managed-run" in
            let id =
              History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
            in
            let call =
              History_entry.create_with_id
                ~id
                (Chat_response.Tool_call.call_item
                   ~kind:Function
                   ~name:T.name
                   ~payload
                   ~call_id
                   ~id:None)
            in
            let request =
              D.
                { kind = Function
                ; original_name = T.name
                ; original_payload = payload
                ; name = T.name
                ; payload
                ; rejection = None
                ; call
                ; history = input.history @ [ call ]
                ; source = None
                ; parent_call_id = None
                }
            in
            assert (dispatch.commit_call request);
            let response =
              dispatch.run request ~authorize:(fun () -> ()) |> Option.value_exn
            in
            let outcome =
              match response.output with
              | Text text -> I.outcome_of_json (Jsonaf.of_string text) |> protocol_ok
              | _ -> assert false
            in
            (summary
             := match outcome with
                | Complete value -> value
                | _ -> I.outcome_to_json outcome);
            let id =
              History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
            in
            let output =
              History_entry.create_with_id
                ~id
                (Chat_response.Tool_call.output_item
                   ~kind:Function
                   ~call_id
                   ~output:response.output)
            in
            (Option.value_exn response.commit_output) output;
            let state = A.state actor |> protocol_ok in
            assert (
              Int.equal
                (List.count state.invocations ~f:(fun invocation ->
                   I.equal_origin invocation.context.origin Model))
                1);
            List.iter state.invocations ~f:(fun invocation ->
              match invocation.I.context.tool_name with
              | ("root" | "leaf") as name ->
                let reference =
                  C.find (Managed.capabilities managed) ~name |> capability |> C.reference
                in
                assert (
                  String.equal
                    invocation.context.implementation_revision
                    reference.implementation_revision);
                let caller_names =
                  match name with
                  | "root" -> [ "root" ]
                  | _ -> [ "leaf" ]
                in
                let caller =
                  C.select (Managed.capabilities managed) ~names:caller_names
                  |> capability
                in
                assert (
                  String.equal
                    invocation.context.capability_fingerprint
                    (C.fingerprint caller));
                let parent =
                  List.find_exn state.invocations ~f:(fun parent ->
                    Option.exists
                      invocation.context.parent_invocation
                      ~f:(Agent_protocol.Id.Invocation.equal parent.context.id))
                in
                assert (
                  String.equal
                    parent.context.tool_name
                    (match name with
                     | "root" -> "chatml.main"
                     | _ -> "root"));
                assert (Option.is_none invocation.context.provider_call_id);
                assert (Option.is_none invocation.output_entry_id)
              | _ -> ());
            finished := true;
            Completed
              { final_history =
                  Agent_session.History_codec.all_of_protocol
                    state.conversation.canonical_history
                  |> protocol_ok
              ; moderator_snapshot = None
              ; runtime_requests = []
              }))
        (fun _env actor _writer backend ->
           let rec await () =
             let state = A.state actor |> protocol_ok in
             match state.active_operation, !finished with
             | None, true -> state
             | _ ->
               Eio.Fiber.yield ();
               await ()
           in
           let state = await () in
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           assert (
             Int.equal
               (List.count state.invocations ~f:(fun invocation ->
                  Option.is_some invocation.output_entry_id))
               1);
           print_s
             [%sexp
               (mode
                : [ `Success
                  | `Denied
                  | `Revoked
                  | `Invalid_output
                  | `Disclosure
                  | `Pre_reject
                  | `Depth
                  ])
             , (!calls : int)
             , (List.length state.invocations : int)
             , (Jsonaf.to_string !summary : string)]));
  [%expect
    {|
    (Success 2 8
     "[\"1:private output\",\"1:private output\",\"invocation.unselected_tool\"]")
    (Denied 0 4 "\"invocation.permission_denied\"")
    (Revoked 0 4 "\"invocation.stale_binding\"")
    (Invalid_output 1 5 "\"invocation.invalid_output\"")
    (Disclosure 1 5 "\"invocation.invalid_output\"")
    (Pre_reject 0 4 "\"invocation.pre_tool_rejected\"")
    (Depth 0 4 "\"chatml.invocation_depth\"")
    |}]
;;
