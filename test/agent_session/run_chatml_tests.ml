open Core
open Fixtures
module T = Agent_session.Run_chatml_tool
module Q = Chat_response.One_off_request
module N = Agent_session.Native_tool_invocation
module C = Chat_response.Tool_capability
module I = Agent_protocol.Invocation
module A = Agent_session.Session_actor

let%expect_test "run_chatml validates submitted policy and returns one native outcome" =
  List.iter
    [ `Success
    ; `Compile
    ; `Widen
    ; `No_calls
    ; `Unselected
    ; `Source_limit
    ; `Duplicate
    ; `Recursive
    ]
    ~f:(fun mode ->
      let calls = ref 0 in
      let done_ = ref false in
      let response = ref None in
      with_handoff_actor
        ~make_worker:(fun env actor_ready ->
          let reader =
            native_registry calls ~raises:false
            |> fun registry ->
            C.find registry ~name:"read_file"
            |> Result.map_error ~f:(fun error -> error.C.message)
            |> Result.ok_or_failwith
            |> C.implementation
          in
          let invoke = ref (fun _ -> failwith "request services not installed") in
          let module Definition = struct
            type input = Jsonaf.t

            let name = T.name
            let description = Some "Run a bounded one-off ChatML program."
            let type_ = "function"
            let parameters = T.parameters
            let input_of_string = Jsonaf.of_string
          end
          in
          let runner =
            Ochat_function.create_function
              (module Definition)
              ~strict:false
              (fun request -> !invoke request)
          in
          let registry =
            C.create
              ~result_contracts:[ T.name, Invocation_v1 ]
              ~owner:"run-chatml-fixture"
              ~resource_fingerprint:
                (Chatmd_shell_spec.Source_ref.digest "run-chatml-fixture")
              [ Chatmd_shell_spec.Source_ref.digest "reader", reader
              ; Chatmd_shell_spec.Source_ref.digest "run-chatml-v1", runner
              ]
            |> Result.map_error ~f:(fun error -> error.C.message)
            |> Result.ok_or_failwith
          in
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
            let actor = Eio.Promise.await actor_ready in
            let tools =
              Agent_session.Script_tool_calls.create
                ~registry:(fun () -> registry)
                ~moderator_names:String.Set.empty
                ~now:Agent_protocol.Timestamp.now
                ~is_halted:(fun () -> (A.state actor |> protocol_ok).halted)
                ~requires_active_moderator:(fun _ -> false)
                ~authorize:(fun _ _ -> Ok ())
                ~prepare_output:(function
                  | Text text -> Ok (`String text)
                  | _ -> assert false)
                ~defer_observation:(fun _ -> failwith "no moderator")
            in
            let policy =
              { Q.default_policy with
                compilation = { Q.default_policy.compilation with max_source_bytes = 512 }
              ; execution = { Q.default_policy.execution with max_calls = 1; fuel = 5000 }
              }
            in
            (invoke
             := fun request ->
                  let result =
                    T.execute
                      ~env
                      ~policy
                      ~script_tools:tools
                      ~now:Agent_protocol.Timestamp.now
                      ~moderate_tool:(fun _ _ -> Ok None)
                      ~prepare_outcome:(fun _ -> Ok ())
                      request
                    |> protocol_ok
                  in
                  response := Some result;
                  T.output result);
            let read_source =
              {|let main input = Task.bind(Tool.call("read_file", input), fun result -> match result with | `Ok(value) -> Task.pure(value) | `Error(code) -> Task.fail(code))|}
            in
            let source =
              match mode with
              | `Compile -> "let main input = Task.pure(input + 1)"
              | `Source_limit -> String.make 513 ' '
              | `Recursive ->
                {|let main input = Task.bind(Tool.call("run_chatml", input), fun result -> match result with | `Ok(value) -> Task.pure(value) | `Error(code) -> Task.fail(code))|}
              | _ -> read_source
            in
            let names =
              match mode with
              | `Unselected -> []
              | `Duplicate -> [ "read_file"; "read_file" ]
              | `Recursive -> [ "read_file"; T.name ]
              | _ -> [ "read_file" ]
            in
            let limits =
              match mode with
              | `Widen -> [ "max_calls", `Number "2" ]
              | `No_calls -> [ "max_calls", `Number "0" ]
              | _ -> []
            in
            let input =
              match mode with
              | `Recursive ->
                `Object
                  [ "source", `String read_source
                  ; "input", `Object []
                  ; "tools", `Array [ `String "read_file" ]
                  ]
              | _ -> `Object []
            in
            let request =
              `Object
                [ "source", `String source
                ; "input", input
                ; "tools", `Array (List.map names ~f:(fun name -> `String name))
                ; "limits", `Object limits
                ]
            in
            let reference =
              C.find registry ~name:T.name
              |> Result.map_error ~f:(fun error -> error.C.message)
              |> Result.ok_or_failwith
              |> C.reference
            in
            let call, invocation = publication_call caps () in
            let item =
              match History_entry.item call with
              | Function_call item ->
                Openai.Responses.Item.Function_call
                  { item with name = T.name; arguments = Jsonaf.to_string request }
              | _ -> assert false
            in
            let call = History_entry.create_with_id ~id:(History_entry.id call) item in
            let invocation =
              I.create
                { invocation.context with
                  tool_name = T.name
                ; input = request
                ; implementation_revision = reference.implementation_revision
                ; capability_fingerprint = C.fingerprint registry
                }
              |> protocol_ok
            in
            caps.commit_invocation_call ~invocation call |> protocol_ok;
            let resolved =
              N.run
                ~capabilities:caps
                ~registry:(fun () -> registry)
                ~reference
                ~invocation
                ~is_halted:(fun () -> false)
                ~authorize:(fun _ _ -> Ok ())
                ~prepare_output:(function
                  | Text text -> Ok (`String text)
                  | _ -> assert false)
              |> protocol_ok
            in
            let result = Option.value_exn !response in
            assert (List.is_empty result.runtime_requests);
            (match resolved.status with
             | Resolved outcome -> assert (I.equal_outcome outcome result.outcome)
             | _ -> failwith "native result was not preserved");
            (match mode, result.outcome with
             | `Compile, Fail error ->
               let diagnostics =
                 Jsonaf.member_exn "diagnostics" error.details |> Jsonaf.list_exn
               in
               let diagnostic =
                 List.hd_exn diagnostics |> Chatmd_shell_spec.Diagnostic.t_of_jsonaf
               in
               let source_ref = Option.value_exn diagnostic.source in
               assert (
                 String.equal
                   source_ref.source_sha256
                   (Chatmd_shell_spec.Source_ref.digest source));
               assert (List.equal String.equal diagnostic.path [ "source" ])
             | _ -> ());
            let output =
              publication_output
                caps
                ~text:(Jsonaf.to_string (I.outcome_to_json result.outcome))
                ()
            in
            caps.publish_invocation_output ~invocation_id:resolved.context.id output
            |> protocol_ok;
            let state = A.state actor |> protocol_ok in
            done_ := true;
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
             match state.active_operation, !done_ with
             | None, true -> state
             | _ ->
               Eio.Fiber.yield ();
               await ()
           in
           let state = await () in
           let result = Option.value_exn !response in
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           assert (
             Int.equal
               1
               (List.count state.invocations ~f:(fun invocation ->
                  Option.is_some invocation.output_entry_id)));
           let summary =
             match result.outcome with
             | Complete (`String text) -> text
             | Fail error -> error.code
             | _ -> failwith "unexpected result"
           in
           print_s
             [%sexp
               (mode
                : [ `Success
                  | `Compile
                  | `Widen
                  | `No_calls
                  | `Unselected
                  | `Source_limit
                  | `Duplicate
                  | `Recursive
                  ])
             , (summary : string)
             , (!calls : int)
             , (Option.is_some result.script_invocation : bool)
             , (List.length state.invocations : int)]));
  [%expect
    {|
    (Success "private output" 1 true 3)
    (Compile chatml.type_error 0 false 1)
    (Widen chatml.limit_escalation 0 false 1)
    (No_calls chatml.call_limit 0 true 2)
    (Unselected chatml.execution_failed 0 true 2)
    (Source_limit chatml.source_limit 0 false 1)
    (Duplicate chatml.invalid_request 0 false 1)
    (Recursive chatml.call_limit 0 true 4)
    |}]
;;
