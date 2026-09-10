open Core
open Fixtures
module T = Agent_session.Run_chatml_tool
module C = Chat_response.Tool_capability
module I = Agent_protocol.Invocation
module A = Agent_session.Session_actor
module R = Chat_response.Moderation.Runtime_request
module D = Chat_response.In_memory_stream.Tool_dispatch

let%expect_test
    "registered run_chatml forwards moderator requests even when its script fails"
  =
  List.iter [ false; true ] ~f:(fun fail_after_call ->
    let calls = ref 0 in
    let observed_requests = ref [] in
    let summary = ref "not run" in
    let finished = ref false in
    with_handoff_actor
      ~make_worker:(fun env actor_ready ->
        let services = ref None in
        let registration =
          T.registration
            ~env
            ~policy:Chat_response.One_off_request.default_policy
            ~services:(fun () -> Result.of_option !services ~error:"not installed")
        in
        (* Descriptor construction and use outside an owned native invocation do
           not obtain services, execute source or manufacture a session. *)
        assert (
          Exn.does_raise (fun () ->
            registration.implementation.run {|{"source":"","input":null,"tools":[]}|}));
        let reader =
          native_registry calls ~raises:false
          |> fun registry ->
          C.find registry ~name:"read_file"
          |> Result.map_error ~f:(fun error -> error.C.message)
          |> Result.ok_or_failwith
          |> C.native_implementation
          |> Option.value_exn
        in
        let registry =
          C.create
            ~result_contracts:[ T.name, registration.result_contract ]
            ~owner:"registered-run-chatml"
            ~resource_fingerprint:
              (Chatmd_shell_spec.Source_ref.digest "registered run fixture")
            [ registration.implementation_revision, registration.implementation
            ; Chatmd_shell_spec.Source_ref.digest "reader", reader
            ]
          |> Result.map_error ~f:(fun error -> error.C.message)
          |> Result.ok_or_failwith
        in
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let actor = Eio.Promise.await actor_ready in
          let script_tools =
            Agent_session.Script_tool_calls.create
              ~registry:(fun () -> registry)
              ~moderator_names:String.Set.empty
              ~now:Agent_protocol.Timestamp.now
              ~is_halted:(fun () -> false)
              ~requires_active_moderator:(fun _ -> false)
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(function
                | Text text -> Ok (`String text)
                | _ -> assert false)
              ~defer_observation:(fun _ -> failwith "unexpected observer")
          in
          services
          := Some
               T.
                 { script_tools
                 ; observer = None
                 ; now = Agent_protocol.Timestamp.now
                 ; moderate_tool =
                     (fun _ _ ->
                       Ok
                         (Some
                            { Chat_response.Moderation.Outcome.empty with
                              runtime_requests = [ R.Request_turn; Request_compaction ]
                            }))
                 ; prepare_outcome =
                     (fun outcome ->
                       I.validate_outcome outcome
                       |> Result.map_error ~f:(fun error ->
                         error.Agent_protocol.Error.message))
                 };
          let source =
            {|let main input = Task.bind(Tool.call("read_file", input), fun result -> |}
            ^
            if fail_after_call
            then {|Task.fail("after native effect"))|}
            else
              {|match result with | `Ok(value) -> Task.pure(value) | `Error(code) -> Task.fail(code))|}
          in
          let payload =
            Jsonaf.to_string
              (`Object
                  [ "source", `String source
                  ; "input", `Object []
                  ; "tools", `Array [ `String "read_file" ]
                  ])
          in
          let dispatch =
            Agent_session.Script_tool_calls.native_dispatch
              ~declared:
                (Agent_session.Script_tool_calls.current_capabilities script_tools)
              script_tools
              ~input
              ~capabilities:caps
          in
          let call_id = "run-chatml-call" in
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
          dispatch.validate_original ~kind:Function ~name:T.name ~payload
          |> Result.ok_or_failwith;
          assert (dispatch.commit_call request);
          let response =
            dispatch.run request ~authorize:(fun () -> ()) |> Option.value_exn
          in
          observed_requests := response.runtime_requests;
          let outcome =
            match response.output with
            | Text text -> I.outcome_of_json (Jsonaf.of_string text) |> protocol_ok
            | _ -> assert false
          in
          (summary
           := match outcome with
              | Complete (`String text) -> text
              | Fail error -> error.code
              | _ -> failwith "unexpected outcome");
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
          (Option.value_exn response.commit_output) output;
          let state = A.state actor |> protocol_ok in
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
         assert (Int.equal (List.length state.invocations) 3);
         assert (
           Int.equal
             (List.count state.invocations ~f:(fun invocation ->
                Option.is_some invocation.output_entry_id))
             1);
         print_s
           [%sexp
             (fail_after_call : bool)
           , (!summary : string)
           , (!calls : int)
           , (!observed_requests : R.t list)]));
  [%expect
    {|
    (false "private output" 1 (Request_turn Request_compaction))
    (true chatml.execution_failed 1 (Request_turn Request_compaction))
    |}]
;;
