open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module C = Chat_response.Tool_capability
module Contract = Agent_session.Standalone_completion_contract

let capabilities entry =
  Agent_server.Runtime_owner.with_background_runtime
    entry.Agent_server.Session_registry.runtime
    (fun runtime ->
       Ok
         (Agent_session.Script_tool_calls.current_capabilities
            (Option.value_exn runtime.moderator_script_tools)))
  |> protocol_ok
;;

let%expect_test
    "standalone Pending captures original completion policy, survives live identity \
     changes and cannot widen on replay"
  =
  with_daemon
    ~runtime_policy:
      { Chat_response.Runtime_semantics.default_policy with honor_request_turn = false }
    ~sources:Standalone_pending_tests.sources
    ~calls:
      [ ( "contract"
        , "compare_reports_async"
        , Standalone_tests.input "report-a.json" "report-b.json" )
      ]
    ~after_turn:(fun env handle entry ->
      Job_launch_tests.settle env entry;
      let state = A.state entry.actor |> protocol_ok in
      let invocation = model_invocation state "contract" in
      let contract = Option.value_exn invocation.completion_contract in
      [%test_eq: string] "compare_reports_async" contract.tool_name;
      [%test_eq: string list]
        [ "compare_reports" ]
        (List.map contract.capability_pins ~f:fst);
      assert (Option.is_some contract.completion_schema);
      let roundtrip =
        P.Invocation.of_json (P.Invocation.to_json invocation) |> protocol_ok
      in
      assert (P.Invocation.equal invocation roundtrip);
      let job = List.hd_exn state.jobs in
      let result = P.Job.terminal_completion job |> protocol_ok |> Option.value_exn in
      Contract.validate_result contract result
      |> Result.map_error ~f:(fun error -> error.P.Invocation.message)
      |> Result.ok_or_failwith;
      let denied value =
        match Contract.validate_result contract (Succeeded value) with
        | Ok () -> failwith "invalid eventual result accepted"
        | Error error ->
          [%test_eq: string] "background.invalid_completion" error.code;
          assert (Jsonaf.exactly_equal error.details `Null);
          assert (not (String.is_substring error.message ~substring:"PRIVATE"))
      in
      denied (`String "PRIVATE-SCHEMA-FAILURE");
      denied
        (`Object
            [ "left", `String (String.make contract.max_output_bytes 'x')
            ; "right", `String "PRIVATE"
            ; "invocation_count", `Number "1"
            ]);
      let replace_contract json transform =
        match json with
        | `Object fields ->
          `Object
            (List.map fields ~f:(function
               | "completion_contract", value -> "completion_contract", transform value
               | field -> field))
        | _ -> assert false
      in
      let changed =
        replace_contract (P.Invocation.to_json invocation) (fun _ ->
          P.Completion_contract.to_json { contract with max_output_depth = 1 })
        |> P.Invocation.of_json
        |> protocol_ok
      in
      assert (
        Result.is_error
          (P.Invocation.validate_transition ~previous:(Some invocation) changed));
      let downgraded =
        match P.Invocation.to_json invocation with
        | `Object fields ->
          `Object
            (List.Assoc.add fields ~equal:String.equal "schema_version" (`Number "10"))
        | _ -> assert false
      in
      assert (Result.is_error (P.Invocation.of_json downgraded));
      let old_selection =
        Contract.rebind contract ~current_capabilities:(capabilities entry) |> protocol_ok
      in
      H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
      unload_idle_runtime env entry.runtime;
      H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
      let current = capabilities entry in
      let selection =
        Contract.rebind contract ~current_capabilities:current |> protocol_ok
      in
      assert (not (String.equal (C.fingerprint old_selection) (C.fingerprint selection)));
      List.iter [ "compare_reports_async"; "compare_reports" ] ~f:(fun removed ->
        let names =
          C.references current
          |> List.filter_map ~f:(fun reference ->
            Option.some_if (not (String.equal reference.C.name removed)) reference.name)
        in
        let narrowed =
          C.select current ~names
          |> Result.map_error ~f:(fun error -> error.C.message)
          |> Result.ok_or_failwith
        in
        assert (Result.is_error (Contract.rebind contract ~current_capabilities:narrowed)));
      let restored =
        A.state entry.actor
        |> protocol_ok
        |> fun state -> model_invocation state "contract"
      in
      assert (
        Option.equal
          P.Completion_contract.equal
          restored.completion_contract
          (Some contract)))
    ~settle:Job_launch_tests.settle
    (fun _ ->
       print_endline
         "original schema and bounds retained; reload rebinds stable pins; \
          publisher/dependency removal and contract rewrite rejected");
  [%expect
    {| original schema and bounds retained; reload rebinds stable pins; publisher/dependency removal and contract rewrite rejected |}]
;;
