open Core
open Fixtures
module P = Agent_protocol
module C = Chat_response.Tool_capability
module N = Agent_session.Native_tool_invocation
module A = Agent_session.Session_actor
module W = Agent_session.Authored_agent_call
module R = Chat_response.Agent_runtime
module CM = Prompt.Chat_markdown

let runtime_ok = function
  | Ok value -> value
  | Error errors ->
    failwith (List.map errors ~f:R.diagnostic_to_string |> String.concat ~sep:"; ")
;;

let%expect_test
    "registered authored wrapper uses admitted actor services and shared session results"
  =
  let service_calls = ref 0 in
  let events = Queue.create () in
  let finished = ref false in
  with_handoff_actor
    ~make_worker:(fun env actor_ready ->
      let dir = Eio.Path.(Eio.Stdenv.fs env / Core_unix.getcwd ()) in
      let source =
        Authored_agent_authority_tests.source ~root_path:dir ~policy:Optional
      in
      let private_calls = ref 0 in
      let private_caps = Generated_definition_tests.registry private_calls in
      let status = ref "assigned" in
      let current_registry = ref None in
      let registration =
        W.registration
          ~source
          ~capabilities:private_caps
          ~wait_timeout_ms:0
          ~services:(fun borrowed ->
            incr service_calls;
            let invocation = N.borrowed_invocation borrowed in
            [%test_eq: string] "researcher" invocation.context.tool_name;
            assert (P.Id.Session.equal invocation.context.session_id session_id);
            assert (P.Invocation.equal_origin invocation.context.origin Model);
            let public = N.borrowed_capabilities borrowed |> protocol_ok in
            let reference =
              C.find (Option.value_exn !current_registry) ~name:"researcher"
              |> Authored_agent_authority_tests.caps_ok
              |> C.reference
            in
            Agent_session.Authored_agent_binding.bind
              ~source
              ~public
              ~reference
              ~capabilities:private_caps
            |> protocol_ok
            |> ignore;
            let check actual id =
              assert (P.Invocation.equal invocation (N.borrowed_invocation actual));
              assert (P.Id.Session.equal id second_session_id)
            in
            let receipt () =
              `Object
                [ "session_id", P.Id.Session.to_json second_session_id
                ; "receipt_id", P.History.Id.to_json history_id
                ; "status", `String !status
                ]
            in
            let host : W.host =
              { create =
                  (fun actual ~key:_ ->
                    check actual second_session_id;
                    Queue.enqueue events "create";
                    Ok second_session_id)
              ; validate =
                  (fun actual id ->
                    check actual id;
                    Ok ())
              ; one_off =
                  (fun actual ~input ->
                    check actual second_session_id;
                    Queue.enqueue events "one_off";
                    Ok (`String ("one-off: " ^ input)))
              }
            in
            let sessions : Agent_session.Managed_session_service.t =
              { send =
                  (fun actual id ~key:_ ~message:_ ->
                    check actual id;
                    Queue.enqueue events "send";
                    Ok (receipt ()))
              ; wait =
                  (fun actual id ~target ~timeout_ms ->
                    check actual id;
                    [%test_eq: int] 0 timeout_ms;
                    (match target with
                     | Receipt id -> assert (P.History.Id.equal id history_id)
                     | Output _ -> failwith "wrong wait target");
                    Queue.enqueue events "wait";
                    Ok (`Object [ "reason", `String "timeout" ]))
              ; read =
                  (fun actual id ~receipt_id ~cursor ~limit ->
                    check actual id;
                    assert (Option.equal P.History.Id.equal receipt_id (Some history_id));
                    assert (Option.is_none cursor);
                    [%test_eq: int] 16 limit;
                    Queue.enqueue events "read";
                    Ok
                      (`Object
                          [ "session_id", P.Id.Session.to_json id
                          ; "receipt", receipt ()
                          ; "items", `Array [ `String "answer" ]
                          ; "next_cursor", `String "cursor"
                          ; "caught_up", `True
                          ]))
              ; status = (fun _ _ -> failwith "unexpected status request")
              ; stop = (fun _ _ ~key:_ ~mode:_ -> failwith "unexpected stop request")
              }
            in
            Ok (host, sessions))
          ()
        |> protocol_ok
      in
      (match registration.implementation.run {|{"input":"outside"}|} with
       | Text text ->
         (match P.Invocation.outcome_of_json (Jsonaf.of_string text) |> protocol_ok with
          | Fail error -> [%test_eq: string] "agent.authored.denied" error.code
          | _ -> failwith "unowned wrapper executed")
       | _ -> failwith "expected structured native result");
      [%test_eq: int] 0 !service_calls;
      Agent_session.Operation_worker.create ~run:(fun ~sw ~input:_ caps ->
        let actor = Eio.Promise.await actor_ready in
        let agent = Agent_session.Authored_agent_source.declaration source in
        let elements = [ CM.Tool (Persistent_agent (agent, Optional)) ] in
        let ctx =
          Chat_response.Ctx.create
            ~env
            ~dir
            ~tool_dir:dir
            ~cache:(Chat_response.Cache.create ~max_size:1 ())
        in
        let host =
          R.host
            ~env
            ~workspace:dir
            ~tool_dir:dir
            ~prompt_dir:dir
            ~session_dir:dir
            ~cache_dir:dir
            ~home:dir
            ~session_id:(P.Id.Session.to_string session_id)
            ~resource_runner:None
            ~prompt_elements:elements
          |> runtime_ok
        in
        let prepare registrations elements =
          R.prepare_extensions
            ~native_registrations:registrations
            ~sw
            ~ctx
            ~host
            ~platform:(R.platform ())
            ~prompt_elements:elements
            ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
            ~approval_provider:Shell_runtime.Approval_broker.None_available
            ~approval_store:(Shell_access.Approval.create_store ())
            ~run_agent:
              (fun
                ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
              failwith "legacy agent runner used")
            ()
        in
        assert (Result.is_error (prepare [] elements));
        (match
           prepare [ registration ] [ CM.Tool (Persistent_agent (agent, Persistent)) ]
         with
         | Error errors ->
           assert (
             List.exists errors ~f:(fun error ->
               String.equal error.R.code "agent.persistence_contract"))
         | Ok _ -> failwith "optional wrapper overrode fixed authored policy");
        let unselected = prepare [ registration ] [] |> runtime_ok in
        assert (List.is_empty unselected.native.functions);
        let prepared = prepare [ registration ] elements |> runtime_ok in
        let registry =
          Lazy.force prepared.native.capabilities
          |> Authored_agent_authority_tests.caps_ok
        in
        current_registry := Some registry;
        assert (Result.is_error (C.find registry ~name:"read_file"));
        let reference =
          C.find registry ~name:"researcher"
          |> Authored_agent_authority_tests.caps_ok
          |> C.reference
        in
        assert (
          Jsonaf.exactly_equal
            reference.input_schema
            (Chat_response.Agent_tool_contract.parameters Optional));
        let dispatch json =
          let original, invocation = publication_call caps () in
          let call =
            History_entry.with_item
              original
              (Chat_response.Tool_call.call_item
                 ~kind:Function
                 ~name:"researcher"
                 ~payload:(Jsonaf.to_string json)
                 ~call_id:"reused"
                 ~id:None)
          in
          let invocation =
            P.Invocation.create
              { invocation.context with
                tool_name = "researcher"
              ; input = json
              ; implementation_revision = reference.implementation_revision
              ; capability_fingerprint = C.fingerprint registry
              }
            |> protocol_ok
          in
          caps.commit_invocation_call ~invocation call |> protocol_ok;
          let resolved =
            N.run_scoped
              ~execute:caps.with_invocation
              ~registry:(fun () -> registry)
              ~reference
              ~invocation
              ~is_halted:(fun () -> false)
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(function
                | Text text -> Ok (`String text)
                | _ -> failwith "expected text")
            |> protocol_ok
          in
          let outcome =
            match resolved.status with
            | Resolved outcome -> outcome
            | _ -> failwith "wrapper did not resolve"
          in
          caps.publish_invocation_output
            ~invocation_id:resolved.context.id
            (publication_output
               caps
               ~text:(P.Invocation.outcome_to_json outcome |> Jsonaf.to_string)
               ())
          |> protocol_ok;
          outcome
        in
        let input extra = `Object (("input", `String "question") :: extra) in
        (match dispatch (input []) with
         | Complete (`String "one-off: question") -> ()
         | _ -> failwith "optional default changed");
        [%test_eq: string list] [ "one_off" ] (Queue.to_list events);
        Queue.clear events;
        (match
           dispatch (input [ "session_id", P.Id.Session.to_json second_session_id ])
         with
         | Fail error -> [%test_eq: string] "agent.authored.invalid_request" error.code
         | _ -> failwith "inconsistent optional call accepted");
        [%test_eq: int] 1 !service_calls;
        assert (Queue.is_empty events);
        let pending = dispatch (input [ "mode", `String "persistent" ]) in
        (match pending with
         | Complete result ->
           assert (
             Jsonaf.exactly_equal (Jsonaf.member_exn "status" result) (`String "pending"))
         | _ -> failwith "pending session did not return a complete tool result");
        [%test_eq: string list]
          [ "create"; "send"; "wait"; "read" ]
          (Queue.to_list events);
        Queue.clear events;
        status := "completed";
        (match
           dispatch
             (input
                [ "mode", `String "persistent"
                ; "session_id", P.Id.Session.to_json second_session_id
                ])
         with
         | Complete result ->
           assert (
             Jsonaf.exactly_equal
               (Jsonaf.member_exn "status" result)
               (`String "completed"));
           assert (
             Jsonaf.exactly_equal
               (Jsonaf.member_exn "session_id" result)
               (P.Id.Session.to_json second_session_id))
         | _ -> failwith "continuation did not return shared session output");
        [%test_eq: string list] [ "send"; "wait"; "read" ] (Queue.to_list events);
        [%test_eq: int] 0 !private_calls;
        finished := true;
        Completed
          { final_history =
              (A.state actor |> protocol_ok).conversation.canonical_history
              |> Agent_session.History_codec.all_of_protocol
              |> protocol_ok
          ; moderator_snapshot = None
          ; runtime_requests = []
          }))
    (fun _env actor _writer _backend ->
       let rec await () =
         match (A.state actor |> protocol_ok).active_operation with
         | None -> ()
         | Some _ ->
           Eio.Fiber.yield ();
           await ()
       in
       await ();
       assert !finished);
  print_endline
    "explicit authored declaration selects the registered wrapper; private tools stay \
     unexposed";
  print_endline
    "unowned and inconsistent calls cannot obtain services; optional one-off uses the \
     admitted callback";
  print_endline
    "real actor dispatch returns correlated pending/completed results through shared \
     services; no legacy runner";
  [%expect
    {|
    explicit authored declaration selects the registered wrapper; private tools stay unexposed
    unowned and inconsistent calls cannot obtain services; optional one-off uses the admitted callback
    real actor dispatch returns correlated pending/completed results through shared services; no legacy runner
    |}]
;;
