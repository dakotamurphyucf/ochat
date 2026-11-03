open! Core
module P = Agent_protocol
module A = Agent_session.Session_actor
module W = Agent_session.Operation_worker
module Selection = Agent_session.Instruction_selection
module C = Chat_response.Tool_capability
module Dispatch = Chat_response.In_memory_stream.Tool_dispatch

let ok = Fixtures.protocol_ok
let object_ fields = `Object fields
let member name value = Jsonaf.member_exn name value
let array name value = member name value |> Jsonaf.list_exn

let complete = function
  | P.Invocation.Complete value -> value
  | outcome -> raise_s [%sexp "native failed", (outcome : P.Invocation.outcome)]
;;

let registry () =
  let registrations =
    List.map
      Agent_session.Instruction_native_admission.Effect.all
      ~f:Agent_session.Instruction_native_tool.registration
  in
  C.create
    ~owner:"instruction-fault-fixture"
    ~resource_fingerprint:
      (Chatmd_shell_spec.Source_ref.digest "instruction-fault-fixture")
    ~result_contracts:
      (List.map Agent_session.Instruction_native_admission.Effect.all ~f:(fun operation ->
         Agent_session.Instruction_native_admission.Effect.name operation, C.Invocation_v1))
    (List.map registrations ~f:(fun registration ->
       ( registration.Chat_response.Agent_runtime.implementation_revision
       , registration.implementation )))
  |> Result.map_error ~f:(fun error -> error.C.message)
  |> Result.ok_or_failwith
;;

let selection_delta = function
  | Agent_session.Session_delta.Instruction_selection_changed _ -> true
  | Batch deltas ->
    List.exists deltas ~f:(function
      | Instruction_selection_changed _ -> true
      | _ -> false)
  | _ -> false
;;

let call actor (caps : W.Capabilities.t) dispatch ~id ~name json =
  let allocate () =
    History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
  in
  let payload = Jsonaf.to_string json in
  let call =
    Openai.Responses_history.create_with_id_exn
      ~id:(allocate ())
      (Function_call
         { name
         ; arguments = payload
         ; call_id = id
         ; _type = "function_call"
         ; id = None
         ; status = None
         })
  in
  let state = A.state actor |> ok in
  let request : Dispatch.request =
    { kind = Function
    ; original_name = name
    ; original_payload = payload
    ; name
    ; payload
    ; rejection = None
    ; call
    ; history =
        Agent_session.History_codec.all_of_protocol state.conversation.canonical_history
        |> ok
    ; source = None
    ; parent_call_id = None
    }
  in
  assert (dispatch.Dispatch.commit_call request);
  let retained_read = ref None in
  let run_native implementation ~payload =
    if String.equal name "instructions_read"
    then (
      let module N = Agent_session.Native_tool_invocation in
      let module Admission = Agent_session.Instruction_native_admission in
      let borrowed = N.borrow () |> ok in
      let selected = N.borrowed_capabilities borrowed |> ok in
      let binding =
        C.find selected ~name
        |> Result.map_error ~f:(fun error -> error.C.message)
        |> Result.ok_or_failwith
      in
      let token = N.Admission.capture borrowed ~binding |> ok in
      let admission = Admission.create token ~binding ~effect_kind:Read |> ok in
      let state = A.state actor |> ok in
      Admission.check_now admission state ~effect_kind:Read |> ok;
      assert (
        Result.is_error (Admission.check_now admission state ~effect_kind:Deactivate));
      retained_read := Some admission);
    implementation.Ochat_function.run payload
  in
  let result =
    dispatch.run ~run_native request ~authorize:(fun () -> ()) |> Option.value_exn
  in
  Option.iter !retained_read ~f:(fun admission ->
    let state = A.state actor |> ok in
    assert (
      Result.is_error
        (Agent_session.Instruction_native_admission.check_now
           admission
           state
           ~effect_kind:Read)));
  let text =
    match result.output with
    | Text text -> text
    | _ -> failwith "instruction native returned nontext"
  in
  let output =
    Openai.Responses_history.create_with_id_exn
      ~id:(allocate ())
      (Function_call_output
         { call_id = id
         ; output = Text text
         ; id = None
         ; _type = "function_call_output"
         ; status = None
         })
  in
  (Option.value_exn result.commit_output) output;
  P.Invocation.outcome_of_json (Jsonaf.of_string text) |> ok
;;

let%expect_test
    "genuine native failed selection save publishes no receipt and exact retry succeeds"
  =
  let principal =
    P.Principal.create
      ~id:Fixtures.principal_id
      ~authentication_kind:"local"
      ~scopes:(P.Scope.Set.of_list [ Send_messages; View_session_transcript ])
      ~attributes:[]
    |> ok
  in
  let original = Operator_authorization.guarded ~principal ~is_current:(fun () -> true) in
  Runtime_tests.with_port
    ~actor:original
    (fun env _home left _right port _original admitted _installed ->
       Support.save left "alpha/SKILL.md" (Support.skill "alpha");
       let rejected = ref 0 in
       let reject_next = ref true in
       let workspace_instance =
         Agent_session.Workspace_resolver.resolve_current
           ~env
           ~instance_id:Fixtures.instance_id
           ~path:(Eio.Path.native_exn left)
           ~access:Shared_write
           ~created_at:Fixtures.timestamp
         |> Fixtures.store_ok
       in
       Job_fixtures.with_actor
         ~environment:(env, workspace_instance)
         ~reject_save:(fun (transition : Agent_session.Session_transition.t) ->
           if !reject_next && selection_delta transition.delta
           then (
             reject_next := false;
             incr rejected;
             true)
           else false)
         (fun env _sw actor writer backend ->
            let finished, finish = Eio.Promise.create () in
            let worker =
              W.create ~run:(fun ~sw:_ ~input caps ->
                let registry = registry () in
                let service =
                  Agent_session.Instruction_service.create
                    ~state:(fun () -> A.state actor)
                    ~apply:(fun ~admission ~source ~request ->
                      A.instruction_command actor ~admission ~source ~request)
                    ~sources:(fun () -> Some port)
                    ~selection_current:(A.instruction_selection_current actor)
                in
                let tools =
                  Agent_session.Script_tool_calls.create
                    ~registry:(fun () -> registry)
                    ~moderator_names:String.Set.empty
                    ~now:(fun () -> Fixtures.timestamp)
                    ~is_halted:(fun () -> false)
                    ~requires_active_moderator:(fun _ -> false)
                    ~authorize:(fun _ _ -> Ok ())
                    ~prepare_output:(function
                      | Text text -> Ok (`String text)
                      | _ -> assert false)
                    ~defer_observation:(fun _ -> Ok ())
                  |> fun tools ->
                  Agent_session.Script_tool_calls.with_instruction_service tools service
                in
                let dispatch =
                  Agent_session.Script_tool_calls.native_dispatch
                    tools
                    ~declared:registry
                    ~input
                    ~capabilities:caps
                in
                let invoke id name value = call actor caps dispatch ~id ~name value in
                let refresh selection = object_ [ "selection", selection ] in
                let roots =
                  invoke "roots" "instructions_refresh" (refresh `Null) |> complete
                in
                let selection =
                  object_
                    [ "kind", `String "skills"
                    ; "roots", `Array [ List.hd_exn (array "roots" roots) ]
                    ; "scope", `Null
                    ]
                in
                let discovery =
                  invoke "discover" "instructions_refresh" (refresh selection) |> complete
                in
                let descriptor =
                  [ "selection", selection
                  ; ( "members"
                    , `Array
                        (List.map (array "members" discovery) ~f:(fun observed ->
                           object_
                             (List.map [ "root"; "member"; "sha256" ] ~f:(fun key ->
                                key, member key observed)))) )
                  ]
                in
                let read =
                  invoke "read" "instructions_read" (object_ descriptor) |> complete
                in
                let activation =
                  object_
                    (descriptor
                     @ [ "capture_id", member "capture_id" read
                       ; "expected_revision", `String "0"
                       ; "key", `String "same-native-key"
                       ])
                in
                let initial = A.state actor |> ok in
                let public =
                  Agent_session.Session_state.snapshot ~now:Fixtures.timestamp initial
                  |> Agent_server.Principal_projection.snapshot principal
                  |> ok
                in
                let projection = ref (Agent_client.Projection.install_snapshot public) in
                let status_events = ref 0 in
                let observe ~receipts ~skills =
                  let fields =
                    Agent_client.Projection.snapshot !projection
                    |> P.Public.Snapshot.fields
                  in
                  let events =
                    Agent_session.Memory_backend.events_after
                      backend
                      fields.latest_event_sequence
                    |> ok
                  in
                  List.iter events ~f:(fun event ->
                    if Option.is_some (P.Event.Durable.instruction_status event |> ok)
                    then incr status_events;
                    let public =
                      Agent_server.Principal_projection.durable principal event |> ok
                    in
                    (* Exercise the wire decoder before the actual client reducer. *)
                    let public =
                      P.Public.Durable.of_json (P.Public.Durable.to_json public) |> ok
                    in
                    projection
                    := Agent_client.Projection.apply_event !projection public |> ok);
                  let fields =
                    Agent_client.Projection.snapshot !projection
                    |> P.Public.Snapshot.fields
                  in
                  let status = Option.value_exn fields.instruction_status in
                  [%test_eq: int] receipts status.retained_receipts;
                  [%test_eq: int] skills status.skills
                in
                (match invoke "failed" "instructions_activate" activation with
                 | Fail
                     { code = "internal_error"
                     ; message = "injected job transition failure"
                     ; _
                     } -> ()
                 | outcome ->
                   raise_s
                     [%sexp
                       "expected actual save failure", (outcome : P.Invocation.outcome)]);
                observe ~receipts:0 ~skills:0;
                [%test_eq: int] 0 !status_events;
                [%test_eq: int] 1 !rejected;
                let before = A.state actor |> ok in
                assert (Option.is_none before.instruction_selection);
                assert (
                  Option.is_none
                    (Agent_session.Memory_backend.state backend).instruction_selection);
                let receipt =
                  invoke "retry" "instructions_activate" activation |> complete
                in
                observe ~receipts:1 ~skills:1;
                [%test_eq: int] 1 !status_events;
                let selected = A.state actor |> ok in
                let selection_state = Option.value_exn selected.instruction_selection in
                [%test_eq: int64] 1L (Selection.revision selection_state);
                [%test_eq: int] 1 (List.length (Selection.receipts selection_state));
                Eio.Path.unlink Eio.Path.(left / "alpha/SKILL.md");
                let replay =
                  invoke "retained" "instructions_activate" activation |> complete
                in
                assert (Document_schema.Json.equal receipt replay);
                observe ~receipts:1 ~skills:1;
                [%test_eq: int] 1 !status_events;
                admitted := false;
                ignore
                  (invoke
                     "clear"
                     "instructions_deactivate"
                     (object_
                        [ "kind", `String "all"
                        ; "members", `Null
                        ; "expected_revision", `String "1"
                        ; "key", `String "clear-after-revoke"
                        ])
                   |> complete
                   : Jsonaf.t);
                let state = A.state actor |> ok in
                observe ~receipts:2 ~skills:0;
                [%test_eq: int] 2 !status_events;
                let cleared = Option.value_exn state.instruction_selection in
                [%test_eq: int64] 2L (Selection.revision cleared);
                [%test_eq: int] 2 (List.length (Selection.receipts cleared));
                assert (List.is_empty (Selection.skills cleared));
                Eio.Promise.resolve finish ();
                W.Completed
                  { final_history =
                      Agent_session.History_codec.all_of_protocol
                        state.conversation.canonical_history
                      |> ok
                  ; moderator_snapshot = None
                  ; runtime_requests = []
                  })
            in
            A.set_operation_worker actor (Some worker) |> ok;
            let context =
              Agent_server.Connection_context.create_authenticated
                ~actor:original
                ~connection_id:"native-fault"
                ~transport:In_memory
                ~publish_notification:ignore
                ~max_attachments:1
            in
            let actor_proof =
              Agent_server.Connection_context.request_actor context None |> ok
            in
            A.submit_message_authenticated
              actor
              ~actor:actor_proof
              ~attachment_id:writer.id
              Fixtures.actor_entry
            |> ok
            |> ignore;
            let clock = Eio.Stdenv.clock env in
            Eio.Time.with_timeout_exn clock 10. (fun () ->
              Eio.Fiber.first
                (fun () -> Eio.Promise.await finished)
                (fun () ->
                   let rec watch () =
                     let state = A.state actor |> ok in
                     Option.iter state.failure ~f:(fun error ->
                       raise_s [%sexp (error : P.Error.t)]);
                     match state.active_operation with
                     | None -> failwith "native fault worker ended before assertions"
                     | Some _ ->
                       Eio.Time.sleep clock 0.001;
                       watch ()
                   in
                   watch ()));
            Eio.Time.with_timeout_exn clock 5. (fun () ->
              let rec wait () =
                let state = A.state actor |> ok in
                Option.iter state.failure ~f:(fun error ->
                  raise_s [%sexp (error : P.Error.t)]);
                if Option.is_some state.active_operation
                then (
                  Eio.Time.sleep clock 0.001;
                  wait ())
              in
              wait ())));
  print_endline
    "real native save rejected before selection/receipt; exact retry ACKed once; \
     revoked-source Clear succeeds";
  [%expect
    {| real native save rejected before selection/receipt; exact retry ACKed once; revoked-source Clear succeeds |}]
;;
