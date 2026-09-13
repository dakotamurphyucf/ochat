open Core
open Fixtures

let%expect_test "idle moderator tools preserve authority, outcomes and scope lifetime" =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let module M = Chat_response.Moderator_manager in
  let module C = Chat_response.Tool_capability in
  List.iter
    [ `Success
    ; `Deny
    ; `Revoke
    ; `Reentrant
    ; `Handler_fail
    ; `Save_fail
    ; `Cancel_native
    ; `Forged_parent
    ]
    ~f:(fun mode ->
      let prepared = ref None in
      let native_calls = ref 0
      and authorizations = ref 0
      and rejected = ref false in
      let on_native = ref (fun () -> ()) in
      let registry =
        ref
          (native_registry ~on_call:(fun () -> !on_native ()) native_calls ~raises:false)
      in
      with_handoff_actor
        ~reject:(fun next ->
          match mode, !rejected with
          | `Save_fail, false
            when List.exists
                   next.Agent_session.Session_transition.state.invocations
                   ~f:(fun invocation ->
                     String.equal invocation.context.tool_name "read_file"
                     &&
                     match invocation.status with
                     | Resolved (Complete _) -> true
                     | _ -> false) ->
            rejected := true;
            true
          | _ -> false)
        ~make_worker:(fun env _ ->
          let finish =
            match mode with
            | `Handler_fail -> "Task.fail(\"after native effect\")"
            | _ -> "Task.pure(state)"
          in
          let manager, _, definition =
            handoff_definition
              env
              ~capability_registry:!registry
              ~declare_tool:false
              ~events:
                ("| `Tool_observed(p) -> let ignored = state[0] <- state[0] + 1 in "
                 ^ "Task.bind(Tool.call(\"read_file\", `Object([])), fun ignored -> "
                 ^ finish
                 ^ ") | _ -> Task.pure(state)")
          in
          let observer = M.invocation_observer manager |> Option.value_exn in
          let snapshot =
            Some
              (Agent_session.Runtime_builder.encode_moderator_snapshot
                 (M.identity_snapshot manager |> Result.ok_or_failwith))
          in
          prepared := Some (manager, definition, observer);
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            caps.commit_moderator snapshot |> protocol_ok;
            let parent =
              I.create { (invocation_fixture ()).context with tool_name = "root" }
              |> protocol_ok
            in
            caps.with_invocation ~invocation:parent (fun ~dispatched:_ ->
              let child =
                I.create
                  ~observer
                  { parent.context with
                    id = Agent_protocol.Id.Invocation.create ()
                  ; origin = Moderator
                  ; parent_invocation = Some parent.context.id
                  ; tool_name = "seed"
                  }
                |> protocol_ok
              in
              caps.with_invocation ~invocation:child (fun ~dispatched:_ ->
                Ok (Complete (`String "seed")))
              |> protocol_ok
              |> ignore;
              Ok (Complete `Null))
            |> protocol_ok
            |> ignore;
            Completed
              { final_history = input.history
              ; moderator_snapshot = snapshot
              ; runtime_requests = []
              }))
        (fun _env actor writer backend ->
           let initial = await_idle actor in
           let manager, definition, observer = Option.value_exn !prepared in
           let seed =
             List.find_exn initial.invocations ~f:(fun i ->
               String.equal i.context.tool_name "seed")
           in
           (on_native
            := fun () ->
                 let owned = A.state actor |> protocol_ok in
                 assert (Option.is_none owned.active_operation);
                 match mode with
                 | `Cancel_native ->
                   A.stop actor ~attachment_id:writer.id ~mode:Cancel
                   |> protocol_ok
                   |> ignore;
                   assert (Result.is_error (A.start actor ~attachment_id:writer.id));
                   Eio.Fiber.yield ()
                 | _ -> ());
           let tools =
             Agent_session.Script_tool_calls.create
               ~registry:(fun () -> !registry)
               ~moderator_names:String.Set.empty
               ~now:Agent_protocol.Timestamp.now
               ~is_halted:(fun () ->
                 let state = A.state actor |> protocol_ok in
                 match state.lifecycle.desired with
                 | Running -> state.halted
                 | Stopped -> true)
               ~requires_active_moderator:(fun _ ->
                 match mode with
                 | `Reentrant -> true
                 | _ -> false)
               ~authorize:(fun _ _ ->
                 Int.incr authorizations;
                 Eio.Fiber.yield ();
                 match mode with
                 | `Deny -> Error (handoff_error "denied")
                 | `Revoke ->
                   registry := native_registry native_calls ~raises:false;
                   Ok ()
                 | _ -> Ok ())
               ~prepare_output:(fun _ -> Ok (`String "disclosed"))
               ~defer_observation:(fun _ -> Ok ())
           in
           let escaped = ref None
           and escaped_executor = ref None in
           let forged_rejected = ref false in
           let run () =
             A.with_idle_moderator_observation_tools
               actor
               ~observer
               (fun ~observing ~execute ~commit ->
                  let reference = List.hd_exn (C.references !registry) in
                  let child () =
                    I.create
                      ~observer
                      { observing.context with
                        id = Agent_protocol.Id.Invocation.create ()
                      ; parent_invocation = Some observing.context.id
                      ; tool_name = "read_file"
                      ; input = `Object []
                      ; implementation_revision = reference.implementation_revision
                      ; capability_fingerprint = C.fingerprint !registry
                      }
                    |> protocol_ok
                  in
                  escaped_executor := Some (execute, child);
                  (match mode with
                   | `Forged_parent ->
                     let invocation = child () in
                     let invocation =
                       I.create
                         ~observer
                         { invocation.context with
                           parent_invocation = seed.context.parent_invocation
                         }
                       |> protocol_ok
                     in
                     forged_rejected
                     := Result.is_error
                          (execute ~invocation (fun ~dispatched:_ -> assert false))
                   | _ -> ());
                  Agent_session.Script_tool_calls.with_observation
                    tools
                    ~definition
                    ~execute
                    ~observing
                    (fun call ->
                       escaped := Some call;
                       let handled =
                         M.handle_observation_entries
                           manager
                           ~retain_follow_up:true
                           ~on_tool_call:call
                           ~invocation:observing
                           ~history:
                             (Agent_session.History_codec.all_of_protocol
                                initial.conversation.canonical_history
                              |> protocol_ok)
                           ~available_tools:[]
                           ~session_meta:`Null
                           ~now_ms:0
                           ~prepare_observation:(fun ~observed ~outcome:_ ~snapshot ->
                             Ok
                               { M.persist =
                                   (fun () ->
                                     commit ~resolved:observed ~snapshot
                                     |> Result.map_error ~f:(fun error ->
                                       error.Agent_protocol.Error.message))
                               ; install = ignore
                               })
                       in
                       (match handled with
                        | Ok _ ->
                          assert (
                            match call ~name:"read_file" ~args:(`Object []) with
                            | Ok (Tool_error "invocation.admission_failed") -> true
                            | _ -> false)
                        | Error _ -> ());
                       handled
                       |> Result.map ~f:ignore
                       |> Result.map_error ~f:handoff_error))
           in
           let completed =
             try Result.is_ok (run ()) with
             | Eio.Cancel.Cancelled _ -> false
           in
           let calls_before_escape = !native_calls in
           let call = Option.value_exn !escaped in
           assert (
             match call ~name:"read_file" ~args:(`Object []) with
             | Ok (Tool_error "invocation.inactive_scope") -> true
             | _ -> false);
           let execute, child = Option.value_exn !escaped_executor in
           assert (
             Result.is_error
               (execute ~invocation:(child ()) (fun ~dispatched:_ -> assert false)));
           [%test_eq: int] calls_before_escape !native_calls;
           let state = A.state actor |> protocol_ok in
           let observed =
             List.find_exn state.invocations ~f:(fun i ->
               String.equal i.context.tool_name "seed")
           in
           let native =
             List.filter state.invocations ~f:(fun i ->
               String.equal i.context.tool_name "read_file")
           in
           assert (I.equal_status observed.status seed.status);
           assert (Option.is_none state.active_operation);
           assert (
             List.equal
               Agent_protocol.History.equal_entry
               initial.conversation.canonical_history
               state.conversation.canonical_history);
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           assert (Result.is_ok (A.change_moderator actor state.moderator));
           let count =
             match
               (M.identity_snapshot manager |> Result.ok_or_failwith).current_state
             with
             | Session.Snapshot.Array [ Int n ] -> n
             | _ -> assert false
           in
           print_s
             [%sexp
               { mode : [ `Success
                        | `Deny
                        | `Revoke
                        | `Reentrant
                        | `Handler_fail
                        | `Save_fail
                        | `Cancel_native
                        | `Forged_parent
                        ]
               ; completed : bool
               ; native_calls = (!native_calls : int)
               ; authorizations = (!authorizations : int)
               ; forged_rejected = (!forged_rejected : bool)
               ; observation =
                   ((Option.value_exn observed.observation).status : I.observation_status)
               ; native = (List.map native ~f:(fun i -> i.status) : I.status list)
               ; state_count = (count : int)
               }]));
  [%expect
    {|
    ((mode Success) (completed true) (native_calls 1) (authorizations 1)
     (forged_rejected false) (observation Observed)
     (native ((Resolved (Complete (String disclosed))))) (state_count 1))
    ((mode Deny) (completed true) (native_calls 0) (authorizations 1)
     (forged_rejected false) (observation Observed)
     (native
      ((Resolved
        (Fail
         ((code invocation.permission_denied)
          (message "Tool execution was not authorized.") (retryable false)
          (details Null))))))
     (state_count 1))
    ((mode Revoke) (completed true) (native_calls 0) (authorizations 1)
     (forged_rejected false) (observation Observed)
     (native
      ((Resolved
        (Fail
         ((code invocation.stale_binding)
          (message "The selected tool capability is no longer valid.")
          (retryable false) (details Null))))))
     (state_count 1))
    ((mode Reentrant) (completed true) (native_calls 0) (authorizations 0)
     (forged_rejected false) (observation Observed)
     (native
      ((Resolved
        (Fail
         ((code moderator_reentrancy)
          (message
           "Tool execution requires a decision from the active moderator.")
          (retryable false) (details Null))))))
     (state_count 1))
    ((mode Handler_fail) (completed false) (native_calls 1) (authorizations 1)
     (forged_rejected false)
     (observation
      (Observation_failed "observation handler failed before acknowledgement"))
     (native ((Resolved (Complete (String disclosed))))) (state_count 0))
    ((mode Save_fail) (completed false) (native_calls 1) (authorizations 1)
     (forged_rejected false)
     (observation
      (Observation_failed "observation handler failed before acknowledgement"))
     (native
      ((Resolved
        (Cancelled
         "idle moderator exited before recording the invocation outcome"))))
     (state_count 0))
    ((mode Cancel_native) (completed false) (native_calls 1) (authorizations 1)
     (forged_rejected false)
     (observation
      (Observation_failed "observation handler cancelled before acknowledgement"))
     (native ((Resolved (Cancelled "idle moderator cancelled"))))
     (state_count 0))
    ((mode Forged_parent) (completed true) (native_calls 1) (authorizations 1)
     (forged_rejected true) (observation Observed)
     (native ((Resolved (Complete (String disclosed))))) (state_count 1))
    |}]
;;

let%test_unit "compiled moderator Tool.call uses persisted scoped native routing" =
  let cases =
    [ `Success
    ; `Custom
    ; `Denied
    ; `Revoked
    ; `Replaced
    ; `Requires_moderator
    ; `Self
    ; `Unknown
    ; `Unselected
    ; `Observation_failed
    ; `Observation_raised
    ; `Invalid
    ; `Disclosure
    ; `Output_limit
    ; `Parent_failed
    ]
  in
  List.iter
    (List.cartesian_product [ false; true ] cases)
    ~f:(fun (observe_nested, mode) ->
      let calls = ref 0
      and authorized = ref 0
      and observations = ref []
      and legacy_calls = ref 0 in
      let custom =
        match mode with
        | `Custom -> true
        | _ -> false
      in
      let registry = ref (native_registry ~custom calls ~raises:false) in
      with_handoff_actor
        ~make_worker:(fun env actor_ready ->
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let selected =
              match mode with
              | `Unselected ->
                Chat_response.Tool_capability.select !registry ~names:[]
                |> Result.map_error ~f:(fun e -> e.Chat_response.Tool_capability.message)
                |> Result.ok_or_failwith
              | _ -> !registry
            in
            let name =
              match mode with
              | `Self -> "counter"
              | `Unknown -> "missing"
              | _ -> "read_file"
            in
            let argument =
              match mode with
              | `Invalid -> "`Null"
              | _ -> if custom then "`String(\"{}\")" else "`Object([])"
            in
            let resolve =
              "Task.bind(Tool.call(\""
              ^ name
              ^ "\", "
              ^ argument
              ^ "), fun result -> "
              ^ "match result with | `Ok(value) -> \
                 Invocation.resolve(p.context.invocation_id, `Complete(value)) "
              ^ "| `Error(code) -> Invocation.resolve(p.context.invocation_id, "
              ^ "`Fail({code = code; message = \"nested call failed\"; retryable = \
                 false; details = `Null})))"
            in
            let finish =
              match mode with
              | `Parent_failed -> "Task.fail(\"parent failed after child\")"
              | _ -> "Task.pure(state)"
            in
            let manager, _, definition =
              handoff_definition
                ~capability_registry:selected
                ~script_limits:{|max_value="256KiB"|}
                ~resolve
                ~finish
                ~events:
                  "| `Internal_event(x) -> Task.bind(Tool.call(\"legacy\", `Null), fun \
                   ignored -> Task.pure(state)) | _ -> Task.pure(state)"
                ~moderator_capabilities:
                  { Chat_response.Moderation.Capabilities.default with
                    on_tool_call =
                      (fun ~name ~args:_ ->
                        assert (String.equal name "legacy");
                        Int.incr legacy_calls;
                        Ok (Tool_ok `Null))
                  }
                env
            in
            let tools =
              Agent_session.Script_tool_calls.create
                ~registry:(fun () -> !registry)
                ~moderator_names:(String.Set.singleton "counter")
                ~now:Agent_protocol.Timestamp.now
                ~is_halted:(fun () ->
                  let state = Agent_session.Session_actor.state actor |> protocol_ok in
                  state.halted)
                ~requires_active_moderator:(fun _ ->
                  match mode with
                  | `Requires_moderator -> true
                  | _ -> false)
                ~authorize:(fun child _ ->
                  assert (
                    Agent_protocol.Invocation.equal_origin child.context.origin Moderator);
                  assert (Option.is_some child.context.parent_invocation);
                  Int.incr authorized;
                  Eio.Fiber.yield ();
                  match mode with
                  | `Denied -> Error (handoff_error "private denial")
                  | `Revoked ->
                    registry
                    := Chat_response.Tool_capability.select !registry ~names:[]
                       |> Result.map_error ~f:(fun e ->
                         e.Chat_response.Tool_capability.message)
                       |> Result.ok_or_failwith;
                    Ok ()
                  | `Replaced ->
                    registry := native_registry ~custom calls ~raises:false;
                    Ok ()
                  | _ -> Ok ())
                ~prepare_output:(fun _ ->
                  match mode with
                  | `Disclosure -> Error (handoff_error "private disclosure")
                  | `Output_limit -> Ok (`String (String.make (300 * 1024) 'x'))
                  | _ -> Ok (`String "disclosed child"))
                ~defer_observation:(fun child ->
                  let state = Agent_session.Session_actor.state actor |> protocol_ok in
                  assert (
                    List.mem
                      state.invocations
                      child
                      ~equal:Agent_protocol.Invocation.equal);
                  observations := child :: !observations;
                  (match child.observation with
                   | Some { status = Awaiting; observer } ->
                     let prepared =
                       List.hd_exn
                         (Chat_response.Extension_compiler.prepared_tools definition)
                     in
                     let script = Chat_response.Extension_compiler.script prepared in
                     [%test_eq: string] script.id observer.script_id;
                     [%test_eq: string] script.source_sha256 observer.source_sha256
                   | _ -> failwith "saved child lost its deferred observation intent");
                  match mode with
                  | `Observation_failed ->
                    Error (handoff_error "private observer failure")
                  | `Observation_raised -> failwith "private observer exception"
                  | _ -> Ok ())
            in
            let call_id = "nested-parent" in
            let id =
              History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
            in
            let call =
              History_entry.create_with_id
                ~id
                (Chat_response.Tool_call.call_item
                   ~kind:Function
                   ~name:"counter"
                   ~payload:"null"
                   ~call_id
                   ~id:None)
            in
            let dispatch =
              Agent_session.Moderator_tool_dispatch.create
                ~script_tools:tools
                ~observe_nested
                ~definition
                ~manager
                ~input
                ~capabilities:caps
                ~available_tools:[]
                ~session_meta:`Null
                ~now:Agent_protocol.Timestamp.now
                ~validate_work:(fun _ -> Error "pending disabled")
                ~admit:(fun _ -> Ok ())
                ~prepare_outcome:(fun _ -> Ok ())
                ()
            in
            let request =
              Chat_response.In_memory_stream.Tool_dispatch.
                { kind = Function
                ; original_name = "counter"
                ; original_payload = "null"
                ; name = "counter"
                ; payload = "null"
                ; rejection = None
                ; call
                ; history = input.history @ [ call ]
                ; source = None
                ; parent_call_id = None
                }
            in
            let result = dispatch.run request ~authorize:ignore |> Option.value_exn in
            let output_id =
              History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
            in
            let output =
              History_entry.create_with_id
                ~id:output_id
                (Chat_response.Tool_call.output_item
                   ~kind:Function
                   ~call_id
                   ~output:result.output)
            in
            (Option.value_exn result.commit_output) output;
            let snapshot =
              Chat_response.Moderator_manager.identity_snapshot manager
              |> Result.ok_or_failwith
            in
            (match snapshot.current_state with
             | Session.Snapshot.Array [ Int count ] ->
               assert (
                 count
                 =
                 match mode with
                 | `Parent_failed -> 0
                 | _ -> 1)
             | _ -> assert false);
            Chat_response.Moderator_manager.handle_event_entries
              manager
              ~session_id:(Agent_protocol.Id.Session.to_string input.session_id)
              ~now_ms:0
              ~history:(input.history @ [ call; output ])
              ~available_tools:[]
              ~session_meta:`Null
              ~event:
                (Internal_event
                   (Chatml.Chatml_lang.VVariant
                      ( "Internal_event"
                      , [ Chatml.Chatml_value_codec.jsonaf_to_value
                            (`String "check restored callback")
                        ] )))
            |> Result.ok_or_failwith
            |> ignore;
            caps.commit_moderator
              (Some
                 (Agent_session.Runtime_builder.encode_moderator_snapshot
                    (Chat_response.Moderator_manager.identity_snapshot manager
                     |> Result.ok_or_failwith)))
            |> protocol_ok;
            let state = Agent_session.Session_actor.state actor |> protocol_ok in
            Completed
              { final_history = input.history @ [ call; output ]
              ; runtime_requests = []
              ; moderator_snapshot = state.moderator
              }))
        (fun _env actor _writer backend ->
           let state = await_idle actor in
           let parent =
             List.find_exn state.invocations ~f:(fun i ->
               Option.is_none i.context.parent_invocation)
           in
           let children =
             List.filter state.invocations ~f:(fun i ->
               Option.is_some i.context.parent_invocation)
           in
           let expected =
             match mode with
             | `Success | `Custom -> None
             | `Denied -> Some "invocation.permission_denied"
             | `Revoked | `Replaced -> Some "invocation.stale_binding"
             | `Requires_moderator | `Self -> Some "moderator_reentrancy"
             | `Unknown | `Unselected -> Some "invocation.unselected_tool"
             | `Observation_failed | `Observation_raised ->
               Some "invocation.observation_failed"
             | `Invalid -> Some "invocation.invalid_input"
             | `Disclosure | `Output_limit -> Some "invocation.disclosure_rejected"
             | `Parent_failed -> Some "invocation.handler_failed"
           in
           (match parent.status, expected with
            | Published (Complete (`String "disclosed child")), None -> ()
            | Published (Fail error), Some code -> [%test_eq: string] code error.code
            | _ ->
              raise_s
                [%sexp
                  "unexpected parent outcome", (parent : Agent_protocol.Invocation.t)]);
           let has_child =
             match mode with
             | `Self | `Unknown | `Unselected -> false
             | _ -> true
           in
           assert (List.length children = if has_child then 1 else 0);
           assert (List.length !observations = List.length children);
           List.iter children ~f:(fun child ->
             (match observe_nested, child.observation with
              | false, Some { status = Awaiting; _ } | true, Some { status = Observed; _ }
                -> ()
              | _ -> failwith "saved child lost its deferred observation intent");
             assert (Option.is_none child.context.provider_call_id);
             assert (Option.is_none child.context.call_entry_id);
             assert (Option.is_none child.output_entry_id);
             assert (
               Option.equal
                 Agent_protocol.Id.Invocation.equal
                 child.context.parent_invocation
                 (Some parent.context.id));
             match mode, child.status with
             | ( ( `Denied
                 | `Revoked
                 | `Replaced
                 | `Requires_moderator
                 | `Invalid
                 | `Disclosure
                 | `Output_limit )
               , Resolved (Fail error) ) ->
               assert (Option.equal String.equal (Some error.code) expected)
             | ( ( `Success
                 | `Custom
                 | `Observation_failed
                 | `Observation_raised
                 | `Parent_failed )
               , Resolved (Complete (`String "disclosed child")) ) -> ()
             | _ -> assert false);
           let executed =
             match mode with
             | `Success
             | `Custom
             | `Observation_failed
             | `Observation_raised
             | `Disclosure
             | `Output_limit
             | `Parent_failed -> true
             | _ -> false
           in
           assert (!calls = if executed then 1 else 0);
           assert (
             !authorized
             =
             match mode with
             | `Self | `Unknown | `Unselected | `Requires_moderator | `Invalid -> 0
             | _ -> 1);
           assert (List.length state.conversation.canonical_history = 3);
           assert (!legacy_calls = 1);
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend)))
;;

let%test_unit "script call scopes bound attempts and reject escaped or closed parents" =
  let calls = ref 0
  and observations = ref 0 in
  let registry = native_registry calls ~raises:false in
  let expect_error code = function
    | Ok (Chat_response.Moderation.Capabilities.Tool_error actual) ->
      assert (String.equal code actual)
    | _ -> assert false
  in
  with_handoff_actor
    ~make_worker:(fun env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let _, make_parent, definition =
          handoff_definition
            ~capability_registry:registry
            ~script_limits:{|max_value="256KiB"|}
            env
        in
        let prepared =
          List.hd_exn (Chat_response.Extension_compiler.prepared_tools definition)
        in
        let parent = make_parent () in
        let tools =
          Agent_session.Script_tool_calls.create
            ~registry:(fun () -> registry)
            ~moderator_names:(String.Set.singleton "counter")
            ~now:Agent_protocol.Timestamp.now
            ~is_halted:(fun () -> false)
            ~requires_active_moderator:(fun _ -> false)
            ~authorize:(fun _ _ -> Ok ())
            ~prepare_output:(fun _ -> Ok (`String "done"))
            ~defer_observation:(fun _ ->
              Int.incr observations;
              Ok ())
        in
        caps.with_moderator_invocation ~invocation:parent (fun ~dispatched ~commit ->
          let scope f =
            Agent_session.Script_tool_calls.with_invocation
              tools
              ~prepared
              ~capabilities:caps
              ~parent:dispatched
              f
          in
          let escaped =
            scope (fun call ->
              for _ = 1 to 100 do
                call ~name:"missing" ~args:`Null
                |> expect_error "invocation.unselected_tool"
              done;
              call ~name:"read_file" ~args:(`Object [])
              |> expect_error "invocation.nested_call_limit";
              call)
          in
          escaped ~name:"read_file" ~args:(`Object [])
          |> expect_error "invocation.inactive_scope";
          assert (!calls = 0);
          scope (fun call ->
            call ~name:"read_file" ~args:(`String (String.make (300 * 1024) 'x'))
            |> expect_error "invocation.invalid_input";
            match call ~name:"read_file" ~args:(`Object []) with
            | Ok (Tool_ok (`String "done")) -> ()
            | _ -> assert false);
          let resolved =
            Agent_protocol.Invocation.resolve
              dispatched
              ~session_id:input.session_id
              ~generation:input.session_generation
              (Complete `Null)
            |> protocol_ok
          in
          commit ~resolved ~snapshot:(handoff_snapshot 1) |> protocol_ok;
          scope (fun call ->
            call ~name:"read_file" ~args:(`Object [])
            |> expect_error "invocation.admission_failed");
          Ok ())
        |> protocol_ok;
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor _writer backend ->
       let state = await_idle actor in
       assert (!calls = 1 && !observations = 1);
       assert (List.length state.invocations = 2);
       assert (List.length state.conversation.canonical_history = 1);
       assert_same_session_snapshot state (Agent_session.Memory_backend.state backend))
;;
