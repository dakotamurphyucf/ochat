open Core
open Fixtures

let%expect_test "ordinary moderator events own foreground calls and preserve queued work" =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  let module E = Agent_protocol.Moderator_execution in
  List.iter
    [ `Success; `Handler_failure; `Save_failure; `Terminal_save_failure; `Cancel ]
    ~f:(fun mode ->
      let calls = ref 0
      and rejected = ref 0
      and prepared = ref None in
      let on_native = ref (fun () -> ()) in
      let registry =
        native_registry calls ~raises:false ~on_call:(fun () -> !on_native ())
      in
      with_handoff_actor
        ~reject:(fun next ->
          let limit =
            match mode with
            | `Save_failure -> 1
            | `Terminal_save_failure -> 2
            | _ -> 0
          in
          if
            !rejected < limit
            && List.exists next.state.moderator_executions ~f:(fun event ->
              not (E.equal_status event.status Running))
          then (
            incr rejected;
            true)
          else false)
        ~make_worker:(fun env ready ->
          let finish =
            match mode with
            | `Handler_failure -> "Task.fail(\"after native effect\")"
            | _ -> "Task.bind(Runtime.request_turn(), fun ignored -> Task.pure(state))"
          in
          let manager, _, _ =
            handoff_definition
              env
              ~declare_tool:false
              ~capability_registry:registry
              ~moderator_capabilities:
                { Chat_response.Moderation.Capabilities.default with
                  on_tool_call = (fun ~name:_ ~args:_ -> failwith "unscoped event call")
                }
              ~events:
                ({| | `Session_start -> Task.bind(Runtime.emit(`String("existing")), fun ignored -> Task.pure(state))
            | `Turn_end -> Task.bind(Tool.call("read_file", `Object([])), fun result ->
                let ignored = state[0] <- state[0] + 1 in
                Task.bind(Runtime.emit(`String("ordinary")), fun ignored -> |}
                 ^ finish
                 ^ {|))
            | `Session_resume -> Task.bind(Tool.call("read_file", `Object([])), fun result ->
                let ignored = state[0] <- state[0] + 10 in Task.pure(state))
            | _ -> Task.pure(state) |}
                )
          in
          M.handle_event_entries_transactional
            manager
            ~session_id:"fixture"
            ~now_ms:0
            ~history:[]
            ~available_tools:[]
            ~session_meta:`Null
            ~event:Session_start
            ~authorize:(fun () -> Ok ())
            ~on_tool_call:(fun ~name:_ ~args:_ -> assert false)
            ~prepare_event:(fun ~outcome:_ ~snapshot:_ -> Ok (M.memory_commit ignore))
          |> Result.ok_or_failwith
          |> ignore;
          let initial_snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let actor = Eio.Promise.await ready in
            caps.commit_moderator
              (Some
                 (Agent_session.Runtime_builder.encode_moderator_snapshot
                    initial_snapshot))
            |> protocol_ok;
            (on_native
             := fun () ->
                  let state = A.state actor |> protocol_ok in
                  let invocation =
                    match Agent_session.Native_tool_invocation.current_scope () with
                    | Active invocation -> invocation
                    | _ -> assert false
                  in
                  let parent = Option.value_exn invocation.parent_event in
                  let event =
                    List.find_exn state.moderator_executions ~f:(fun event ->
                      Agent_protocol.Id.Moderator_execution.equal event.context.id parent)
                  in
                  assert (
                    Option.equal
                      Agent_protocol.Id.Operation.equal
                      event.context.operation_id
                      (Option.map state.active_operation ~f:(fun operation ->
                         operation.Agent_protocol.Operation.id)));
                  match mode with
                  | `Cancel ->
                    let writer = List.hd_exn state.attachments in
                    A.cancel_operation
                      actor
                      ~attachment_id:writer.id
                      ~operation_id:input.operation.id
                    |> protocol_ok
                    |> ignore;
                    Eio.Fiber.yield ()
                  | _ -> Eio.Fiber.yield ());
            let tools =
              Agent_session.Script_tool_calls.create
                ~registry:(fun () -> registry)
                ~moderator_names:String.Set.empty
                ~now:Agent_protocol.Timestamp.now
                ~is_halted:(fun () -> (A.state actor |> protocol_ok).halted)
                ~requires_active_moderator:(fun _ -> false)
                ~authorize:(fun _ _ -> Ok ())
                ~prepare_output:(fun _ -> Ok (`String "disclosed"))
                ~defer_observation:(fun _ -> Ok ())
            in
            let history () =
              (A.state actor |> protocol_ok).conversation.canonical_history
              |> Agent_session.History_codec.all_of_protocol
              |> protocol_ok
            in
            let run ~operation_id event =
              Agent_session.Moderator_event.run_ordinary
                ~event
                ~claim:(A.with_current_moderator_event actor ~operation_id ~event)
                ~script_tools:tools
                ~manager
                ~history
                ~available_tools:[]
                ~session_meta:`Null
                ~now:Agent_protocol.Timestamp.now
                ()
            in
            prepared := Some (manager, run);
            assert (Result.is_error (run ~operation_id:None Turn_end));
            assert (
              Result.is_error
                (run
                   ~operation_id:(Some (Agent_protocol.Id.Operation.create ()))
                   Turn_end));
            let run_worker () =
              Agent_session.Moderator_event.run_ordinary
                ~event:Turn_end
                ~claim:(caps.with_moderator_event ~event:Turn_end)
                ~script_tools:tools
                ~manager
                ~history
                ~available_tools:[]
                ~session_meta:`Null
                ~now:Agent_protocol.Timestamp.now
                ()
            in
            match run_worker () with
            | Ok (Some _) ->
              assert (Option.is_some (run_worker () |> protocol_ok));
              Completed
                { final_history = input.history
                ; runtime_requests = []
                ; moderator_snapshot =
                    Some
                      (Agent_session.Runtime_builder.encode_moderator_snapshot
                         (M.identity_snapshot manager |> Result.ok_or_failwith))
                }
            | Ok None -> assert false
            | Error failure ->
              let before_retry = !calls in
              (match run_worker () with
               | Error _ | Ok None -> ()
               | Ok (Some _) -> assert false);
              [%test_eq: int] before_retry !calls;
              Failed failure))
        (fun _env actor _writer backend ->
           let rec terminal () =
             let state = A.state actor |> protocol_ok in
             match state.active_operation with
             | None -> state
             | Some _ ->
               Eio.Fiber.yield ();
               terminal ()
           in
           let state = terminal () in
           let manager, run = Option.value_exn !prepared in
           (match mode with
            | `Success ->
              assert (Option.is_some (run ~operation_id:None Session_resume |> protocol_ok))
            | _ -> ());
           let final = A.state actor |> protocol_ok in
           let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
           assert_same_session_snapshot final (Agent_session.Memory_backend.state backend);
           [%test_eq: int] 1 (List.length final.conversation.canonical_history);
           assert (
             Option.equal
               Jsonaf.exactly_equal
               final.moderator
               (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)));
           assert (Result.is_ok (A.set_operation_worker actor None));
           let pending =
             List.count
               final.moderator_executions
               ~f:Agent_session.Observation_follow_up.pending_event
           in
           let completed =
             List.count final.moderator_executions ~f:(fun event ->
               match event.status with
               | Completed _ -> true
               | _ -> false)
           in
           let failed =
             List.count final.moderator_executions ~f:(fun event ->
               match event.status with
               | Failed _ | Interrupted _ -> true
               | _ -> false)
           in
           let count =
             match snapshot.current_state with
             | Session.Snapshot.Array [ Int count ] -> count
             | _ -> assert false
           in
           print_s
             [%sexp
               { mode : [ `Success
                        | `Handler_failure
                        | `Save_failure
                        | `Terminal_save_failure
                        | `Cancel
                        ]
               ; calls = (!calls : int)
               ; rejected = (!rejected : int)
               ; completed : int
               ; failed : int
               ; pending : int
               ; queued = (List.length snapshot.queued_internal_events : int)
               ; state = (count : int)
               ; operation_finished = (Option.is_none state.active_operation : bool)
               }]));
  [%expect
    {|
    ((mode Success) (calls 3) (rejected 0) (completed 3) (failed 0) (pending 2)
     (queued 3) (state 12) (operation_finished true))
    ((mode Handler_failure) (calls 1) (rejected 0) (completed 0) (failed 1)
     (pending 0) (queued 1) (state 0) (operation_finished true))
    ((mode Save_failure) (calls 1) (rejected 1) (completed 0) (failed 1)
     (pending 0) (queued 1) (state 0) (operation_finished true))
    ((mode Terminal_save_failure) (calls 1) (rejected 2) (completed 0) (failed 1)
     (pending 0) (queued 1) (state 0) (operation_finished true))
    ((mode Cancel) (calls 1) (rejected 0) (completed 0) (failed 1) (pending 0)
     (queued 1) (state 0) (operation_finished true))
    |}]
;;

let%expect_test "owned stream events consume requests at provider and terminal boundaries"
  =
  let module A = Agent_session.Session_actor in
  let module E = Agent_protocol.Moderator_execution in
  List.iter
    [ `Turns
    ; `Observed
    ; `Provider_tool
    ; `Queued
    ; `Compact
    ; `End
    ; `Early_end
    ; `Budget
    ; `Disabled
    ; `Reject
    ; `Stale
    ]
    ~f:(fun mode ->
      let requests = ref 0
      and calls = ref 0
      and rejected = ref false in
      let registry = native_registry calls ~raises:false in
      with_handoff_actor
        ~reject:(fun next ->
          match mode, !rejected with
          | `Reject, false
            when List.exists next.state.moderator_executions ~f:(fun event ->
                   Option.exists event.requests ~f:(fun requests -> requests.request_turn)
                   && Option.equal E.equal_intent event.intent (Some Applied)) ->
            rejected := true;
            true
          | _ -> false)
        ~make_worker:(fun env actor_ready ->
          let follow_up =
            match mode with
            | `Turns | `Reject ->
              "if state[0] < 3 then Task.bind(Runtime.request_turn(), fun ignored -> \
               Task.pure(state)) else Task.pure(state)"
            | `Budget | `Disabled ->
              "Task.bind(Runtime.request_turn(), fun ignored -> Task.pure(state))"
            | `Queued ->
              "if state[0] < 2 then Task.bind(Runtime.emit(`String(\"wake\")), fun \
               ignored -> Task.pure(state)) else Task.pure(state)"
            | `Compact ->
              "Task.bind(Runtime.request_compaction(), fun ignored -> \
               Task.bind(Runtime.request_turn(), fun ignored -> Task.pure(state)))"
            | `End ->
              "Task.bind(Runtime.end_session(\"done\"), fun ignored -> Task.pure(state))"
            | `Early_end | `Observed | `Provider_tool | `Stale -> "Task.pure(state)"
          in
          let early =
            match mode with
            | `Early_end ->
              " | `Item_appended(item) -> Task.bind(Runtime.end_session(\"early\"), fun \
               ignored -> Task.pure(state))"
            | `Observed ->
              " | `Tool_observed(event) -> if state[0] < 3 then \
               Task.bind(Runtime.request_turn(), fun ignored -> Task.pure(state)) else \
               Task.pure(state)"
            | _ -> ""
          in
          let manager, _, _ =
            handoff_definition
              env
              ~declare_tool:false
              ~capability_registry:registry
              ~moderator_capabilities:
                { Chat_response.Moderation.Capabilities.default with
                  on_tool_call =
                    (fun ~name:_ ~args:_ -> failwith "unowned stream tool call")
                }
              ~events:
                ("| `Turn_end -> Task.bind(Tool.call(\"read_file\", `Object([])), fun \
                  result -> match result with | `Error(code) -> Task.fail(code) | \
                  `Ok(value) -> let ignored = state[0] <- state[0] + 1 in "
                 ^ follow_up
                 ^ ") | `Internal_event(event) -> Task.bind(Runtime.request_turn(), fun \
                    ignored -> Task.pure(state))"
                 ^ early
                 ^ " | _ -> Task.pure(state)")
          in
          Agent_session.Operation_worker.create ~run:(fun ~sw ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let state = A.state actor |> protocol_ok in
            let response_dir =
              Eio.Path.(
                Eio.Stdenv.fs env
                / state.spec.workspace_instance.canonical_root.native_path
                / "response")
            in
            Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 response_dir;
            let installed =
              Chat_response.Moderator_manager.identity_snapshot manager
              |> Result.ok_or_failwith
            in
            let installed =
              match mode with
              | `Stale -> { installed with script_source_hash = String.make 64 'a' }
              | _ -> installed
            in
            caps.commit_moderator
              (Some (Agent_session.Runtime_builder.encode_moderator_snapshot installed))
            |> protocol_ok;
            let script_tools =
              Agent_session.Script_tool_calls.create
                ~registry:(fun () -> registry)
                ~moderator_names:String.Set.empty
                ~now:Agent_protocol.Timestamp.now
                ~is_halted:(fun () -> (A.state actor |> protocol_ok).halted)
                ~requires_active_moderator:(fun _ -> false)
                ~authorize:(fun invocation _ ->
                  let parent =
                    Option.value_exn invocation.Agent_protocol.Invocation.parent_event
                  in
                  let state = A.state actor |> protocol_ok in
                  let event =
                    List.find_exn state.moderator_executions ~f:(fun event ->
                      Agent_protocol.Id.Moderator_execution.equal event.context.id parent)
                  in
                  assert (
                    Option.exists
                      event.context.operation_id
                      ~f:(Agent_protocol.Id.Operation.equal input.operation.id));
                  Ok ())
                ~prepare_output:(fun _ -> Ok (`String "disclosed"))
                ~defer_observation:(fun _ -> Ok ())
            in
            let policy = Chat_response.Runtime_semantics.default_policy in
            let policy =
              { policy with
                honor_request_turn =
                  (match mode with
                   | `Disabled -> false
                   | _ -> true)
              ; budget = { policy.budget with max_self_triggered_turns = 2 }
              }
            in
            let worker =
              Agent_session.Turn_worker.create
                ~dispatch_tool:(fun ~input ~capabilities ->
                  Agent_session.Native_tool_dispatch.create
                    ~input
                    ~capabilities
                    ~declared:registry
                    ~registry:(fun () -> registry)
                    ~now:Agent_protocol.Timestamp.now
                    ~is_halted:(fun () -> (A.state actor |> protocol_ok).halted)
                    ~admit:(fun _ _ -> Ok ())
                    ~prepare_output:(fun _ -> Ok (`String "disclosed")))
                ~moderator_events:(fun ~input:_ ~capabilities ->
                  Agent_session.Moderator_event.foreground_handlers
                    ~script_tools
                    ~capabilities
                    ~manager
                    ~session_meta:`Null
                    ~now:Agent_protocol.Timestamp.now
                    ())
                { env
                ; response_dir
                ; tools = []
                ; tool_tbl = String.Table.create ()
                ; temperature = None
                ; max_output_tokens = None
                ; reasoning = None
                ; moderator =
                    Some
                      { manager
                      ; session_id = Agent_protocol.Id.Session.to_string input.session_id
                      ; session_meta = `Null
                      ; runtime_policy = policy
                      ; event_handlers = None
                      }
                ; permission_profile =
                    permission_policy
                      ~tool_default:Allow
                      ~fallback:Fallback_deny
                      ~evaluator:None
                      ~reviewer:None
                ; review_permission = (fun _ -> assert false)
                ; history_compaction = false
                ; parallel_tool_calls = true
                ; model = Openai.Responses.Request.O3
                ; prompt_cache_key = None
                ; prompt_cache_retention = None
                ; post_stream =
                    Some
                      (fun ~sw:_ ~inputs:_ ->
                        incr requests;
                        let current = A.state actor |> protocol_ok in
                        List.iter current.moderator_executions ~f:(fun event ->
                          match event.requests with
                          | Some
                              { request_turn = true
                              ; request_compaction = false
                              ; end_session = None
                              } ->
                            assert (
                              Option.equal E.equal_intent event.intent (Some Applied))
                          | _ -> ());
                        List.iter current.invocations ~f:(fun invocation ->
                          match invocation.observation with
                          | Some
                              { follow_up =
                                  Some
                                    (Pending_follow_up
                                       { request_turn = true
                                       ; request_compaction = false
                                       ; end_session = None
                                       })
                              ; _
                              } ->
                            failwith
                              "provider dispatched before observation request \
                               acknowledgement"
                          | _ -> ());
                        match mode, !requests with
                        | `Provider_tool, 1 ->
                          let open Openai.Responses.Response_stream in
                          Stdlib.List.to_seq
                            [ Output_item_added
                                { item =
                                    Function_call
                                      { name = "read_file"
                                      ; arguments = ""
                                      ; call_id = "owned-call"
                                      ; _type = "function_call"
                                      ; id = Some "owned-item"
                                      ; status = None
                                      }
                                ; output_index = 0
                                ; type_ = "response.output_item.added"
                                }
                            ; Function_call_arguments_done
                                { arguments = "{}"
                                ; item_id = "owned-item"
                                ; output_index = 0
                                ; type_ = "response.function_call_arguments.done"
                                }
                            ]
                        | _ -> Stdlib.Seq.empty)
                ; agent_page_classifications = []
                ; delegated_permission_tools = String.Set.empty
                ; redact_tool_payload = (fun ~name:_ value -> value)
                }
            in
            Agent_session.Operation_worker.run worker ~sw ~input caps))
        (fun _env actor _writer backend ->
           let rec finished () =
             let state = A.state actor |> protocol_ok in
             match state.active_operation with
             | None -> state
             | Some _ ->
               Eio.Fiber.yield ();
               finished ()
           in
           let state = finished () in
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           (match mode with
            | `Stale ->
              let installed =
                Agent_session.Runtime_builder.moderator_snapshot_observer state.moderator
                |> protocol_ok
                |> Option.value_exn
              in
              [%test_eq: string] (String.make 64 'a') installed.source_sha256
            | `Provider_tool ->
              List.iter [ E.Pre_tool_call; E.Post_tool_response ] ~f:(fun phase ->
                assert (
                  List.exists state.moderator_executions ~f:(fun event ->
                    E.equal_phase event.context.phase phase
                    &&
                    match event.status with
                    | Completed _ -> true
                    | _ -> false)));
              assert (
                List.exists state.invocations ~f:(fun invocation ->
                  Option.is_some invocation.output_entry_id))
            | _ -> ());
           let applied, discarded, pending =
             List.fold
               state.moderator_executions
               ~init:(0, 0, 0)
               ~f:(fun (a, d, p) event ->
                 match event.intent with
                 | Some Applied -> a + 1, d, p
                 | Some (Discarded _) -> a, d + 1, p
                 | Some (Pending | Waiting_compaction _) -> a, d, p + 1
                 | None -> a, d, p)
           in
           (match mode with
            | `Compact ->
              let observer =
                Agent_session.Runtime_builder.moderator_snapshot_observer state.moderator
                |> protocol_ok
              in
              let plan =
                Agent_session.Observation_follow_up.plan
                  ~state
                  ~observer
                  ~halted:false
                  ~compaction_operation_id:(Agent_protocol.Id.Operation.create ())
                |> protocol_ok
              in
              (match plan.action with
               | Compact -> ()
               | _ -> assert false);
              assert (
                List.exists plan.events ~f:(fun event ->
                  match event.intent with
                  | Some (Waiting_compaction _) -> true
                  | _ -> false))
            | _ -> assert (not (A.apply_moderator_follow_up actor |> protocol_ok)));
           print_s
             [%sexp
               { mode : [ `Turns
                        | `Observed
                        | `Provider_tool
                        | `Queued
                        | `Compact
                        | `End
                        | `Early_end
                        | `Budget
                        | `Disabled
                        | `Reject
                        | `Stale
                        ]
               ; provider_calls = (!requests : int)
               ; native_calls = (!calls : int)
               ; rejected = (!rejected : bool)
               ; applied : int
               ; discarded : int
               ; pending : int
               ; observed_turns =
                   (List.count state.invocations ~f:(fun invocation ->
                      match invocation.observation with
                      | Some { follow_up = Some (Applied_follow_up _); _ } -> true
                      | _ -> false)
                    : int)
               ; halted = (state.halted : bool)
               }]));
  [%expect
    {|
    ((mode Turns) (provider_calls 3) (native_calls 3) (rejected false)
     (applied 2) (discarded 0) (pending 0) (observed_turns 0) (halted false))
    ((mode Observed) (provider_calls 3) (native_calls 3) (rejected false)
     (applied 0) (discarded 0) (pending 0) (observed_turns 2) (halted false))
    ((mode Provider_tool) (provider_calls 2) (native_calls 3) (rejected false)
     (applied 0) (discarded 0) (pending 0) (observed_turns 0) (halted false))
    ((mode Queued) (provider_calls 2) (native_calls 2) (rejected false)
     (applied 1) (discarded 0) (pending 0) (observed_turns 0) (halted false))
    ((mode Compact) (provider_calls 1) (native_calls 1) (rejected false)
     (applied 0) (discarded 0) (pending 1) (observed_turns 0) (halted false))
    ((mode End) (provider_calls 1) (native_calls 1) (rejected false) (applied 1)
     (discarded 0) (pending 0) (observed_turns 0) (halted true))
    ((mode Early_end) (provider_calls 0) (native_calls 0) (rejected false)
     (applied 1) (discarded 0) (pending 0) (observed_turns 0) (halted true))
    ((mode Budget) (provider_calls 3) (native_calls 3) (rejected false)
     (applied 2) (discarded 1) (pending 0) (observed_turns 0) (halted false))
    ((mode Disabled) (provider_calls 1) (native_calls 1) (rejected false)
     (applied 0) (discarded 1) (pending 0) (observed_turns 0) (halted false))
    ((mode Reject) (provider_calls 1) (native_calls 1) (rejected true)
     (applied 0) (discarded 1) (pending 0) (observed_turns 0) (halted false))
    ((mode Stale) (provider_calls 0) (native_calls 0) (rejected false)
     (applied 0) (discarded 0) (pending 0) (observed_turns 0) (halted false))
    |}]
;;

let%expect_test
    "owned lifecycle activation is serialized and failed effects are not replayed"
  =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  let module L = Agent_session.Moderator_event.Lifecycle in
  let module E = Agent_protocol.Moderator_execution in
  List.iter
    [ `Success
    ; `Stopped
    ; `Concurrent
    ; `Reentrant
    ; `Handler_failure
    ; `Save_failure
    ; `Cancel
    ]
    ~f:(fun mode ->
      let calls = ref 0
      and rejected = ref false
      and reentrant = ref false in
      let on_native = ref (fun () -> ())
      and manager_ref = ref None in
      let registry =
        native_registry calls ~raises:false ~on_call:(fun () -> !on_native ())
      in
      with_handoff_actor
        ~reject:(fun next ->
          match mode, !rejected with
          | `Save_failure, false
            when List.exists next.state.moderator_executions ~f:(fun event ->
                   match event.status with
                   | Completed _ -> true
                   | _ -> false) ->
            rejected := true;
            true
          | _ -> false)
        ~make_worker:(fun env _ ->
          let body =
            "Task.bind(Tool.call(\"read_file\", `Object([])), fun result -> match result \
             with | `Error(code) -> Task.fail(code) | `Ok(value) -> let ignored = \
             state[0] <- state[0] + 1 in "
            ^
            match mode with
            | `Handler_failure -> "Task.fail(\"after effect\"))"
            | _ -> "Task.pure(state))"
          in
          let manager, _, _ =
            handoff_definition
              env
              ~declare_tool:false
              ~capability_registry:registry
              ~moderator_capabilities:
                { Chat_response.Moderation.Capabilities.default with
                  on_tool_call =
                    (fun ~name:_ ~args:_ -> failwith "unowned lifecycle tool")
                }
              ~events:
                ("| `Session_start -> "
                 ^ body
                 ^ " | `Session_resume -> "
                 ^ body
                 ^ " | _ -> Task.pure(state)")
          in
          manager_ref := Some manager;
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let snapshot =
              M.identity_snapshot manager
              |> Result.ok_or_failwith
              |> Agent_session.Runtime_builder.encode_moderator_snapshot
              |> Option.some
            in
            caps.commit_moderator snapshot |> protocol_ok;
            Completed
              { final_history = input.history
              ; runtime_requests = []
              ; moderator_snapshot = snapshot
              }))
        (fun _env actor writer backend ->
           ignore (await_idle actor : Agent_session.Session_state.t);
           let manager = Option.value_exn !manager_ref in
           let tools =
             Agent_session.Script_tool_calls.create
               ~registry:(fun () -> registry)
               ~moderator_names:String.Set.empty
               ~now:Agent_protocol.Timestamp.now
               ~is_halted:(fun () -> (A.state actor |> protocol_ok).halted)
               ~requires_active_moderator:(fun _ -> false)
               ~authorize:(fun _ _ -> Ok ())
               ~prepare_output:(fun _ -> Ok (`String "disclosed"))
               ~defer_observation:(fun _ -> Ok ())
           in
           let lifecycle = L.create ~manager ~resume:false in
           let run lifecycle =
             L.run
               lifecycle
               ~claim:(fun ~event ->
                 A.with_current_moderator_event actor ~operation_id:None ~event)
               ~script_tools:tools
               ~history:(fun () ->
                 (A.state actor |> protocol_ok).conversation.canonical_history
                 |> Agent_session.History_codec.all_of_protocol
                 |> protocol_ok)
               ~available_tools:[]
               ~session_meta:`Null
               ~now:Agent_protocol.Timestamp.now
               ()
           in
           (on_native
            := fun () ->
                 Eio.Fiber.yield ();
                 match mode with
                 | `Reentrant -> reentrant := Result.is_error (run lifecycle)
                 | `Cancel ->
                   A.stop actor ~attachment_id:writer.id ~mode:Cancel
                   |> protocol_ok
                   |> ignore;
                   Eio.Fiber.yield ()
                 | _ -> ());
           (match mode with
            | `Stopped ->
              A.stop actor ~attachment_id:writer.id ~mode:Graceful
              |> protocol_ok
              |> ignore;
              (match run lifecycle |> protocol_ok with
               | Unavailable -> ()
               | _ -> assert false);
              [%test_eq: int] 0 !calls;
              A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore
            | _ -> ());
           let describe result =
             match result with
             | Ok L.Unavailable -> "unavailable"
             | Ok (Activated _) -> "activated"
             | Ok Already_active -> "already_active"
             | Error _ -> "failed"
           in
           let attempt () =
             try describe (run lifecycle) with
             | Eio.Cancel.Cancelled _ -> "cancelled"
           in
           let outcomes =
             match mode with
             | `Concurrent ->
               let a, b = Eio.Fiber.pair attempt attempt in
               [ a; b ] |> List.sort ~compare:String.compare
             | _ -> [ attempt () ]
           in
           let retried = describe (run lifecycle) in
           (match mode with
            | `Cancel -> A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore
            | _ -> ());
           let rebuilt =
             match mode with
             | `Handler_failure | `Save_failure | `Cancel ->
               let resumed = L.create ~manager ~resume:true in
               Some (describe (run resumed))
             | _ -> None
           in
           let state = A.state actor |> protocol_ok in
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
           let count =
             match snapshot.current_state with
             | Session.Snapshot.Array [ Int n ] -> n
             | _ -> assert false
           in
           print_s
             [%sexp
               { mode : [ `Success
                        | `Stopped
                        | `Concurrent
                        | `Reentrant
                        | `Handler_failure
                        | `Save_failure
                        | `Cancel
                        ]
               ; outcomes : string list
               ; retried : string
               ; rebuilt : string option
               ; native_calls = (!calls : int)
               ; rejected = (!rejected : bool)
               ; reentrant = (!reentrant : bool)
               ; receipts = (List.length state.moderator_executions : int)
               ; state = (count : int)
               }]));
  [%expect
    {|
    ((mode Success) (outcomes (activated)) (retried already_active) (rebuilt ())
     (native_calls 1) (rejected false) (reentrant false) (receipts 1) (state 1))
    ((mode Stopped) (outcomes (activated)) (retried already_active) (rebuilt ())
     (native_calls 1) (rejected false) (reentrant false) (receipts 1) (state 1))
    ((mode Concurrent) (outcomes (activated already_active))
     (retried already_active) (rebuilt ()) (native_calls 1) (rejected false)
     (reentrant false) (receipts 1) (state 1))
    ((mode Reentrant) (outcomes (activated)) (retried already_active)
     (rebuilt ()) (native_calls 1) (rejected false) (reentrant true) (receipts 1)
     (state 1))
    ((mode Handler_failure) (outcomes (failed)) (retried failed)
     (rebuilt (failed)) (native_calls 1) (rejected false) (reentrant false)
     (receipts 1) (state 0))
    ((mode Save_failure) (outcomes (failed)) (retried failed) (rebuilt (failed))
     (native_calls 1) (rejected true) (reentrant false) (receipts 1) (state 0))
    ((mode Cancel) (outcomes (cancelled)) (retried failed) (rebuilt (failed))
     (native_calls 1) (rejected false) (reentrant false) (receipts 1) (state 0))
    |}]
;;
