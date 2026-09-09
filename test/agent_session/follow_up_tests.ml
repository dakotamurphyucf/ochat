open Core
open Fixtures

let%expect_test
    "follow-up scheduling survives save failure and reload without repeating compaction"
  =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let module E = Agent_protocol.Moderator_execution in
  List.iter
    [ `Continue; `Reload; `Stop; `End; `Obsolete; `Cancel; `Failure; `Interrupted ]
    ~f:(fun mode ->
      with_actor_workspace (fun env workspace_instance ->
        Eio.Switch.run (fun sw ->
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
            let source = String.make 64 'a' in
            let observer : I.observer =
              { script_id = "handoff"; source_sha256 = source }
            in
            let parent = invocation_fixture () |> I.dispatch |> protocol_ok in
            let parent =
              I.resolve parent ~session_id ~generation:0 (Complete `Null) |> protocol_ok
            in
            let child id follow_up =
              I.create
                ~observer
                { parent.context with
                  id = Agent_protocol.Id.Invocation.of_string id |> protocol_ok
                ; origin = Moderator
                ; parent_invocation = Some parent.context.id
                }
              |> protocol_ok
              |> I.dispatch
              |> protocol_ok
              |> fun child ->
              I.resolve child ~session_id ~generation:0 (Complete (`String "native"))
              |> protocol_ok
              |> I.claim_observation
              |> protocol_ok
              |> I.complete_observation ~follow_up
              |> protocol_ok
            in
            let initial =
              actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
            in
            let event ?(source = observer) id requests =
              E.create
                { id = Agent_protocol.Id.Moderator_execution.of_string id |> protocol_ok
                ; session_id
                ; generation = 0
                ; source
                ; operation_id = None
                ; job = None
                ; phase = Internal_event
                ; event = `Null
                ; checkpoint_sha256 = String.make 64 'b'
                ; created_at = timestamp
                }
              |> protocol_ok
              |> E.complete ~checkpoint_sha256:(String.make 64 'c') ~requests
              |> protocol_ok
            in
            let snapshot =
              { (handoff_snapshot 1) with
                script_source_hash = source
              ; halted =
                  (match mode with
                   | `End -> true
                   | _ -> false)
              ; halted_reason =
                  (match mode with
                   | `End -> Some "done"
                   | _ -> None)
              }
            in
            let initial =
              { initial with
                lifecycle = { desired = Running; observed = Idle }
              ; moderator =
                  Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)
              ; moderator_executions =
                  ([ event
                       "mex_follow_both"
                       { request_turn = true
                       ; request_compaction = true
                       ; end_session = None
                       }
                   ; event
                       "mex_follow_turn"
                       { request_turn = true
                       ; request_compaction = false
                       ; end_session = None
                       }
                   ; event
                       ~source:{ observer with source_sha256 = String.make 64 'd' }
                       "mex_follow_obsolete"
                       { request_turn = false
                       ; request_compaction = false
                       ; end_session = Some "obsolete stop"
                       }
                   ]
                   @
                   match mode with
                   | `End ->
                     [ event
                         "mex_follow_end"
                         { request_turn = false
                         ; request_compaction = false
                         ; end_session = Some "done"
                         }
                     ]
                   | _ -> [])
              ; invocations =
                  ((parent
                    :: [ child
                           "inv_follow_both"
                           { request_turn = true
                           ; request_compaction = true
                           ; end_session = None
                           }
                       ; child
                           "inv_follow_turn"
                           { request_turn = true
                           ; request_compaction = false
                           ; end_session = None
                           }
                       ])
                   @
                   match mode with
                   | `End ->
                     [ child
                         "inv_follow_end"
                         { request_turn = false
                         ; request_compaction = false
                         ; end_session = Some "done"
                         }
                     ]
                   | _ -> [])
              }
            in
            let initial =
              match mode with
              | `Obsolete ->
                { initial with identity = { initial.identity with generation = 1 } }
              | `Cancel | `Failure | `Interrupted ->
                let other =
                  child
                    "inv_follow_other"
                    { request_turn = true; request_compaction = true; end_session = None }
                  |> I.accept_observation_compaction
                       ~operation_id:
                         (Agent_protocol.Id.Operation.of_string "op_previous_compaction"
                          |> protocol_ok)
                  |> protocol_ok
                in
                let other_event =
                  event
                    "mex_follow_other"
                    { request_turn = true; request_compaction = true; end_session = None }
                  |> E.accept_compaction
                       ~operation_id:
                         (Agent_protocol.Id.Operation.of_string "op_previous_compaction"
                          |> protocol_ok)
                  |> protocol_ok
                in
                { initial with
                  invocations = other :: initial.invocations
                ; moderator_executions = other_event :: initial.moderator_executions
                }
              | _ -> initial
            in
            let restore (state : Agent_session.Session_state.t) =
              let state =
                { state with
                  Agent_session.Session_state.invocations =
                    List.map state.invocations ~f:(fun invocation ->
                      I.of_json (I.to_json invocation) |> protocol_ok)
                }
              in
              Agent_session.Session_persistence.restore_snapshot
                (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
              |> store_ok
            in
            let starts = ref []
            and model_runs = ref 0
            and reject = ref true in
            let reject_completion =
              ref
                (match mode with
                 | `Failure -> true
                 | _ -> false)
            in
            let captured_compaction = ref None in
            let cancel_ready, cancel_ready_u = Eio.Promise.create () in
            let create (initial : Agent_session.Session_state.t) =
              let backend =
                Agent_session.Memory_backend.create
                  ~event_capacity:128
                  ~initial_state:initial
              in
              let persistence = Agent_session.Memory_backend.persistence backend in
              let actor =
                A.create
                  ~sw
                  ~clock:(Eio.Stdenv.clock env)
                  ~mailbox_capacity:32
                  ~compaction_env:None
                  ~initial_state:initial
                  ~persistence:
                    { commit =
                        (fun ~command_audit ~previous next ->
                          if !reject
                          then (
                            reject := false;
                            Error (handoff_error "injected follow-up save failure"))
                          else if
                            !reject_completion
                            && Option.is_none
                                 next.Agent_session.Session_transition.state
                                   .active_operation
                            &&
                            match previous.active_operation with
                            | Some { kind = Compaction; _ } -> true
                            | _ -> false
                          then (
                            reject_completion := false;
                            Error (handoff_error "injected compaction checkpoint failure"))
                          else persistence.commit ~command_audit ~previous next)
                    }
                  ~operation_worker:
                    (Some
                       (Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
                          Int.incr model_runs;
                          Completed
                            { final_history = input.history
                            ; moderator_snapshot = initial.moderator
                            ; runtime_requests = []
                            })))
                  ~services:
                    { now = Agent_protocol.Timestamp.now
                    ; create_attachment_id = Agent_protocol.Id.Attachment.create
                    ; create_reclaim_token = (fun () -> "follow-up-test")
                    ; state_committed =
                        (fun committed events ->
                          List.iter events ~f:(fun event ->
                            match
                              Agent_protocol.Event.Durable.Payload.of_json
                                ~kind:event.kind
                                event.payload
                              |> protocol_ok
                            with
                            | Operation_started operation ->
                              starts := !starts @ [ operation.kind ];
                              (match operation.kind with
                               | Compaction ->
                                 captured_compaction := Some committed;
                                 let bound =
                                   List.find_exn
                                     committed.invocations
                                     ~f:(fun invocation ->
                                       String.equal
                                         (Agent_protocol.Id.Invocation.to_string
                                            invocation.context.id)
                                         "inv_follow_both")
                                 in
                                 assert (
                                   Option.equal
                                     Agent_protocol.Id.Operation.equal
                                     (Option.value_exn bound.observation)
                                       .compaction_operation_id
                                     (Some operation.id));
                                 let bound_event =
                                   List.find_exn
                                     committed.moderator_executions
                                     ~f:(fun event ->
                                       String.equal
                                         (Agent_protocol.Id.Moderator_execution.to_string
                                            event.context.id)
                                         "mex_follow_both")
                                 in
                                 assert (
                                   Option.equal
                                     Agent_protocol.Id.Operation.equal
                                     bound_event.compaction_operation_id
                                     (Some operation.id));
                                 (match mode with
                                  | `Cancel ->
                                    Eio.Promise.resolve cancel_ready_u operation.id;
                                    Eio.Fiber.yield ()
                                  | _ -> ())
                               | _ -> ())
                            | _ -> ()))
                    }
              in
              actor, backend
            in
            let actor, backend = create (restore initial) in
            (match mode with
             | `Cancel ->
               reject := false;
               let writer, _ =
                 A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
               in
               reject := true;
               Eio.Fiber.fork ~sw (fun () ->
                 let operation_id = Eio.Promise.await cancel_ready in
                 A.cancel_operation actor ~attachment_id:writer.id ~operation_id
                 |> protocol_ok
                 |> ignore)
             | _ -> ());
            let before = A.state actor |> protocol_ok in
            assert (Result.is_error (A.apply_moderator_follow_up actor));
            assert_same_session_snapshot before (A.state actor |> protocol_ok);
            assert (List.is_empty !starts && !model_runs = 0);
            assert (A.apply_observation_follow_up actor |> protocol_ok);
            let actor, backend =
              match mode with
              | `End -> actor, backend
              | _ ->
                let compacted = await_idle actor in
                (match mode with
                 | `Reload ->
                   A.shutdown actor;
                   let restored = restore compacted in
                   let recovery =
                     Agent_session.Invocation_recovery.plan
                       ~state:restored
                       ~namespace:"follow-up-reload"
                       ~first_sequence:
                         (Int64.to_int_exn restored.conversation.reserved_history_through)
                       ~reason:"reload after completed compaction"
                     |> protocol_ok
                   in
                   assert (List.is_empty recovery.deltas);
                   create restored
                 | `Interrupted ->
                   A.shutdown actor;
                   let captured = restore (Option.value_exn !captured_compaction) in
                   let first_sequence =
                     Int64.to_int_exn
                       (Int64.max
                          captured.conversation.next_history_sequence
                          captured.conversation.reserved_history_through)
                   in
                   let recover state =
                     Agent_session.Invocation_recovery.plan
                       ~state
                       ~first_sequence
                       ~namespace:(Agent_protocol.Id.Session.to_string session_id)
                       ~reason:"simulated restart"
                     |> protocol_ok
                   in
                   let recovered =
                     List.fold
                       (recover captured).deltas
                       ~init:captured
                       ~f:(fun state delta ->
                         Agent_session.Session_delta.apply state delta |> protocol_ok)
                   in
                   let recovered =
                     { recovered with
                       active_operation = None
                     ; lifecycle = { desired = Running; observed = Idle }
                     }
                   in
                   assert (List.is_empty (recover recovered).deltas);
                   create (restore recovered)
                 | _ -> actor, backend)
            in
            (match mode with
             | `Stop ->
               let writer, _ =
                 A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
               in
               A.stop actor ~attachment_id:writer.id ~mode:Cancel |> protocol_ok |> ignore;
               A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore
             | `Continue | `Reload | `Cancel | `Failure | `Interrupted ->
               (* Compaction acceptance retains and coalesces the two requested turns. *)
               assert (A.apply_observation_follow_up actor |> protocol_ok);
               ignore (await_idle actor : Agent_session.Session_state.t)
             | `End | `Obsolete -> ());
            assert (not (A.apply_observation_follow_up actor |> protocol_ok));
            let state = A.state actor |> protocol_ok in
            [%test_eq: int]
              (match mode with
               | `Continue | `Reload | `Stop -> 1
               | _ -> 0)
              state.conversation.compaction_generation;
            assert_same_session_snapshot
              state
              (Agent_session.Memory_backend.state backend);
            let receipts =
              List.filter_map state.invocations ~f:(fun invocation ->
                Option.bind invocation.observation ~f:(fun observation ->
                  Option.map observation.follow_up ~f:(fun receipt ->
                    assert (
                      I.equal_status
                        invocation.status
                        (Resolved (Complete (`String "native"))));
                    Agent_protocol.Id.Invocation.to_string invocation.context.id, receipt)))
              |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
            in
            let events =
              List.map state.moderator_executions ~f:(fun event ->
                assert (E.equal_status event.status (Completed (String.make 64 'c')));
                ( Agent_protocol.Id.Moderator_execution.to_string event.context.id
                , (Agent_protocol.Extension_status.moderator_execution event).state ))
              |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
            in
            print_s
              [%sexp
                { mode =
                    ((match mode with
                      | `Continue -> "continue"
                      | `Reload -> "reload"
                      | `Stop -> "stop then restart"
                      | `End -> "end overrides work"
                      | `Obsolete -> "old generation"
                      | `Cancel -> "compaction cancelled"
                      | `Failure -> "compaction checkpoint failed"
                      | `Interrupted -> "compaction interrupted")
                     : string)
                ; starts = (!starts : Agent_protocol.Operation.kind list)
                ; model_runs = (!model_runs : int)
                ; receipts : (string * I.follow_up_status) list
                ; events : (string * string) list
                }];
            A.shutdown actor))));
  [%expect
    {|
    ((mode continue) (starts (Compaction (Turn Moderator_request)))
     (model_runs 1)
     (receipts
      ((inv_follow_both
        (Applied_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))))
       (inv_follow_turn
        (Applied_follow_up
         ((request_turn true) (request_compaction false) (end_session ()))))))
     (events
      ((mex_follow_both completed.applied)
       (mex_follow_obsolete completed.discarded)
       (mex_follow_turn completed.applied))))
    ((mode reload) (starts (Compaction (Turn Moderator_request))) (model_runs 1)
     (receipts
      ((inv_follow_both
        (Applied_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))))
       (inv_follow_turn
        (Applied_follow_up
         ((request_turn true) (request_compaction false) (end_session ()))))))
     (events
      ((mex_follow_both completed.applied)
       (mex_follow_obsolete completed.discarded)
       (mex_follow_turn completed.applied))))
    ((mode "stop then restart") (starts (Compaction)) (model_runs 0)
     (receipts
      ((inv_follow_both
        (Discarded_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))
         "session stopped"))
       (inv_follow_turn
        (Discarded_follow_up
         ((request_turn true) (request_compaction false) (end_session ()))
         "session stopped"))))
     (events
      ((mex_follow_both completed.discarded)
       (mex_follow_obsolete completed.discarded)
       (mex_follow_turn completed.discarded))))
    ((mode "end overrides work") (starts ()) (model_runs 0)
     (receipts
      ((inv_follow_both
        (Discarded_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))
         "moderator ended session"))
       (inv_follow_end
        (Applied_follow_up
         ((request_turn false) (request_compaction false) (end_session (done)))))
       (inv_follow_turn
        (Discarded_follow_up
         ((request_turn true) (request_compaction false) (end_session ()))
         "moderator ended session"))))
     (events
      ((mex_follow_both completed.discarded) (mex_follow_end completed.applied)
       (mex_follow_obsolete completed.discarded)
       (mex_follow_turn completed.discarded))))
    ((mode "old generation") (starts ()) (model_runs 0)
     (receipts
      ((inv_follow_both
        (Discarded_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))
         "observation owner is no longer installed"))
       (inv_follow_turn
        (Discarded_follow_up
         ((request_turn true) (request_compaction false) (end_session ()))
         "observation owner is no longer installed"))))
     (events
      ((mex_follow_both completed.discarded)
       (mex_follow_obsolete completed.discarded)
       (mex_follow_turn completed.discarded))))
    ((mode "compaction cancelled") (starts (Compaction (Turn Moderator_request)))
     (model_runs 1)
     (receipts
      ((inv_follow_both
        (Discarded_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))
         "compaction cancelled"))
       (inv_follow_other
        (Applied_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))))
       (inv_follow_turn
        (Applied_follow_up
         ((request_turn true) (request_compaction false) (end_session ()))))))
     (events
      ((mex_follow_both completed.discarded)
       (mex_follow_obsolete completed.discarded)
       (mex_follow_other completed.applied) (mex_follow_turn completed.applied))))
    ((mode "compaction checkpoint failed")
     (starts (Compaction (Turn Moderator_request))) (model_runs 1)
     (receipts
      ((inv_follow_both
        (Discarded_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))
         "compaction failed"))
       (inv_follow_other
        (Applied_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))))
       (inv_follow_turn
        (Applied_follow_up
         ((request_turn true) (request_compaction false) (end_session ()))))))
     (events
      ((mex_follow_both completed.discarded)
       (mex_follow_obsolete completed.discarded)
       (mex_follow_other completed.applied) (mex_follow_turn completed.applied))))
    ((mode "compaction interrupted")
     (starts (Compaction (Turn Moderator_request))) (model_runs 1)
     (receipts
      ((inv_follow_both
        (Discarded_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))
         "compaction interrupted before durable completion"))
       (inv_follow_other
        (Applied_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))))
       (inv_follow_turn
        (Applied_follow_up
         ((request_turn true) (request_compaction false) (end_session ()))))))
     (events
      ((mex_follow_both completed.discarded)
       (mex_follow_obsolete completed.discarded)
       (mex_follow_other completed.applied) (mex_follow_turn completed.applied))))
    |}]
;;

let%expect_test "runtime owner drains observation batches and applies durable termination"
  =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let module M = Chat_response.Moderator_manager in
  let module B = Agent_session.Runtime_builder in
  List.iter [ false; true ] ~f:(fun tool_calls ->
    let prepared = ref None in
    let native_calls = ref 0
    and nested_calls = ref 0 in
    let registry = native_registry nested_calls ~raises:false in
    with_handoff_actor
      ~make_worker:(fun env _ ->
        let call =
          {|match p.tool_name with
            | "seed" ->
              Task.bind(Tool.call("read_file", `Object([])), fun ignored ->
              Task.bind(Tool.call("read_file", `Object([])), fun ignored ->
              Task.bind(Tool.call("read_file", `Object([])), fun ignored ->
              Task.bind(Tool.call("read_file", `Object([])), fun ignored -> Task.pure(state)))))
            | _ -> Task.pure(state)|}
        in
        let total =
          match tool_calls with
          | false -> 35
          | true -> 175
        in
        let manager, _, _ =
          handoff_definition
            env
            ~capability_registry:registry
            ~declare_tool:false
            ~moderator_capabilities:
              { Chat_response.Moderation.Capabilities.default with
                on_tool_call = (fun ~name:_ ~args:_ -> failwith "unscoped callback used")
              }
            ~events:
              ({| | `Tool_observed(p) -> Task.bind((|}
               ^ call
               ^ {|), fun ignored ->
                   let ignored = state[0] <- state[0] + 1 in
                   Task.bind(Runtime.emit(`String("observed")), fun ignored ->
                   match state[0] with
                   | |}
               ^ Int.to_string total
               ^ {| ->
                     Task.bind(Runtime.end_session("all observed"), fun ignored -> Task.pure(state))
                   | _ -> Task.pure(state)))
                 | _ -> Task.pure(state) |}
              )
        in
        let observer = M.invocation_observer manager |> Option.value_exn in
        let snapshot =
          Some
            (B.encode_moderator_snapshot
               (M.identity_snapshot manager |> Result.ok_or_failwith))
        in
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          caps.commit_moderator snapshot |> protocol_ok;
          let parent = invocation_fixture () in
          caps.with_invocation ~invocation:parent (fun ~dispatched:_ ->
            List.iter (List.range 0 36) ~f:(fun n ->
              let observer =
                match n with
                | 35 -> { observer with source_sha256 = String.make 64 'b' }
                | _ -> observer
              in
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
                Int.incr native_calls;
                Ok (Complete (`String "native result")))
              |> protocol_ok
              |> ignore);
            Ok (Complete `Null))
          |> protocol_ok
          |> ignore;
          prepared := Some manager;
          Completed
            { final_history = input.history
            ; moderator_snapshot = snapshot
            ; runtime_requests = []
            }))
      (fun _env actor _writer backend ->
         let initial = await_idle actor in
         let manager = Option.value_exn !prepared in
         let snapshot () =
           Some
             (B.encode_moderator_snapshot
                (M.identity_snapshot manager |> Result.ok_or_failwith))
         in
         let internal_batches = ref 0 in
         let script_tools =
           match tool_calls with
           | false -> None
           | true ->
             Some
               (Agent_session.Script_tool_calls.create
                  ~registry:(fun () -> registry)
                  ~moderator_names:String.Set.empty
                  ~now:Agent_protocol.Timestamp.now
                  ~is_halted:(fun () ->
                    let state = A.state actor |> protocol_ok in
                    match state.lifecycle.desired with
                    | Running -> state.halted
                    | Stopped -> true)
                  ~requires_active_moderator:(fun _ -> false)
                  ~authorize:(fun _ _ ->
                    let state = A.state actor |> protocol_ok in
                    assert (Option.is_none state.active_operation);
                    Ok ())
                  ~prepare_output:(fun _ -> Ok (`String "disclosed"))
                  ~defer_observation:(fun _ -> Ok ()))
         in
         let runtime : B.t =
           { worker =
               Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ _ ->
                 failwith "unexpected model turn")
           ; parse_user_content = (fun ~id:_ _ -> failwith "unexpected input")
           ; initial_history = []
           ; initial_prompt_entry_count = 0
           ; reserved_history_through = 0
           ; moderator_snapshot = snapshot ()
           ; moderator_manager = Some manager
           ; moderator_tools = []
           ; moderator_script_tools = script_tools
           ; background_executor = None
           ; moderator_activation = None
           ; start_moderator = (fun () -> failwith "unexpected startup")
           ; enqueue_internal_event =
               (fun ?prepare:_ _ -> failwith "unexpected external event")
           ; drain_internal_events =
               (fun _ ->
                 Int.incr internal_batches;
                 failwith "v1 owner used legacy event drain")
           ; execute_model_job =
               (fun ~recipe:_ ~payload:_ -> failwith "unexpected model job")
           ; enqueue_model_job_completion =
               (fun ?prepare:_ _ -> failwith "unexpected completion")
           ; close = (fun () -> ())
           }
         in
         let owner =
           Agent_server.Runtime_owner.create
             ~actor
             ~initial:(Some runtime)
             ~build:(fun () -> failwith "unexpected runtime rebuild")
         in
         let changed_source =
           { (M.identity_snapshot manager |> Result.ok_or_failwith) with
             script_source_hash = String.make 64 'f'
           ; queued_internal_events = [ Session.Snapshot.String "wake" ]
           }
         in
         A.change_moderator actor (Some (B.encode_moderator_snapshot changed_source))
         |> protocol_ok
         |> ignore;
         let changed = A.state actor |> protocol_ok in
         let stale_source_rejected =
           Agent_server.Runtime_owner.drain_idle_moderator owner |> Result.is_error
         in
         [%test_eq: bool] true stale_source_rejected;
         assert_same_session_snapshot changed (A.state actor |> protocol_ok);
         assert_same_session_snapshot changed (Agent_session.Memory_backend.state backend);
         [%test_eq: int] 0 !nested_calls;
         A.change_moderator actor initial.moderator |> protocol_ok |> ignore;
         let poll () =
           Agent_server.Runtime_owner.drain_idle_moderator owner |> protocol_ok
         in
         let summarize more =
           let state = A.state actor |> protocol_ok in
           let count status =
             List.count state.invocations ~f:(fun invocation ->
               Option.exists invocation.observation ~f:(fun observation ->
                 I.equal_observation_status observation.status status))
           in
           assert (Option.is_none state.active_operation);
           [%test_eq: int] 1 (List.length state.conversation.canonical_history);
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           print_s
             [%sexp
               { tool_calls : bool
               ; more : bool
               ; observed = (count Observed : int)
               ; awaiting = (count Awaiting : int)
               ; desired =
                   (state.lifecycle.desired : Agent_protocol.Session.desired_state)
               ; seed_calls = (!native_calls : int)
               ; native_calls = (!nested_calls : int)
               ; internal_batches = (!internal_batches : int)
               }]
         in
         List.iter
           (List.range 0 (if tool_calls then 7 else 3))
           ~f:(fun _ -> summarize (poll ()))));
  [%expect
    {|
    ((tool_calls false) (more true) (observed 32) (awaiting 4) (desired Running)
     (seed_calls 36) (native_calls 0) (internal_batches 0))
    ((tool_calls false) (more true) (observed 35) (awaiting 1) (desired Stopped)
     (seed_calls 36) (native_calls 0) (internal_batches 0))
    ((tool_calls false) (more false) (observed 35) (awaiting 1) (desired Stopped)
     (seed_calls 36) (native_calls 0) (internal_batches 0))
    ((tool_calls true) (more true) (observed 32) (awaiting 132) (desired Running)
     (seed_calls 36) (native_calls 128) (internal_batches 0))
    ((tool_calls true) (more true) (observed 64) (awaiting 112) (desired Running)
     (seed_calls 36) (native_calls 140) (internal_batches 0))
    ((tool_calls true) (more true) (observed 96) (awaiting 80) (desired Running)
     (seed_calls 36) (native_calls 140) (internal_batches 0))
    ((tool_calls true) (more true) (observed 128) (awaiting 48) (desired Running)
     (seed_calls 36) (native_calls 140) (internal_batches 0))
    ((tool_calls true) (more true) (observed 160) (awaiting 16) (desired Running)
     (seed_calls 36) (native_calls 140) (internal_batches 0))
    ((tool_calls true) (more true) (observed 175) (awaiting 1) (desired Stopped)
     (seed_calls 36) (native_calls 140) (internal_batches 0))
    ((tool_calls true) (more false) (observed 175) (awaiting 1) (desired Stopped)
     (seed_calls 36) (native_calls 140) (internal_batches 0))
    |}]
;;

let%expect_test
    "runtime owner bounds queued events and retains failed effects without replay"
  =
  let module A = Agent_session.Session_actor in
  let module B = Agent_session.Runtime_builder in
  let module M = Chat_response.Moderator_manager in
  let module E = Agent_protocol.Moderator_execution in
  List.iter
    [ `Batch
    ; `Unavailable
    ; `Failure
    ; `Save_failure
    ; `End
    ; `Cancel
    ; `Cancel_poll
    ; `Approve
    ; `Deny
    ; `Permission_stop
    ; `Permission_cancel
    ; `Permission_cancel_save
    ; `Permission_timeout
    ]
    ~f:(fun mode ->
      let prepared = ref None
      and calls = ref 0
      and authorized = ref 0
      and rejected = ref false in
      let interactive =
        match mode with
        | `Approve
        | `Deny
        | `Permission_stop
        | `Permission_cancel
        | `Permission_cancel_save
        | `Permission_timeout -> true
        | _ -> false
      in
      let cancellation = ref None in
      let on_native = ref (fun () -> ()) in
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
          | `Permission_cancel_save, false
            when List.exists next.state.permissions ~f:(fun permission ->
                   Agent_protocol.Permission.equal_state permission.state Cancelled) ->
            rejected := true;
            true
          | _ -> false)
        ~make_worker:(fun env _ ->
          let finish =
            match mode with
            | `Failure -> "Task.fail(\"after native effect\")"
            | `End ->
              {|match state[0] with
              | 2 -> Task.bind(Runtime.end_session("finished"), fun ignored -> Task.pure(state))
              | _ -> Task.pure(state)|}
            | _ -> "Task.pure(state)"
          in
          let manager, _, _ =
            handoff_definition
              env
              ~declare_tool:false
              ~capability_registry:registry
              ~moderator_capabilities:
                { Chat_response.Moderation.Capabilities.default with
                  on_tool_call =
                    (fun ~name:_ ~args:_ -> failwith "unscoped native fallback")
                }
              ~events:
                ({| | `Session_start ->
                   let rec emit = fun n -> match n with
                   | 0 -> Task.pure(state)
                   | _ -> Task.bind(Runtime.emit(`Null), fun ignored -> emit(n - 1))
                   in emit(|}
                 ^ (if interactive then "1" else "35")
                 ^ {|)
                 | `Internal_event(payload) ->
                   Task.bind(Tool.call("read_file", `Object([])), fun result ->
                     let increment = match result with
                     | `Ok(value) -> 1
                     | `Error("invocation.unavailable") -> 10
                     | `Error("invocation.observation_failed") -> 1
                     | _ -> 1000 in
                     let ignored = state[0] <- state[0] + increment in
                     |}
                 ^ finish
                 ^ {|)
                 | `Tool_observed(p) ->
                   (match p.parent_event with
                    | `Some(id) -> let ignored = state[0] <- state[0] + 100 in Task.pure(state)
                    | _ -> Task.fail("missing event lineage"))
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
          let snapshot =
            Some
              (B.encode_moderator_snapshot
                 (M.identity_snapshot manager |> Result.ok_or_failwith))
          in
          prepared := Some manager;
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            caps.commit_moderator snapshot |> protocol_ok;
            Completed
              { final_history = input.history
              ; moderator_snapshot = snapshot
              ; runtime_requests = []
              }))
        (fun _env actor writer backend ->
           Eio.Switch.run (fun probe_sw ->
             let initial = await_idle actor in
             let manager = Option.value_exn !prepared in
             let release_probe, release_probe_u = Eio.Promise.create () in
             let probed, probed_u = Eio.Promise.create () in
             (on_native
              := fun () ->
                   let state = A.state actor |> protocol_ok in
                   assert (Option.is_none state.active_operation);
                   (match Agent_session.Native_tool_invocation.current_scope () with
                    | Active invocation ->
                      assert (
                        List.exists state.invocations ~f:(fun current ->
                          Agent_protocol.Id.Invocation.equal
                            current.context.id
                            invocation.context.id))
                    | Unbound | Expired -> assert false);
                   [%test_eq: int]
                     1
                     (List.count state.moderator_executions ~f:(fun event ->
                        E.equal_status event.status Running));
                   match mode with
                   | `Cancel ->
                     A.stop actor ~attachment_id:writer.id ~mode:Cancel
                     |> protocol_ok
                     |> ignore;
                     Eio.Fiber.yield ()
                   | `Cancel_poll ->
                     Eio.Cancel.cancel (Option.value_exn !cancellation) Exit;
                     Eio.Fiber.yield ()
                   | _ -> Eio.Fiber.yield ());
             let script_tools =
               match mode with
               | `Unavailable -> None
               | _ ->
                 Some
                   (Agent_session.Script_tool_calls.create
                      ~registry:(fun () -> registry)
                      ~moderator_names:String.Set.empty
                      ~now:Agent_protocol.Timestamp.now
                      ~is_halted:(fun () ->
                        let state = A.state actor |> protocol_ok in
                        match state.lifecycle.desired with
                        | Running -> state.halted
                        | Stopped -> true)
                      ~requires_active_moderator:(fun _ -> false)
                      ~authorize:(fun child _ ->
                        incr authorized;
                        (match Agent_session.Native_tool_invocation.current_scope () with
                         | Active invocation ->
                           assert (
                             Agent_protocol.Id.Invocation.equal
                               child.context.id
                               invocation.context.id)
                         | Unbound | Expired -> assert false);
                        (match mode with
                         | `Approve ->
                           Eio.Fiber.fork ~sw:probe_sw (fun () ->
                             Eio.Promise.await release_probe;
                             (match
                                Agent_session.Native_tool_invocation.current_scope ()
                              with
                              | Expired -> ()
                              | Unbound | Active _ -> assert false);
                             Eio.Promise.resolve probed_u ())
                         | _ -> ());
                        let state = A.state actor |> protocol_ok in
                        let parent = Option.value_exn child.parent_event in
                        assert (Option.is_none child.context.parent_invocation);
                        assert (
                          List.exists state.moderator_executions ~f:(fun event ->
                            Agent_protocol.Id.Moderator_execution.equal
                              parent
                              event.context.id
                            && E.equal_status event.status Running));
                        if not interactive
                        then Ok ()
                        else (
                          let permission : Agent_protocol.Permission.t =
                            { id = Agent_protocol.Id.Permission.create ()
                            ; session_id = child.context.session_id
                            ; generation = child.context.generation
                            ; owner = Invocation child.context.id
                            ; call_id =
                                Agent_protocol.Id.Invocation.to_string child.context.id
                            ; tool_name = child.context.tool_name
                            ; runtime_identity = Some child.context.capability_fingerprint
                            ; invocation_display = "read_file fixture"
                            ; rationale = None
                            ; effects = [ "read" ]
                            ; choices = [ Approve_once; Deny ]
                            ; created_at = Agent_protocol.Timestamp.now ()
                            ; expires_at = None
                            ; state = Pending
                            ; resolution = None
                            }
                          in
                          let forged =
                            { permission with
                              owner = Invocation (Agent_protocol.Id.Invocation.create ())
                            }
                          in
                          assert (
                            Result.is_error
                              (A.request_permission
                                 actor
                                 ~permission:forged
                                 ~timeout_seconds:None
                                 ~fallback:Deny));
                          let timeout_seconds =
                            match mode with
                            | `Permission_timeout -> Some 0.01
                            | _ -> None
                          in
                          match
                            A.request_permission
                              actor
                              ~permission
                              ~timeout_seconds
                              ~fallback:Deny
                          with
                          | Ok { choice = Approve_once; _ } -> Ok ()
                          | _ -> Error (handoff_error "permission denied")))
                      ~prepare_output:(fun _ -> Ok (`String "disclosed"))
                      ~defer_observation:(fun _ -> Error (handoff_error "lost wakeup")))
             in
             let runtime : B.t =
               { worker =
                   Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ _ ->
                     failwith "unexpected model turn")
               ; parse_user_content = (fun ~id:_ _ -> failwith "unexpected input")
               ; initial_history = []
               ; initial_prompt_entry_count = 0
               ; reserved_history_through = 0
               ; moderator_snapshot = initial.moderator
               ; moderator_manager = Some manager
               ; moderator_tools = []
               ; moderator_script_tools = script_tools
               ; background_executor = None
               ; moderator_activation = None
               ; start_moderator = (fun () -> failwith "unexpected startup")
               ; enqueue_internal_event =
                   (fun ?prepare:_ _ -> failwith "unexpected external event")
               ; drain_internal_events = (fun _ -> failwith "legacy event drain used")
               ; execute_model_job =
                   (fun ~recipe:_ ~payload:_ -> failwith "unexpected model job")
               ; enqueue_model_job_completion =
                   (fun ?prepare:_ _ -> failwith "unexpected completion")
               ; close = (fun () -> ())
               }
             in
             let owner =
               Agent_server.Runtime_owner.create
                 ~actor
                 ~initial:(Some runtime)
                 ~build:(fun () -> failwith "unexpected runtime build")
             in
             let poll () =
               try
                 Eio.Cancel.sub (fun context ->
                   cancellation := Some context;
                   Agent_server.Runtime_owner.drain_idle_moderator owner)
               with
               | Eio.Cancel.Cancelled _ -> Error (handoff_error "cancelled")
             in
             let summarize result =
               let state = A.state actor |> protocol_ok in
               let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
               assert (
                 Option.equal
                   Jsonaf.exactly_equal
                   state.moderator
                   (Some (B.encode_moderator_snapshot snapshot)));
               assert_same_session_snapshot
                 state
                 (Agent_session.Memory_backend.state backend);
               assert (
                 List.equal
                   Agent_protocol.History.equal_entry
                   initial.conversation.canonical_history
                   state.conversation.canonical_history);
               assert (Option.is_none state.active_operation);
               let completed =
                 List.count state.moderator_executions ~f:(fun event ->
                   match event.status, event.intent with
                   | Completed _, (None | Some Applied) -> true
                   | _ -> false)
               in
               let failed =
                 List.count state.moderator_executions ~f:(fun event ->
                   match event.status with
                   | Failed _ | Interrupted _ -> true
                   | _ -> false)
               in
               let observed =
                 List.count state.invocations ~f:(fun child ->
                   (match mode, child.status with
                    | ( ( `Cancel
                        | `Cancel_poll
                        | `Permission_stop
                        | `Permission_cancel
                        | `Permission_cancel_save )
                      , Resolved (Cancelled _) ) -> ()
                    | (`Deny | `Permission_timeout), Resolved (Fail _) -> ()
                    | _, Resolved (Complete (`String "disclosed")) -> ()
                    | _ -> assert false);
                   match child.observation with
                   | Some { status = Observed; _ } -> true
                   | _ -> false)
               in
               let count =
                 match snapshot.current_state with
                 | Session.Snapshot.Array [ Int n ] -> n
                 | _ -> assert false
               in
               print_s
                 [%sexp
                   { mode : [ `Batch
                            | `Unavailable
                            | `Failure
                            | `Save_failure
                            | `End
                            | `Cancel
                            | `Cancel_poll
                            | `Approve
                            | `Deny
                            | `Permission_stop
                            | `Permission_cancel
                            | `Permission_cancel_save
                            | `Permission_timeout
                            ]
                   ; result =
                       (Result.map_error result ~f:(fun _ -> "failed")
                        : (bool, string) result)
                   ; calls = (!calls : int)
                   ; authorized = (!authorized : int)
                   ; completed : int
                   ; failed : int
                   ; observed : int
                   ; queued = (List.length snapshot.queued_internal_events : int)
                   ; state = (count : int)
                   ; desired =
                       (state.lifecycle.desired : Agent_protocol.Session.desired_state)
                   ; permissions =
                       (List.map state.permissions ~f:(fun permission ->
                          permission.Agent_protocol.Permission.state)
                        : Agent_protocol.Permission.state list)
                   }]
             in
             let first = ref None in
             (match mode with
              | `Approve
              | `Deny
              | `Permission_stop
              | `Permission_cancel
              | `Permission_cancel_save ->
                Eio.Fiber.both
                  (fun () -> first := Some (poll ()))
                  (fun () ->
                     let rec pending () =
                       let state = A.state actor |> protocol_ok in
                       match
                         List.find state.permissions ~f:(fun p ->
                           Agent_protocol.Permission.equal_state p.state Pending)
                       with
                       | Some permission -> permission
                       | None ->
                         Eio.Fiber.yield ();
                         pending ()
                     in
                     let permission = pending () in
                     [%test_eq: int] 0 !calls;
                     let persisted = Agent_session.Memory_backend.state backend in
                     assert (Option.is_none persisted.active_operation);
                     assert (
                       List.exists persisted.permissions ~f:(fun p ->
                         Agent_protocol.Id.Permission.equal p.id permission.id));
                     (match mode with
                      | `Approve ->
                        let module S = Agent_session.Session_state in
                        let restored =
                          Agent_session.Session_persistence.restore_snapshot
                            (Sexp.to_string_mach (S.sexp_of_t persisted))
                          |> store_ok
                        in
                        assert_same_session_snapshot persisted restored;
                        assert (
                          Result.is_error
                            (S.upgrade_schema { restored with schema_version = 7 }));
                        assert (
                          Result.is_error (S.validate { restored with invocations = [] }));
                        let changed =
                          { permission with
                            owner =
                              Agent_protocol.Permission.Operation
                                (Agent_protocol.Id.Operation.create ())
                          }
                        in
                        assert (
                          Result.is_error
                            (Agent_session.Session_delta.apply
                               restored
                               (Permission_changed changed)));
                        let legacy =
                          { persisted with permissions = [ changed ]; schema_version = 7 }
                        in
                        let rec old_permission_field = function
                          | Sexp.List [ Atom "owner"; List [ Atom "Operation"; id ] ] ->
                            Sexp.List [ Atom "operation_id"; id ]
                          | List fields -> List (List.map fields ~f:old_permission_field)
                          | Atom _ as value -> value
                        in
                        let migrated =
                          Agent_session.Session_persistence.restore_snapshot
                            (Sexp.to_string_mach
                               (old_permission_field (S.sexp_of_t legacy)))
                          |> store_ok
                        in
                        [%test_eq: int] 8 migrated.schema_version;
                        assert (
                          Agent_protocol.Permission.equal_owner
                            (List.hd_exn migrated.permissions).owner
                            changed.owner)
                      | _ -> ());
                     match mode with
                     | `Permission_stop ->
                       A.stop actor ~attachment_id:writer.id ~mode:Cancel
                       |> protocol_ok
                       |> ignore
                     | `Permission_cancel | `Permission_cancel_save ->
                       Eio.Cancel.cancel (Option.value_exn !cancellation) Exit
                     | _ ->
                       let choice =
                         match mode with
                         | `Approve -> Agent_protocol.Permission.Approve_once
                         | _ -> Deny
                       in
                       A.respond_permission
                         actor
                         ~attachment_id:writer.id
                         ~principal_id:(Some principal_id)
                         ~permission_id:permission.id
                         ~permission_generation:permission.generation
                         ~choice
                         ~reason:None
                       |> protocol_ok
                       |> ignore)
              | _ -> first := Some (poll ()));
             summarize (Option.value_exn !first);
             Eio.Promise.resolve release_probe_u ();
             (match mode with
              | `Approve -> Eio.Promise.await probed
              | _ -> ());
             (match Agent_session.Native_tool_invocation.current_scope () with
              | Unbound -> ()
              | Active _ | Expired -> assert false);
             summarize (poll ());
             summarize (poll ());
             let before = A.state actor |> protocol_ok in
             (match mode with
              | `Permission_cancel_save -> assert !rejected
              | _ -> ());
             List.iter before.permissions ~f:(fun permission ->
               let late =
                 { permission with
                   id = Agent_protocol.Id.Permission.create ()
                 ; state = Pending
                 ; resolution = None
                 }
               in
               assert (
                 Result.is_error
                   (A.request_permission
                      actor
                      ~permission:late
                      ~timeout_seconds:None
                      ~fallback:Deny)));
             [%test_eq: bool] false (poll () |> protocol_ok);
             assert_same_session_snapshot before (A.state actor |> protocol_ok))));
  [%expect
    {|
    ((mode Batch) (result (Ok true)) (calls 32) (authorized 32) (completed 32)
     (failed 0) (observed 0) (queued 3) (state 32) (desired Running)
     (permissions ()))
    ((mode Batch) (result (Ok true)) (calls 35) (authorized 35) (completed 35)
     (failed 0) (observed 32) (queued 0) (state 3235) (desired Running)
     (permissions ()))
    ((mode Batch) (result (Ok false)) (calls 35) (authorized 35) (completed 35)
     (failed 0) (observed 35) (queued 0) (state 3535) (desired Running)
     (permissions ()))
    ((mode Unavailable) (result (Ok true)) (calls 0) (authorized 0)
     (completed 32) (failed 0) (observed 0) (queued 3) (state 320)
     (desired Running) (permissions ()))
    ((mode Unavailable) (result (Ok true)) (calls 0) (authorized 0)
     (completed 35) (failed 0) (observed 0) (queued 0) (state 350)
     (desired Running) (permissions ()))
    ((mode Unavailable) (result (Ok false)) (calls 0) (authorized 0)
     (completed 35) (failed 0) (observed 0) (queued 0) (state 350)
     (desired Running) (permissions ()))
    ((mode Failure) (result (Error failed)) (calls 1) (authorized 1)
     (completed 0) (failed 1) (observed 0) (queued 35) (state 0)
     (desired Running) (permissions ()))
    ((mode Failure) (result (Ok false)) (calls 1) (authorized 1) (completed 0)
     (failed 1) (observed 1) (queued 35) (state 100) (desired Running)
     (permissions ()))
    ((mode Failure) (result (Ok false)) (calls 1) (authorized 1) (completed 0)
     (failed 1) (observed 1) (queued 35) (state 100) (desired Running)
     (permissions ()))
    ((mode Save_failure) (result (Error failed)) (calls 1) (authorized 1)
     (completed 0) (failed 1) (observed 0) (queued 35) (state 0)
     (desired Running) (permissions ()))
    ((mode Save_failure) (result (Ok false)) (calls 1) (authorized 1)
     (completed 0) (failed 1) (observed 1) (queued 35) (state 100)
     (desired Running) (permissions ()))
    ((mode Save_failure) (result (Ok false)) (calls 1) (authorized 1)
     (completed 0) (failed 1) (observed 1) (queued 35) (state 100)
     (desired Running) (permissions ()))
    ((mode End) (result (Ok true)) (calls 2) (authorized 2) (completed 2)
     (failed 0) (observed 0) (queued 33) (state 2) (desired Stopped)
     (permissions ()))
    ((mode End) (result (Ok false)) (calls 2) (authorized 2) (completed 2)
     (failed 0) (observed 0) (queued 33) (state 2) (desired Stopped)
     (permissions ()))
    ((mode End) (result (Ok false)) (calls 2) (authorized 2) (completed 2)
     (failed 0) (observed 0) (queued 33) (state 2) (desired Stopped)
     (permissions ()))
    ((mode Cancel) (result (Error failed)) (calls 1) (authorized 1) (completed 0)
     (failed 1) (observed 0) (queued 35) (state 0) (desired Stopped)
     (permissions ()))
    ((mode Cancel) (result (Ok false)) (calls 1) (authorized 1) (completed 0)
     (failed 1) (observed 0) (queued 35) (state 0) (desired Stopped)
     (permissions ()))
    ((mode Cancel) (result (Ok false)) (calls 1) (authorized 1) (completed 0)
     (failed 1) (observed 0) (queued 35) (state 0) (desired Stopped)
     (permissions ()))
    ((mode Cancel_poll) (result (Error failed)) (calls 1) (authorized 1)
     (completed 0) (failed 1) (observed 0) (queued 35) (state 0)
     (desired Running) (permissions ()))
    ((mode Cancel_poll) (result (Ok false)) (calls 1) (authorized 1)
     (completed 0) (failed 1) (observed 1) (queued 35) (state 100)
     (desired Running) (permissions ()))
    ((mode Cancel_poll) (result (Ok false)) (calls 1) (authorized 1)
     (completed 0) (failed 1) (observed 1) (queued 35) (state 100)
     (desired Running) (permissions ()))
    ((mode Approve) (result (Ok true)) (calls 1) (authorized 1) (completed 1)
     (failed 0) (observed 0) (queued 0) (state 1) (desired Running)
     (permissions (Approved)))
    ((mode Approve) (result (Ok false)) (calls 1) (authorized 1) (completed 1)
     (failed 0) (observed 1) (queued 0) (state 101) (desired Running)
     (permissions (Approved)))
    ((mode Approve) (result (Ok false)) (calls 1) (authorized 1) (completed 1)
     (failed 0) (observed 1) (queued 0) (state 101) (desired Running)
     (permissions (Approved)))
    ((mode Deny) (result (Ok true)) (calls 0) (authorized 1) (completed 1)
     (failed 0) (observed 0) (queued 0) (state 1) (desired Running)
     (permissions (Denied)))
    ((mode Deny) (result (Ok false)) (calls 0) (authorized 1) (completed 1)
     (failed 0) (observed 1) (queued 0) (state 101) (desired Running)
     (permissions (Denied)))
    ((mode Deny) (result (Ok false)) (calls 0) (authorized 1) (completed 1)
     (failed 0) (observed 1) (queued 0) (state 101) (desired Running)
     (permissions (Denied)))
    ((mode Permission_stop) (result (Error failed)) (calls 0) (authorized 1)
     (completed 0) (failed 1) (observed 0) (queued 1) (state 0) (desired Stopped)
     (permissions (Cancelled)))
    ((mode Permission_stop) (result (Ok false)) (calls 0) (authorized 1)
     (completed 0) (failed 1) (observed 0) (queued 1) (state 0) (desired Stopped)
     (permissions (Cancelled)))
    ((mode Permission_stop) (result (Ok false)) (calls 0) (authorized 1)
     (completed 0) (failed 1) (observed 0) (queued 1) (state 0) (desired Stopped)
     (permissions (Cancelled)))
    ((mode Permission_cancel) (result (Error failed)) (calls 0) (authorized 1)
     (completed 0) (failed 1) (observed 0) (queued 1) (state 0) (desired Running)
     (permissions (Cancelled)))
    ((mode Permission_cancel) (result (Ok false)) (calls 0) (authorized 1)
     (completed 0) (failed 1) (observed 1) (queued 1) (state 100)
     (desired Running) (permissions (Cancelled)))
    ((mode Permission_cancel) (result (Ok false)) (calls 0) (authorized 1)
     (completed 0) (failed 1) (observed 1) (queued 1) (state 100)
     (desired Running) (permissions (Cancelled)))
    ((mode Permission_cancel_save) (result (Error failed)) (calls 0)
     (authorized 1) (completed 0) (failed 1) (observed 0) (queued 1) (state 0)
     (desired Running) (permissions (Cancelled)))
    ((mode Permission_cancel_save) (result (Ok false)) (calls 0) (authorized 1)
     (completed 0) (failed 1) (observed 1) (queued 1) (state 100)
     (desired Running) (permissions (Cancelled)))
    ((mode Permission_cancel_save) (result (Ok false)) (calls 0) (authorized 1)
     (completed 0) (failed 1) (observed 1) (queued 1) (state 100)
     (desired Running) (permissions (Cancelled)))
    ((mode Permission_timeout) (result (Ok true)) (calls 0) (authorized 1)
     (completed 1) (failed 0) (observed 0) (queued 0) (state 1) (desired Running)
     (permissions (Denied)))
    ((mode Permission_timeout) (result (Ok false)) (calls 0) (authorized 1)
     (completed 1) (failed 0) (observed 1) (queued 0) (state 101)
     (desired Running) (permissions (Denied)))
    ((mode Permission_timeout) (result (Ok false)) (calls 0) (authorized 1)
     (completed 1) (failed 0) (observed 1) (queued 0) (state 101)
     (desired Running) (permissions (Denied)))
    |}]
;;
