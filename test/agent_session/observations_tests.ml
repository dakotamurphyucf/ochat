open Core
open Fixtures

let%expect_test
    "bounded observation drains select atomically and leave unrelated intent alone"
  =
  List.iter [ `Budget; `Concurrent; `Failure; `End ] ~f:(fun mode ->
    let module I = Agent_protocol.Invocation in
    let module M = Chat_response.Moderator_manager in
    let calls = ref []
    and native_calls = ref 0 in
    with_handoff_actor
      ~make_worker:(fun env actor_ready ->
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let actor = Eio.Promise.await actor_ready in
          let finish =
            match mode with
            | `Failure -> "Task.fail(\"observer failed\")"
            | `End ->
              "Task.bind(Runtime.end_session(\"observations done\"), fun ignored -> \
               Task.pure(state))"
            | _ -> "Task.pure(state)"
          in
          let manager, _, definition =
            handoff_definition
              env
              ~events:
                ("| `Tool_observed(p) -> let ignored = state[0] <- state[0] + 1 in \
                  Task.bind(Tool.call(p.invocation_id, p.outcome), fun ignored -> "
                 ^ finish
                 ^ ") | _ -> Task.pure(state)")
          in
          let script =
            Chat_response.Extension_compiler.script
              (List.hd_exn (Chat_response.Extension_compiler.prepared_tools definition))
          in
          let observer : I.observer =
            { script_id = script.id; source_sha256 = script.source_sha256 }
          in
          caps.commit_moderator
            (Some
               (Agent_session.Runtime_builder.encode_moderator_snapshot
                  (M.identity_snapshot manager |> Result.ok_or_failwith)))
          |> protocol_ok;
          let parent = invocation_fixture () in
          caps.with_invocation ~invocation:parent (fun ~dispatched:_ ~capabilities:_ ->
            List.iter [ 2; 0; 1; 3 ] ~f:(fun index ->
              let observer =
                match index with
                | 3 -> { observer with script_id = "unrelated" }
                | _ -> observer
              in
              let child =
                I.create
                  ~observer
                  { parent.context with
                    id =
                      Agent_protocol.Id.Invocation.of_string
                        ("inv_queue_" ^ Int.to_string index)
                      |> protocol_ok
                  ; origin = Moderator
                  ; parent_invocation = Some parent.context.id
                  }
                |> protocol_ok
              in
              caps.with_invocation ~invocation:child (fun ~dispatched:_ ~capabilities:_ ->
                Int.incr native_calls;
                Ok (Complete (`Number (Int.to_string index))))
              |> protocol_ok
              |> ignore);
            assert (
              not
                (caps.with_next_moderator_observation
                   ~observer
                   (fun ~observing:_ ~capabilities:_ ~commit:_ -> assert false)
                 |> protocol_ok));
            Ok (Complete `Null))
          |> protocol_ok
          |> ignore;
          let drain max_observations =
            Agent_session.Moderator_observation.drain
              ~max_observations
              ~capabilities:caps
              ~observer
              ~manager
              ~history:(fun () ->
                (Agent_session.Session_actor.state actor |> protocol_ok).conversation
                  .canonical_history
                |> Agent_session.History_codec.all_of_protocol
                |> protocol_ok)
              ~available_tools:[]
              ~session_meta:`Null
              ~now:Agent_protocol.Timestamp.now
              ~on_tool_call:(fun ~name ~args:_ ->
                calls := !calls @ [ name ];
                Eio.Fiber.yield ();
                Ok (Tool_ok `Null))
              ()
          in
          assert (Result.is_error (drain 0));
          assert (Result.is_error (drain 257));
          let outcomes =
            match mode with
            | `Budget ->
              let first = drain 2 |> protocol_ok in
              assert first.budget_exhausted;
              [%test_eq: int] 2 (List.length first.outcomes);
              let second = drain 2 |> protocol_ok in
              assert (not second.budget_exhausted);
              [%test_eq: int] 1 (List.length second.outcomes);
              let empty = drain 2 |> protocol_ok in
              assert (List.is_empty empty.outcomes && not empty.budget_exhausted);
              first.outcomes @ second.outcomes
            | `Concurrent ->
              let results = ref [] in
              Eio.Fiber.both
                (fun () ->
                   let result = drain 256 |> protocol_ok in
                   results := result :: !results)
                (fun () ->
                   let result = drain 256 |> protocol_ok in
                   results := result :: !results);
              assert (
                List.for_all !results ~f:(fun result ->
                  not result.Agent_session.Moderator_observation.budget_exhausted));
              let outcomes =
                List.concat_map !results ~f:(fun result ->
                  result.Agent_session.Moderator_observation.outcomes)
              in
              [%test_eq: int] 3 (List.length outcomes);
              outcomes
            | `Failure ->
              assert (Result.is_error (drain 32));
              []
            | `End ->
              let result = drain 32 |> protocol_ok in
              assert (not result.budget_exhausted);
              [%test_eq: int] 1 (List.length result.outcomes);
              let halted = drain 32 |> protocol_ok in
              assert (List.is_empty halted.outcomes && not halted.budget_exhausted);
              result.outcomes
          in
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
          (match snapshot.current_state with
           | Session.Snapshot.Array [ Int count ] ->
             [%test_eq: int]
               (match mode with
                | `Failure -> 0
                | `End -> 1
                | _ -> 3)
               count
           | _ -> assert false);
          assert (
            Option.equal
              Jsonaf.exactly_equal
              state.moderator
              (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)));
          Completed
            { final_history = input.history
            ; moderator_snapshot = state.moderator
            ; runtime_requests =
                List.concat_map outcomes ~f:(fun outcome ->
                  outcome.Chat_response.Moderation.Outcome.runtime_requests)
            }))
      (fun _env actor _writer backend ->
         let rec finished () =
           let state = Agent_session.Session_actor.state actor |> protocol_ok in
           match state.active_operation with
           | None -> state
           | Some _ ->
             Eio.Fiber.yield ();
             finished ()
         in
         let state = finished () in
         let observations =
           List.filter_map state.invocations ~f:(fun invocation ->
             Option.map invocation.observation ~f:(fun observation ->
               ( Agent_protocol.Id.Invocation.to_string invocation.context.id
               , observation.status )))
           |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
         in
         let mode =
           match mode with
           | `Budget -> "budget"
           | `Concurrent -> "concurrent"
           | `Failure -> "failure"
           | `End -> "end"
         in
         print_s
           [%sexp
             { mode : string
             ; native_calls = (!native_calls : int)
             ; handled = (!calls : string list)
             ; observations : (string * I.observation_status) list
             }];
         [%test_eq: int] 1 (List.length state.conversation.canonical_history);
         assert_same_session_snapshot state (Agent_session.Memory_backend.state backend)));
  [%expect
    {|
    ((mode budget) (native_calls 4)
     (handled (inv_queue_0 inv_queue_1 inv_queue_2))
     (observations
      ((inv_queue_0 Observed) (inv_queue_1 Observed) (inv_queue_2 Observed)
       (inv_queue_3 Awaiting))))
    ((mode concurrent) (native_calls 4)
     (handled (inv_queue_0 inv_queue_1 inv_queue_2))
     (observations
      ((inv_queue_0 Observed) (inv_queue_1 Observed) (inv_queue_2 Observed)
       (inv_queue_3 Awaiting))))
    ((mode failure) (native_calls 4) (handled (inv_queue_0))
     (observations
      ((inv_queue_0
        (Observation_failed "observation handler failed before acknowledgement"))
       (inv_queue_1 Awaiting) (inv_queue_2 Awaiting) (inv_queue_3 Awaiting))))
    ((mode end) (native_calls 4) (handled (inv_queue_0))
     (observations
      ((inv_queue_0 Observed) (inv_queue_1 Awaiting) (inv_queue_2 Awaiting)
       (inv_queue_3 Awaiting))))
    |}]
;;

let%expect_test "idle observations own state without starting a model operation" =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let module M = Chat_response.Moderator_manager in
  List.iter
    [ `Success
    ; `Request
    ; `End
    ; `Concurrent
    ; `Reentrant
    ; `Handler_fail
    ; `Claim_rejected
    ; `Ack_rejected
    ; `Cancelled
    ; `Stopped
    ; `Stop_cancel
    ; `Graceful_then_cancel
    ; `Late_stop
    ]
    ~f:(fun mode ->
      let prepared = ref None
      and calls = ref 0
      and rejected = ref false in
      with_handoff_actor
        ~reject:(fun next ->
          let matches =
            List.exists
              next.Agent_session.Session_transition.state.invocations
              ~f:(fun invocation ->
                match mode, invocation.observation with
                | `Claim_rejected, Some { status = Observing; _ }
                | `Ack_rejected, Some { status = Observed; _ } -> true
                | _ -> false)
          in
          match matches && not !rejected with
          | true ->
            rejected := true;
            true
          | false -> false)
        ~make_worker:(fun env _ ->
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let finish =
              match mode with
              | `Request ->
                "Task.bind(Runtime.request_turn(), fun ignored -> Task.pure(state))"
              | `End ->
                "Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
                 Task.pure(state))"
              | `Handler_fail -> "Task.fail(\"observer failed\")"
              | _ -> "Task.pure(state)"
            in
            let manager, _, definition =
              handoff_definition
                env
                ~events:
                  ("| `Tool_observed(p) -> let ignored = state[0] <- state[0] + 1 in "
                   ^ "Task.bind(Tool.call(\"probe\", p.outcome), fun ignored -> "
                   ^ finish
                   ^ ") | _ -> Task.pure(state)")
            in
            let script =
              Chat_response.Extension_compiler.script
                (List.hd_exn (Chat_response.Extension_compiler.prepared_tools definition))
            in
            let observer : I.observer =
              { script_id = script.id; source_sha256 = script.source_sha256 }
            in
            let snapshot =
              Some
                (Agent_session.Runtime_builder.encode_moderator_snapshot
                   (M.identity_snapshot manager |> Result.ok_or_failwith))
            in
            caps.commit_moderator snapshot |> protocol_ok;
            let parent = invocation_fixture () in
            caps.with_invocation ~invocation:parent (fun ~dispatched:_ ~capabilities:_ ->
              let child =
                I.create
                  ~observer
                  { parent.context with
                    id =
                      Agent_protocol.Id.Invocation.of_string "inv_idle_observation"
                      |> protocol_ok
                  ; origin = Moderator
                  ; parent_invocation = Some parent.context.id
                  }
                |> protocol_ok
              in
              caps.with_invocation ~invocation:child (fun ~dispatched:_ ~capabilities:_ ->
                Ok (Complete (`String "native result")))
              |> protocol_ok
              |> ignore;
              Ok (Complete `Null))
            |> protocol_ok
            |> ignore;
            prepared := Some (manager, observer);
            Completed
              { final_history = input.history
              ; moderator_snapshot = snapshot
              ; runtime_requests = []
              }))
        (fun _env actor writer backend ->
           let initial = await_idle actor in
           let manager, observer = Option.value_exn !prepared in
           let cancellation = ref None in
           let before_observation_return = ref (fun () -> ()) in
           let run () =
             Agent_session.Moderator_observation.drain_idle
               ~claim:(fun callback ->
                 A.with_idle_moderator_observation
                   actor
                   ~observer
                   (fun ~observing ~capabilities ~commit ->
                      let result = callback ~observing ~capabilities ~commit in
                      !before_observation_return ();
                      result))
               ~manager
               ~history:(fun () ->
                 (A.state actor |> protocol_ok).conversation.canonical_history
                 |> Agent_session.History_codec.all_of_protocol
                 |> protocol_ok)
               ~available_tools:[]
               ~session_meta:`Null
               ~now:Agent_protocol.Timestamp.now
               ~on_tool_call:(fun ~name:_ ~args:_ ->
                 Int.incr calls;
                 let owned = A.state actor |> protocol_ok in
                 assert (Option.is_none owned.active_operation);
                 assert (Result.is_error (A.change_moderator actor None));
                 assert (
                   Result.is_error
                     (A.commit_extensions
                        actor
                        ~generation:owned.identity.generation
                        ~expected_revision:owned.counters.revision
                        [ Moderator_state None ]));
                 assert (Result.is_error (A.set_operation_worker actor None));
                 assert (Option.is_none (A.claim_idle_moderator actor |> protocol_ok));
                 assert (
                   Result.is_error (A.fail_idle_moderator actor (handoff_error "foreign")));
                 assert (
                   Result.is_error
                     (A.complete_idle_moderator
                        actor
                        { moderator_snapshot = None
                        ; runtime_requests = []
                        ; notifications = []
                        ; remaining_events = false
                        }));
                 Eio.Fiber.yield ();
                 (match mode with
                  | `Reentrant ->
                    assert (
                      Result.is_error
                        (A.with_idle_moderator_observation
                           actor
                           ~observer
                           (fun ~observing:_ ~capabilities:_ ~commit:_ -> assert false)))
                  | `Cancelled ->
                    Eio.Cancel.cancel (Option.value_exn !cancellation) Exit;
                    Eio.Fiber.yield ();
                    assert false
                  | `Stopped ->
                    A.stop actor ~attachment_id:writer.id ~mode:Graceful
                    |> protocol_ok
                    |> ignore;
                    assert (Result.is_error (A.start actor ~attachment_id:writer.id))
                  | `Stop_cancel | `Graceful_then_cancel ->
                    (match mode with
                     | `Graceful_then_cancel ->
                       A.stop actor ~attachment_id:writer.id ~mode:Graceful
                       |> protocol_ok
                       |> ignore
                     | _ -> ());
                    A.stop actor ~attachment_id:writer.id ~mode:Cancel
                    |> protocol_ok
                    |> ignore;
                    Eio.Fiber.yield ();
                    assert false
                  | _ -> ());
                 Ok (Tool_ok `Null))
               ()
           in
           let safe_run () =
             try
               match mode with
               | `Cancelled ->
                 Eio.Cancel.sub (fun context ->
                   cancellation := Some context;
                   run ())
               | _ -> run ()
             with
             | Eio.Cancel.Cancelled _ -> Error (handoff_error "cancelled")
           in
           let results = ref [] in
           (match mode with
            | `Concurrent ->
              Eio.Fiber.both
                (fun () ->
                   let result = safe_run () in
                   results := result :: !results)
                (fun () ->
                   let result = safe_run () in
                   results := result :: !results)
            | `Late_stop ->
              let callback_ready, callback_ready_u = Eio.Promise.create () in
              let release_callback, release_callback_u = Eio.Promise.create () in
              let callback_returned, callback_returned_u = Eio.Promise.create () in
              let actor_blocked, actor_blocked_u = Eio.Promise.create () in
              let release_actor, release_actor_u = Eio.Promise.create () in
              let stop_requested, stop_requested_u = Eio.Promise.create () in
              let checkpoint_finished = ref false in
              let stop_finished = ref false in
              let release promise resolver =
                if not (Eio.Promise.is_resolved promise)
                then Eio.Promise.resolve resolver ()
              in
              (before_observation_return
               := fun () ->
                    Eio.Promise.resolve callback_ready_u ();
                    Eio.Promise.await release_callback;
                    Eio.Promise.resolve callback_returned_u ());
              Eio.Switch.run (fun sw ->
                Exn.protect
                  ~finally:(fun () ->
                    release release_callback release_callback_u;
                    release release_actor release_actor_u)
                  ~f:(fun () ->
                    Eio.Fiber.fork ~sw (fun () -> results := [ safe_run () ]);
                    Eio.Promise.await callback_ready;
                    Eio.Fiber.fork ~sw (fun () ->
                      A.checkpoint actor ~persist:(fun checkpoint ->
                        let child =
                          List.find_exn checkpoint.invocations ~f:(fun invocation ->
                            I.equal_origin invocation.context.origin Moderator)
                        in
                        (match child.observation with
                         | Some { status = Observed; _ } -> ()
                         | Some _ | None ->
                           failwith "late stop preceded observation acknowledgement");
                        Eio.Promise.resolve actor_blocked_u ();
                        Eio.Promise.await release_actor;
                        Ok ())
                      |> protocol_ok;
                      checkpoint_finished := true);
                    Eio.Promise.await actor_blocked;
                    Eio.Promise.resolve release_callback_u ();
                    Eio.Promise.await callback_returned;
                    (* Resolving these signals does not yield. The observer queues
                       normal Finish before this controller resumes; the stop fiber
                       queues priority Stop before this controller releases the actor. *)
                    Eio.Fiber.fork ~sw (fun () ->
                      Eio.Promise.resolve stop_requested_u ();
                      A.stop actor ~attachment_id:writer.id ~mode:Cancel
                      |> protocol_ok
                      |> ignore;
                      stop_finished := true);
                    Eio.Promise.await stop_requested;
                    Eio.Promise.resolve release_actor_u ()));
              assert !checkpoint_finished;
              assert !stop_finished;
              A.set_runtime_worker actor ~worker:None ~inference:None |> protocol_ok
            | _ -> results := [ safe_run () ]);
           let state = A.state actor |> protocol_ok in
           let child =
             List.find_exn state.invocations ~f:(fun invocation ->
               I.equal_origin invocation.context.origin Moderator)
           in
           let observation = Option.value_exn child.observation in
           let count =
             match
               (M.identity_snapshot manager |> Result.ok_or_failwith).current_state
             with
             | Session.Snapshot.Array [ Int count ] -> count
             | _ -> assert false
           in
           assert (Option.is_none state.active_operation);
           assert (
             I.equal_status child.status (Resolved (Complete (`String "native result"))));
           assert (
             List.equal
               Agent_protocol.History.equal_entry
               initial.conversation.canonical_history
               state.conversation.canonical_history);
           assert (
             Option.equal
               Jsonaf.exactly_equal
               state.moderator
               (Some
                  (Agent_session.Runtime_builder.encode_moderator_snapshot
                     (M.identity_snapshot manager |> Result.ok_or_failwith))));
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           let mode =
             match mode with
             | `Success -> "success"
             | `Request -> "request"
             | `End -> "end"
             | `Concurrent -> "concurrent"
             | `Reentrant -> "reentrant"
             | `Handler_fail -> "handler failure"
             | `Claim_rejected -> "claim rejected"
             | `Ack_rejected -> "ack rejected"
             | `Cancelled -> "cancelled"
             | `Stopped -> "stopped"
             | `Stop_cancel -> "cancel stop"
             | `Graceful_then_cancel -> "graceful then cancel"
             | `Late_stop -> "stop after return"
           in
           print_s
             [%sexp
               { mode : string
               ; callbacks = (!calls : int)
               ; state = (count : int)
               ; errors = (List.count !results ~f:Result.is_error : int)
               ; observation = (observation.status : I.observation_status)
               ; follow_up = (observation.follow_up : I.follow_up_status option)
               }];
           (* The legacy borrow is available again even after callback failure. *)
           match mode with
           | "stopped" | "cancel stop" | "graceful then cancel" | "stop after return" ->
             assert (Option.is_none (A.claim_idle_moderator actor |> protocol_ok))
           | _ ->
             assert (Option.is_some (A.claim_idle_moderator actor |> protocol_ok));
             A.complete_idle_moderator
               actor
               { moderator_snapshot = state.moderator
               ; runtime_requests = []
               ; notifications = []
               ; remaining_events = false
               }
             |> protocol_ok));
  [%expect
    {|
    ((mode success) (callbacks 1) (state 1) (errors 0) (observation Observed)
     (follow_up ()))
    ((mode request) (callbacks 1) (state 1) (errors 0) (observation Observed)
     (follow_up
      ((Pending_follow_up
        ((request_turn true) (request_compaction false) (end_session ()))))))
    ((mode end) (callbacks 1) (state 1) (errors 0) (observation Observed)
     (follow_up
      ((Pending_follow_up
        ((request_turn false) (request_compaction false) (end_session (done)))))))
    ((mode concurrent) (callbacks 1) (state 1) (errors 0) (observation Observed)
     (follow_up ()))
    ((mode reentrant) (callbacks 1) (state 1) (errors 0) (observation Observed)
     (follow_up ()))
    ((mode "handler failure") (callbacks 1) (state 0) (errors 1)
     (observation
      (Observation_failed "observation handler failed before acknowledgement"))
     (follow_up ()))
    ((mode "claim rejected") (callbacks 0) (state 0) (errors 1)
     (observation Awaiting) (follow_up ()))
    ((mode "ack rejected") (callbacks 1) (state 0) (errors 1)
     (observation
      (Observation_failed "observation handler failed before acknowledgement"))
     (follow_up ()))
    ((mode cancelled) (callbacks 1) (state 0) (errors 1)
     (observation
      (Observation_failed "observation handler cancelled before acknowledgement"))
     (follow_up ()))
    ((mode stopped) (callbacks 1) (state 0) (errors 1)
     (observation
      (Observation_failed "observation handler failed before acknowledgement"))
     (follow_up ()))
    ((mode "cancel stop") (callbacks 1) (state 0) (errors 1)
     (observation
      (Observation_failed "observation handler cancelled before acknowledgement"))
     (follow_up ()))
    ((mode "graceful then cancel") (callbacks 1) (state 0) (errors 1)
     (observation
      (Observation_failed "observation handler cancelled before acknowledgement"))
     (follow_up ()))
    ((mode "stop after return") (callbacks 1) (state 1) (errors 0)
     (observation Observed) (follow_up ()))
    |}]
;;

let%test_unit
    "claimed observations commit moderator state atomically and never replay tools"
  =
  List.iter
    [ `Success
    ; `Handler_fail
    ; `Raise
    ; `Cancelled
    ; `Claim_rejected
    ; `Ack_rejected
    ; `Wrong_source
    ; `Wrong_snapshot
    ; `Resolve_again
    ; `Concurrent
    ; `After_commit_error
    ]
    ~f:(fun mode ->
      let module I = Agent_protocol.Invocation in
      let module M = Chat_response.Moderator_manager in
      let native_calls = ref 0
      and observer_calls = ref 0
      and rejected = ref false in
      let succeeded =
        match mode with
        | `Success | `Concurrent | `After_commit_error -> true
        | _ -> false
      in
      with_handoff_actor
        ~reject:(fun next ->
          let matches =
            List.exists
              next.Agent_session.Session_transition.state.invocations
              ~f:(fun invocation ->
                match mode, invocation.observation with
                | `Claim_rejected, Some { status = Observing; _ }
                | `Ack_rejected, Some { status = Observed; _ } -> true
                | _ -> false)
          in
          if matches && not !rejected
          then (
            rejected := true;
            true)
          else false)
        ~make_worker:(fun env actor_ready ->
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let finish =
              match mode with
              | `Handler_fail -> "Task.fail(\"observer failed\")"
              | `Resolve_again ->
                "Task.bind(Invocation.resolve(p.invocation_id, `Complete(`Null)), fun \
                 ignored -> Task.pure(state))"
              | _ -> "Task.pure(state)"
            in
            let manager, _, definition =
              handoff_definition
                env
                ~events:
                  ("| `Tool_observed(p) -> let ignored = state[0] <- state[0] + 1 in "
                   ^ "Task.bind(Tool.call(\"observe\", p.outcome), fun ignored -> "
                   ^ "Task.bind(Runtime.emit(p.outcome), fun ignored -> "
                   ^ finish
                   ^ ")) | _ -> Task.pure(state)")
            in
            let script =
              Chat_response.Extension_compiler.script
                (List.hd_exn (Chat_response.Extension_compiler.prepared_tools definition))
            in
            let initial_snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
            caps.commit_moderator
              (Some
                 (Agent_session.Runtime_builder.encode_moderator_snapshot
                    initial_snapshot))
            |> protocol_ok;
            let parent = invocation_fixture () in
            let child =
              I.create
                ~observer:
                  { script_id = script.id
                  ; source_sha256 =
                      (match mode with
                       | `Wrong_source -> String.make 64 'f'
                       | _ -> script.source_sha256)
                  }
                { parent.context with
                  id = Agent_protocol.Id.Invocation.create ()
                ; origin = Moderator
                ; parent_invocation = Some parent.context.id
                }
              |> protocol_ok
            in
            caps.with_invocation ~invocation:parent (fun ~dispatched:_ ~capabilities:_ ->
              caps.with_invocation ~invocation:child (fun ~dispatched:_ ~capabilities:_ ->
                Int.incr native_calls;
                Ok (Complete (`String "native result")))
              |> protocol_ok
              |> ignore;
              assert (
                Result.is_error
                  (caps.with_moderator_observation
                     ~invocation_id:child.context.id
                     (fun ~observing:_ ~capabilities:_ ~commit:_ -> assert false)));
              Ok (Complete `Null))
            |> protocol_ok
            |> ignore;
            let escaped = ref None in
            let cancellation = ref None in
            let run () =
              caps.with_moderator_observation
                ~invocation_id:child.context.id
                (fun ~observing ~capabilities:_ ~commit ->
                   let state = Agent_session.Session_actor.state actor |> protocol_ok in
                   assert (List.mem state.invocations observing ~equal:I.equal);
                   assert (
                     Result.is_error
                       (caps.with_moderator_observation
                          ~invocation_id:child.context.id
                          (fun ~observing:_ ~capabilities:_ ~commit:_ -> assert false)));
                   let result =
                     M.handle_observation_entries
                       manager
                       ~invocation:observing
                       ~history:input.history
                       ~available_tools:[]
                       ~session_meta:`Null
                       ~now_ms:0
                       ~on_tool_call:(fun ~name ~args ->
                         [%test_eq: string] "observe" name;
                         assert (
                           Jsonaf.exactly_equal
                             args
                             (I.outcome_to_json (Complete (`String "native result"))));
                         Int.incr observer_calls;
                         Eio.Fiber.yield ();
                         match mode with
                         | `Raise -> failwith "observer external helper raised"
                         | `Cancelled ->
                           Eio.Cancel.cancel (Option.value_exn !cancellation) Exit;
                           Eio.Fiber.yield ();
                           assert false
                         | _ -> Ok (Tool_ok `Null))
                       ~prepare_observation:(fun ~observed ~outcome:_ ~snapshot ->
                         let snapshot =
                           match mode with
                           | `Wrong_snapshot -> { snapshot with script_id = "foreign" }
                           | _ -> snapshot
                         in
                         let save () = commit ~resolved:observed ~snapshot in
                         escaped := Some save;
                         Ok
                           { M.persist =
                               (fun () ->
                                 save ()
                                 |> Result.map_error ~f:(fun e ->
                                   e.Agent_protocol.Error.message))
                           ; install = ignore
                           })
                     |> Result.map_error ~f:handoff_error
                   in
                   match result, mode with
                   | Ok _, `After_commit_error ->
                     Error (handoff_error "after acknowledgement")
                   | Ok _, _ -> Ok ()
                   | Error e, _ -> Error e)
            in
            let safe_run () =
              let execute () =
                match mode with
                | `Cancelled ->
                  Eio.Cancel.sub (fun context ->
                    cancellation := Some context;
                    run ())
                | _ -> run ()
              in
              match execute () with
              | result -> result
              | exception Failure message
                when String.equal message "observer external helper raised" ->
                Error (handoff_error "observer raised")
              | exception Eio.Cancel.Cancelled _ ->
                Error (handoff_error "observer cancelled")
            in
            (match mode with
             | `Concurrent ->
               let a = ref None
               and b = ref None in
               Eio.Fiber.both
                 (fun () -> a := Some (safe_run ()))
                 (fun () -> b := Some (safe_run ()));
               assert (
                 Bool.equal
                   (Result.is_ok (Option.value_exn !a))
                   (Result.is_error (Option.value_exn !b)))
             | _ ->
               let result = safe_run () in
               assert (
                 Bool.equal
                   (Result.is_ok result)
                   (match mode with
                    | `Success -> true
                    | _ -> false)));
            Option.iter !escaped ~f:(fun save -> assert (Result.is_error (save ())));
            (match mode with
             | `Claim_rejected -> ()
             | _ -> assert (Result.is_error (safe_run ())));
            let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
            (match snapshot.current_state with
             | Session.Snapshot.Array [ Int value ] ->
               [%test_eq: int] (if succeeded then 1 else 0) value
             | _ -> assert false);
            let state = Agent_session.Session_actor.state actor |> protocol_ok in
            assert (
              Option.equal
                Jsonaf.exactly_equal
                state.moderator
                (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)));
            Completed
              { final_history = input.history
              ; runtime_requests = []
              ; moderator_snapshot = state.moderator
              }))
        (fun _env actor _writer backend ->
           let state = await_idle actor in
           [%test_eq: int] 1 !native_calls;
           [%test_eq: int]
             (match mode with
              | `Claim_rejected | `Wrong_source -> 0
              | _ -> 1)
             !observer_calls;
           let child =
             List.find_exn state.invocations ~f:(fun invocation ->
               I.equal_origin invocation.context.origin Moderator)
           in
           assert (
             I.equal_status child.status (Resolved (Complete (`String "native result"))));
           (match child.observation with
            | Some { status = Observed; _ } when succeeded -> ()
            | Some { status = Awaiting; _ } ->
              assert (
                match mode with
                | `Claim_rejected | `Wrong_source -> true
                | _ -> false)
            | Some { status = Observation_failed _; _ } when not succeeded -> ()
            | _ -> assert false);
           [%test_eq: int] 1 (List.length state.conversation.canonical_history);
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend)))
;;

let%test_unit "cancellation during an idle observation claim releases its runtime borrow" =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let observer : I.observer =
    { script_id = "handoff"; source_sha256 = String.make 64 'a' }
  in
  let child_id =
    Agent_protocol.Id.Invocation.of_string "inv_cancelled_idle_claim" |> protocol_ok
  in
  let claim_entered, claim_entered_u = Eio.Promise.create () in
  let release_claim, release_claim_u = Eio.Promise.create () in
  let armed = ref false in
  let cancellation = ref None in
  let cancelled = ref false in
  let callback_entered = ref false in
  let original = Failure "idle observation cancelled during protected claim" in
  with_handoff_actor
    ~reject:(fun next ->
      let observing =
        List.exists
          next.Agent_session.Session_transition.state.invocations
          ~f:(fun invocation ->
            Agent_protocol.Id.Invocation.equal invocation.context.id child_id
            &&
            match invocation.Agent_protocol.Invocation.observation with
            | Some { status = Observing; _ } -> true
            | Some _ | None -> false)
      in
      if !armed && observing
      then (
        armed := false;
        Eio.Promise.resolve claim_entered_u ();
        Eio.Promise.await release_claim);
      false)
    ~make_worker:(fun _env _actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let snapshot =
          Some
            (Agent_session.Runtime_builder.encode_moderator_snapshot
               { (handoff_snapshot 0) with script_source_hash = observer.source_sha256 })
        in
        caps.commit_moderator snapshot |> protocol_ok;
        let parent = invocation_fixture () in
        caps.with_invocation ~invocation:parent (fun ~dispatched:_ ~capabilities:_ ->
          let child =
            I.create
              ~observer
              { parent.context with
                id = child_id
              ; origin = Moderator
              ; parent_invocation = Some parent.context.id
              }
            |> protocol_ok
          in
          caps.with_invocation ~invocation:child (fun ~dispatched:_ ~capabilities:_ ->
            Ok (Complete (`String "recorded native result")))
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
    (fun _env actor _writer backend ->
       let initial = await_idle actor in
       let child =
         match
           List.find initial.invocations ~f:(fun invocation ->
             Agent_protocol.Id.Invocation.equal invocation.context.id child_id)
         with
         | Some child -> child
         | None -> failwith "idle claim fixture did not prepare its native child"
       in
       assert (
         I.equal_status
           child.status
           (Resolved (Complete (`String "recorded native result"))));
       (match child.observation with
        | Some { status = Awaiting; observer = actual; _ } ->
          assert (I.equal_observer observer actual)
        | other ->
          raise_s
            [%sexp
              "idle claim fixture observation is not ready"
            , (other : I.observation option)]);
       armed := true;
       Eio.Fiber.both
         (fun () ->
            match
              Eio.Cancel.sub (fun context ->
                cancellation := Some context;
                A.with_idle_moderator_observation
                  actor
                  ~observer
                  (fun ~observing:_ ~capabilities:_ ~commit:_ ->
                     callback_entered := true;
                     failwith "cancelled observation callback entered"))
            with
            | Ok returned ->
              raise_s
                [%sexp
                  "observation claim returned instead of cancelling"
                , (returned : bool)
                , (!armed : bool)]
            | Error error ->
              raise_s
                [%sexp
                  "observation claim failed before cancellation"
                , (error : Agent_protocol.Error.t)
                , (!armed : bool)]
            | exception Eio.Cancel.Cancelled reason ->
              assert (phys_equal reason original);
              cancelled := true)
         (fun () ->
            Exn.protect
              ~finally:(fun () ->
                if not (Eio.Promise.is_resolved release_claim)
                then Eio.Promise.resolve release_claim_u ())
              ~f:(fun () ->
                Eio.Promise.await claim_entered;
                Eio.Cancel.cancel (Option.value_exn !cancellation) original));
       assert !cancelled;
       assert (not !armed);
       assert (not !callback_entered);
       (* The protected claim must be paired with cleanup even when cancellation
         precedes entry into the callback's local cancellation sub-context. *)
       A.set_runtime_worker actor ~worker:None ~inference:None |> protocol_ok;
       let state = A.state actor |> protocol_ok in
       let child =
         List.find_exn state.invocations ~f:(fun invocation ->
           I.equal_origin invocation.context.origin Moderator)
       in
       assert (
         I.equal_status
           child.status
           (Resolved (Complete (`String "recorded native result"))));
       (match child.observation with
        | Some { status = Observation_failed _; _ } -> ()
        | Some _ | None -> failwith "cancelled observation claim was not retired");
       assert_same_session_snapshot state (Agent_session.Memory_backend.state backend))
;;

let%expect_test "observation follow-up keeps every original through final input admission"
  =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let module P = Agent_protocol in
  let module M = Chat_response.Moderator_manager in
  let module N = Agent_session.Native_tool_invocation in
  let learned_final_clock = ref None in
  List.iter
    [ `Mixed_turn; `Mixed_compaction; `Adopted_input_baseline; `Adopted_input ]
    ~f:(fun mode ->
      let a_live = Atomic.make true
      and b_live = Atomic.make true in
      let armed = ref false
      and input_selected = ref false
      and revoked = ref false
      and b_checked_before_input = ref false
      and b_checked_after_revocation = ref false
      and samples = ref 0
      and turn_writes = ref 0 in
      let a_original = Agent_authority_tests.authentication a_live in
      let b_original = Agent_authority_tests.authentication b_live in
      let a =
        Operator_authorization.guarded
          ~principal:(Operator_authorization.principal a_original)
          ~is_current:(fun () ->
            if !armed then input_selected := true;
            Operator_authorization.is_current a_original)
      in
      let b =
        Operator_authorization.guarded
          ~principal:(Operator_authorization.principal b_original)
          ~is_current:(fun () ->
            if !armed
            then
              if !revoked
              then b_checked_after_revocation := true
              else if not !input_selected
              then b_checked_before_input := true;
            Operator_authorization.is_current b_original)
      in
      let now () =
        if !armed
        then (
          Int.incr samples;
          match mode, !learned_final_clock with
          | `Adopted_input, Some final when Int.equal final !samples ->
            revoked := true;
            Atomic.set b_live false
          | ( (`Mixed_turn | `Mixed_compaction | `Adopted_input_baseline | `Adopted_input)
            , (None | Some _) ) -> ());
        timestamp
      in
      Job_fixtures.with_actor
        ~now
        ~reject_save:(fun transition ->
          let starts_turn =
            List.exists
              transition.Agent_session.Session_transition.events
              ~f:(fun event ->
                match
                  P.Event.Durable.Payload.of_json
                    ~kind:event.P.Event.Durable.kind
                    event.payload
                  |> protocol_ok
                with
                | Operation_started { kind = Turn User_submit; _ } -> true
                | _ -> false)
          in
          if !armed && starts_turn
          then (
            Int.incr turn_writes;
            match mode with
            | `Adopted_input_baseline -> learned_final_clock := Some !samples
            | `Mixed_turn | `Mixed_compaction | `Adopted_input -> ());
          false)
        ~monotonic_now:(fun () -> Mtime.min_stamp)
        (fun env _sw actor writer backend ->
           let native_calls = ref 0 in
           let registry = native_registry native_calls ~raises:false in
           let manager, _, _ =
             handoff_definition env ~capability_registry:registry ~declare_tool:false
           in
           let observer = M.invocation_observer manager |> Option.value_exn in
           let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
           let encoded =
             Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)
           in
           let produced = ref []
           and workers = ref 0 in
           let follow_up_worker = ref false in
           let worker =
             Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
               caps.commit_moderator encoded |> protocol_ok;
               if !follow_up_worker
               then (
                 let proof =
                   caps.original_agent_actor () |> protocol_ok |> Option.value_exn
                 in
                 assert (Operator_authorization.is_current proof);
                 Atomic.set a_live false;
                 assert (not (Operator_authorization.is_current proof));
                 Atomic.set a_live true;
                 assert (Operator_authorization.is_current proof))
               else (
                 let parent =
                   I.create
                     { (invocation_fixture ()).context with
                       id = P.Id.Invocation.create ()
                     ; tool_name = "producer"
                     }
                   |> protocol_ok
                 in
                 caps.with_invocation
                   ~invocation:parent
                   (fun ~dispatched:_ ~capabilities:_ ->
                      let reference =
                        List.hd_exn (Chat_response.Tool_capability.references registry)
                      in
                      let invocation =
                        I.create
                          ~observer
                          { parent.context with
                            id = P.Id.Invocation.create ()
                          ; origin = Moderator
                          ; parent_invocation = Some parent.context.id
                          ; tool_name = "read_file"
                          ; input = `Object []
                          ; implementation_revision = reference.implementation_revision
                          ; capability_fingerprint =
                              Chat_response.Tool_capability.fingerprint registry
                          }
                        |> protocol_ok
                      in
                      let result =
                        N.run_scoped
                          ~execute:caps.with_invocation
                          ~registry:(fun () -> registry)
                          ~reference
                          ~invocation
                          ~is_halted:(fun () -> false)
                          ~authorize:(fun _ _ -> Ok ())
                          ~prepare_output:(fun _ -> Ok (`String "native result"))
                        |> protocol_ok
                      in
                      produced := result :: !produced;
                      Ok (Complete `Null))
                 |> protocol_ok
                 |> ignore);
               Int.incr workers;
               Completed
                 { final_history = input.history
                 ; moderator_snapshot = encoded
                 ; runtime_requests = []
                 })
           in
           A.set_operation_worker actor (Some worker) |> protocol_ok;
           let entry text =
             let reservation = A.reserve_history_block actor ~count:1 |> protocol_ok in
             Pending_input_tests.occurrence
               (Int64.to_int_exn reservation.first_sequence)
               text
           in
           let produce original =
             let entry = entry "native producer" in
             let count = !workers in
             let before_events = (A.state actor |> protocol_ok).counters.event_sequence in
             let submitted =
               A.submit_message_authenticated
                 actor
                 ~actor:original
                 ~attachment_id:writer.id
                 entry
               |> protocol_ok
             in
             let operation_id = Option.value_exn submitted.operation_id in
             await_idle actor |> ignore;
             [%test_eq: int] (count + 1) !workers;
             assert (
               List.exists
                 (Agent_session.Memory_backend.events_after backend before_events
                  |> protocol_ok)
                 ~f:(fun event ->
                   match
                     P.Event.Durable.Payload.of_json
                       ~kind:event.P.Event.Durable.kind
                       event.payload
                     |> protocol_ok
                   with
                   | Operation_completed { id; state = Completed; _ } ->
                     P.Id.Operation.equal id operation_id
                   | _ -> false));
             (List.hd_exn !produced).context.id
           in
           let owners =
             match mode with
             | `Mixed_turn | `Mixed_compaction ->
               let first = produce a in
               let second = produce b in
               [ first, a_live; second, b_live ]
             | `Adopted_input_baseline | `Adopted_input -> [ produce b, b_live ]
           in
           let deferred =
             match mode with
             | `Mixed_turn | `Mixed_compaction -> None
             | `Adopted_input_baseline | `Adopted_input -> Some (entry "input owned by A")
           in
           let follow_up : I.follow_up =
             { request_turn =
                 (match mode with
                  | `Mixed_turn | `Adopted_input_baseline | `Adopted_input -> true
                  | `Mixed_compaction -> false)
             ; request_compaction =
                 (match mode with
                  | `Mixed_compaction -> true
                  | `Mixed_turn | `Adopted_input_baseline | `Adopted_input -> false)
             ; end_session = None
             }
           in
           List.iter owners ~f:(fun _ ->
             let completed = ref false
             and escaped = ref None in
             let ran =
               A.with_idle_moderator_observation
                 actor
                 ~observer
                 (fun ~observing ~capabilities ~commit ->
                    let _, original_live =
                      List.find_exn owners ~f:(fun (id, _) ->
                        P.Id.Invocation.equal id observing.context.id)
                    in
                    let proof =
                      capabilities.borrow_agent_actor ~invocation:observing
                      |> protocol_ok
                      |> Option.value_exn
                    in
                    assert (Operator_authorization.is_current proof);
                    Atomic.set original_live false;
                    assert (not (Operator_authorization.is_current proof));
                    Atomic.set original_live true;
                    assert (Operator_authorization.is_current proof);
                    escaped := Some proof;
                    Option.iter deferred ~f:(fun entry ->
                      let submitted =
                        A.submit_message_authenticated
                          actor
                          ~actor:a
                          ~attachment_id:writer.id
                          entry
                        |> protocol_ok
                      in
                      match submitted.disposition with
                      | Deferred -> ()
                      | Started -> failwith "observation did not retain deferred A input");
                    commit
                      ~resolved:
                        (I.complete_observation ~follow_up observing |> protocol_ok)
                      ~snapshot
                    |> protocol_ok;
                    completed := true;
                    Ok ())
               |> protocol_ok
             in
             assert ran;
             assert !completed;
             assert (not (Operator_authorization.is_current (Option.value_exn !escaped))));
           let before = A.state actor |> protocol_ok in
           let workers_before = !workers in
           (match mode with
            | `Mixed_turn | `Mixed_compaction -> ()
            | `Adopted_input_baseline | `Adopted_input ->
              [%test_eq: int] 1 (List.length before.conversation.deferred_user_entries);
              follow_up_worker := true;
              armed := true);
           (* The successful identical topology records the actual last wall sample
            at real Turn persistence, after every guard. The fresh negative case
            revokes at that learned ordinal, rather than a hard-coded clock count. *)
           let result = A.apply_moderator_follow_up actor in
           armed := false;
           (match mode, result with
            | `Adopted_input_baseline, Ok true ->
              [%test_eq: int] 1 !turn_writes;
              assert (Option.exists !learned_final_clock ~f:(fun ordinal -> ordinal > 0));
              let after = await_idle actor in
              [%test_eq: int] (workers_before + 1) !workers;
              assert (
                List.exists
                  (Agent_session.Memory_backend.events_after
                     backend
                     before.counters.event_sequence
                   |> protocol_ok)
                  ~f:(fun event ->
                    match
                      P.Event.Durable.Payload.of_json
                        ~kind:event.P.Event.Durable.kind
                        event.payload
                      |> protocol_ok
                    with
                    | Operation_completed
                        { kind = Turn User_submit; state = Completed; _ } -> true
                    | _ -> false));
              assert !b_checked_before_input;
              assert !input_selected;
              assert (not !revoked);
              assert (Operator_authorization.is_current b);
              assert (List.is_empty after.conversation.deferred_user_entries);
              assert (Option.is_none after.active_operation);
              assert_same_session_snapshot
                after
                (Agent_session.Memory_backend.state backend)
            | ( ((`Mixed_turn | `Mixed_compaction | `Adopted_input) as denied_mode)
              , Error error ) ->
              assert (P.Error.equal_code error.code Permission_denied);
              let expected =
                match denied_mode with
                | `Mixed_turn | `Mixed_compaction ->
                  "observation follow-up has missing or mixed original admissions"
                | `Adopted_input ->
                  "original agent execution authority is no longer current"
              in
              if not (String.equal error.message expected)
              then raise_s [%sexp (error : P.Error.t)];
              [%test_eq: int] 0 !turn_writes;
              (match denied_mode with
               | `Mixed_turn | `Mixed_compaction ->
                 assert (not !input_selected);
                 assert (not !revoked);
                 assert (Operator_authorization.is_current b)
               | `Adopted_input ->
                 assert !b_checked_before_input;
                 assert !input_selected;
                 assert !revoked;
                 assert !b_checked_after_revocation;
                 assert (not (Operator_authorization.is_current b)));
              [%test_eq: int] workers_before !workers;
              let after = A.state actor |> protocol_ok in
              assert (Option.is_none after.active_operation);
              assert_same_session_snapshot before after;
              assert_same_session_snapshot
                after
                (Agent_session.Memory_backend.state backend)
            | ( ( `Mixed_turn
                | `Mixed_compaction
                | `Adopted_input_baseline
                | `Adopted_input )
              , unexpected ) ->
              raise_s
                [%sexp
                  "unexpected follow-up result"
                , (mode
                   : [ `Mixed_turn
                     | `Mixed_compaction
                     | `Adopted_input_baseline
                     | `Adopted_input
                     ])
                , (unexpected : (bool, P.Error.t) result)]);
           assert (Operator_authorization.is_current a)));
  print_endline
    "mixed A/B Turn and Compact deny; adopted input A cannot replace observation B after \
     final clock revocation";
  [%expect
    {| mixed A/B Turn and Compact deny; adopted input A cannot replace observation B after final clock revocation |}]
;;

let%expect_test
    "historical observation absence settles results without admitting new work"
  =
  let module A = Agent_session in
  let module I = Agent_protocol.Invocation in
  let module P = Agent_protocol in
  let module M = Chat_response.Moderator_manager in
  with_actor_workspace (fun env workspace_instance ->
    let manager, _, _ = handoff_definition env ~declare_tool:false in
    let observer = M.invocation_observer manager |> Option.value_exn in
    let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
    let parent = invocation_fixture () in
    let child =
      I.create
        ~observer
        { parent.context with
          id = P.Id.Invocation.create ()
        ; parent_invocation = Some parent.context.id
        ; origin = Moderator
        }
      |> protocol_ok
    in
    let resolved invocation outcome =
      I.dispatch invocation
      |> protocol_ok
      |> fun dispatched ->
      I.resolve dispatched ~session_id ~generation:0 outcome |> protocol_ok
    in
    let child = resolved child (Complete (`String "historical native result")) in
    let parent = resolved parent (Complete `Null) in
    (* Imported records pass through the actual storage document decoder. Their
       historical absence supplies no process-local continuation or original. *)
    let prepare_state state =
      let state =
        { state with
          A.Session_state.moderator =
            Some (A.Runtime_builder.encode_moderator_snapshot snapshot)
        ; invocations = [ child; parent ]
        }
      in
      A.Session_state_document.authored state
      |> A.Session_state_document.encode ~limits:document_limits
      |> document_ok
      |> A.Session_state_document.decode ~limits:document_limits
      |> document_ok
      |> A.Session_state_document.value
    in
    Job_fixtures.with_actor
      ~environment:(env, workspace_instance)
      ~prepare_state
      (fun _env _sw actor _writer backend ->
         let workers = ref 0 in
         let worker =
           A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
             Int.incr workers;
             Completed
               { final_history = input.history
               ; moderator_snapshot =
                   Some (A.Runtime_builder.encode_moderator_snapshot snapshot)
               ; runtime_requests = []
               })
         in
         A.Session_actor.set_operation_worker actor (Some worker) |> protocol_ok;
         let callback_completed = ref false in
         let ran =
           A.Session_actor.with_idle_moderator_observation
             actor
             ~observer
             (fun ~observing ~capabilities ~commit ->
                assert (Option.is_none observing.inference_origin);
                let before = A.Session_actor.state actor |> protocol_ok in
                let denied result =
                  match result with
                  | Error error ->
                    assert (P.Error.equal_code error.P.Error.code Permission_denied)
                  | Ok _ -> failwith "historical absence became an execution admission"
                in
                denied (capabilities.borrow_agent_actor ~invocation:observing);
                denied
                  (A.Model_job_admission.admit
                     capabilities.model_jobs
                     (Inference_target_tests.job before));
                assert_same_session_snapshot
                  before
                  (A.Session_actor.state actor |> protocol_ok);
                let follow_up : I.follow_up =
                  { request_turn = true; request_compaction = false; end_session = None }
                in
                commit
                  ~resolved:(I.complete_observation ~follow_up observing |> protocol_ok)
                  ~snapshot
                |> protocol_ok;
                callback_completed := true;
                Ok ())
           |> protocol_ok
         in
         assert ran;
         assert !callback_completed;
         let before = A.Session_actor.state actor |> protocol_ok in
         let settled =
           List.find_exn before.invocations ~f:(fun invocation ->
             P.Id.Invocation.equal invocation.context.id child.context.id)
         in
         assert (I.equal_status child.status settled.status);
         assert (Option.is_none settled.inference_origin);
         assert (
           I.equal_observation_status
             (Option.value_exn settled.observation).status
             Observed);
         (match A.Session_actor.apply_moderator_follow_up actor with
          | Error error ->
            assert (P.Error.equal_code error.code Permission_denied);
            assert (
              String.equal
                error.message
                "original observation admission is unavailable or expired")
          | Ok _ -> failwith "historical observation requested fresh legacy work");
         [%test_eq: int] 0 !workers;
         assert (List.is_empty before.jobs);
         assert (Option.is_none before.active_operation);
         assert_same_session_snapshot before (A.Session_actor.state actor |> protocol_ok);
         assert_same_session_snapshot before (A.Memory_backend.state backend)));
  print_endline
    "decoded historical absence remains unavailable; native result and observation \
     settle, Model and Turn deny";
  [%expect
    {| decoded historical absence remains unavailable; native result and observation settle, Model and Turn deny |}]
;;
