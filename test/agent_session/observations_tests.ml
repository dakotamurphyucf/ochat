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
          caps.with_invocation ~invocation:parent (fun ~dispatched:_ ->
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
              caps.with_invocation ~invocation:child (fun ~dispatched:_ ->
                Int.incr native_calls;
                Ok (Complete (`Number (Int.to_string index))))
              |> protocol_ok
              |> ignore);
            assert (
              not
                (caps.with_next_moderator_observation
                   ~observer
                   (fun ~observing:_ ~commit:_ -> assert false)
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
            caps.with_invocation ~invocation:parent (fun ~dispatched:_ ->
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
              caps.with_invocation ~invocation:child (fun ~dispatched:_ ->
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
           let run () =
             Agent_session.Moderator_observation.drain_idle
               ~claim:(A.with_idle_moderator_observation actor ~observer)
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
                           (fun ~observing:_ ~commit:_ -> assert false)))
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
           | "stopped" | "cancel stop" | "graceful then cancel" ->
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
            caps.with_invocation ~invocation:parent (fun ~dispatched:_ ->
              caps.with_invocation ~invocation:child (fun ~dispatched:_ ->
                Int.incr native_calls;
                Ok (Complete (`String "native result")))
              |> protocol_ok
              |> ignore;
              assert (
                Result.is_error
                  (caps.with_moderator_observation
                     ~invocation_id:child.context.id
                     (fun ~observing:_ ~commit:_ -> assert false)));
              Ok (Complete `Null))
            |> protocol_ok
            |> ignore;
            let escaped = ref None in
            let cancellation = ref None in
            let run () =
              caps.with_moderator_observation
                ~invocation_id:child.context.id
                (fun ~observing ~commit ->
                   let state = Agent_session.Session_actor.state actor |> protocol_ok in
                   assert (List.mem state.invocations observing ~equal:I.equal);
                   assert (
                     Result.is_error
                       (caps.with_moderator_observation
                          ~invocation_id:child.context.id
                          (fun ~observing:_ ~commit:_ -> assert false)));
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
