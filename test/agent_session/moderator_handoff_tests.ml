open Core
open Fixtures

let%test_unit "actor handoff persists actual manager state and resolution atomically" =
  let done_, done_u = Eio.Promise.create () in
  let reject_once = ref true in
  let reject transition =
    if
      !reject_once
      && List.exists
           transition.Agent_session.Session_transition.state.invocations
           ~f:(fun invocation ->
             match invocation.Agent_protocol.Invocation.status with
             | Resolved (Complete _) -> true
             | _ -> false)
    then (
      reject_once := false;
      true)
    else false
  in
  with_handoff_actor
    ~reject
    ~make_worker:(fun env actor_ready ->
      let manager, invocation = handoff_manager env in
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let invoke () =
          caps.with_moderator_invocation
            ~invocation:(invocation ())
            (fun ~dispatched ~commit ->
               Chat_response.Moderator_manager.handle_invocation_entries
                 manager
                 ~invocation:dispatched
                 ~history:input.history
                 ~available_tools:[]
                 ~session_meta:`Null
                 ~now_ms:0
                 ~validate_work:(fun _ -> Error "no pending work")
                 ~prepare_resolution:(fun ~resolved ~outcome:_ ~snapshot ->
                   Ok
                     { Chat_response.Moderator_manager.persist =
                         (fun () ->
                           commit ~resolved ~snapshot
                           |> Result.map_error ~f:(fun e ->
                             e.Agent_protocol.Error.message))
                     ; install = ignore
                     })
               |> Result.map ~f:(fun _ -> ())
               |> Result.map_error ~f:handoff_error)
        in
        assert (Result.is_error (invoke ()));
        let failed = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (Option.is_none failed.moderator);
        assert (
          match (List.hd_exn failed.invocations).status with
          | Resolved (Fail _) -> true
          | _ -> false);
        let snapshot =
          Chat_response.Moderator_manager.identity_snapshot manager
          |> Result.ok_or_failwith
        in
        assert (Poly.equal snapshot.current_state (Session.Snapshot.Array [ Int 0 ]));
        assert (List.is_empty snapshot.queued_internal_events);
        invoke () |> protocol_ok;
        let snapshot =
          Chat_response.Moderator_manager.identity_snapshot manager
          |> Result.ok_or_failwith
        in
        let encoded =
          Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)
        in
        assert (Poly.equal snapshot.current_state (Session.Snapshot.Array [ Int 1 ]));
        assert (List.length snapshot.queued_internal_events = 1);
        let saved = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (Poly.equal saved.moderator encoded);
        Eio.Promise.resolve done_u encoded;
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = encoded
          }))
    (fun _env actor _writer backend ->
       let expected = Eio.Promise.await done_ in
       let final = await_idle actor in
       let saved = Agent_session.Memory_backend.state backend in
       assert (Poly.equal saved.moderator expected);
       assert (Poly.equal final.moderator expected);
       assert (List.length saved.invocations = 2);
       assert (List.length saved.conversation.canonical_history = 1);
       let restored =
         Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t saved))
         |> store_ok
       in
       assert (Poly.equal restored.moderator expected);
       assert (List.length restored.invocations = 2))
;;

let%test_unit
    "active moderator borrow rejects reentrancy and stale saved commit callbacks"
  =
  let done_, done_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let saved_commit = ref None in
        let saved_resolution = ref None in
        let invocation = invocation_fixture () in
        caps.with_moderator_invocation ~invocation (fun ~dispatched ~commit ->
          saved_commit := Some commit;
          (* Actor reads and unrelated mailbox work continue during the borrow. *)
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          assert (List.length state.invocations = 1);
          let nested =
            Agent_protocol.Invocation.create
              { invocation.context with id = Agent_protocol.Id.Invocation.create () }
            |> protocol_ok
          in
          let ran = ref false in
          assert (
            Result.is_error
              (caps.with_moderator_invocation
                 ~invocation:nested
                 (fun ~dispatched:_ ~commit:_ ->
                    ran := true;
                    Ok ())));
          assert (not !ran);
          assert (Result.is_error (caps.commit_moderator None));
          assert (
            Result.is_error (Agent_session.Session_actor.change_moderator actor None));
          assert (
            Result.is_error (Agent_session.Session_actor.set_operation_worker actor None));
          assert (
            Result.is_error
              (Agent_session.Session_actor.commit_extensions
                 actor
                 ~generation:0
                 ~expected_revision:state.counters.revision
                 [ Moderator_state None ]));
          let resolved =
            Agent_protocol.Invocation.resolve
              dispatched
              ~session_id:input.session_id
              ~generation:input.session_generation
              (Complete `Null)
            |> protocol_ok
          in
          saved_resolution := Some resolved;
          commit ~resolved ~snapshot:(handoff_snapshot 1) |> protocol_ok;
          assert (Result.is_error (commit ~resolved ~snapshot:(handoff_snapshot 2)));
          Ok ())
        |> protocol_ok;
        let commit = Option.value_exn !saved_commit in
        assert (
          Result.is_error
            (commit
               ~resolved:(Option.value_exn !saved_resolution)
               ~snapshot:(handoff_snapshot 3)));
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor _writer _backend ->
       Eio.Promise.await done_;
       ignore (await_idle actor))
;;

let%test_unit
    "cancelled worker releases its borrow and persists cancellation without state"
  =
  let entered, entered_u = Eio.Promise.create () in
  let never, _ = Eio.Promise.create () in
  let stale_commit = ref None in
  let stale_resolution = ref None in
  with_handoff_actor
    ~make_worker:(fun _env _actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        caps.with_moderator_invocation
          ~invocation:(invocation_fixture ())
          (fun ~dispatched ~commit ->
             stale_commit := Some commit;
             stale_resolution
             := Some
                  (Agent_protocol.Invocation.resolve
                     dispatched
                     ~session_id:input.session_id
                     ~generation:input.session_generation
                     (Complete `Null)
                   |> protocol_ok);
             Eio.Promise.resolve entered_u ();
             Eio.Promise.await never)
        |> protocol_ok;
        failwith "cancelled handler returned"))
    (fun _env actor writer backend ->
       Eio.Promise.await entered;
       let before = Agent_session.Session_actor.state actor |> protocol_ok in
       assert (
         match (List.hd_exn before.invocations).status with
         | Dispatching -> true
         | _ -> false);
       Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
       |> protocol_ok
       |> ignore;
       let rec await_stopped () =
         let state = Agent_session.Session_actor.state actor |> protocol_ok in
         if Option.is_none state.active_operation
         then state
         else (
           Eio.Fiber.yield ();
           await_stopped ())
       in
       let after = await_stopped () in
       assert (Option.is_none after.moderator);
       assert (
         match (List.hd_exn after.invocations).status with
         | Resolved (Cancelled _) -> true
         | _ -> false);
       let commit = Option.value_exn !stale_commit in
       assert (
         Result.is_error
           (commit
              ~resolved:(Option.value_exn !stale_resolution)
              ~snapshot:(handoff_snapshot 1)));
       assert (Option.is_none (Agent_session.Memory_backend.state backend).moderator))
;;

let%test_unit "cancellation after the atomic commit preserves its recorded outcome" =
  let committed, committed_u = Eio.Promise.create () in
  let never, _ = Eio.Promise.create () in
  let snapshot = handoff_snapshot 7 in
  with_handoff_actor
    ~make_worker:(fun _env _actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        caps.with_moderator_invocation
          ~invocation:(invocation_fixture ())
          (fun ~dispatched ~commit ->
             let resolved =
               Agent_protocol.Invocation.resolve
                 dispatched
                 ~session_id:input.session_id
                 ~generation:input.session_generation
                 (Complete (`String "already saved"))
               |> protocol_ok
             in
             commit ~resolved ~snapshot |> protocol_ok;
             Eio.Promise.resolve committed_u ();
             Eio.Promise.await never)
        |> protocol_ok;
        failwith "cancelled worker returned"))
    (fun _env actor writer backend ->
       Eio.Promise.await committed;
       Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
       |> protocol_ok
       |> ignore;
       let rec finished () =
         let state = Agent_session.Session_actor.state actor |> protocol_ok in
         if Option.is_some state.active_operation
         then (
           Eio.Fiber.yield ();
           finished ())
         else state
       in
       let state = finished () in
       assert (
         match (List.hd_exn state.invocations).status with
         | Resolved (Complete (`String "already saved")) -> true
         | _ -> false);
       assert (
         Poly.equal
           state.moderator
           (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)));
       assert (
         Poly.equal state.moderator (Agent_session.Memory_backend.state backend).moderator))
;;

let%test_unit "independent worker calls queue before actor admission" =
  let done_, done_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let first_held, first_held_u = Eio.Promise.create () in
        let release, release_u = Eio.Promise.create () in
        let attempted, attempted_u = Eio.Promise.create () in
        let second_entered = ref false in
        let fresh () =
          Agent_protocol.Invocation.create
            { (invocation_fixture ()).context with
              id = Agent_protocol.Id.Invocation.create ()
            }
          |> protocol_ok
        in
        let run ~invocation count wait =
          caps.with_moderator_invocation ~invocation (fun ~dispatched ~commit ->
            wait ();
            let resolved =
              Agent_protocol.Invocation.resolve
                dispatched
                ~session_id:input.session_id
                ~generation:input.session_generation
                (Complete `Null)
              |> protocol_ok
            in
            commit ~resolved ~snapshot:(handoff_snapshot count))
          |> protocol_ok
        in
        Eio.Switch.run (fun sw ->
          Eio.Fiber.fork ~sw (fun () ->
            run ~invocation:(fresh ()) 1 (fun () ->
              Eio.Promise.resolve first_held_u ();
              Eio.Promise.await release));
          Eio.Promise.await first_held;
          let second = fresh () in
          Eio.Fiber.fork ~sw (fun () ->
            Eio.Promise.resolve attempted_u ();
            run ~invocation:second 2 (fun () -> second_entered := true));
          Eio.Promise.await attempted;
          Eio.Fiber.yield ();
          let while_queued = Agent_session.Session_actor.state actor |> protocol_ok in
          assert (List.length while_queued.invocations = 1);
          assert (not !second_entered);
          Eio.Promise.resolve release_u ());
        assert !second_entered;
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (List.length state.invocations = 2);
        assert (
          List.for_all state.invocations ~f:(fun inv ->
            match inv.status with
            | Resolved (Complete _) -> true
            | _ -> false));
        assert (
          Poly.equal
            state.moderator
            (Some
               (Agent_session.Runtime_builder.encode_moderator_snapshot
                  (handoff_snapshot 2))));
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor _writer _backend ->
       Eio.Promise.await done_;
       ignore (await_idle actor))
;;

let%test_unit "failed admission and handler errors cannot strand a moderator borrow" =
  let done_, done_u = Eio.Promise.create () in
  let reject_once = ref true in
  let reject next =
    if
      !reject_once
      && not (List.is_empty next.Agent_session.Session_transition.state.invocations)
    then (
      reject_once := false;
      true)
    else false
  in
  with_handoff_actor
    ~reject
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let fresh () =
          Agent_protocol.Invocation.create
            { (invocation_fixture ()).context with
              id = Agent_protocol.Id.Invocation.create ()
            }
          |> protocol_ok
        in
        let admitted = fresh () in
        let ran = ref false in
        assert (
          Result.is_error
            (caps.with_moderator_invocation
               ~invocation:admitted
               (fun ~dispatched:_ ~commit:_ ->
                  ran := true;
                  Ok ())));
        assert (not !ran);
        assert (
          List.is_empty
            (Agent_session.Session_actor.state actor |> protocol_ok).invocations);
        (* An admission with no effects/commit may be retried with its original ID. *)
        assert (
          Result.is_error
            (caps.with_moderator_invocation
               ~invocation:admitted
               (fun ~dispatched:_ ~commit:_ -> Ok ())));
        List.iter
          [ ""; String.make 20_000 'x' ]
          ~f:(fun message ->
            assert (
              Result.is_error
                (caps.with_moderator_invocation
                   ~invocation:(fresh ())
                   (fun ~dispatched:_ ~commit:_ -> Error (handoff_error message)))));
        (match
           caps.with_moderator_invocation
             ~invocation:(fresh ())
             (fun ~dispatched:_ ~commit:_ -> raise Exit)
         with
         | _ -> assert false
         | exception Exit -> ());
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (List.length state.invocations = 4);
        assert (
          List.for_all state.invocations ~f:(fun invocation ->
            match invocation.status with
            | Resolved (Fail _) -> true
            | _ -> false));
        caps.with_moderator_invocation ~invocation:(fresh ()) (fun ~dispatched ~commit ->
          let resolved =
            Agent_protocol.Invocation.resolve
              dispatched
              ~session_id:input.session_id
              ~generation:input.session_generation
              (Complete `Null)
            |> protocol_ok
          in
          commit ~resolved ~snapshot:(handoff_snapshot 1))
        |> protocol_ok;
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor _writer _backend ->
       Eio.Promise.await done_;
       ignore (await_idle actor))
;;

let%test_unit "graceful stop lets an admitted moderator invocation commit" =
  let entered, entered_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let done_, done_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env _actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        caps.with_moderator_invocation
          ~invocation:(invocation_fixture ())
          (fun ~dispatched ~commit ->
             Eio.Promise.resolve entered_u ();
             Eio.Promise.await release;
             let resolved =
               Agent_protocol.Invocation.resolve
                 dispatched
                 ~session_id:input.session_id
                 ~generation:input.session_generation
                 (Complete `Null)
               |> protocol_ok
             in
             commit ~resolved ~snapshot:(handoff_snapshot 1))
        |> protocol_ok;
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot =
              Some
                (Agent_session.Runtime_builder.encode_moderator_snapshot
                   (handoff_snapshot 1))
          }))
    (fun _env actor writer _backend ->
       Eio.Promise.await entered;
       Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Graceful
       |> protocol_ok
       |> ignore;
       Eio.Promise.resolve release_u ();
       Eio.Promise.await done_;
       let rec finished () =
         let state = Agent_session.Session_actor.state actor |> protocol_ok in
         if Option.is_some state.active_operation
         then (
           Eio.Fiber.yield ();
           finished ())
         else state
       in
       let state = finished () in
       assert (
         match state.lifecycle.observed with
         | Stopped -> true
         | _ -> false);
       assert (
         match (List.hd_exn state.invocations).status with
         | Resolved (Complete _) -> true
         | _ -> false);
       assert (Option.is_some state.moderator))
;;
