open Core
open Fixtures

let%test_unit
    "invocation recovery preserves results, repairs pairs and never replays work"
  =
  List.iter [ false; true ] ~f:(fun custom ->
    List.iter
      [ `Admitted
      ; `Dispatching
      ; `Resolved
      ; `Cancelled
      ; `Existing
      ; `Published
      ; `Removed
      ; `Old_generation
      ; `Bad_output
      ; `Reused_id
      ; `Collision
      ]
      ~f:(fun mode ->
        with_actor_workspace (fun _env workspace_instance ->
          let module I = Agent_protocol.Invocation in
          let module D = Agent_session.Session_delta in
          let initial =
            actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
          in
          let id n =
            History_entry.Id.create ~namespace:"recover" ~sequence:n
            |> Result.ok_or_failwith
          in
          let call_item =
            if custom
            then
              Openai.Responses.Item.Custom_tool_call
                { name = "read_file"
                ; input = "null"
                ; call_id = "same"
                ; _type = "custom_tool_call"
                ; id = None
                }
            else
              Function_call
                { name = "read_file"
                ; arguments = "null"
                ; call_id = "same"
                ; _type = "function_call"
                ; id = None
                ; status = None
                }
          in
          let call =
            History_entry.create_with_id ~id:(id 0) call_item
            |> Agent_session.History_codec.to_protocol
          in
          let admitted =
            I.create
              { (invocation_fixture ()).context with
                origin = Model
              ; provider_call_id = Some "same"
              ; call_entry_id = Some (id 0)
              }
            |> protocol_ok
          in
          let dispatched = I.dispatch admitted |> protocol_ok in
          let resolved =
            I.resolve dispatched ~session_id ~generation:0 (Complete (`String "saved"))
            |> protocol_ok
          in
          let output =
            Openai.Responses.Tool_output.Output.Text
              (if Poly.equal mode `Bad_output
               then "wrong"
               else Jsonaf.to_string (I.outcome_to_json (Complete (`String "saved"))))
          in
          let output_item =
            if custom
            then
              Openai.Responses.Item.Custom_tool_call_output
                { output; call_id = "same"; _type = "custom_tool_call_output"; id = None }
            else
              Function_call_output
                { output
                ; call_id = "same"
                ; _type = "function_call_output"
                ; id = None
                ; status = None
                }
          in
          let output =
            History_entry.create_with_id ~id:(id 1) output_item
            |> Agent_session.History_codec.to_protocol
          in
          let invocation =
            match mode with
            | `Admitted -> admitted
            | `Dispatching -> dispatched
            | `Cancelled -> I.cancel dispatched ~reason:"already cancelled" |> protocol_ok
            | `Published ->
              I.publish_with_history resolved ~output_entry_id:(id 1) |> protocol_ok
            | _ -> resolved
          in
          let history =
            match mode with
            | `Removed -> []
            | `Existing | `Bad_output | `Published -> [ call; output ]
            | `Reused_id ->
              [ call
              ; History_entry.create_with_id ~id:(id 1) call_item
                |> Agent_session.History_codec.to_protocol
              ]
            | `Collision ->
              let other =
                match call_item with
                | Openai.Responses.Item.Function_call c ->
                  Openai.Responses.Item.Function_call { c with call_id = "other" }
                | Custom_tool_call c -> Custom_tool_call { c with call_id = "other" }
                | _ -> assert false
              in
              [ call
              ; History_entry.create_with_id ~id:(id 8) other
                |> Agent_session.History_codec.to_protocol
              ]
            | _ -> [ call ]
          in
          let state =
            { initial with
              invocations = [ invocation ]
            ; identity =
                { initial.identity with
                  generation = (if Poly.equal mode `Old_generation then 1 else 0)
                }
            ; conversation =
                { initial.conversation with
                  canonical_history = history
                ; next_history_sequence = 8L
                ; reserved_history_through = 8L
                }
            }
          in
          Agent_session.Session_state.validate state |> protocol_ok;
          let plan state =
            Agent_session.Invocation_recovery.plan
              ~state
              ~namespace:"recover"
              ~first_sequence:(Int64.to_int_exn state.conversation.next_history_sequence)
              ~reason:"restart"
          in
          if
            Poly.equal mode `Bad_output
            || Poly.equal mode `Reused_id
            || Poly.equal mode `Collision
          then assert (Result.is_error (plan state))
          else (
            List.iter [ false; true ] ~f:(fun keep_history ->
              let candidate =
                Agent_session.Administration.reset
                  state
                  { keep_history
                  ; keep_tasks = false
                  ; keep_grants = false
                  ; keep_labels = true
                  ; workspace_instance = None
                  }
                |> protocol_ok
              in
              let administrative =
                Agent_session.Administration.archive ~previous:state candidate Reset
                |> protocol_ok
              in
              Agent_session.Session_state.validate administrative |> protocol_ok;
              let archive = List.hd_exn administrative.conversation.compaction_archives in
              assert (List.is_empty administrative.invocations);
              assert (
                List.length archive.invocation_dispositions
                = if Poly.equal mode `Published then 0 else 1);
              if not keep_history
              then assert (List.is_empty administrative.conversation.canonical_history);
              List.iter archive.invocation_dispositions ~f:(fun disposition ->
                assert (
                  Agent_protocol.Id.Invocation.compare
                    disposition.invocation_id
                    invocation.context.id
                  = 0);
                if keep_history && not (Poly.equal mode `Removed)
                then assert (Option.is_some disposition.output_entry_id)
                else assert (Option.is_some disposition.publication_discarded));
              let restored_admin =
                Agent_session.Session_persistence.restore_snapshot
                  (Sexp.to_string_mach
                     (Agent_session.Session_state.sexp_of_t administrative))
                |> store_ok
              in
              assert (
                Sexp.equal
                  (Agent_session.Session_state.sexp_of_t administrative)
                  (Agent_session.Session_state.sexp_of_t restored_admin)));
            let result = plan state |> protocol_ok in
            let delta =
              D.Batch
                (History_block_reserved (Int64.of_int result.next_sequence)
                 :: result.deltas)
            in
            let delta = D.t_of_sexp (D.sexp_of_t delta) in
            let restored = D.apply state delta |> protocol_ok in
            Agent_session.Session_state.validate restored |> protocol_ok;
            let restored =
              Agent_session.Session_persistence.restore_snapshot
                (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t restored))
              |> store_ok
            in
            let actual = List.hd_exn restored.invocations in
            (match mode, actual.status with
             | (`Admitted | `Dispatching), Published (Cancelled "restart") -> ()
             | `Cancelled, Published (Cancelled "already cancelled") -> ()
             | `Removed, Resolved (Complete (`String "saved")) ->
               assert (Option.is_some actual.publication_discarded)
             | _, Published (Complete (`String "saved")) -> ()
             | _ -> assert false);
            let reused = Poly.equal mode `Existing || Poly.equal mode `Published in
            assert (
              List.length result.appended
              = if reused || Poly.equal mode `Removed then 0 else 1);
            if reused
            then
              assert (
                Option.equal History_entry.Id.equal actual.output_entry_id (Some (id 1)));
            if (not reused) && not (Poly.equal mode `Removed)
            then
              assert (
                Option.equal History_entry.Id.equal actual.output_entry_id (Some (id 8)));
            let again = plan restored |> protocol_ok in
            assert (List.is_empty again.deltas && List.is_empty again.appended);
            assert (again.next_sequence = result.next_sequence);
            assert (
              Result.is_error
                (Agent_session.Invocation_recovery.plan
                   ~state:restored
                   ~namespace:"recover"
                   ~first_sequence:0
                   ~reason:"restart"));
            if Poly.equal mode `Removed
            then (
              assert (Result.is_error (I.publish actual));
              assert (
                Result.is_error
                  (Agent_session.Session_state.validate
                     { restored with
                       conversation =
                         { restored.conversation with canonical_history = [ call ] }
                     })))))))
;;

let%test_unit
    "recovery does not fabricate provider outputs for scripts or unbound legacy calls"
  =
  with_actor_workspace (fun _env workspace_instance ->
    List.iter [ false; true ] ~f:(fun model ->
      List.iter [ false; true ] ~f:(fun resolved ->
        let module I = Agent_protocol.Invocation in
        let initial =
          actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
        in
        let invocation =
          I.create
            { (invocation_fixture ()).context with
              origin = (if model then Model else Script)
            ; provider_call_id = (if model then Some "legacy" else None)
            }
          |> protocol_ok
          |> I.dispatch
          |> protocol_ok
        in
        let invocation =
          if resolved
          then
            I.resolve invocation ~session_id ~generation:0 (Complete `Null) |> protocol_ok
          else invocation
        in
        let state = { initial with invocations = [ invocation ] } in
        let plan =
          Agent_session.Invocation_recovery.plan
            ~state
            ~namespace:"unbound"
            ~first_sequence:(Int64.to_int_exn state.conversation.next_history_sequence)
            ~reason:"restart"
          |> protocol_ok
        in
        assert (List.is_empty plan.appended);
        let result =
          Agent_session.Session_delta.apply state (Batch plan.deltas) |> protocol_ok
        in
        Agent_session.Session_state.validate result |> protocol_ok;
        let actual = List.hd_exn result.invocations in
        assert (Bool.equal (Option.is_some actual.publication_discarded) model);
        assert (Option.is_none actual.output_entry_id);
        match resolved, actual.status with
        | true, Resolved (Complete `Null) | false, Resolved (Cancelled "restart") -> ()
        | _ -> assert false)))
;;

let%test_unit
    "restart preserves waiting observations and fails interrupted handling without replay"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let module I = Agent_protocol.Invocation in
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let observer : I.observer =
      { script_id = "moderator"; source_sha256 = String.make 64 'a' }
    in
    let admitted =
      I.create
        ~observer
        { (invocation_fixture ()).context with
          origin = Moderator
        ; provider_call_id = None
        ; parent_invocation = Some (Agent_protocol.Id.Invocation.create ())
        }
      |> protocol_ok
    in
    let dispatched = I.dispatch admitted |> protocol_ok in
    let resolved =
      I.resolve dispatched ~session_id ~generation:0 (Complete (`String "saved"))
      |> protocol_ok
    in
    let observing = I.claim_observation resolved |> protocol_ok in
    let observed = I.complete_observation observing |> protocol_ok in
    let pending_actions =
      I.complete_observation
        observing
        ~follow_up:{ request_turn = true; request_compaction = true; end_session = None }
      |> protocol_ok
    in
    let applied_actions = I.apply_observation_follow_up pending_actions |> protocol_ok in
    let failed = I.fail_observation observing ~reason:"handler failed" |> protocol_ok in
    let published = I.publish observing |> protocol_ok in
    List.iter
      [ admitted
      ; dispatched
      ; resolved
      ; observing
      ; observed
      ; failed
      ; published
      ; pending_actions
      ; applied_actions
      ]
      ~f:(fun invocation ->
        let state = { initial with invocations = [ invocation ] } in
        let plan state =
          Agent_session.Invocation_recovery.plan
            ~state
            ~namespace:"observation-restart"
            ~first_sequence:(Int64.to_int_exn state.conversation.next_history_sequence)
            ~reason:"restart"
          |> protocol_ok
        in
        let recovery = plan state in
        assert (List.is_empty recovery.appended);
        let repaired =
          Agent_session.Session_delta.apply state (Batch recovery.deltas) |> protocol_ok
        in
        Agent_session.Session_state.validate repaired |> protocol_ok;
        let actual = List.hd_exn repaired.invocations in
        (match invocation.status, actual.status with
         | (Admitted | Dispatching), Resolved (Cancelled "restart") -> ()
         | old, current -> assert (I.equal_status old current));
        (match invocation.observation, actual.observation with
         | ( Some { status = Observing; observer = old }
           , Some { status = Observation_failed _; observer = current } ) ->
           assert (I.equal_observer old current);
           assert (Result.is_error (I.claim_observation actual))
         | old, current -> assert (Option.equal I.equal_observation old current));
        assert (List.is_empty (plan repaired).deltas);
        (* Recovery may fail interrupted handling, but must never claim successful execution. *)
        assert (
          Result.is_error
            (Agent_session.Session_delta.apply state (Invocation_reconciled observed)))))
;;

let%test_unit "foreground recovery leaves script and background invocations running" =
  with_actor_workspace (fun _env workspace_instance ->
    let module I = Agent_protocol.Invocation in
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let make origin parent_job =
      I.create
        { (invocation_fixture ()).context with
          id = Agent_protocol.Id.Invocation.create ()
        ; origin
        ; provider_call_id = (if I.equal_origin origin Model then Some "legacy" else None)
        ; parent_job
        }
      |> protocol_ok
      |> I.dispatch
      |> protocol_ok
    in
    let foreground = make Model None in
    let script = make Script None in
    let background = make Model (Some (Agent_protocol.Id.Job.create ())) in
    let state = { initial with invocations = [ script; foreground; background ] } in
    let plan =
      Agent_session.Invocation_recovery.plan_foreground
        ~state
        ~namespace:"foreground"
        ~first_sequence:(Int64.to_int_exn state.conversation.next_history_sequence)
        ~reason:"worker stopped"
      |> protocol_ok
    in
    let repaired =
      Agent_session.Session_delta.apply state (Batch plan.deltas) |> protocol_ok
    in
    assert (List.is_empty plan.appended);
    List.iter [ script; background ] ~f:(fun invocation ->
      assert (List.mem repaired.invocations invocation ~equal:Poly.equal));
    let actual =
      List.find_exn repaired.invocations ~f:(fun invocation ->
        Agent_protocol.Id.Invocation.compare invocation.context.id foreground.context.id
        = 0)
    in
    assert (Poly.equal actual.status (Resolved (Cancelled "worker stopped")));
    assert (Option.is_some actual.publication_discarded);
    let again =
      Agent_session.Invocation_recovery.plan_foreground
        ~state:repaired
        ~namespace:"foreground"
        ~first_sequence:plan.next_sequence
        ~reason:"worker stopped"
      |> protocol_ok
    in
    assert (List.is_empty again.deltas))
;;

let%test_unit
    "model call intent and history commit atomically and retries preserve outcomes"
  =
  let reject = ref true in
  with_handoff_actor
    ~reject:(fun next ->
      if
        !reject
        && not (List.is_empty next.Agent_session.Session_transition.state.invocations)
      then (
        reject := false;
        true)
      else false)
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let call, invocation = publication_call caps () in
        let before = Agent_session.Session_actor.state actor |> protocol_ok in
        let save () = caps.commit_invocation_call ~invocation call in
        assert (Result.is_error (save ()));
        let failed = Agent_session.Session_actor.state actor |> protocol_ok in
        assert_same_session_snapshot before failed;
        Eio.Fiber.both
          (fun () -> save () |> protocol_ok)
          (fun () -> save () |> protocol_ok);
        let admitted = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (List.length admitted.conversation.canonical_history = 2);
        assert (List.length admitted.invocations = 1);
        (match (List.hd_exn admitted.invocations).status with
         | Admitted -> ()
         | _ -> assert false);
        assert (
          Int64.equal admitted.counters.revision Int64.(before.counters.revision + 1L));
        let changed =
          Agent_protocol.Invocation.create
            { invocation.context with input = `String "different" }
          |> protocol_ok
        in
        assert (Result.is_error (caps.commit_invocation_call ~invocation:changed call));
        let competing =
          Agent_protocol.Invocation.create
            { invocation.context with id = Agent_protocol.Id.Invocation.create () }
          |> protocol_ok
        in
        assert (Result.is_error (caps.commit_invocation_call ~invocation:competing call));
        resolve_publication caps invocation |> protocol_ok;
        let output = publication_output caps () in
        caps.publish_invocation_output ~invocation_id:invocation.context.id output
        |> protocol_ok;
        let published = Agent_session.Session_actor.state actor |> protocol_ok in
        save () |> protocol_ok;
        let retried = Agent_session.Session_actor.state actor |> protocol_ok in
        assert_same_session_snapshot published retried;
        Completed
          { final_history = input.history @ [ call; output ]
          ; runtime_requests = []
          ; moderator_snapshot = published.moderator
          }))
    (fun _env actor _writer backend ->
       let state = await_idle actor in
       assert (List.length state.conversation.canonical_history = 3);
       assert (List.length state.invocations = 1);
       assert_same_session_snapshot state (Agent_session.Memory_backend.state backend))
;;

let%test_unit
    "invocation publication saves history and receipt atomically and retries only once"
  =
  let done_, done_u = Eio.Promise.create () in
  let reject_publication = ref true in
  let stale_publish = ref None in
  with_handoff_actor
    ~reject:(fun next ->
      let publishing =
        List.exists next.Agent_session.Session_transition.state.invocations ~f:(fun i ->
          Option.is_some i.output_entry_id)
      in
      if publishing && !reject_publication
      then (
        reject_publication := false;
        true)
      else false)
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let call, invocation = publication_call caps () in
        (* Binding a nonexistent call must not invoke the handler. *)
        assert (Result.is_error (resolve_publication caps invocation));
        caps.commit_entry call |> protocol_ok;
        let output = publication_output caps () in
        let publish entry =
          caps.publish_invocation_output ~invocation_id:invocation.context.id entry
        in
        assert (Result.is_error (publish output));
        resolve_publication caps invocation |> protocol_ok;
        let duplicate =
          Agent_protocol.Invocation.create
            { invocation.context with id = Agent_protocol.Id.Invocation.create () }
          |> protocol_ok
        in
        assert (Result.is_error (resolve_publication caps duplicate));
        let wrong = publication_output caps ~text:"wrong" () in
        assert (Result.is_error (publish wrong));
        let wrong_kind = publication_output caps ~custom:true () in
        assert (Result.is_error (publish wrong_kind));
        let before = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (Result.is_error (publish output));
        let failed = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (
          Sexp.equal
            (Agent_session.Session_state.sexp_of_t before)
            (Agent_session.Session_state.sexp_of_t failed));
        Eio.Fiber.both
          (fun () -> publish output |> protocol_ok)
          (fun () -> publish output |> protocol_ok);
        let published = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (
          Int64.equal published.counters.revision Int64.(before.counters.revision + 1L));
        assert (List.length published.conversation.canonical_history = 3);
        assert (
          Option.equal
            History_entry.Id.equal
            (List.hd_exn published.invocations).output_entry_id
            (Some (History_entry.id output)));
        assert (Result.is_error (publish (publication_output caps ())));
        assert (
          Result.is_error
            (publish (History_entry.with_item output (History_entry.item wrong))));
        let restored =
          Agent_session.Session_persistence.restore_snapshot
            (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t published))
          |> store_ok
        in
        assert (Poly.equal restored.invocations published.invocations);
        (* History compaction retains the receipt and permits no new publication identity. *)
        let compacted =
          Agent_session.Session_delta.apply restored (Canonical_history_replaced [])
          |> protocol_ok
        in
        let repeated =
          Agent_session.Session_delta.apply
            compacted
            (Invocation_changed (List.hd_exn published.invocations))
          |> protocol_ok
        in
        assert (List.is_empty repeated.conversation.canonical_history);
        Agent_session.Session_state.validate repeated |> protocol_ok;
        stale_publish := Some (fun () -> publish output);
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = input.history @ [ call; output ]
          ; runtime_requests = []
          ; moderator_snapshot = published.moderator
          }))
    (fun _env actor _writer backend ->
       Eio.Promise.await done_;
       ignore (await_idle actor);
       assert (Result.is_error ((Option.value_exn !stale_publish) ()));
       let saved = Agent_session.Memory_backend.state backend in
       assert (List.length saved.conversation.canonical_history = 3);
       assert (List.length saved.invocations = 1))
;;

let%test_unit
    "publication binds repeated provider IDs to their exact function or custom call"
  =
  let done_, done_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let history = ref input.history in
        List.iter [ false; true; false ] ~f:(fun custom ->
          let call, invocation = publication_call caps ~custom () in
          caps.commit_entry call |> protocol_ok;
          resolve_publication caps invocation |> protocol_ok;
          let output = publication_output caps ~custom () in
          caps.publish_invocation_output ~invocation_id:invocation.context.id output
          |> protocol_ok;
          history := !history @ [ call; output ]);
        let old_call, old_invocation = publication_call caps () in
        caps.commit_entry old_call |> protocol_ok;
        let next_call, _ = publication_call caps () in
        caps.commit_entry next_call |> protocol_ok;
        assert (Result.is_error (resolve_publication caps old_invocation));
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (List.length state.invocations = 3);
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = !history @ [ old_call; next_call ]
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor _writer _backend ->
       Eio.Promise.await done_;
       ignore (await_idle actor))
;;

let%test_unit
    "cancelled publishers cannot append late results and committed receipts survive \
     cancellation"
  =
  List.iter [ false; true ] ~f:(fun publish_first ->
    let ready, ready_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    with_handoff_actor
      ~make_worker:(fun _env _actor_ready ->
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
          let call, invocation = publication_call caps () in
          caps.commit_entry call |> protocol_ok;
          resolve_publication caps invocation |> protocol_ok;
          let output = publication_output caps () in
          let publish () =
            caps.publish_invocation_output ~invocation_id:invocation.context.id output
          in
          if publish_first then publish () |> protocol_ok;
          Eio.Promise.resolve ready_u publish;
          Eio.Promise.await never))
      (fun _env actor writer backend ->
         let publish = Eio.Promise.await ready in
         Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
         |> protocol_ok
         |> ignore;
         assert (Result.is_error (publish ()));
         let rec finished () =
           let s = Agent_session.Session_actor.state actor |> protocol_ok in
           if Option.is_none s.active_operation
           then s
           else (
             Eio.Fiber.yield ();
             finished ())
         in
         let state = finished () in
         assert (List.length state.conversation.canonical_history = 3);
         let invocation = List.hd_exn state.invocations in
         assert (Option.is_some invocation.output_entry_id);
         assert (
           match invocation.status with
           | Published (Complete (`String "done")) -> true
           | _ -> false);
         assert (
           Poly.equal
             state.invocations
             (Agent_session.Memory_backend.state backend).invocations)))
;;

let%test_unit "worker cancellation publishes an interrupted handler result exactly once" =
  List.iter [ false; true ] ~f:(fun custom ->
    let ready, ready_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let calls = ref 0 in
    with_handoff_actor
      ~make_worker:(fun _env _actor_ready ->
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
          let call, invocation = publication_call caps ~custom () in
          caps.commit_entry call |> protocol_ok;
          ignore
            (caps.with_moderator_invocation ~invocation (fun ~dispatched:_ ~commit:_ ->
               Int.incr calls;
               Eio.Promise.resolve ready_u ();
               Eio.Promise.await never)
             : (unit, Agent_protocol.Error.t) result);
          assert false))
      (fun _env actor writer backend ->
         Eio.Promise.await ready;
         let running = Agent_session.Session_actor.state actor |> protocol_ok in
         assert (Option.is_some running.active_operation);
         Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
         |> protocol_ok
         |> ignore;
         let rec finished () =
           let state = Agent_session.Session_actor.state actor |> protocol_ok in
           if Option.is_none state.active_operation
           then state
           else (
             Eio.Fiber.yield ();
             finished ())
         in
         let state = finished () in
         let invocation = List.hd_exn state.invocations in
         assert (!calls = 1);
         assert (
           match invocation.status with
           | Published (Cancelled _) -> true
           | _ -> false);
         assert (List.length state.conversation.canonical_history = 3);
         ignore
           (Agent_session.Invocation_history.recover_output
              ~history:state.conversation.canonical_history
              invocation
            |> protocol_ok);
         let events =
           Agent_session.Memory_backend.events_after backend 0L |> protocol_ok
         in
         assert (
           List.count events ~f:(fun event ->
             Agent_protocol.Event.Durable.equal_kind event.kind Operation_cancelled)
           = 1);
         assert (Poly.equal state (Agent_session.Memory_backend.state backend))))
;;

let%test_unit "worker failure repairs a transient publication failure without replay" =
  let publication_attempts = ref 0 in
  let handler_calls = ref 0 in
  with_handoff_actor
    ~reject:(fun next ->
      if
        List.exists
          next.Agent_session.Session_transition.state.invocations
          ~f:(fun invocation -> Option.is_some invocation.output_entry_id)
      then (
        Int.incr publication_attempts;
        !publication_attempts = 1)
      else false)
    ~make_worker:(fun _env _actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
        let call, invocation = publication_call caps () in
        caps.commit_entry call |> protocol_ok;
        Int.incr handler_calls;
        resolve_publication caps invocation |> protocol_ok;
        match
          caps.publish_invocation_output
            ~invocation_id:invocation.context.id
            (publication_output caps ())
        with
        | Ok () -> assert false
        | Error failure -> Failed failure))
    (fun _env actor _writer backend ->
       let state = await_idle actor in
       assert (!handler_calls = 1 && !publication_attempts = 2);
       assert (Option.is_none state.failure);
       assert (List.length state.conversation.canonical_history = 3);
       let invocation = List.hd_exn state.invocations in
       assert (Poly.equal invocation.status (Published (Complete (`String "done"))));
       ignore
         (Agent_session.Invocation_history.recover_output
            ~history:state.conversation.canonical_history
            invocation
          |> protocol_ok);
       let events = Agent_session.Memory_backend.events_after backend 0L |> protocol_ok in
       assert (
         List.count events ~f:(fun event ->
           Agent_protocol.Event.Durable.equal_kind event.kind Operation_failed)
         = 1);
       assert (Poly.equal state (Agent_session.Memory_backend.state backend)))
;;

let%test_unit
    "native invocation policy and disclosure are shared by model and script calls"
  =
  List.iter [ false; true ] ~f:(fun model ->
    List.iter
      [ `Success; `Deny; `Revoke; `Replace; `Input; `Raise; `Disclosure; `Output ]
      ~f:(fun mode ->
        let calls = ref 0
        and authorized = ref 0
        and disclosed = ref 0 in
        let registry = ref (native_registry calls ~raises:(Poly.equal mode `Raise)) in
        let stale = ref None in
        with_handoff_actor
          ~make_worker:(fun _env actor_ready ->
            Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
              let actor = Eio.Promise.await actor_ready in
              let call, invocation =
                if model
                then (
                  let call, invocation = publication_call caps () in
                  caps.commit_entry call |> protocol_ok;
                  Some call, invocation)
                else None, invocation_fixture ()
              in
              let reference, invocation = native_context !registry invocation in
              let invocation =
                if Poly.equal mode `Input
                then
                  Agent_protocol.Invocation.create
                    { invocation.context with input = `Null }
                  |> protocol_ok
                else invocation
              in
              let run () =
                Agent_session.Native_tool_invocation.run
                  ~is_halted:(fun () -> false)
                  ~capabilities:caps
                  ~registry:(fun () -> !registry)
                  ~reference
                  ~invocation
                  ~authorize:(fun dispatched _binding ->
                    assert (Poly.equal dispatched.status Dispatching);
                    Int.incr authorized;
                    Eio.Fiber.yield ();
                    (match mode with
                     | `Revoke ->
                       registry
                       := Chat_response.Tool_capability.select !registry ~names:[]
                          |> Result.map_error ~f:(fun error ->
                            error.Chat_response.Tool_capability.message)
                          |> Result.ok_or_failwith
                     | `Replace -> registry := native_registry calls ~raises:false
                     | _ -> ());
                    if Poly.equal mode `Deny
                    then Error (handoff_error "private denial")
                    else Ok ())
                  ~prepare_output:(fun _ ->
                    Int.incr disclosed;
                    if Poly.equal mode `Disclosure
                    then Error (handoff_error "private disclosure diagnostic")
                    else if Poly.equal mode `Output
                    then Ok (`Object [ "duplicate", `Null; "duplicate", `Null ])
                    else Ok (`String "disclosed"))
              in
              let recorded = run () |> protocol_ok in
              stale := Some run;
              let expected =
                match mode with
                | `Success -> None
                | `Deny -> Some "invocation.permission_denied"
                | `Revoke | `Replace -> Some "invocation.stale_binding"
                | `Input -> Some "invocation.invalid_input"
                | `Raise -> Some "invocation.handler_failed"
                | `Disclosure -> Some "invocation.disclosure_rejected"
                | `Output -> Some "invocation.invalid_output"
              in
              let outcome =
                match recorded.status, expected with
                | Resolved (Complete (`String "disclosed") as outcome), None -> outcome
                | Resolved (Fail error as outcome), Some code ->
                  assert (String.equal error.code code);
                  assert (not (String.is_substring error.message ~substring:"private"));
                  outcome
                | _ -> assert false
              in
              let tail =
                match call with
                | None -> []
                | Some call ->
                  let output =
                    publication_output
                      caps
                      ~text:
                        (Jsonaf.to_string
                           (Agent_protocol.Invocation.outcome_to_json outcome))
                      ()
                  in
                  caps.publish_invocation_output ~invocation_id:recorded.context.id output
                  |> protocol_ok;
                  [ call; output ]
              in
              let state = Agent_session.Session_actor.state actor |> protocol_ok in
              Completed
                { final_history = input.history @ tail
                ; runtime_requests = []
                ; moderator_snapshot = state.moderator
                }))
          (fun _env actor _writer backend ->
             let state = await_idle actor in
             assert (Result.is_error ((Option.value_exn !stale) ()));
             assert (!authorized = if Poly.equal mode `Input then 0 else 1);
             let runs =
               List.mem [ `Success; `Raise; `Disclosure; `Output ] mode ~equal:Poly.equal
             in
             assert (!calls = if runs then 1 else 0);
             assert (!disclosed = if runs && not (Poly.equal mode `Raise) then 1 else 0);
             assert (
               List.length state.conversation.canonical_history = if model then 3 else 1);
             assert (List.length state.invocations = 1);
             assert (Poly.equal state (Agent_session.Memory_backend.state backend)))))
;;

let%test_unit
    "native custom tools receive raw strings without provider history for scripts"
  =
  List.iter [ false; true ] ~f:(fun model ->
    let calls = ref 0 in
    let registry = native_registry ~custom:true calls ~raises:false in
    with_handoff_actor
      ~make_worker:(fun _env actor_ready ->
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let actor = Eio.Promise.await actor_ready in
          let call, invocation =
            if model
            then (
              let call, invocation = publication_call caps ~custom:true () in
              caps.commit_entry call |> protocol_ok;
              Some call, invocation)
            else None, invocation_fixture ()
          in
          let reference, invocation =
            native_context ~input:(`String "{}") registry invocation
          in
          let result =
            Agent_session.Native_tool_invocation.run
              ~is_halted:(fun () -> false)
              ~capabilities:caps
              ~registry:(fun () -> registry)
              ~reference
              ~invocation
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok `Null)
            |> protocol_ok
          in
          assert (Poly.equal result.status (Resolved (Complete `Null)));
          let tail =
            match call with
            | None -> []
            | Some call ->
              let output =
                publication_output
                  caps
                  ~custom:true
                  ~text:
                    (Jsonaf.to_string
                       (Agent_protocol.Invocation.outcome_to_json (Complete `Null)))
                  ()
              in
              caps.publish_invocation_output ~invocation_id:result.context.id output
              |> protocol_ok;
              [ call; output ]
          in
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          Completed
            { final_history = input.history @ tail
            ; runtime_requests = []
            ; moderator_snapshot = state.moderator
            }))
      (fun _env actor _writer _backend ->
         let state = await_idle actor in
         assert (!calls = 1);
         assert (List.length state.conversation.canonical_history = if model then 3 else 1)))
;;

let%test_unit "independent ordinary invocations run concurrently outside the actor" =
  let started = ref 0 in
  let ready, ready_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let run () =
          let invocation =
            Agent_protocol.Invocation.create
              { (invocation_fixture ()).context with
                id = Agent_protocol.Id.Invocation.create ()
              }
            |> protocol_ok
          in
          let result =
            caps.with_invocation ~invocation (fun ~dispatched:_ ->
              Int.incr started;
              if !started = 2 then Eio.Promise.resolve ready_u ();
              Eio.Promise.await ready;
              let live = Agent_session.Session_actor.state actor |> protocol_ok in
              assert (List.length live.invocations = 2);
              Ok (Complete `Null))
            |> protocol_ok
          in
          assert (Poly.equal result.status (Resolved (Complete `Null)))
        in
        Eio.Fiber.both run run;
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor _writer backend ->
       let state = await_idle actor in
       assert (List.length state.invocations = 2);
       assert (List.length state.conversation.canonical_history = 1);
       assert (Poly.equal state (Agent_session.Memory_backend.state backend)))
;;

let%test_unit "native nested invocation persists without reentering a borrowed moderator" =
  List.iter [ false; true ] ~f:(fun parent_fails ->
    let calls = ref 0 in
    let registry = native_registry calls ~raises:false in
    with_handoff_actor
      ~make_worker:(fun _env actor_ready ->
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let actor = Eio.Promise.await actor_ready in
          let parent = invocation_fixture () in
          let result =
            caps.with_moderator_invocation ~invocation:parent (fun ~dispatched ~commit ->
              let reference, child =
                native_context
                  registry
                  (Agent_protocol.Invocation.create
                     { parent.context with
                       id = Agent_protocol.Id.Invocation.create ()
                     ; origin = Moderator
                     ; parent_invocation = Some dispatched.context.id
                     }
                   |> protocol_ok)
              in
              let child_result =
                Agent_session.Native_tool_invocation.run
                  ~is_halted:(fun () -> false)
                  ~capabilities:caps
                  ~registry:(fun () -> registry)
                  ~reference
                  ~invocation:child
                  ~authorize:(fun _ _ -> Ok ())
                  ~prepare_output:(fun _ -> Ok `Null)
                |> protocol_ok
              in
              assert (Poly.equal child_result.status (Resolved (Complete `Null)));
              let resolved =
                Agent_protocol.Invocation.resolve
                  dispatched
                  ~session_id:input.session_id
                  ~generation:input.session_generation
                  (Complete `Null)
                |> protocol_ok
              in
              if parent_fails
              then Error (handoff_error "parent handler failed after native effect")
              else commit ~resolved ~snapshot:(handoff_snapshot 1))
          in
          assert (Bool.equal (Result.is_error result) parent_fails);
          let stale_child =
            Agent_protocol.Invocation.create
              { parent.context with
                id = Agent_protocol.Id.Invocation.create ()
              ; parent_invocation = Some parent.context.id
              }
            |> protocol_ok
          in
          assert (
            Result.is_error
              (caps.with_invocation ~invocation:stale_child (fun ~dispatched:_ ->
                 assert false)));
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          Completed
            { final_history = input.history
            ; runtime_requests = []
            ; moderator_snapshot = state.moderator
            }))
      (fun _env actor _writer backend ->
         let state = await_idle actor in
         assert (!calls = 1);
         assert (List.length state.invocations = 2);
         List.iter state.invocations ~f:(fun invocation ->
           if Option.is_some invocation.context.parent_invocation
           then assert (Poly.equal invocation.status (Resolved (Complete `Null)))
           else
             assert (
               match invocation.status with
               | Resolved (Complete `Null) -> not parent_fails
               | Resolved (Fail _) -> parent_fails
               | _ -> false));
         assert (List.length state.conversation.canonical_history = 1);
         assert (Poly.equal state (Agent_session.Memory_backend.state backend))))
;;

let%test_unit
    "ordinary invocation cancellation and persistence failures retain terminal evidence"
  =
  List.iter [ false; true ] ~f:(fun model ->
    List.iter [ `Cancel; `Reject_result; `Error; `Raise; `Malformed ] ~f:(fun mode ->
      let ready, ready_u = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let calls = ref 0 in
      with_handoff_actor
        ~reject:(fun next ->
          Poly.equal mode `Reject_result
          && List.exists
               next.Agent_session.Session_transition.state.invocations
               ~f:(fun invocation ->
                 match invocation.status with
                 | Resolved (Complete _) -> true
                 | _ -> false))
        ~make_worker:(fun _env actor_ready ->
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let history, invocation =
              if model
              then (
                let call, invocation = publication_call caps () in
                caps.commit_entry call |> protocol_ok;
                input.history @ [ call ], invocation)
              else input.history, invocation_fixture ()
            in
            let result =
              caps.with_invocation ~invocation (fun ~dispatched ->
                Int.incr calls;
                let live = Agent_session.Session_actor.state actor |> protocol_ok in
                assert (List.mem live.invocations dispatched ~equal:Poly.equal);
                (* Neither a concurrent extension transaction nor a duplicate claim
                 may replace the live callback's record. *)
                let replacement =
                  Agent_protocol.Invocation.cancel dispatched ~reason:"forged"
                  |> protocol_ok
                in
                assert (
                  Result.is_error
                    (Agent_session.Session_actor.commit_extensions
                       actor
                       ~generation:input.session_generation
                       ~expected_revision:live.counters.revision
                       [ Invocation replacement ]));
                assert (
                  Result.is_error
                    (caps.with_invocation ~invocation (fun ~dispatched:_ -> assert false)));
                Eio.Promise.resolve ready_u ();
                match mode with
                | `Cancel -> Eio.Promise.await never
                | `Error -> Error (handoff_error "private callback diagnostic")
                | `Raise -> failwith "private callback diagnostic"
                | `Malformed ->
                  Ok (Complete (`Object [ "duplicate", `Null; "duplicate", `Null ]))
                | `Reject_result -> Ok (Complete `Null))
            in
            let stale_child =
              Agent_protocol.Invocation.create
                { invocation.context with
                  id = Agent_protocol.Id.Invocation.create ()
                ; origin = Script
                ; provider_call_id = None
                ; call_entry_id = None
                ; parent_invocation = Some invocation.context.id
                }
              |> protocol_ok
            in
            assert (
              Result.is_error
                (caps.with_invocation ~invocation:stale_child (fun ~dispatched:_ ->
                   assert false)));
            match result with
            | Error failure -> Failed failure
            | Ok _ ->
              let state = Agent_session.Session_actor.state actor |> protocol_ok in
              Completed
                { final_history = history
                ; runtime_requests = []
                ; moderator_snapshot = state.moderator
                }))
        (fun _env actor writer backend ->
           Eio.Promise.await ready;
           if Poly.equal mode `Cancel
           then
             Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
             |> protocol_ok
             |> ignore;
           let rec finished () =
             let state = Agent_session.Session_actor.state actor |> protocol_ok in
             if Option.is_none state.active_operation
             then state
             else (
               Eio.Fiber.yield ();
               finished ())
           in
           let state = finished () in
           assert (!calls = 1);
           let invocation = List.hd_exn state.invocations in
           let outcome =
             match invocation.status with
             | Published outcome ->
               assert model;
               outcome
             | Resolved outcome ->
               assert (not model);
               outcome
             | _ -> assert false
           in
           (match mode, outcome with
            | (`Cancel | `Reject_result), Cancelled _ -> ()
            | (`Error | `Raise | `Malformed), Fail failure ->
              assert (
                String.equal
                  failure.code
                  (if Poly.equal mode `Malformed
                   then "invocation.invalid_output"
                   else "invocation.handler_failed"));
              assert (not (String.is_substring failure.message ~substring:"private"))
            | _ -> assert false);
           assert (
             List.length state.conversation.canonical_history = if model then 3 else 1);
           assert (Poly.equal state (Agent_session.Memory_backend.state backend)))))
;;

let%test_unit
    "graceful stop allows an admitted invocation to publish its initial response"
  =
  let ready, ready_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let done_, done_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let call, invocation = publication_call caps () in
        caps.commit_entry call |> protocol_ok;
        resolve_publication caps invocation |> protocol_ok;
        let output = publication_output caps () in
        Eio.Promise.resolve ready_u ();
        Eio.Promise.await release;
        caps.publish_invocation_output ~invocation_id:invocation.context.id output
        |> protocol_ok;
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = input.history @ [ call; output ]
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor writer _backend ->
       Eio.Promise.await ready;
       Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Graceful
       |> protocol_ok
       |> ignore;
       Eio.Promise.resolve release_u ();
       Eio.Promise.await done_;
       let state = Agent_session.Session_actor.state actor |> protocol_ok in
       assert (Option.is_some (List.hd_exn state.invocations).output_entry_id))
;;
