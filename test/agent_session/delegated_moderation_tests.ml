open Core
open Fixtures
module P = Agent_protocol
module E = P.Moderator_execution
module Q = Agent_session.Queued_moderator_event
module State = Agent_session.Session_state
module B = Agent_session.Runtime_builder
module Persistence = Agent_session.Session_persistence

let encode state = State.sexp_of_t state |> Sexp.to_string_mach

let requests : P.Invocation.follow_up =
  { request_turn = true; request_compaction = false; end_session = None }
;;

let delegation () : E.delegation =
  { child_session_id = second_session_id
  ; child_generation = 0
  ; child_invocation_id = P.Id.Invocation.create ()
  ; admission_sha256 = String.make 64 'b'
  }
;;

let policy_event (delegation : E.delegation) =
  Chat_response.Moderation.Event.Pre_tool_call
    { id = P.Id.Invocation.to_string delegation.child_invocation_id
    ; name = "read_file"
    ; args = `Object []
    ; kind = Function
    ; payload_text = "{}"
    ; meta = `Null
    }
;;

let%expect_test
    "live parent manager commits one policy decision and rolls back failed handoffs"
  =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  List.iter [ `Reject; `Save_fail; `End ] ~f:(fun mode ->
    let failed = ref false in
    Job_fixtures.with_actor
      ~reject_save:(fun next ->
        match mode, !failed with
        | `Save_fail, false
          when List.exists next.state.moderator_executions ~f:(fun event ->
                 Option.is_some event.E.decision) ->
          failed := true;
          true
        | _ -> false)
      (fun env _sw actor _writer backend ->
         let action =
           match mode with
           | `Reject | `Save_fail -> {|Tool.reject("denied by parent")|}
           | `End -> {|Runtime.end_session("policy stopped")|}
         in
         let manager, _, _ =
           handoff_definition
             env
             ~declare_tool:false
             ~events:
               ({| | `Pre_tool_call(call) ->
              let ignored = state[0] <- state[0] + 1 in
              let* ignored = |}
                ^ action
                ^ {| in Task.pure(state)
              | _ -> Task.pure(state) |}
               )
         in
         let snapshot () = M.identity_snapshot manager |> Result.ok_or_failwith in
         let before = snapshot () in
         A.change_moderator actor (Some (B.encode_moderator_snapshot before))
         |> protocol_ok
         |> ignore;
         let delegated = delegation () in
         let event = policy_event delegated in
         let run () =
           Agent_session.Moderator_event.run_delegated
             ~event
             ~claim:
               (A.with_delegated_moderator_event
                  actor
                  ~delegation:delegated
                  ~event
                  ~authorize:(fun () -> Ok ()))
             ~manager
             ~history:(fun () -> [])
             ~available_tools:[]
             ~session_meta:`Null
             ~now:(fun () -> timestamp)
             ()
         in
         let result = run () in
         let live = snapshot () in
         let saved = Agent_session.Memory_backend.state backend in
         assert (
           Option.exists
             saved.moderator
             ~f:(Jsonaf.exactly_equal (B.encode_moderator_snapshot live)));
         (match mode, result with
          | `Save_fail, Error _ ->
            [%test_eq: Sexp.t]
              (Session.Moderator_state.Identity_snapshot.sexp_of_t before)
              (Session.Moderator_state.Identity_snapshot.sexp_of_t live);
            assert (Result.is_error (run ()))
          | (`Reject | `End), Ok (Some result) ->
            assert (Option.is_some result.outcome);
            let decision =
              match mode with
              | `Reject -> E.Decision.Reject "denied by parent"
              | `End -> E.Decision.Reject "parent moderator ended session"
              | `Save_fail -> assert false
            in
            assert (Option.equal E.Decision.equal result.receipt.decision (Some decision));
            (match mode with
             | `Reject ->
               let replay = run () |> protocol_ok |> Option.value_exn in
               assert (Option.is_none replay.outcome);
               assert (E.equal result.receipt replay.receipt);
               [%test_eq: Sexp.t]
                 (Session.Moderator_state.Identity_snapshot.sexp_of_t live)
                 (Session.Moderator_state.Identity_snapshot.sexp_of_t (snapshot ()))
             | `End ->
               assert live.halted;
               assert (
                 Option.exists result.receipt.requests ~f:(fun requests ->
                   Option.equal String.equal requests.end_session (Some "policy stopped")))
             | `Save_fail -> assert false)
          | _ -> failwith "unexpected manager policy result");
         print_s
           [%sexp
             (mode : [ `Reject | `Save_fail | `End ]), "parent state and decision agree"]));
  [%expect
    {|
    (Reject "parent state and decision agree")
    (Save_fail "parent state and decision agree")
    (End "parent state and decision agree") |}]
;;

let%expect_test
    "parent actor commits delegated decision and state together and never repeats failed \
     effects"
  =
  let module A = Agent_session.Session_actor in
  List.iter [ false; true ] ~f:(fun fail_commit ->
    let rejected = ref false in
    Job_fixtures.with_actor
      ~reject_save:(fun snapshot ->
        if
          fail_commit
          && (not !rejected)
          && List.exists snapshot.state.moderator_executions ~f:(fun receipt ->
            Option.is_some receipt.E.decision)
        then (
          rejected := true;
          true)
        else false)
      (fun _env _sw actor _writer backend ->
         let before =
           { (handoff_snapshot 0) with script_source_hash = String.make 64 'a' }
         in
         let after = { before with current_state = Session.Snapshot.Int 1 } in
         let live = ref before in
         A.change_moderator actor (Some (B.encode_moderator_snapshot before))
         |> protocol_ok
         |> ignore;
         let delegated = delegation () in
         let calls = ref 0 in
         let escaped = ref None in
         let run () =
           A.with_delegated_moderator_event
             actor
             ~delegation:delegated
             ~event:(policy_event delegated)
             ~authorize:(fun () -> Ok ())
             ~snapshot:(fun () -> Ok !live)
             (fun ~executing ~event:_ ~execute ~commit ->
                let invocation =
                  P.Invocation.create
                    ~observer:executing.context.source
                    ~parent_event:executing.context.id
                    { (invocation_fixture ()).context with
                      id = P.Id.Invocation.create ()
                    ; origin = Moderator
                    ; parent_invocation = None
                    ; parent_job = None
                    ; provider_call_id = None
                    ; call_entry_id = None
                    }
                  |> protocol_ok
                in
                let open Result.Let_syntax in
                let%bind _ =
                  execute ~invocation (fun ~dispatched:_ ->
                    Int.incr calls;
                    Ok (P.Invocation.Complete (`String "parent policy effect")))
                in
                let finish () =
                  commit ~decision:(Reject "parent rule") ~snapshot:after ~requests
                in
                escaped := Some finish;
                let%map () = finish () in
                live := after)
         in
         let result = run () in
         (match fail_commit, result with
          | false, Ok (Some receipt) ->
            assert (
              Option.equal E.Decision.equal receipt.decision (Some (Reject "parent rule")))
          | true, Error _ -> ()
          | _ -> failwith "unexpected parent policy commit result");
         (match fail_commit, run () with
          | false, Ok (Some _) | true, Error _ -> ()
          | _ -> failwith "policy retry repeated or lost its decision");
         [%test_eq: int] 1 !calls;
         assert (Result.is_error ((Option.value_exn !escaped) ()));
         let saved = Agent_session.Memory_backend.state backend in
         let expected = if fail_commit then before else after in
         assert (
           Option.exists
             saved.moderator
             ~f:(Jsonaf.exactly_equal (B.encode_moderator_snapshot expected)));
         let receipt = List.hd_exn saved.moderator_executions in
         [%test_eq: int] 1 (List.length saved.invocations);
         assert (Option.is_some (List.hd_exn saved.invocations).parent_event);
         assert (
           List.for_all saved.invocations ~f:(fun invocation ->
             P.Id.Session.equal invocation.context.session_id session_id));
         let restored = Persistence.restore_snapshot (encode saved) |> store_ok in
         assert (E.equal receipt (List.hd_exn restored.moderator_executions));
         print_s
           [%sexp
             (fail_commit : bool), "one parent effect; atomic decision; scoped retry"]));
  [%expect
    {|
    (false "one parent effect; atomic decision; scoped retry")
    (true "one parent effect; atomic decision; scoped retry") |}]
;;

let%expect_test
    "parent stop interrupts delegated policy with and without a foreground operation"
  =
  let module A = Agent_session.Session_actor in
  List.iter [ false; true ] ~f:(fun foreground ->
    Job_fixtures.with_actor (fun env sw actor writer backend ->
      let before =
        { (handoff_snapshot 0) with script_source_hash = String.make 64 'a' }
      in
      A.change_moderator actor (Some (B.encode_moderator_snapshot before))
      |> protocol_ok
      |> ignore;
      let worker_entered, worker_entered_u = Eio.Promise.create () in
      if foreground
      then (
        let worker =
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ _ ->
            Eio.Promise.resolve worker_entered_u ();
            Eio.Fiber.await_cancel ())
        in
        A.set_operation_worker actor (Some worker) |> protocol_ok;
        let entry =
          Agent_session.History_codec.user_text ~id:history_id "parent work"
          |> Agent_session.History_codec.to_protocol
        in
        A.submit_message actor ~attachment_id:writer.id entry |> protocol_ok |> ignore;
        Eio.Promise.await worker_entered);
      let entered, entered_u = Eio.Promise.create () in
      let delegated = delegation () in
      let escaped = ref None in
      let running =
        Eio.Fiber.fork_promise ~sw (fun () ->
          Result.try_with (fun () ->
            A.with_delegated_moderator_event
              actor
              ~delegation:delegated
              ~event:(policy_event delegated)
              ~authorize:(fun () -> Ok ())
              ~snapshot:(fun () -> Ok before)
              (fun ~executing:_ ~event:_ ~execute:_ ~commit ->
                 escaped
                 := Some (fun () -> commit ~decision:Approve ~snapshot:before ~requests);
                 Eio.Promise.resolve entered_u ();
                 Eio.Fiber.await_cancel ())))
      in
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
        Eio.Promise.await entered;
        A.stop actor ~attachment_id:writer.id ~mode:Cancel |> protocol_ok |> ignore;
        match Eio.Promise.await_exn running with
        | Error (Eio.Cancel.Cancelled _) -> ()
        | _ -> failwith "parent stop did not cancel policy execution");
      assert (Result.is_error ((Option.value_exn !escaped) ()));
      let saved = Agent_session.Memory_backend.state backend in
      let receipt = List.hd_exn saved.moderator_executions in
      (match receipt.status with
       | Interrupted _ -> ()
       | _ -> failwith "missing interrupted policy receipt");
      assert (Option.is_none receipt.decision);
      assert (
        Option.exists
          saved.moderator
          ~f:(Jsonaf.exactly_equal (B.encode_moderator_snapshot before)));
      print_s [%sexp (foreground : bool), "policy cancelled; no late decision"]));
  [%expect
    {|
    (false "policy cancelled; no late decision")
    (true "policy cancelled; no late decision") |}]
;;

let%expect_test
    "delegated policy decisions survive checkpoint restore without repeating policy \
     effects"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let before = { (handoff_snapshot 0) with script_source_hash = String.make 64 'a' } in
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:true
    in
    let initial =
      { initial with moderator = Some (B.encode_moderator_snapshot before) }
    in
    let delegation : E.delegation =
      { child_session_id = second_session_id
      ; child_generation = 3
      ; child_invocation_id = P.Id.Invocation.create ()
      ; admission_sha256 = String.make 64 'b'
      }
    in
    let call : Chat_response.Moderation.Tool_call.t =
      { id = P.Id.Invocation.to_string delegation.child_invocation_id
      ; name = "read_file"
      ; args = `Object [ "file", `String "input.txt" ]
      ; kind = Function
      ; payload_text = {|{"file":"input.txt"}|}
      ; meta = `Null
      }
    in
    let claim ?(delegation = delegation) ?(call = call) state snapshot =
      Q.claim_delegated
        ~delegation
        ~state
        ~id:(P.Id.Moderator_execution.create ())
        ~snapshot
        ~event:(Pre_tool_call call)
        ~now:timestamp
    in
    let receipt =
      match claim initial before |> protocol_ok with
      | Claimed (receipt, _) -> receipt
      | Replayed _ -> failwith "new check replayed"
    in
    let running = { initial with moderator_executions = [ receipt ] } in
    let rejected label result =
      match result with
      | Error _ -> print_endline label
      | Ok _ -> failwith ("unexpected admission: " ^ label)
    in
    rejected "running check cannot be replayed" (claim running before);
    let after =
      { before with
        current_state = Session.Snapshot.Int 1
      ; queued_internal_events = [ Session.Snapshot.String "parent-event" ]
      }
    in
    rejected
      "decision and checkpoint must complete together"
      (Q.complete_ordinary ~claimed:receipt ~before ~snapshot:after ~requests);
    let decision =
      E.Decision.Redirect ("read_file", `Object [ "file", `String "safe.txt" ])
    in
    let completed =
      Q.complete_delegated ~decision ~claimed:receipt ~before ~snapshot:after ~requests
      |> protocol_ok
    in
    let saved =
      { initial with
        moderator = Some (B.encode_moderator_snapshot after)
      ; moderator_executions = [ completed ]
      }
    in
    let restored = Persistence.restore_snapshot (encode saved) |> store_ok in
    let later = { after with current_state = Session.Snapshot.Int 2 } in
    let later_state =
      { restored with moderator = Some (B.encode_moderator_snapshot later) }
    in
    (match claim later_state later |> protocol_ok with
     | Replayed previous -> assert (E.equal completed previous)
     | Claimed _ -> failwith "completed check repeated after parent state changed");
    print_endline "restored decision replayed after unrelated parent state change";
    let applied = E.apply_intent completed |> protocol_ok in
    assert (Option.equal E.Decision.equal (Some decision) applied.decision);
    let wire = E.to_json completed in
    assert (E.equal completed (E.of_json wire |> protocol_ok));
    let replace key value = function
      | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal key value)
      | _ -> assert false
    in
    rejected
      "saved decision cannot change with runtime intent"
      (E.of_json (replace "decision" (E.Decision.to_json Approve) (E.to_json applied))
       |> Result.bind ~f:(E.validate_transition ~previous:(Some completed)));
    rejected
      "different input cannot reuse the decision"
      (claim ~call:{ call with args = `Object []; payload_text = "{}" } later_state later);
    rejected
      "different admission cannot reuse the decision"
      (claim
         ~delegation:{ delegation with admission_sha256 = String.make 64 'c' }
         later_state
         later);
    let foreign =
      E.of_json
        (replace "session_id" (P.Id.Session.to_json (P.Id.Session.create ())) wire)
      |> protocol_ok
    in
    rejected
      "foreign parent receipt cannot answer the request"
      (claim { later_state with moderator_executions = [ foreign ] } later);
    let new_source = { later with script_source_hash = String.make 64 'd' } in
    rejected
      "different parent source cannot reuse the decision"
      (claim
         { later_state with moderator = Some (B.encode_moderator_snapshot new_source) }
         new_source);
    List.iter
      [ E.fail
          receipt
          { code = "failed"
          ; message = "policy failed"
          ; retryable = false
          ; details = `Null
          }
      ; E.interrupt receipt ~reason:"process stopped"
      ]
      ~f:(fun terminal ->
        rejected
          "failed or interrupted effects cannot replay"
          (claim { initial with moderator_executions = [ protocol_ok terminal ] } before));
    rejected
      "codec3 cannot carry delegated decisions"
      (E.of_json (replace "schema_version" (`Number "3") wire));
    rejected
      "schema14 cannot carry delegated decisions"
      (Persistence.restore_snapshot (encode { saved with schema_version = 14 }));
    let legacy = { initial with schema_version = 14 } in
    let upgraded = Persistence.restore_snapshot (encode legacy) |> store_ok in
    [%test_eq: int] 15 upgraded.schema_version;
    assert (List.is_empty upgraded.moderator_executions);
    let ordinary =
      E.create { receipt.context with operation_id = Some operation_id } |> protocol_ok
    in
    let legacy_wire = E.to_json ordinary in
    List.iter [ "1"; "2"; "3" ] ~f:(fun version ->
      assert (
        E.equal
          ordinary
          (E.of_json (replace "schema_version" (`Number version) legacy_wire)
           |> protocol_ok)));
    print_endline "legacy ordinary receipts retain their meaning");
  [%expect
    {|
    running check cannot be replayed
    decision and checkpoint must complete together
    restored decision replayed after unrelated parent state change
    saved decision cannot change with runtime intent
    different input cannot reuse the decision
    different admission cannot reuse the decision
    foreign parent receipt cannot answer the request
    different parent source cannot reuse the decision
    failed or interrupted effects cannot replay
    failed or interrupted effects cannot replay
    codec3 cannot carry delegated decisions
    schema14 cannot carry delegated decisions
    legacy ordinary receipts retain their meaning |}]
;;
