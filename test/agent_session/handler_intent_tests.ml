open Core
open Fixtures
module I = Agent_protocol.Invocation
module F = Agent_session.Observation_follow_up
module A = Agent_session.Session_actor

let observer : I.observer = { script_id = "handler"; source_sha256 = String.make 64 'a' }

let requests : I.follow_up =
  { request_turn = true; request_compaction = false; end_session = None }
;;

let paired_invocation parent =
  let dispatched =
    I.create
      ~observer
      { parent.I.context with
        id = Agent_protocol.Id.Invocation.create ()
      ; parent_invocation = Some parent.context.id
      ; origin = Script
      }
    |> protocol_ok
    |> I.dispatch
    |> protocol_ok
  in
  let bare =
    I.resolve dispatched ~session_id ~generation:0 (Complete (`String "result"))
    |> protocol_ok
  in
  let resolved =
    I.record_handler_intent bare ~requests:{ requests with request_compaction = true }
    |> protocol_ok
  in
  assert (Result.is_error (I.validate_transition ~previous:(Some bare) resolved));
  I.validate_transition ~previous:(Some dispatched) resolved |> protocol_ok;
  resolved
  |> I.claim_observation
  |> protocol_ok
  |> I.complete_observation ~follow_up:requests
  |> protocol_ok
;;

let%expect_test "failed outcome persistence cannot arm a native handler action" =
  let rejected = ref false in
  Job_fixtures.with_actor
    ~reject_save:(fun next ->
      match
        (not !rejected)
        && List.exists next.state.invocations ~f:(fun invocation ->
          Option.is_some invocation.handler_intent)
      with
      | false -> false
      | true ->
        rejected := true;
        true)
    (fun env _sw actor _writer backend ->
       let calls = ref 0 in
       let registry =
         native_registry calls ~raises:false ~on_call:(fun () ->
           Chat_response.Runtime_request_scope.emit [ Request_turn ]
           |> Result.ok_or_failwith)
       in
       let request =
         Background_execution_tests.capture_tool
           registry
           Chat_response.One_off_request.default_policy
       in
       let job =
         Job_fixtures.add_claimed_job
           ~payload:(Chat_response.Background_request.to_json request)
           actor
       in
       let outcome =
         Background_execution_tests.run
           env
           actor
           job
           request
           (Background_execution_tests.tools (fun () -> registry))
       in
       (match outcome with
        | Error error -> print_s [%sexp (error.code : Agent_protocol.Error.code)]
        | Ok _ -> failwith "rejected outcome was reported as committed");
       [%test_eq: bool] true !rejected;
       [%test_eq: int] 1 !calls;
       let state = Agent_session.Memory_backend.state backend in
       assert (
         List.for_all state.invocations ~f:(fun invocation ->
           Option.is_none invocation.handler_intent));
       [%test_eq: bool] false (A.apply_moderator_follow_up actor |> protocol_ok);
       print_endline "effect ran once; no durable or applied action");
  [%expect
    {|
    Conflict
    effect ran once; no durable or applied action
    |}]
;;

let%expect_test
    "handler and observation actions share an invocation without overwriting independent \
     progress"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let parent =
      invocation_fixture ()
      |> I.dispatch
      |> protocol_ok
      |> fun invocation ->
      I.resolve invocation ~session_id ~generation:0 (Complete `Null) |> protocol_ok
    in
    let invocation = paired_invocation parent in
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let state = { initial with invocations = [ parent; invocation ] } in
    let compaction_operation_id =
      Agent_protocol.Id.Operation.of_string "op_handler_compaction" |> protocol_ok
    in
    let plan =
      F.plan ~state ~observer:(Some observer) ~halted:false ~compaction_operation_id
      |> protocol_ok
    in
    print_s [%sexp (plan.action : F.action)];
    [%test_eq: int] 1 (List.length plan.invocations);
    let compacting = List.hd_exn plan.invocations in
    I.validate_transition ~previous:(Some invocation) compacting |> protocol_ok;
    print_s
      [%sexp
        (compacting.handler_intent : I.handler_intent option)
      , (Option.bind compacting.observation ~f:(fun o -> o.follow_up)
         : I.follow_up_status option)];
    List.iter [ false; true ] ~f:(fun failed ->
      let current =
        match failed with
        | false -> compacting
        | true ->
          F.discard_compaction
            [ compacting ]
            ~operation_id:compaction_operation_id
            ~reason:"compaction failed"
          |> protocol_ok
          |> List.hd_exn
      in
      let state = { state with invocations = [ parent; current ] } in
      let turn =
        F.plan
          ~state
          ~observer:(Some observer)
          ~halted:false
          ~compaction_operation_id:(Agent_protocol.Id.Operation.create ())
        |> protocol_ok
      in
      print_s [%sexp (failed : bool), (turn.action : F.action)];
      [%test_eq: int] 1 (List.length turn.invocations);
      let applied = List.hd_exn turn.invocations in
      I.validate_transition ~previous:(Some current) applied |> protocol_ok;
      let restored = I.of_json (I.to_json applied) |> protocol_ok in
      assert (I.equal applied restored);
      assert (not (F.pending applied));
      (match applied.handler_intent, failed with
       | Some { follow_up = Discarded_follow_up _; _ }, true
       | Some { follow_up = Applied_follow_up _; _ }, false -> ()
       | _ -> failwith "handler disposition was overwritten");
      match applied.observation with
      | Some { follow_up = Some (Applied_follow_up _); _ } -> ()
      | _ -> failwith "independent observation turn was lost"));
  [%expect
    {|
    Compact
    ((((follow_up
        (Compaction_accepted_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))))
       (compaction_operation_id op_handler_compaction)))
     ((Pending_follow_up
       ((request_turn true) (request_compaction false) (end_session ())))))
    (false Turn)
    (true Turn)
    |}]
;;

let%expect_test
    "reconciliation cannot hide action admission behind another discarded intent"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let parent =
      invocation_fixture ()
      |> I.dispatch
      |> protocol_ok
      |> fun invocation ->
      I.resolve invocation ~session_id ~generation:0 (Complete `Null) |> protocol_ok
    in
    let invocation = paired_invocation parent in
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let state =
      { initial with
        invocations = [ parent; invocation ]
      ; identity = { initial.identity with generation = 1 }
      }
    in
    let check next =
      I.validate_transition ~previous:(Some invocation) next |> protocol_ok;
      match Agent_session.Session_delta.apply state (Invocation_reconciled next) with
      | Ok _ -> failwith "reconciliation admitted an action"
      | Error error -> print_endline error.message
    in
    invocation
    |> I.discard_handler_intent ~reason:"retired"
    |> protocol_ok
    |> I.apply_observation_follow_up
    |> protocol_ok
    |> check;
    invocation
    |> I.discard_observation_follow_up ~reason:"retired"
    |> protocol_ok
    |> I.apply_handler_intent
    |> protocol_ok
    |> check;
    let discarded =
      F.discard [ invocation ] ~reason:"retired" |> protocol_ok |> List.hd_exn
    in
    Agent_session.Session_delta.apply state (Invocation_reconciled discarded)
    |> protocol_ok
    |> ignore;
    print_endline "both retired without admitting work");
  [%expect
    {|
    reconciliation cannot admit handler or observation actions
    reconciliation cannot admit handler or observation actions
    both retired without admitting work
    |}]
;;

let%expect_test
    "native job actions remain durable without a moderator and wait for job completion"
  =
  Job_fixtures.with_actor (fun env _sw actor _writer backend ->
    let calls = ref 0 in
    let registry =
      native_registry calls ~raises:false ~on_call:(fun () ->
        Chat_response.Runtime_request_scope.emit [ End_session "native finished" ]
        |> Result.ok_or_failwith)
    in
    let request =
      Background_execution_tests.capture_tool
        registry
        Chat_response.One_off_request.default_policy
    in
    let job =
      Job_fixtures.add_claimed_job
        ~payload:(Chat_response.Background_request.to_json request)
        actor
    in
    let result =
      Background_execution_tests.run
        env
        actor
        job
        request
        (Background_execution_tests.tools (fun () -> registry))
      |> protocol_ok
    in
    assert (Option.is_some result.resolved.handler_intent);
    assert (List.is_empty result.runtime_requests);
    [%test_eq: bool] false (A.apply_moderator_follow_up actor |> protocol_ok);
    A.complete_background_job
      actor
      ~job_id:job.id
      ~generation:job.generation
      ~attempt:job.attempt
      (Succeeded `Null)
    |> protocol_ok
    |> ignore;
    let other = Job_fixtures.add_claimed_job actor in
    [%test_eq: bool] true (A.apply_moderator_follow_up actor |> protocol_ok);
    [%test_eq: bool] false (A.apply_moderator_follow_up actor |> protocol_ok);
    let state = Agent_session.Memory_backend.state backend in
    assert state.halted;
    assert (Option.is_none state.moderator);
    let cancelled =
      List.find_exn state.jobs ~f:(fun job -> Agent_protocol.Id.Job.equal job.id other.id)
    in
    (match cancelled.status with
     | Cancelled -> ()
     | _ -> failwith "end-session left owned work running");
    [%test_eq: int] 1 !calls;
    let saved =
      List.find_exn state.invocations ~f:(fun invocation ->
        Agent_protocol.Id.Invocation.equal
          invocation.context.id
          result.resolved.context.id)
    in
    let restored = I.of_json (I.to_json saved) |> protocol_ok in
    assert (I.equal saved restored);
    print_s [%sexp (saved.handler_intent : I.handler_intent option)]);
  [%expect
    {|
    (((follow_up
       (Applied_follow_up
        ((request_turn false) (request_compaction false)
         (end_session ("native finished")))))))
    |}]
;;

let%expect_test
    "cancelled and interrupted jobs cannot apply already recorded handler requests"
  =
  List.iter [ false; true ] ~f:(fun interrupted ->
    Job_fixtures.with_actor (fun env _sw actor _writer backend ->
      let calls = ref 0 in
      let registry =
        native_registry calls ~raises:false ~on_call:(fun () ->
          Chat_response.Runtime_request_scope.emit [ End_session "must not run" ]
          |> Result.ok_or_failwith)
      in
      let request =
        Background_execution_tests.capture_tool
          registry
          Chat_response.One_off_request.default_policy
      in
      let job =
        Job_fixtures.add_claimed_job
          ~payload:(Chat_response.Background_request.to_json request)
          actor
      in
      Background_execution_tests.run
        env
        actor
        job
        request
        (Background_execution_tests.tools (fun () -> registry))
      |> protocol_ok
      |> ignore;
      (match interrupted with
       | false -> A.cancel_job_internal actor ~job_id:job.id
       | true ->
         A.interrupt_job
           actor
           ~job_id:job.id
           ~generation:job.generation
           ~attempt:job.attempt
           ~reason:"fixture interrupted")
      |> protocol_ok
      |> ignore;
      [%test_eq: bool] true (A.apply_moderator_follow_up actor |> protocol_ok);
      [%test_eq: bool] false (A.apply_moderator_follow_up actor |> protocol_ok);
      let state = Agent_session.Memory_backend.state backend in
      assert (not state.halted);
      let invocation =
        List.find_exn state.invocations ~f:(fun invocation ->
          Option.is_some invocation.handler_intent)
      in
      print_s
        [%sexp
          (interrupted : bool), (invocation.handler_intent : I.handler_intent option)]));
  [%expect
    {|
    (false
     (((follow_up
        (Discarded_follow_up
         ((request_turn false) (request_compaction false)
          (end_session ("must not run")))
         "handler job was cancelled or interrupted")))))
    (true
     (((follow_up
        (Discarded_follow_up
         ((request_turn false) (request_compaction false)
          (end_session ("must not run")))
         "handler job was cancelled or interrupted")))))
    |}]
;;
