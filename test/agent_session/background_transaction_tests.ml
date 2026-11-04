open Core
open Fixtures
open Job_fixtures
open Background_scheduler_tests
module Capacity = Agent_server.Job_capacity

let%expect_test "ending the owner between preparation and staging releases capacity" =
  with_capacity_scheduler
    ~reject_save:(fun _ -> false)
    (fun _env actor _backend calls _registry _scheduler _start capacity request ->
       let parent = add_claimed_job actor in
       let invocation = root parent in
       let owner = J.Invocation invocation.context.id in
       let prepared = ref None in
       with_job actor parent (fun ~job:_ ~execute ->
         execute ~invocation (fun ~dispatched:_ ~capabilities:_ ->
           let job =
             A.prepare_background_job_launch actor ~owner request |> protocol_ok
           in
           let reservation =
             Background_admission_tests.reserve
               capacity
               (Background_admission_tests.key actor job)
               job
           in
           prepared := Some (job, reservation);
           Ok (I.Complete `Null)))
       |> protocol_ok
       |> ignore;
       let job, reservation = Option.value_exn !prepared in
       assert (
         Result.is_error
           (A.stage_background_job
              actor
              ~job
              ~capacity:
                { publish = (fun () -> failwith "ended owner published work")
                ; abort = (fun () -> Capacity.abort reservation)
                }));
       let lease =
         Capacity.try_acquire capacity (Background_admission_tests.key actor job)
         |> protocol_ok
         |> Option.value_exn
       in
       Capacity.release lease;
       [%test_eq: int] 0 !calls;
       print_endline "ended callback rejected; reservation released without running work");
  [%expect {| ended callback rejected; reservation released without running work |}]
;;

let stage actor backend capacity request owner =
  let job = A.prepare_background_job_launch actor ~owner request |> protocol_ok in
  assert (Option.equal J.Inference_origin.equal job.inference_origin (Some Unbound));
  let quota = Background_admission_tests.key actor job in
  let reservation = Background_admission_tests.reserve capacity quota job in
  A.stage_background_job
    actor
    ~job
    ~capacity:
      { publish =
          (fun () ->
            let stored = current backend job in
            assert (
              Agent_protocol.Job.equal_launch
                (Option.value_exn job.launch)
                (Option.value_exn stored.launch));
            Capacity.publish reservation)
      ; abort = (fun () -> Capacity.abort reservation)
      }
  |> protocol_ok;
  job
;;

let%expect_test
    "native transaction publishes only selected launches after its outcome saves"
  =
  List.iter
    [ `Success; `Rejected; `Returned_failure; `Error; `Cancelled; `Unselected ]
    ~f:(fun mode ->
      let reject = ref false in
      with_capacity_scheduler
        ~reject_save:(fun _ -> !reject)
        (fun env actor backend calls _registry scheduler _start capacity request ->
           let parent = add_claimed_job actor in
           let invocation = root parent in
           let owner = J.Invocation invocation.context.id in
           let selected = ref None in
           let result =
             with_job actor parent (fun ~job:_ ~execute ->
               execute ~invocation (fun ~dispatched:_ ~capabilities:_ ->
                 let discarded = stage actor backend capacity request owner in
                 assert (
                   A.has_staged_background_job actor ~owner ~id:discarded.id
                   |> protocol_ok);
                 [%test_eq: int]
                   1
                   (List.length (Agent_session.Memory_backend.state backend).jobs);
                 A.abort_background_job actor ~owner ~id:discarded.id |> protocol_ok;
                 let job = stage actor backend capacity request owner in
                 selected := Some job;
                 A.select_background_jobs actor ~owner ~ids:[ job.id ] |> protocol_ok;
                 Eio.Time.sleep (Eio.Stdenv.clock env) 0.06;
                 [%test_eq: int] 0 !calls;
                 match mode with
                 | `Success -> Ok (I.Pending (Job job.id, `String "accepted"))
                 | `Rejected ->
                   reject := true;
                   Ok (I.Pending (Job job.id, `String "accepted"))
                 | `Returned_failure ->
                   Ok
                     (I.Fail
                        { code = "fixture.failure"
                        ; message = "handled failure"
                        ; retryable = false
                        ; details = `Null
                        })
                 | `Error -> Error (handoff_error "callback failed")
                 | `Unselected ->
                   A.select_background_jobs actor ~owner ~ids:[] |> protocol_ok;
                   Ok (I.Pending (Job job.id, `Null))
                 | `Cancelled -> Ok (I.Cancelled "cancelled")))
           in
           reject := false;
           let job = Option.value_exn !selected in
           assert (Result.is_error (A.prepare_background_job_launch actor ~owner request));
           let admitted =
             match mode with
             | `Success | `Returned_failure -> true
             | _ -> false
           in
           (match admitted with
            | true ->
              result |> protocol_ok |> ignore;
              until env (fun () ->
                terminal backend job && Scheduler.running_count scheduler = 0);
              [%test_eq: int] 1 !calls
            | false ->
              [%test_eq: int]
                1
                (List.length (Agent_session.Memory_backend.state backend).jobs);
              [%test_eq: int] 0 !calls);
           let lease =
             Capacity.try_acquire capacity (Background_admission_tests.key actor job)
             |> protocol_ok
             |> Option.value_exn
           in
           Capacity.release lease;
           let state = Agent_session.Memory_backend.state backend in
           Agent_session.Session_state.validate state |> protocol_ok;
           (match admitted with
            | false -> ()
            | true ->
              let stored = current backend job in
              let restored =
                Agent_session.Session_state_document.encode
                  (Agent_session.Session_state_document.authored state)
                  ~limits:document_limits
                |> document_ok
                |> Agent_session.Session_state_document.decode ~limits:document_limits
                |> document_ok
                |> Agent_session.Session_state_document.value
              in
              Agent_session.Session_state.validate restored |> protocol_ok;
              let decoded = J.of_json (J.to_json stored) |> protocol_ok in
              assert (Option.equal J.equal_launch stored.launch decoded.launch);
              assert (
                Option.equal
                  J.Inference_origin.equal
                  stored.inference_origin
                  (Some Unbound));
              assert (
                Option.equal
                  J.Inference_origin.equal
                  stored.inference_origin
                  decoded.inference_origin);
              assert (
                Result.is_error
                  (Agent_session.Session_delta.apply
                     state
                     (Job_changed { stored with launch = None })));
              let launch = Option.value_exn stored.launch in
              let corrupted =
                { stored with launch = Some { launch with nested_depth = 0 } }
              in
              assert (
                Result.is_error
                  (Agent_session.Session_state.validate
                     { state with
                       jobs =
                         corrupted
                         :: List.filter state.jobs ~f:(fun other ->
                           not (Agent_protocol.Id.Job.equal other.id job.id))
                     })));
           print_s
             [%sexp
               (mode
                : [ `Success
                  | `Rejected
                  | `Returned_failure
                  | `Error
                  | `Cancelled
                  | `Unselected
                  ])
             , (admitted : bool)]));
  [%expect
    {|
    (Success true)
    (Rejected false)
    (Returned_failure true)
    (Error false)
    (Cancelled false)
    (Unselected false)
    |}]
;;

let%expect_test "job starts commit atomically with managed handler and event checkpoints" =
  List.iter [ `Handler; `Handler_cancelled; `Event ] ~f:(fun mode ->
    List.iter [ false; true ] ~f:(fun fail_commit ->
      let reject = ref false in
      with_capacity_scheduler
        ~reject_save:(fun _ -> !reject)
        (fun env actor backend calls _registry scheduler _start capacity request ->
           let before =
             { (handoff_snapshot 0) with script_source_hash = String.make 64 'a' }
           in
           let after = { before with current_state = Session.Snapshot.Int 1 } in
           A.change_moderator
             actor
             (Some (Agent_session.Runtime_builder.encode_moderator_snapshot before))
           |> protocol_ok
           |> ignore;
           let parent = add_claimed_job actor in
           let launched = ref None in
           let start owner =
             let job = stage actor backend capacity request owner in
             launched := Some job;
             A.select_background_jobs actor ~owner ~ids:[ job.id ] |> protocol_ok;
             reject := fail_commit;
             job
           in
           let result =
             A.with_job_execution
               actor
               ~job_id:parent.id
               ~generation:0
               ~attempt:parent.attempt
               ~deadline:(Some deadline)
               (fun services ->
                  let result =
                    match mode with
                    | `Handler | `Handler_cancelled ->
                      services.execute
                        ~invocation:(root parent)
                        (fun ~dispatched:root ~capabilities:_ ->
                           let observer : I.observer =
                             { script_id = before.script_id
                             ; source_sha256 = before.script_source_hash
                             }
                           in
                           let invocation =
                             I.create
                               ~observer
                               { root.context with
                                 id = Agent_protocol.Id.Invocation.create ()
                               ; parent_job = None
                               ; parent_invocation = Some root.context.id
                               ; tool_name = "counter"
                               }
                             |> protocol_ok
                           in
                           let result =
                             services.moderator_execute
                               ~invocation
                               (fun ~dispatched ~capabilities:_ ~commit ->
                                  let job = start (J.Invocation dispatched.context.id) in
                                  let outcome =
                                    match mode with
                                    | `Handler_cancelled ->
                                      I.Cancelled "handler cancelled"
                                    | `Handler | `Event -> I.Pending (Job job.id, `Null)
                                  in
                                  let resolved =
                                    I.resolve dispatched ~session_id ~generation:0 outcome
                                    |> protocol_ok
                                  in
                                  commit ~resolved ~snapshot:after)
                           in
                           reject := false;
                           Result.map result ~f:(fun () -> I.Complete `Null))
                      |> Result.map ~f:ignore
                    | `Event ->
                      let event =
                        Chat_response.Moderation.Event.Pre_tool_call
                          { id = "launch"
                          ; name = "read_file"
                          ; args = `Object []
                          ; kind = Function
                          ; payload_text = "{}"
                          ; meta = `Null
                          }
                      in
                      services.claim_event
                        ~event
                        ~snapshot:(fun () -> Ok before)
                        (fun ~executing
                          ~model_jobs:_
                          ~allocate_inference:_
                          ~retirement_reason:_
                          ~event:_
                          ~execute:_
                          ~commit ->
                           start (J.Moderator_event executing.context.id) |> ignore;
                           commit
                             ~snapshot:after
                             ~requests:
                               { request_turn = false
                               ; request_compaction = false
                               ; end_session = None
                               })
                      |> Result.map ~f:ignore
                  in
                  reject := false;
                  result)
           in
           reject := false;
           let job = Option.value_exn !launched in
           (match fail_commit, mode with
            | false, (`Handler | `Event) ->
              result |> protocol_ok;
              until env (fun () ->
                terminal backend job && Scheduler.running_count scheduler = 0);
              [%test_eq: int] 1 !calls
            | true, _ | false, `Handler_cancelled ->
              (match fail_commit with
               | true -> assert (Result.is_error result)
               | false -> result |> protocol_ok);
              [%test_eq: int]
                1
                (List.length (Agent_session.Memory_backend.state backend).jobs);
              [%test_eq: int] 0 !calls);
           let state = Agent_session.Memory_backend.state backend in
           Agent_session.Session_state.validate state |> protocol_ok;
           let expected =
             match fail_commit with
             | true -> before
             | false -> after
           in
           assert (
             Option.equal
               Jsonaf.exactly_equal
               state.moderator
               (Some (Agent_session.Runtime_builder.encode_moderator_snapshot expected)));
           let lease =
             Capacity.try_acquire capacity (Background_admission_tests.key actor job)
             |> protocol_ok
             |> Option.value_exn
           in
           Capacity.release lease;
           print_s
             [%sexp
               (mode : [ `Handler | `Handler_cancelled | `Event ]), (fail_commit : bool)])));
  [%expect
    {|
    (Handler false)
    (Handler true)
    (Handler_cancelled false)
    (Handler_cancelled true)
    (Event false)
    +queued moderator cleanup failed: injected job transition failure
    (Event true)
    |}]
;;

let%expect_test
    "selected Job loans survive an intervening claim and defer only actual owned work"
  =
  let module P = Agent_protocol in
  let module Auth = Agent_authority_tests in
  List.iter [ `Legacy; `Original; `Rejected_save; `Changed_source ] ~f:(fun mode ->
    let authenticated =
      match mode with
      | `Legacy -> false
      | `Original | `Rejected_save | `Changed_source -> true
    in
    let original =
      if authenticated then Some (Auth.authentication (Atomic.make true)) else None
    in
    let reject = ref false in
    let completed = ref [] in
    let terminal_operations = ref [] in
    with_actor
      ~start:false
      ~prepare_state:(fun state ->
        { state with runtime_initialization = Pending { fresh_history = true } })
      ~reject_save:(fun _ -> !reject)
      ~state_committed:(fun _ events ->
        List.iter events ~f:(fun (event : P.Event.Durable.t) ->
          match
            P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok
          with
          | Operation_completed operation ->
            completed := operation.id :: !completed;
            terminal_operations := operation :: !terminal_operations
          | Operation_failed operation
          | Operation_cancelled operation
          | Operation_interrupted operation ->
            terminal_operations := operation :: !terminal_operations
          | _ -> ()))
      (fun env _sw actor writer backend ->
         let expected = A.state actor |> protocol_ok in
         let initialization =
           (match original with
            | None -> A.begin_initialization actor ~expected
            | Some original ->
              A.begin_initialization_authenticated actor ~actor:original ~expected)
           |> protocol_ok
         in
         let independent : J.t =
           { id = P.Id.Job.create ()
           ; session_id
           ; generation = expected.identity.generation
           ; kind = Model_call
           ; payload = `Object []
           ; status = Queued
           ; retry_policy = Never
           ; attempt = 0
           ; created_at = timestamp
           ; started_at = None
           ; next_run_at = None
           ; completed_at = None
           ; result = None
           ; delivery = Not_required
           ; launch = None
           ; inference_origin = None
           ; progress = None
           }
         in
         let independent =
           A.add_initialization_model_job actor ~scope:initialization independent
           |> protocol_ok
         in
         A.complete_initialization actor ~scope:initialization ~candidate:expected
         |> protocol_ok
         |> ignore;
         A.end_initialization actor ~scope:initialization |> protocol_ok;
         let registry = native_registry (ref 0) ~raises:false in
         let request =
           Background_execution_tests.capture_tool
             registry
             Chat_response.One_off_request.default_policy
         in
         let replacement, _, _ = handoff_definition env in
         let replacement =
           Chat_response.Moderator_manager.identity_snapshot replacement
           |> Result.ok_or_failwith
         in
         let selected = ref [] in
         let claimed_independent = ref None in
         let published = ref []
         and aborted = ref [] in
         let duplicate_aborts = ref 0 in
         let duplicate_released = ref false in
         let callback_result = ref None in
         let worker_finished = ref false in
         let stage owner =
           let job =
             A.prepare_background_job_launch actor ~owner request |> protocol_ok
           in
           A.stage_background_job
             actor
             ~job
             ~capacity:
               { publish =
                   (fun () ->
                     assert (
                       List.for_all !selected ~f:(fun selected ->
                         List.exists
                           (Agent_session.Memory_backend.state backend).jobs
                           ~f:(fun saved -> P.Id.Job.equal saved.id selected.J.id)));
                     published := job.id :: !published)
               ; abort = (fun () -> aborted := job.id :: !aborted)
               }
           |> protocol_ok;
           job
         in
         let worker =
           Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
             let call, invocation = publication_call caps () in
             let _, invocation = native_context registry invocation in
             caps.commit_invocation_call ~invocation call |> protocol_ok;
             let result =
               caps.with_invocation ~invocation (fun ~dispatched ~capabilities:_ ->
                 let owner = J.Invocation dispatched.context.id in
                 let first = stage owner in
                 let duplicate =
                   A.stage_background_job
                     actor
                     ~job:first
                     ~capacity:
                       { publish = (fun () -> failwith "duplicate staged Job published")
                       ; abort =
                           (fun () ->
                             if not !duplicate_released
                             then (
                               duplicate_released := true;
                               Int.incr duplicate_aborts))
                       }
                 in
                 (match duplicate with
                  | Error _ -> ()
                  | Ok () -> failwith "duplicate staged ID was accepted");
                 claimed_independent
                 := A.claim_job
                      actor
                      ~job_id:independent.id
                      ~generation:independent.generation
                    |> protocol_ok;
                 let second = stage owner in
                 selected := [ first; second ];
                 A.select_background_jobs actor ~owner ~ids:[ first.id; second.id ]
                 |> protocol_ok;
                 (match mode with
                  | `Changed_source ->
                    A.change_moderator
                      actor
                      (Some (Agent_session.Moderator_checkpoint.encode replacement))
                    |> protocol_ok
                    |> ignore
                  | `Rejected_save -> reject := true
                  | `Legacy | `Original -> ());
                 Ok (I.Complete `Null))
             in
             reject := false;
             callback_result := Some result;
             (match result with
              | Ok resolved ->
                let outcome =
                  match resolved.I.status with
                  | Resolved outcome -> outcome
                  | _ -> failwith "native callback returned no truthful outcome"
                in
                caps.publish_invocation_output
                  ~invocation_id:resolved.context.id
                  (publication_output
                     caps
                     ~text:(Jsonaf.to_string (I.outcome_to_json outcome))
                     ())
                |> protocol_ok
              | Error _ -> ());
             let state = A.state actor |> protocol_ok in
             worker_finished := true;
             Agent_session.Operation_worker.Completed
               { final_history =
                   Agent_session.History_codec.all_of_protocol
                     state.conversation.canonical_history
                   |> protocol_ok
               ; moderator_snapshot = state.moderator
               ; runtime_requests = []
               })
         in
         A.set_operation_worker actor (Some worker) |> protocol_ok;
         A.start ?actor:original actor ~attachment_id:writer.id |> protocol_ok |> ignore;
         let submitted =
           (match original with
            | None ->
              A.submit_message
                actor
                ~submitting_principal:principal_id
                ~attachment_id:writer.id
                actor_entry
            | Some original ->
              A.submit_message_authenticated
                actor
                ~actor:original
                ~attachment_id:writer.id
                actor_entry)
           |> protocol_ok
         in
         let state = await_idle actor in
         assert !worker_finished;
         assert (Option.is_none state.failure);
         let operation_id = Option.value_exn submitted.operation_id in
         let terminal =
           List.find_exn !terminal_operations ~f:(fun operation ->
             P.Id.Operation.equal operation.id operation_id)
         in
         (match mode, terminal.state with
          | (`Rejected_save | `Changed_source), Failed failure ->
            [%test_eq: P.Error.code] Internal_error failure.code;
            [%test_eq: string]
              "worker completed with an active invocation"
              failure.message
          | (`Legacy | `Original), Completed ->
            assert (List.exists !completed ~f:(P.Id.Operation.equal operation_id))
          | _ ->
            raise_s
              [%sexp
                "unexpected staged worker terminal"
              , (mode : [ `Legacy | `Original | `Rejected_save | `Changed_source ])
              , (terminal : P.Operation.t)
              , (!callback_result : (I.t, P.Error.t) Result.t option)]);
         [%test_eq: int] 1 !duplicate_aborts;
         let ids = List.map !selected ~f:(fun job -> job.J.id) in
         [%test_eq: int] 2 (List.length ids);
         let present id =
           List.exists state.jobs ~f:(fun job -> P.Id.Job.equal job.id id)
         in
         match mode, Option.value_exn !callback_result with
         | (`Rejected_save | `Changed_source), Error failure ->
           (match mode with
            | `Rejected_save ->
              [%test_eq: P.Error.code] Internal_error failure.code;
              [%test_eq: string] "injected job transition failure" failure.message
            | `Changed_source ->
              [%test_eq: P.Error.code] Conflict failure.code;
              [%test_eq: string]
                "Job admission acquired a different source"
                failure.message
            | `Legacy | `Original -> assert false);
           assert (List.for_all ids ~f:(fun id -> not (present id)));
           assert (List.is_empty !published);
           assert (List.equal P.Id.Job.equal ids (List.rev !aborted))
         | (`Legacy | `Original), Ok { status = Resolved (Complete `Null); _ } ->
           assert (List.for_all ids ~f:present);
           assert (List.equal P.Id.Job.equal ids (List.rev !published));
           assert (List.is_empty !aborted);
           let check_loan job =
             let observed = ref None in
             let resolved =
               with_job actor job (fun ~job:_ ~execute ->
                 execute ~invocation:(root job) (fun ~dispatched ~capabilities ->
                   let borrow_agent_actor =
                     capabilities.Agent_session.Invocation_capabilities.borrow_agent_actor
                   in
                   let borrowed = borrow_agent_actor ~invocation:dispatched in
                   observed
                   := Some
                        ( borrowed
                        , match borrowed with
                          | Ok (Some proof) -> Operator_authorization.is_current proof
                          | Ok None -> not authenticated
                          | Error _ -> false );
                   Ok (I.Complete `Null)))
               |> protocol_ok
             in
             (match resolved.I.status with
              | Resolved (Complete `Null) -> ()
              | _ -> failwith "Job oracle callback did not complete");
             match authenticated, Option.value_exn !observed with
             | false, (Ok None, true) -> ()
             | true, (Ok (Some proof), true) -> Auth.expired proof
             | _, (Error failure, _) -> raise_s [%sexp (failure : P.Error.t)]
             | _ -> failwith "selected commit lost independent or selected loan"
           in
           let independent = Option.value_exn !claimed_independent in
           check_loan independent;
           let claim (job : J.t) =
             A.claim_job actor ~job_id:job.id ~generation:job.generation
             |> protocol_ok
             |> Option.value_exn
           in
           let first = claim (List.nth_exn !selected 0) in
           let second = claim (List.nth_exn !selected 1) in
           check_loan first;
           check_loan second;
           let child = ref None in
           let dependency_call = root first in
           let pending =
             with_job actor first (fun ~job:_ ~execute ->
               execute ~invocation:dependency_call (fun ~dispatched ~capabilities:_ ->
                 let owner = J.Invocation dispatched.context.id in
                 let job = stage owner in
                 child := Some job;
                 A.select_background_jobs actor ~owner ~ids:[ job.id ] |> protocol_ok;
                 Ok (I.Pending (Job job.id, `Null))))
             |> protocol_ok
           in
           let child = Option.value_exn !child in
           (match pending.status with
            | Resolved (Pending (Job id, `Null)) -> assert (P.Id.Job.equal id child.id)
            | _ -> failwith "defer target did not acknowledge actual owned child");
           let dependency : J.dependency =
             { invocation_id = dependency_call.context.id
             ; work = Job child.id
             ; deadline
             ; completion_schema = None
             ; max_output_bytes = 1_000_000
             ; max_output_depth = 128
             }
           in
           let defer () =
             A.defer_background_job
               actor
               ~job_id:first.id
               ~generation:first.generation
               ~attempt:first.attempt
               dependency
           in
           reject := true;
           assert (Result.is_error (defer ()));
           reject := false;
           (match (current backend first).status with
            | Running -> ()
            | _ -> failwith "rejected defer changed current Job");
           let waiting = defer () |> protocol_ok in
           (match waiting.status with
            | Waiting_completion saved -> assert (J.equal_dependency dependency saved)
            | _ -> failwith "real defer failed to retain the actual dependency");
           let child = claim child in
           check_loan child;
           A.complete_background_job
             actor
             ~job_id:child.id
             ~generation:child.generation
             ~attempt:child.attempt
             (Succeeded `Null)
           |> protocol_ok
           |> ignore;
           let settled =
             A.refresh_background_job
               actor
               ~job_id:first.id
               ~generation:first.generation
               ~attempt:first.attempt
             |> protocol_ok
           in
           (match settled.status with
            | Succeeded -> ()
            | _ -> failwith "saved child completion did not settle deferred parent");
           complete actor independent |> protocol_ok |> ignore;
           A.complete_background_job
             actor
             ~job_id:second.id
             ~generation:second.generation
             ~attempt:second.attempt
             (Succeeded `Null)
           |> protocol_ok
           |> ignore
         | _, Error failure -> raise_s [%sexp (failure : P.Error.t)]
         | _, Ok _ -> failwith "staged callback outcome disagrees with its ACK cut"));
  print_endline
    "one current inventory across A/C/B; original and legacy admissions survive parent; \
     failed ACK/source change publish no child; real dependency defers and settles";
  [%expect
    {| one current inventory across A/C/B; original and legacy admissions survive parent; failed ACK/source change publish no child; real dependency defers and settles |}]
;;
