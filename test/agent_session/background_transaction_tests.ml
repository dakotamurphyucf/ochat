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
         execute ~invocation (fun ~dispatched:_ ->
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
               execute ~invocation (fun ~dispatched:_ ->
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
                Agent_session.Session_state.t_of_sexp
                  (Agent_session.Session_state.sexp_of_t state)
              in
              Agent_session.Session_state.validate restored |> protocol_ok;
              let decoded = J.of_json (J.to_json stored) |> protocol_ok in
              assert (Option.equal J.equal_launch stored.launch decoded.launch);
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
                      services.execute ~invocation:(root parent) (fun ~dispatched:root ->
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
                            (fun ~dispatched ~commit ->
                               let job = start (J.Invocation dispatched.context.id) in
                               let outcome =
                                 match mode with
                                 | `Handler_cancelled -> I.Cancelled "handler cancelled"
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
    (Event true)
    |}]
;;
