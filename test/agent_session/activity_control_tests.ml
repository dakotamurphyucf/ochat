open! Core
open Fixtures
module P = Agent_protocol
module A = Agent_session.Session_actor

let with_actor ?(prepare = Fn.id) f =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let actor =
        A.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:16
          ~compaction_env:None
          ~initial_state:
            (prepare
               (actor_state
                  ~workspace_instance
                  ~liveness:Process_bound
                  ~start_immediately:false))
          ~persistence:
            { archive_reference; commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id = P.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "activity-test")
            ; job_results = None
            ; monotonic_now = (fun () -> Mtime.min_stamp)
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; notification_limits = Agent_session.Staged_notifications.default_limits
            ; ingress_limits = Agent_session.Staged_ingress.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let writer, _ = A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok in
      let value = f actor writer in
      A.shutdown actor;
      value))
;;

let code = function
  | Ok _ -> "accepted"
  | Error (error : P.Error.t) -> P.Error.code_to_string error.code
;;

let%expect_test "job occurrence controls deny stale attempts before any transition" =
  with_actor (fun actor writer ->
    let job =
      P.Job.
        { id = P.Id.Job.create ()
        ; session_id
        ; generation = 0
        ; kind = Model_call
        ; payload = `Object []
        ; status = Queued
        ; retry_policy = Never
        ; attempt = 2
        ; created_at = timestamp
        ; started_at = None
        ; next_run_at = None
        ; completed_at = None
        ; result = None
        ; delivery = Not_required
        ; launch = None
        ; progress = None
        }
    in
    A.add_job actor job |> protocol_ok |> ignore;
    let before = A.snapshot actor |> protocol_ok in
    let stale =
      A.cancel_job
        actor
        ~attachment_id:writer.id
        ~job_id:job.id
        ~expected_generation:0
        ~expected_attempt:1
        ()
    in
    let malformed =
      A.cancel_job actor ~attachment_id:writer.id ~job_id:job.id ~expected_generation:0 ()
    in
    let after = A.snapshot actor |> protocol_ok in
    let cancelled =
      A.cancel_job
        actor
        ~attachment_id:writer.id
        ~job_id:job.id
        ~expected_generation:0
        ~expected_attempt:2
        ()
      |> protocol_ok
    in
    let terminal =
      match cancelled.status with
      | Cancelled -> true
      | Queued
      | Running
      | Waiting_permission _
      | Waiting_completion _
      | Succeeded
      | Failed _
      | Interrupted _ -> false
    in
    print_s
      [%sexp
        { stale = (code stale : string)
        ; malformed = (code malformed : string)
        ; unchanged = (Int64.equal before.revision after.revision : bool)
        ; terminal : bool
        ; attempt_retained = (cancelled.attempt : int)
        }]);
  [%expect
    {|
    ((stale conflict) (malformed invalid_request) (unchanged true)
     (terminal true) (attempt_retained 2))
    |}]
;;

let%expect_test
    "schedule occurrence controls retain the current schedule on stale witness"
  =
  with_actor (fun actor writer ->
    let schedule =
      P.Schedule.
        { id = P.Id.Schedule.create ()
        ; session_id
        ; generation = 0
        ; payload = `String "event"
        ; created_at = timestamp
        ; next_due_at = timestamp
        ; misfire = Deliver_once_immediately
        ; status = Scheduled
        ; delivery_count = 0
        ; last_delivery_at = None
        ; ownership = None
        ; delivery_cancellation = None
        }
    in
    A.add_schedule actor schedule |> protocol_ok |> ignore;
    let before = A.snapshot actor |> protocol_ok in
    let stale =
      A.cancel_schedule
        actor
        ~attachment_id:writer.id
        ~schedule_id:schedule.id
        ~expected_generation:1
        ()
    in
    let after = A.snapshot actor |> protocol_ok in
    let cancelled =
      A.cancel_schedule
        actor
        ~attachment_id:writer.id
        ~schedule_id:schedule.id
        ~expected_generation:0
        ()
      |> protocol_ok
    in
    let terminal =
      match cancelled.status with
      | Cancelled -> true
      | Scheduled | Delivering | Delivered | Failed _ -> false
    in
    print_s
      [%sexp
        { stale = (code stale : string)
        ; unchanged = (Int64.equal before.revision after.revision : bool)
        ; terminal : bool
        }]);
  [%expect {| ((stale conflict) (unchanged true) (terminal true)) |}]
;;

let%expect_test
    "witnessed old-generation records cannot be cancelled after session generation \
     changes"
  =
  let job =
    P.Job.
      { id = P.Id.Job.of_string "job_activity_retained" |> protocol_ok
      ; session_id
      ; generation = 0
      ; kind = Async_tool
      ; payload = `Object [ "fixture", `String "retained tool job" ]
      ; status = Queued
      ; retry_policy = Never
      ; attempt = 1
      ; created_at = timestamp
      ; started_at = None
      ; next_run_at = None
      ; completed_at = None
      ; result = None
      ; delivery = Not_required
      ; launch = None
      ; progress = None
      }
  in
  let schedule =
    P.Schedule.
      { id = P.Id.Schedule.of_string "sch_activity_retained" |> protocol_ok
      ; session_id
      ; generation = 0
      ; payload = `String "retained"
      ; created_at = timestamp
      ; next_due_at = timestamp
      ; misfire = Deliver_once_immediately
      ; status = Scheduled
      ; delivery_count = 0
      ; last_delivery_at = None
      ; ownership = None
      ; delivery_cancellation = None
      }
  in
  with_actor
    ~prepare:(fun (state : Agent_session.Session_state.t) ->
      let inference_ledger =
        Agent_session.Inference_ledger.with_generation
          state.inference_ledger
          ~generation:1
        |> Result.map_error ~f:(fun error ->
          Sexp.to_string_hum (Agent_session.Inference_ledger.Error.sexp_of_t error))
        |> Result.ok_or_failwith
      in
      let state =
        { state with
          identity = { state.identity with generation = 1 }
        ; inference_ledger
        ; jobs = [ job ]
        ; schedules = [ schedule ]
        }
      in
      Agent_session.Session_state.validate state |> protocol_ok;
      state)
    (fun actor writer ->
       let before = A.snapshot actor |> protocol_ok in
       let job_result =
         A.cancel_job
           actor
           ~attachment_id:writer.id
           ~job_id:job.id
           ~expected_generation:0
           ~expected_attempt:1
           ()
       in
       let schedule_result =
         A.cancel_schedule
           actor
           ~attachment_id:writer.id
           ~schedule_id:schedule.id
           ~expected_generation:0
           ()
       in
       let after = A.snapshot actor |> protocol_ok in
       print_s
         [%sexp
           { job = (code job_result : string)
           ; schedule = (code schedule_result : string)
           ; unchanged = (Int64.equal before.revision after.revision : bool)
           ; current_generation = (after.session.generation : int)
           }]);
  [%expect
    {| ((job conflict) (schedule conflict) (unchanged true) (current_generation 1)) |}]
;;
