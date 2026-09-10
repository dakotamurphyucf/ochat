open Core
open Fixtures

let%expect_test "schedule delivery is generation-checked and actor-committed" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:16
          ~compaction_env:None
          ~initial_state:
            (actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id =
                (fun () ->
                  Agent_protocol.Id.Attachment.of_string "att_schedule_test"
                  |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let attachment, _ =
        Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
        |> protocol_ok
      in
      let schedule =
        Agent_protocol.Schedule.
          { id = Agent_protocol.Id.Schedule.of_string "sch_actor_test" |> protocol_ok
          ; session_id
          ; generation = 0
          ; payload = `Object [ "event", `String "wake" ]
          ; created_at = timestamp
          ; next_due_at = timestamp
          ; misfire = Deliver_once_immediately
          ; status = Scheduled
          ; delivery_count = 0
          ; last_delivery_at = None
          ; ownership = None
          }
      in
      Agent_session.Session_actor.change_schedule
        actor
        ~attachment_id:attachment.id
        ~event:`Created
        schedule
      |> protocol_ok
      |> ignore;
      let stale_rejected =
        Agent_session.Session_actor.claim_schedule
          actor
          ~schedule_id:schedule.id
          ~generation:1
        |> Result.is_error
      in
      let claimed =
        Agent_session.Session_actor.claim_schedule
          actor
          ~schedule_id:schedule.id
          ~generation:0
        |> protocol_ok
        |> Option.value_exn
      in
      let retried =
        Agent_session.Session_actor.retry_schedule
          actor
          ~schedule_id:schedule.id
          ~generation:0
        |> protocol_ok
      in
      let reclaimed =
        Agent_session.Session_actor.claim_schedule
          actor
          ~schedule_id:schedule.id
          ~generation:0
        |> protocol_ok
        |> Option.value_exn
      in
      let completed =
        Agent_session.Session_actor.complete_schedule
          actor
          ~schedule_id:schedule.id
          ~generation:0
          ~moderator_snapshot:(Some (`Object [ "queued", `True ]))
        |> protocol_ok
      in
      let state = Agent_session.Session_actor.state actor |> protocol_ok in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { stale_rejected : bool
          ; claimed = (claimed.status : Agent_protocol.Schedule.status)
          ; retried = (retried.status : Agent_protocol.Schedule.status)
          ; reclaimed = (reclaimed.status : Agent_protocol.Schedule.status)
          ; completed = (completed.status : Agent_protocol.Schedule.status)
          ; delivery_count = (completed.delivery_count : int)
          ; moderator_persisted = (Option.is_some state.moderator : bool)
          }]));
  [%expect
    {|
    ((stale_rejected true) (claimed Delivering) (retried Scheduled)
     (reclaimed Delivering) (completed Delivered) (delivery_count 1)
     (moderator_persisted true))
    |}]
;;

let%expect_test "model jobs are claimed, completed, and delivered atomically" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:16
          ~compaction_env:None
          ~initial_state:
            (actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id =
                (fun () ->
                  Agent_protocol.Id.Attachment.of_string "att_job_test" |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let job =
        Agent_protocol.Job.
          { id = Agent_protocol.Id.Job.of_string "job_actor_test" |> protocol_ok
          ; session_id
          ; generation = 0
          ; kind = Model_call
          ; payload =
              `Object
                [ "recipe", `String "agent_prompt_v1"
                ; "payload", `Object [ "input", `String "test" ]
                ]
          ; status = Queued
          ; retry_policy = Never
          ; attempt = 0
          ; created_at = timestamp
          ; started_at = None
          ; next_run_at = None
          ; completed_at = None
          ; result = None
          ; delivery = Pending
          ; launch = None
          ; progress = None
          }
      in
      Agent_session.Session_actor.add_job actor job |> protocol_ok |> ignore;
      let stale_rejected =
        Agent_session.Session_actor.claim_job actor ~job_id:job.id ~generation:1
        |> Result.is_error
      in
      let claimed =
        Agent_session.Session_actor.claim_job actor ~job_id:job.id ~generation:0
        |> protocol_ok
        |> Option.value_exn
      in
      let completed =
        Agent_session.Session_actor.complete_job
          actor
          ~job_id:job.id
          ~generation:0
          ~attempt:claimed.attempt
          (Agent_session.Runtime_builder.Model_succeeded
             (`Object [ "answer", `String "done" ]))
        |> protocol_ok
      in
      let delivered =
        Agent_session.Session_actor.deliver_job
          actor
          ~job_id:job.id
          ~generation:0
          ~moderator_snapshot:(Some (`Object [ "queued", `True ]))
        |> protocol_ok
      in
      let repeated_cancel =
        Agent_session.Session_actor.cancel_job_internal actor ~job_id:job.id
        |> protocol_ok
      in
      let state = Agent_session.Session_actor.state actor |> protocol_ok in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { stale_rejected : bool
          ; claimed = (claimed.status : Agent_protocol.Job.status)
          ; attempt = (claimed.attempt : int)
          ; completed = (completed.status : Agent_protocol.Job.status)
          ; delivered = (delivered.delivery : Agent_protocol.Job.delivery)
          ; repeated_cancel = (repeated_cancel.status : Agent_protocol.Job.status)
          ; result_persisted = (Option.is_some completed.result : bool)
          ; moderator_persisted = (Option.is_some state.moderator : bool)
          }]));
  [%expect
    {|
    ((stale_rejected true) (claimed Running) (attempt 1) (completed Succeeded)
     (delivered (Delivered 2026-08-15T12:00:00.000000000Z))
     (repeated_cancel Succeeded) (result_persisted true)
     (moderator_persisted true))
    |}]
;;

let%expect_test "durable job retry policy persists backoff before terminal delivery" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let now = ref timestamp in
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:16
          ~compaction_env:None
          ~initial_state:
            (actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> !now)
            ; create_attachment_id =
                (fun () ->
                  Agent_protocol.Id.Attachment.of_string "att_retry_test" |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let job =
        Agent_protocol.Job.
          { id = Agent_protocol.Id.Job.of_string "job_retry_test" |> protocol_ok
          ; session_id
          ; generation = 0
          ; kind = Model_call
          ; payload = `Object []
          ; status = Queued
          ; retry_policy = Safe_retry { max_attempts = 2; backoff_ms = 500 }
          ; attempt = 0
          ; created_at = timestamp
          ; started_at = None
          ; next_run_at = None
          ; completed_at = None
          ; result = None
          ; delivery = Pending
          ; launch = None
          ; progress = None
          }
      in
      Agent_session.Session_actor.add_job actor job |> protocol_ok |> ignore;
      let first_claim =
        Agent_session.Session_actor.claim_job actor ~job_id:job.id ~generation:0
        |> protocol_ok
        |> Option.value_exn
      in
      let retry =
        Agent_session.Session_actor.complete_job
          actor
          ~job_id:job.id
          ~generation:0
          ~attempt:first_claim.attempt
          (Agent_session.Runtime_builder.Model_failed "temporary")
        |> protocol_ok
      in
      let early_claim =
        Agent_session.Session_actor.claim_job actor ~job_id:job.id ~generation:0
        |> protocol_ok
      in
      now
      := Agent_protocol.Timestamp.to_time_ns timestamp
         |> Fn.flip Time_ns.add (Time_ns.Span.of_sec 1.)
         |> Agent_protocol.Timestamp.of_time_ns;
      let second_claim =
        Agent_session.Session_actor.claim_job actor ~job_id:job.id ~generation:0
        |> protocol_ok
        |> Option.value_exn
      in
      let before_stale = Agent_session.Session_actor.state actor |> protocol_ok in
      let conflict = function
        | Error { Agent_protocol.Error.code = Conflict; _ } -> true
        | Ok _ | Error _ -> false
      in
      let stale_completion =
        Agent_session.Session_actor.complete_job
          actor
          ~job_id:job.id
          ~generation:0
          ~attempt:first_claim.attempt
          (Agent_session.Runtime_builder.Model_succeeded (`String "late first result"))
        |> conflict
      in
      let stale_interruption =
        Agent_session.Session_actor.interrupt_job
          actor
          ~job_id:job.id
          ~generation:0
          ~attempt:first_claim.attempt
          ~reason:"late first cleanup"
        |> conflict
      in
      let after_stale = Agent_session.Session_actor.state actor |> protocol_ok in
      assert (stale_completion && stale_interruption);
      [%test_eq: int64]
        before_stale.counters.transaction_sequence
        after_stale.counters.transaction_sequence;
      let terminal =
        Agent_session.Session_actor.complete_job
          actor
          ~job_id:job.id
          ~generation:0
          ~attempt:second_claim.attempt
          (Agent_session.Runtime_builder.Model_failed "permanent")
        |> protocol_ok
      in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { retry_status = (retry.status : Agent_protocol.Job.status)
          ; retry_attempt = (retry.attempt : int)
          ; retry_due = (retry.next_run_at : Agent_protocol.Timestamp.t option)
          ; retry_result = (retry.result : Jsonaf.t option)
          ; early_claim_blocked = (Option.is_none early_claim : bool)
          ; second_attempt = (second_claim.attempt : int)
          ; stale_completion : bool
          ; stale_interruption : bool
          ; terminal_status = (terminal.status : Agent_protocol.Job.status)
          ; terminal_delivery = (terminal.delivery : Agent_protocol.Job.delivery)
          }]));
  [%expect
    {|
    ((retry_status Queued) (retry_attempt 1)
     (retry_due (2026-08-15T12:00:00.500000000Z))
     (retry_result ((Object ((last_error (String temporary))))))
     (early_claim_blocked true) (second_attempt 2) (stale_completion true)
     (stale_interruption true)
     (terminal_status
      (Failed
       ((code Internal_error) (message permanent) (retryable false)
        (data (Object ())))))
     (terminal_delivery Pending))
    |}]
;;
