open! Core

let interrupted_reason = "daemon restarted while the job was running"

let reconcile_job entry (job : Agent_protocol.Job.t) =
  match job.status with
  | Agent_protocol.Job.Running ->
    Agent_session.Session_actor.interrupt_job
      entry.Session_registry.actor
      ~job_id:job.id
      ~generation:job.generation
      ~attempt:job.attempt
      ~reason:interrupted_reason
    |> Result.map ~f:(fun (_ : Agent_protocol.Job.t) -> ())
  | Queued
  | Waiting_permission _
  | Waiting_completion _
  | Succeeded
  | Failed _
  | Cancelled
  | Interrupted _ -> Ok ()
;;

let jobs entry ~max_count ~max_total_bytes =
  let open Result.Let_syntax in
  let%bind () =
    Agent_session.Session_actor.recover_background_results
      entry.Session_registry.actor
      ~max_count
      ~max_total_bytes
  in
  let%bind state = Agent_session.Session_actor.state entry.actor in
  List.fold_result state.jobs ~init:() ~f:(fun () job -> reconcile_job entry job)
;;

let failure message = Agent_protocol.Error.create Interrupted ~message ~retryable:false ()

let is_overdue schedule timestamp =
  Agent_protocol.Timestamp.compare schedule.Agent_protocol.Schedule.next_due_at timestamp
  <= 0
;;

let rec reconcile_schedule entry startup_time (schedule : Agent_protocol.Schedule.t) =
  match schedule.status with
  | Agent_protocol.Schedule.Delivering ->
    (match
       Agent_session.Session_actor.retry_schedule
         entry.Session_registry.actor
         ~schedule_id:schedule.id
         ~generation:schedule.generation
     with
     | Ok schedule -> reconcile_schedule entry startup_time schedule
     | Error error -> Error error)
  | Scheduled when is_overdue schedule startup_time ->
    (match schedule.misfire with
     | Deliver_once_immediately -> Ok ()
     | Skip_if_expired ->
       Agent_session.Session_actor.skip_schedule
         entry.Session_registry.actor
         ~schedule_id:schedule.id
         ~generation:schedule.generation
       |> Result.map ~f:(fun (_ : Agent_protocol.Schedule.t) -> ())
     | Fail ->
       Agent_session.Session_actor.fail_schedule
         entry.actor
         ~schedule_id:schedule.id
         ~generation:schedule.generation
         (failure "schedule expired while the daemon was unavailable")
       |> Result.map ~f:(fun (_ : Agent_protocol.Schedule.t) -> ()))
  | Scheduled | Delivered | Cancelled | Failed _ -> Ok ()
;;

let schedules entry ~startup_time =
  let open Result.Let_syntax in
  let%bind _ =
    Agent_session.Session_actor.expire_subscriptions entry.Session_registry.actor
  in
  let%bind state = Agent_session.Session_actor.state entry.actor in
  List.fold_result state.schedules ~init:() ~f:(fun () schedule ->
    reconcile_schedule entry startup_time schedule)
;;
