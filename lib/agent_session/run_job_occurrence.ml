open! Core
module P = Agent_protocol

let attempt (job : P.Job.t) =
  match job.status with
  | Queued -> if job.attempt < Int.max_value then Some (Int.succ job.attempt) else None
  | Running
  | Waiting_permission _
  | Waiting_completion _
  | Succeeded
  | Failed _
  | Cancelled
  | Interrupted _ -> Some job.attempt
;;

let work (job : P.Job.t) =
  let open Result.Let_syntax in
  let%bind attempt =
    Result.of_option
      (attempt job)
      ~error:(P.Error.invalid_request "queued run job has exhausted its attempt number")
  in
  P.Run_work.create
    ~key:(Retained (Job { id = job.id; attempt }))
    ~generation:job.generation
;;

let matches (job : P.Job.t) ~(work : P.Run_work.t) =
  Int.equal job.generation work.generation
  &&
  match work.key, attempt job with
  | Retained (Job { id; attempt }), Some expected ->
    P.Id.Job.equal job.id id && Int.equal attempt expected
  | Operation _, _
  | ( Retained
        (Schedule _ | Invocation _ | Subscription _ | Delivery _ | Moderator_execution _)
    , _ )
  | Retained (Job _), None -> false
;;
