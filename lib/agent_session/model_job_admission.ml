open! Core

type t =
  { admit : Agent_protocol.Job.t -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result
  }

let admit t job = t.admit job

module Host = struct
  let create ~admit = { admit }
end

let prepare (job : Agent_protocol.Job.t) ~session_id ~generation ~origin =
  let module P = Agent_protocol in
  let open Result.Let_syntax in
  let%bind _ = P.Id.Job.of_json (P.Id.Job.to_json job.id) in
  let%bind () = P.Job.Inference_origin.validate origin in
  let%bind () =
    if
      P.Id.Session.equal job.session_id session_id
      && Int.equal job.generation generation
      && (match job.kind, job.status, job.retry_policy, job.delivery with
          | Model_call, Queued, Never, (Pending | Not_required) -> true
          | _, _, _, _ -> false)
      && Int.equal job.attempt 0
      && Option.is_none job.started_at
      && Option.is_none job.next_run_at
      && Option.is_none job.completed_at
      && Option.is_none job.result
      && Option.is_none job.launch
      && Option.is_none job.inference_origin
      && Option.is_none job.progress
    then Ok ()
    else
      Error
        (P.Error.invalid_request
           "model admission requires a fresh unclassified queued recipe Job")
  in
  let%map () = P.Job.validate_result job in
  { job with inference_origin = Some origin }
;;
