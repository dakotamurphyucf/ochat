open! Core
module P = Agent_protocol
module B = Chat_response.Background_request

module Prepared = struct
  type t = { job : P.Developer_job.t }

  let job t = t.job
end

let background
      ~owner
      ~capture_fingerprint
      ~launch
      ~selected
      ~policy
      ~request
      ~created_at
      ~checkpoint
  =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind request = B.of_json ~policy (B.to_json request) in
  checkpoint ();
  let%bind () = B.validate_capabilities request ~capabilities:selected in
  checkpoint ();
  let state : P.Developer_job.State.t =
    { status = P.Job.Queued
    ; attempt = 0
    ; started_at = None
    ; next_run_at = None
    ; completed_at = None
    ; result = None
    ; retry_failure = None
    }
  in
  let%map created_job =
    P.Developer_job.create
      ~owner
      ~capture_fingerprint
      ~id:(P.Id.Job.create ())
      ~launch
      ~kind:P.Job.Async_tool
      ~payload:(B.to_json request)
      ~retry_policy:P.Job.Never
      ~created_at
      ~state
  in
  Prepared.{ job = created_job }
;;
