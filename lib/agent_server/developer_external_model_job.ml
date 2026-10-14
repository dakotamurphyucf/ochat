open! Core
module P = Agent_protocol

let create ~owner ~capture_fingerprint ~launch ~request ~target ~limits ~created_at =
  let open Result.Let_syntax in
  let%bind call =
    P.Developer_model_call.create
      ~request:(P.Model_spawn_request.original request)
      ~target
      ~limits
  in
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
  P.Developer_job.create
    ~owner
    ~capture_fingerprint
    ~id:(P.Id.Job.create ())
    ~launch
    ~kind:P.Job.Model_call
    ~payload:(P.Developer_model_call.to_json call)
    ~retry_policy:P.Job.Never
    ~created_at
    ~state
;;
