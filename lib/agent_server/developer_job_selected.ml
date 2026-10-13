open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store

type t =
  { claim : Store.Claim.t
  ; proposed : P.Developer_job.State.t
  ; after : P.Developer_job.State.t
  ; ensure_attempted : unit -> (unit, P.Error.t) result
  ; acknowledge_installed : Store.Installed.t -> (unit, P.Error.t) result
  }

let same_status a b =
  Jsonaf.exactly_equal (P.Job.status_to_json a) (P.Job.status_to_json b)
;;

let same_error a b = Jsonaf.exactly_equal (P.Error.to_json a) (P.Error.to_json b)

let same_result a b =
  match a, b with
  | P.Developer_job.Result_value.Inline a, P.Developer_job.Result_value.Inline b ->
    P.Completion.equal a b
  | P.Developer_job.Result_value.Artifact a, P.Developer_job.Result_value.Artifact b ->
    P.Stored_completion.equal_outcome a.outcome b.outcome
    && Jsonaf.exactly_equal
         (P.Blob.Metadata.to_json a.blob)
         (P.Blob.Metadata.to_json b.blob)
  | P.Developer_job.Result_value.Inline _, P.Developer_job.Result_value.Artifact _
  | P.Developer_job.Result_value.Artifact _, P.Developer_job.Result_value.Inline _ ->
    false
;;

let same_state (a : P.Developer_job.State.t) (b : P.Developer_job.State.t) =
  same_status a.status b.status
  && Int.equal a.attempt b.attempt
  && Option.equal P.Timestamp.equal a.started_at b.started_at
  && Option.equal P.Timestamp.equal a.next_run_at b.next_run_at
  && Option.equal P.Timestamp.equal a.completed_at b.completed_at
  && Option.equal same_result a.result b.result
  && Option.equal same_error a.retry_failure b.retry_failure
;;

let create
      ~claim
      ~(proposed : P.Developer_job.State.t)
      ~(after : P.Developer_job.State.t)
      ~ensure_attempted
      ~acknowledge_installed
  =
  let job = Store.Claim.job claim in
  let terminal =
    match proposed.status with
    | P.Job.Succeeded | Failed _ | Cancelled | Interrupted _ -> true
    | Queued | Running | Waiting_permission _ | Waiting_completion _ -> false
  in
  let valid_result =
    match proposed.result, after.result with
    | ( Some (P.Developer_job.Result_value.Inline completion)
      , Some (P.Developer_job.Result_value.Artifact value) ) ->
      P.Stored_completion.equal_outcome
        (P.Stored_completion.outcome (P.Stored_completion.Inline completion))
        value.outcome
    | None, None -> true
    | Some a, Some b -> same_result a b
    | None, Some _ | Some _, None -> false
  in
  let same_lifecycle =
    same_status proposed.status after.status
    && Int.equal proposed.attempt after.attempt
    && Option.equal P.Timestamp.equal proposed.started_at after.started_at
    && Option.equal P.Timestamp.equal proposed.next_run_at after.next_run_at
    && Option.equal P.Timestamp.equal proposed.completed_at after.completed_at
    && Option.equal same_error proposed.retry_failure after.retry_failure
  in
  if (not terminal) || (not same_lifecycle) || not valid_result
  then Error (P.Error.invalid_request "selected result changes the completed job")
  else (
    match
      P.Developer_job.with_state job proposed, P.Developer_job.with_state job after
    with
    | Error error, _ | _, Error error -> Error error
    | Ok _, Ok _ -> Ok { claim; proposed; after; ensure_attempted; acknowledge_installed })
;;

let claim t = t.claim
let proposed t = t.proposed
let after t = t.after
let ensure_attempted t = t.ensure_attempted ()
let acknowledge_installed t installed = t.acknowledge_installed installed
