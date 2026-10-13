open Core
module Error = Protocol_error

module Launch = struct
  type parent =
    | Run_root
    | Invocation of Id.Invocation.t
  [@@deriving equal, sexp_of]

  type t =
    { parent : parent
    ; parent_job : (Id.Job.t * int) option
    ; nested_depth : int
    }

  let validate_id to_json of_json id = Extension_codec.validate_id to_json of_json id

  let create ~parent ~parent_job ~nested_depth =
    let open Result.Let_syntax in
    let%bind () =
      match parent with
      | Run_root -> Ok ()
      | Invocation id -> validate_id Id.Invocation.to_json Id.Invocation.of_json id
    in
    let%bind () =
      match parent_job with
      | None -> Ok ()
      | Some (id, attempt) ->
        let%bind () = validate_id Id.Job.to_json Id.Job.of_json id in
        if attempt < 0
        then Error (Error.invalid_request "negative parent job attempt")
        else Ok ()
    in
    if nested_depth < 0
    then Error (Error.invalid_request "negative developer job nesting depth")
    else Ok { parent; parent_job; nested_depth }
  ;;

  let parent t = t.parent
  let parent_job t = t.parent_job
  let nested_depth t = t.nested_depth
end

module Result_value = struct
  type t =
    | Inline of Completion.t
    | Artifact of
        { outcome : Stored_completion.outcome
        ; blob : Blob.Metadata.t
        }
  [@@deriving sexp_of]

  type disposition =
    | Success
    | Failure
    | Cancellation

  let disposition = function
    | Inline (Succeeded _) | Artifact { outcome = Succeeded; _ } -> Success
    | Inline (Failed _ | Expired) | Artifact { outcome = Failed | Expired; _ } -> Failure
    | Inline (Cancelled _) | Artifact { outcome = Cancelled; _ } -> Cancellation
  ;;

  let validate = function
    | Inline completion -> Completion.validate completion
    | Artifact { blob; _ } ->
      Result.map (Blob.Metadata.of_json (Blob.Metadata.to_json blob)) ~f:(fun _ -> ())
  ;;
end

module State = struct
  type t =
    { status : Job.status
    ; attempt : int
    ; started_at : Timestamp.t option
    ; next_run_at : Timestamp.t option
    ; completed_at : Timestamp.t option
    ; result : Result_value.t option
    ; retry_failure : Protocol_error.t option
    }
  [@@deriving sexp_of]

  let invalid message = Error (Error.invalid_request message)

  let validate_dependency ~job_id ~kind ~created_at dependency =
    let open Result.Let_syntax in
    let%bind () =
      if Job.equal_kind kind Async_tool
      then Ok ()
      else invalid "only async tool jobs may wait on a dependency"
    in
    let%bind () =
      Extension_codec.validate_id
        Id.Invocation.to_json
        Id.Invocation.of_json
        dependency.Job.invocation_id
    in
    let%bind () =
      match dependency.work with
      | Invocation.Job target ->
        let%bind () = Extension_codec.validate_id Id.Job.to_json Id.Job.of_json target in
        if Id.Job.equal job_id target
        then invalid "developer job cannot wait on itself"
        else Ok ()
      | Invocation.Subscription subscription ->
        Extension_codec.validate_id
          Id.Subscription.to_json
          Id.Subscription.of_json
          subscription
    in
    if Timestamp.compare dependency.deadline created_at < 0
    then invalid "developer job dependency deadline precedes creation"
    else if dependency.max_output_bytes < 0 || dependency.max_output_depth < 0
    then invalid "negative dependency result budget"
    else Ok ()
  ;;

  let validate_terminal_result status result =
    match status, Option.map result ~f:Result_value.disposition with
    | Job.Succeeded, None -> invalid "successful developer job has no result"
    | Succeeded, Some Result_value.Success
    | Failed _, Some Result_value.Failure
    | Interrupted _, Some Result_value.Failure
    | Cancelled, Some Result_value.Cancellation -> Ok ()
    | (Succeeded | Failed _ | Interrupted _ | Cancelled), Some _ ->
      invalid "developer job status/result mismatch"
    | (Failed _ | Cancelled | Interrupted _), None -> Ok ()
    | (Queued | Running | Waiting_permission _ | Waiting_completion _), _ ->
      invalid "nonterminal job cannot have terminal validation"
  ;;

  let validate ~job_id ~kind ~created_at t =
    let open Result.Let_syntax in
    let%bind () =
      if t.attempt < 0 then invalid "negative developer job attempt" else Ok ()
    in
    let%bind () =
      match t.result with
      | None -> Ok ()
      | Some result -> Result_value.validate result
    in
    match t.status with
    | Queued ->
      if Option.is_some t.completed_at
      then invalid "queued developer job has completion time"
      else if Int.equal t.attempt 0
      then
        if
          Option.is_some t.started_at
          || Option.is_some t.next_run_at
          || Option.is_some t.result
          || Option.is_some t.retry_failure
        then invalid "new developer job has prior execution state"
        else Ok ()
      else if
        Option.is_some t.started_at
        || Option.is_none t.next_run_at
        || Option.is_some t.result
        || Option.is_none t.retry_failure
      then invalid "invalid queued developer job retry state"
      else Ok ()
    | Running ->
      if
        t.attempt <= 0
        || Option.is_none t.started_at
        || Option.is_some t.next_run_at
        || Option.is_some t.completed_at
        || Option.is_some t.result
        || Option.is_some t.retry_failure
      then invalid "invalid active developer job state"
      else Ok ()
    | Waiting_permission permission ->
      let%bind () =
        Extension_codec.validate_id Id.Permission.to_json Id.Permission.of_json permission
      in
      if
        t.attempt <= 0
        || Option.is_none t.started_at
        || Option.is_some t.next_run_at
        || Option.is_some t.completed_at
        || Option.is_some t.result
        || Option.is_some t.retry_failure
      then invalid "invalid permission-waiting developer job state"
      else Ok ()
    | Waiting_completion dependency ->
      let%bind () = validate_dependency ~job_id ~kind ~created_at dependency in
      if
        t.attempt <= 0
        || Option.is_none t.started_at
        || Option.is_some t.next_run_at
        || Option.is_some t.completed_at
        || Option.is_some t.result
        || Option.is_some t.retry_failure
      then invalid "invalid waiting developer job state"
      else Ok ()
    | (Succeeded | Failed _ | Cancelled | Interrupted _) as status ->
      if
        Option.is_some t.next_run_at
        || Option.is_none t.completed_at
        || Option.is_some t.retry_failure
      then invalid "invalid terminal developer job time"
      else if Int.equal t.attempt 0 && Option.is_some t.started_at
      then invalid "unclaimed developer job has a start"
      else if t.attempt > 0 && Option.is_none t.started_at
      then invalid "claimed developer job has no start"
      else if
        match status with
        | Succeeded -> Int.equal t.attempt 0 || Option.is_none t.started_at
        | Failed _ | Cancelled | Interrupted _ -> false
        | Queued | Running | Waiting_permission _ | Waiting_completion _ -> false
      then invalid "successful developer job was not claimed"
      else validate_terminal_result status t.result
  ;;
end

type t =
  { owner : Execution_owner.developer
  ; capture_fingerprint : Chatml_source.Digest.t
  ; id : Id.Job.t
  ; launch : Launch.t
  ; kind : Job.kind
  ; payload : Jsonaf.t
  ; retry_policy : Job.retry_policy
  ; created_at : Timestamp.t
  ; state : State.t
  }

let validate_retry_policy = function
  | Job.Never -> Ok ()
  | Safe_retry { max_attempts; backoff_ms } ->
    if max_attempts <= 0 || backoff_ms < 0
    then Error (Error.invalid_request "invalid developer job retry policy")
    else Ok ()
  | Idempotent { key; max_attempts; backoff_ms } ->
    if max_attempts <= 0 || backoff_ms < 0
    then Error (Error.invalid_request "invalid developer job retry policy")
    else
      Result.map
        (Idempotency_key.of_string (Idempotency_key.to_string key))
        ~f:(fun _ -> ())
;;

let validate_attempt retry_policy attempt =
  let maximum =
    match retry_policy with
    | Job.Never -> 1
    | Safe_retry { max_attempts; _ } | Idempotent { max_attempts; _ } -> max_attempts
  in
  if attempt > maximum
  then Error (Error.invalid_request "developer job attempt exceeds retry policy")
  else Ok ()
;;

let validate_retry_state retry_policy (state : State.t) =
  match retry_policy, state.status with
  | Job.Never, Queued when state.attempt > 0 ->
    Error (Error.invalid_request "nonretrying job cannot queue another attempt")
  | (Safe_retry { max_attempts; _ } | Idempotent { max_attempts; _ }), Queued
    when state.attempt > 0 && state.attempt >= max_attempts ->
    Error (Error.invalid_request "queued developer job has no retry attempt remaining")
  | ( Job.Never
    , ( Queued
      | Running
      | Waiting_permission _
      | Waiting_completion _
      | Succeeded
      | Failed _
      | Cancelled
      | Interrupted _ ) )
  | (Safe_retry _ | Idempotent _), _ -> Ok ()
;;

let create
      ~owner
      ~capture_fingerprint
      ~id
      ~launch
      ~kind
      ~payload
      ~retry_policy
      ~created_at
      ~(state : State.t)
  =
  let open Result.Let_syntax in
  let%bind () = Execution_owner.validate (Execution_owner.Developer owner) in
  let%bind () = Extension_codec.validate_id Id.Job.to_json Id.Job.of_json id in
  let%bind () = validate_retry_policy retry_policy in
  let%bind () = validate_attempt retry_policy state.attempt in
  let%bind () = validate_retry_state retry_policy state in
  let%bind () =
    match Launch.parent_job launch with
    | Some (parent_id, _) when Id.Job.equal id parent_id ->
      Error (Error.invalid_request "developer job cannot parent itself")
    | None | Some _ -> Ok ()
  in
  let%map () = State.validate ~job_id:id ~kind ~created_at state in
  { owner
  ; capture_fingerprint
  ; id
  ; launch
  ; kind
  ; payload
  ; retry_policy
  ; created_at
  ; state
  }
;;

let with_state t (state : State.t) =
  let open Result.Let_syntax in
  let%bind () = validate_attempt t.retry_policy state.attempt in
  let%bind () = validate_retry_state t.retry_policy state in
  let%map () = State.validate ~job_id:t.id ~kind:t.kind ~created_at:t.created_at state in
  { t with state }
;;

let owner t = t.owner
let capture_fingerprint t = t.capture_fingerprint
let id t = t.id
let launch t = t.launch
let kind t = t.kind
let payload t = t.payload
let retry_policy t = t.retry_policy
let created_at t = t.created_at
let state t = t.state
