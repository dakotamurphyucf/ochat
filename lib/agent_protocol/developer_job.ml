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

let optional name value encode =
  Option.to_list (Option.map value ~f:(fun item -> name, encode item))
;;

let launch_to_json launch =
  let parent =
    match Launch.parent launch with
    | Run_root -> `Object [ "type", `String "run_root" ]
    | Invocation id ->
      `Object [ "type", `String "invocation"; "id", Id.Invocation.to_json id ]
  in
  `Object
    ([ "parent", parent
     ; "nested_depth", `Number (Int.to_string (Launch.nested_depth launch))
     ]
     @ optional "parent_job" (Launch.parent_job launch) (fun (id, attempt) ->
       `Object [ "id", Id.Job.to_json id; "attempt", `Number (Int.to_string attempt) ]))
;;

let launch_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed fields [ "parent"; "parent_job"; "nested_depth" ]
  in
  let%bind parent_json = Json_codec.required fields "parent" in
  let%bind parent_fields = Json_codec.fields parent_json in
  let%bind kind = Json_codec.required_as parent_fields "type" Json_codec.string in
  let%bind parent =
    match kind with
    | "run_root" ->
      let%map () = Extension_codec.closed parent_fields [ "type" ] in
      Launch.Run_root
    | "invocation" ->
      let%bind () = Extension_codec.closed parent_fields [ "type"; "id" ] in
      let%map id = Json_codec.required_as parent_fields "id" Id.Invocation.of_json in
      Launch.Invocation id
    | _ -> Error (Error.invalid_request "invalid developer job launch parent")
  in
  let%bind parent_job =
    Json_codec.optional_as fields "parent_job" (fun json ->
      let%bind fields = Json_codec.fields json in
      let%bind () = Extension_codec.closed fields [ "id"; "attempt" ] in
      let%bind id = Json_codec.required_as fields "id" Id.Job.of_json in
      let%map attempt =
        Json_codec.required_as
          fields
          "attempt"
          (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
      in
      id, attempt)
  in
  let%bind nested_depth =
    Json_codec.required_as
      fields
      "nested_depth"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  Launch.create ~parent ~parent_job ~nested_depth
;;

let result_to_json = function
  | Result_value.Inline completion ->
    `Object [ "type", `String "inline"; "completion", Completion.to_json completion ]
  | Artifact { outcome; blob } ->
    let outcome =
      match outcome with
      | Stored_completion.Succeeded -> "succeeded"
      | Failed -> "failed"
      | Cancelled -> "cancelled"
      | Expired -> "expired"
    in
    `Object
      [ "type", `String "artifact"
      ; "outcome", `String outcome
      ; "blob", Blob.Metadata.to_json blob
      ]
;;

let result_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind kind = Json_codec.required_as fields "type" Json_codec.string in
  match kind with
  | "inline" ->
    let%bind () = Extension_codec.closed fields [ "type"; "completion" ] in
    let%map completion = Json_codec.required_as fields "completion" Completion.of_json in
    Result_value.Inline completion
  | "artifact" ->
    let%bind () = Extension_codec.closed fields [ "type"; "outcome"; "blob" ] in
    let%bind outcome =
      Json_codec.required_as
        fields
        "outcome"
        (Json_codec.enum
           ~name:"developer job artifact outcome"
           [ "succeeded", Stored_completion.Succeeded
           ; "failed", Failed
           ; "cancelled", Cancelled
           ; "expired", Expired
           ])
    in
    let%map blob = Json_codec.required_as fields "blob" Blob.Metadata.of_json in
    Result_value.Artifact { outcome; blob }
  | _ -> Error (Error.invalid_request "invalid developer job result type")
;;

let state_to_json (state : State.t) =
  `Object
    ([ "status", Job.status_to_json state.status
     ; "attempt", `Number (Int.to_string state.attempt)
     ]
     @ optional "started_at" state.started_at Timestamp.to_json
     @ optional "next_run_at" state.next_run_at Timestamp.to_json
     @ optional "completed_at" state.completed_at Timestamp.to_json
     @ optional "result" state.result result_to_json
     @ optional "retry_failure" state.retry_failure Error.to_json)
;;

let state_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed
      fields
      [ "status"
      ; "attempt"
      ; "started_at"
      ; "next_run_at"
      ; "completed_at"
      ; "result"
      ; "retry_failure"
      ]
  in
  let%bind status = Json_codec.required_as fields "status" Job.status_of_json in
  let%bind attempt =
    Json_codec.required_as
      fields
      "attempt"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind started_at = Json_codec.optional_as fields "started_at" Timestamp.of_json in
  let%bind next_run_at = Json_codec.optional_as fields "next_run_at" Timestamp.of_json in
  let%bind completed_at =
    Json_codec.optional_as fields "completed_at" Timestamp.of_json
  in
  let%bind result = Json_codec.optional_as fields "result" result_of_json in
  let%map retry_failure = Json_codec.optional_as fields "retry_failure" Error.of_json in
  State.{ status; attempt; started_at; next_run_at; completed_at; result; retry_failure }
;;

let to_json t =
  `Object
    [ "schema_version", `Number "1"
    ; "host_id", Id.Server.to_json t.owner.host_id
    ; "run_id", Id.Developer_run.to_json t.owner.run_id
    ; "epoch", `Number (Int.to_string t.owner.epoch)
    ; "principal_id", Id.Principal.to_json t.owner.principal_id
    ; "capture_fingerprint", `String (Chatml_source.Digest.to_hex t.capture_fingerprint)
    ; "id", Id.Job.to_json t.id
    ; "launch", launch_to_json t.launch
    ; "kind", Job.kind_to_json t.kind
    ; "payload", t.payload
    ; "retry_policy", Job.retry_to_json t.retry_policy
    ; "created_at", Timestamp.to_json t.created_at
    ; "state", state_to_json t.state
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed
      fields
      [ "schema_version"
      ; "host_id"
      ; "run_id"
      ; "epoch"
      ; "principal_id"
      ; "capture_fingerprint"
      ; "id"
      ; "launch"
      ; "kind"
      ; "payload"
      ; "retry_policy"
      ; "created_at"
      ; "state"
      ]
  in
  let%bind _version =
    Json_codec.required_as fields "schema_version" (Json_codec.bounded_int ~min:1 ~max:1)
  in
  let%bind host_id = Json_codec.required_as fields "host_id" Id.Server.of_json in
  let%bind run_id = Json_codec.required_as fields "run_id" Id.Developer_run.of_json in
  let%bind epoch =
    Json_codec.required_as
      fields
      "epoch"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind principal_id =
    Json_codec.required_as fields "principal_id" Id.Principal.of_json
  in
  let%bind owner = Execution_owner.developer ~host_id ~run_id ~epoch ~principal_id in
  let%bind capture_hex =
    Json_codec.required_as fields "capture_fingerprint" Json_codec.string
  in
  let%bind capture_fingerprint =
    Chatml_source.Digest.of_hex capture_hex
    |> Result.map_error ~f:(fun _ -> Error.invalid_request "invalid developer capture")
  in
  let%bind id = Json_codec.required_as fields "id" Id.Job.of_json in
  let%bind launch = Json_codec.required_as fields "launch" launch_of_json in
  let%bind kind = Json_codec.required_as fields "kind" Job.kind_of_json in
  let%bind payload = Json_codec.required fields "payload" in
  let%bind retry_policy =
    Json_codec.required_as fields "retry_policy" Job.retry_of_json
  in
  let%bind created_at = Json_codec.required_as fields "created_at" Timestamp.of_json in
  let%bind state = Json_codec.required_as fields "state" state_of_json in
  let%bind job =
    create
      ~owner:
        (match owner with
         | Execution_owner.Developer owner -> owner
         | Conversation _ -> assert false)
      ~capture_fingerprint
      ~id
      ~launch
      ~kind
      ~payload
      ~retry_policy
      ~created_at
      ~state
  in
  if Jsonaf.exactly_equal json (to_json job)
  then Ok job
  else Error (Error.invalid_request "noncanonical developer job record")
;;
