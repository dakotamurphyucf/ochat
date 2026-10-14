open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Blobs = Agent_store.Blob_store
module Actor = Operator_authorization

let error code message = P.Error.create code ~message ~retryable:false ()
let invalid message = Error (P.Error.invalid_request message)
let unauthorized () = Error (error Permission_denied "developer result is unauthorized")

module Selection = struct
  type t =
    { owner : P.Execution_owner.developer
    ; capture_fingerprint : Chatml_source.Digest.t
    ; job_id : P.Id.Job.t
    ; attempt : int
    }

  let create ~owner ~capture_fingerprint ~job_id ~attempt =
    if
      attempt < 0
      || Core.Result.is_error
           (P.Execution_owner.validate (P.Execution_owner.Developer owner))
      || Core.Result.is_error (P.Id.Job.of_json (P.Id.Job.to_json job_id))
      || Core.Result.is_error
           (Chatml_source.Digest.of_hex (Chatml_source.Digest.to_hex capture_fingerprint))
    then invalid "invalid developer result selection"
    else Ok { owner; capture_fingerprint; job_id; attempt }
  ;;

  let owner t = t.owner
  let capture_fingerprint t = t.capture_fingerprint
  let job_id t = t.job_id
  let attempt t = t.attempt
end

module Policy = struct
  type t = { authorize : Actor.t -> Selection.t -> bool }

  let create ~authorize = { authorize }
  let deny_all = { authorize = (fun _ _ -> false) }
end

module Limits = struct
  type t =
    { max_result_bytes : int
    ; json : Document_schema.Limits.t
    }

  let create ~max_result_bytes ~json =
    if
      max_result_bytes <= 0
      || max_result_bytes > 9 * 1024 * 1024
      || Document_schema.Limits.max_bytes json < max_result_bytes
    then invalid "invalid developer result disclosure limits"
    else Ok { max_result_bytes; json }
  ;;
end

module Result = struct
  type t =
    | Pending of P.Job.status
    | Terminal_without_result of P.Job.status
    | Completion of P.Completion.t
  [@@deriving sexp_of]
end

type t =
  { runs : Store.t
  ; blobs : Blobs.t
  ; policy : Policy.t
  ; limits : Limits.t
  }

type request =
  { host : t
  ; original_actor : Actor.t
  ; selection : Selection.t
  }

let create ~runs ~blobs ~policy ~limits =
  if not (Blobs.developer_binding_matches blobs runs)
  then invalid "developer result Store and Blob roots do not match"
  else if Int64.(of_int limits.Limits.max_result_bytes > Blobs.max_upload_bytes blobs)
  then invalid "developer result read exceeds Blob storage limit"
  else Ok { runs; blobs; policy; limits }
;;

let same_principal a b =
  P.Id.Principal.equal (Actor.principal a).id (Actor.principal b).id
;;

let check_actor host selection original_actor current_actor =
  if (not (Actor.is_current original_actor)) || not (Actor.is_current current_actor)
  then Error (error Unauthenticated "developer result actor expired")
  else if not (same_principal original_actor current_actor)
  then unauthorized ()
  else if
    (not (host.policy.authorize original_actor selection))
    || not (host.policy.authorize current_actor selection)
  then unauthorized ()
  else Ok ()
;;

let begin_request host ~original_actor selection =
  let open Core.Result.Let_syntax in
  let%map () = check_actor host selection original_actor original_actor in
  { host; original_actor; selection }
;;

let store_error = function
  | Store.Error.Store error -> Agent_store.Store_error.to_protocol_error error
  | Conflict | Prewrite_conflict ->
    error Persistence_error "developer result Store has an uncertain write"
  | Foreign_or_stale -> error Operation_not_found "developer result selection is stale"
  | Limit_exceeded -> error Resource_limit "developer result Store limit exceeded"
  | Closed -> error Server_shutting_down "developer result Store is closed"
;;

let store result = Core.Result.map_error result ~f:store_error

let blob result =
  Core.Result.map_error result ~f:Agent_store.Store_error.to_protocol_error
;;

let same_job a b =
  P.Developer_job.same_immutable a b
  && Jsonaf.exactly_equal
       (P.Developer_job.State.to_json (P.Developer_job.state a))
       (P.Developer_job.State.to_json (P.Developer_job.state b))
;;

let observe_job root selection =
  let open Core.Result.Let_syntax in
  let%bind job =
    Store.Result_root.find_job_snapshot
      root
      ~owner:selection.Selection.owner
      ~capture_fingerprint:selection.capture_fingerprint
      ~job_id:selection.job_id
      ~attempt:selection.attempt
    |> store
  in
  match job with
  | None -> Error (error Operation_not_found "developer job is absent")
  | Some job ->
    (match P.Developer_job.with_state job (P.Developer_job.state job) with
     | Ok _ -> Ok job
     | Error _ -> Error (error Persistence_error "developer job state is invalid"))
;;

let completion_outcome completion =
  P.Stored_completion.outcome (P.Stored_completion.Inline completion)
;;

let validate_completion host completion =
  let open Core.Result.Let_syntax in
  let json = P.Completion.to_json completion in
  let%bind bytes =
    Document_schema.Json.validate_and_measure ~limits:host.limits.json json
    |> Core.Result.map_error ~f:(fun _ ->
      error Resource_limit "developer completion exceeds decoder limits")
  in
  if bytes > host.limits.max_result_bytes
  then Error (error Resource_limit "developer completion exceeds byte limit")
  else Ok ()
;;

let compare_metadata reference expected actual =
  let owner = Agent_store.Developer_result_reference.owner reference in
  let target : Agent_store.Blob_metadata.Target.t =
    Developer
      { host_id = owner.host_id
      ; run_id = owner.run_id
      ; principal_id = owner.principal_id
      }
  in
  actual.Blobs.Metadata.durable
  && Option.is_none actual.expires_at
  && P.Id.Principal.equal actual.creating_principal owner.principal_id
  && Agent_store.Blob_metadata.Target.equal actual.target target
  && String.equal
       actual.allowed_use
       (Agent_store.Developer_result_reference.allowed_use reference)
  && Blobs.Metadata.blob_equal actual.blob expected
  && P.Blob.equal_kind actual.blob.kind File
  && String.equal actual.blob.media_type "application/vnd.ochat.developer-job-result+json"
  && Option.equal String.equal actual.blob.display_name (Some "developer-job-result.json")
;;

let load_artifact host ~sw root job outcome expected =
  let open Core.Result.Let_syntax in
  let%bind reference =
    Agent_store.Developer_result_reference.of_terminal_job job |> blob
  in
  let%bind () =
    if
      Agent_store.Developer_result_reference.matches_job reference job
      && P.Stored_completion.equal_outcome
           outcome
           (Agent_store.Developer_result_reference.outcome reference)
    then Ok ()
    else Error (error Persistence_error "developer result reference changed")
  in
  let root = Blobs.Result_root.developer root in
  let%bind read = Blobs.open_result host.blobs root expected.P.Blob.Metadata.id |> blob in
  let actual = Blobs.Result_read.metadata read in
  let%bind () =
    if compare_metadata reference expected actual
    then Ok ()
    else Error (error Persistence_error "developer result metadata is mismatched")
  in
  let%bind bytes =
    Blobs.load_result_verified host.blobs ~sw read ~max_bytes:host.limits.max_result_bytes
    |> blob
  in
  let%bind json =
    Document_schema.Json.decode ~limits:host.limits.json bytes
    |> Core.Result.map_error ~f:(fun _ ->
      error Persistence_error "developer result JSON is malformed or oversized")
  in
  let%bind completion = P.Completion.of_json json in
  let%bind () = validate_completion host completion in
  if P.Stored_completion.equal_outcome (completion_outcome completion) outcome
  then Ok completion
  else Error (error Persistence_error "developer result outcome is mismatched")
;;

let read host request ~sw ~current_actor =
  let open Core.Result.Let_syntax in
  if not (phys_equal host request.host)
  then unauthorized ()
  else (
    Eio.Switch.check sw;
    let selection = request.selection in
    let%bind () = check_actor host selection request.original_actor current_actor in
    let%bind root =
      Store.Result_root.find_existing host.runs selection.owner.run_id |> store
    in
    match root with
    | None -> Error (error Operation_not_found "developer run is absent")
    | Some root ->
      let%bind () =
        if Store.Result_root.issued_by host.runs root then Ok () else unauthorized ()
      in
      let%bind before = observe_job root selection in
      let state = P.Developer_job.state before in
      let%bind value =
        match state.status, state.result with
        | (Queued | Running | Waiting_permission _ | Waiting_completion _), None ->
          Ok (Result.Pending state.status)
        | (Failed _ | Cancelled | Interrupted _), None ->
          Ok (Result.Terminal_without_result state.status)
        | (Succeeded | Failed _ | Cancelled | Interrupted _), Some (Inline completion) ->
          let%map () = validate_completion host completion in
          Result.Completion completion
        | (Succeeded | Failed _ | Cancelled | Interrupted _), Some (Artifact artifact) ->
          let%map completion =
            load_artifact host ~sw root before artifact.outcome artifact.blob
          in
          Result.Completion completion
        | (Queued | Running | Waiting_permission _ | Waiting_completion _), Some _
        | Succeeded, None ->
          Error (error Persistence_error "developer job result state is invalid")
      in
      let%bind after = observe_job root selection in
      if not (same_job before after)
      then Error (error Conflict "developer job result changed during read")
      else (
        Eio.Switch.check sw;
        let%map () = check_actor host selection request.original_actor current_actor in
        value))
;;
