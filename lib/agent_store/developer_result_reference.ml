open! Core
module P = Agent_protocol
module F = Document_fields

type t =
  { owner : P.Execution_owner.developer
  ; capture_fingerprint : Chatml_source.Digest.t
  ; job_id : P.Id.Job.t
  ; attempt : int
  ; outcome : P.Stored_completion.outcome
  ; blob : P.Blob.Metadata.t
  }

let owner t = t.owner
let capture_fingerprint t = t.capture_fingerprint
let job_id t = t.job_id
let attempt t = t.attempt
let outcome t = t.outcome
let blob t = t.blob
let corrupt message = Error (Store_error.Corrupt message)

let create ~claim ~outcome ~blob =
  let job = Developer_run_store.Claim.job claim in
  let attempt = Developer_run_store.Claim.attempt claim in
  let open Result.Let_syntax in
  let%bind () =
    if attempt > 0 && Int.equal attempt (P.Developer_job.state job).attempt
    then Ok ()
    else corrupt "developer result reference has no claimed attempt"
  in
  let%map blob =
    P.Blob.Metadata.of_json (P.Blob.Metadata.to_json blob)
    |> Result.map_error ~f:(fun _ ->
      Store_error.Corrupt "developer result blob metadata is invalid")
  in
  { owner = P.Developer_job.owner job
  ; capture_fingerprint = P.Developer_job.capture_fingerprint job
  ; job_id = P.Developer_job.id job
  ; attempt
  ; outcome
  ; blob
  }
;;

let allowed_use t =
  String.concat
    ~sep:":"
    [ "developer-job-result"
    ; P.Id.Server.to_string t.owner.host_id
    ; P.Id.Developer_run.to_string t.owner.run_id
    ; Int.to_string t.owner.epoch
    ; P.Id.Principal.to_string t.owner.principal_id
    ; Chatml_source.Digest.to_hex t.capture_fingerprint
    ; P.Id.Job.to_string t.job_id
    ; Int.to_string t.attempt
    ; P.Id.Blob.to_string t.blob.id
    ]
;;

let matches_job t job =
  P.Execution_owner.equal_developer t.owner (P.Developer_job.owner job)
  && Chatml_source.Digest.equal
       t.capture_fingerprint
       (P.Developer_job.capture_fingerprint job)
  && P.Id.Job.equal t.job_id (P.Developer_job.id job)
  && Int.equal t.attempt (P.Developer_job.state job).attempt
;;

let outcome_to_string = function
  | P.Stored_completion.Succeeded -> "succeeded"
  | Failed -> "failed"
  | Cancelled -> "cancelled"
  | Expired -> "expired"
;;

let outcome_of_string = function
  | "succeeded" -> Ok P.Stored_completion.Succeeded
  | "failed" -> Ok Failed
  | "cancelled" -> Ok Cancelled
  | "expired" -> Ok Expired
  | _ -> F.invalid "outcome" "invalid developer result outcome"
;;

let machine field json =
  let open Result.Let_syntax in
  let%bind value = F.decimal json in
  match Int64.to_int value with
  | Some value -> Ok value
  | None -> F.invalid field "integer exceeds machine range"
;;

let to_json t =
  `Object
    [ "owner", Developer_owner_document.to_json t.owner
    ; "capture_fingerprint", `String (Chatml_source.Digest.to_hex t.capture_fingerprint)
    ; "job_id", P.Id.Job.to_json t.job_id
    ; "attempt", F.decimal_json (Int64.of_int t.attempt)
    ; "outcome", `String (outcome_to_string t.outcome)
    ; "blob", P.Blob.Metadata.to_json t.blob
    ]
;;

let decode json =
  let open Result.Let_syntax in
  let%bind owner = F.required json "owner" Developer_owner_document.of_json in
  let%bind capture_fingerprint =
    F.required json "capture_fingerprint" (fun json ->
      let%bind hex = F.string json in
      Chatml_source.Digest.of_hex hex
      |> Result.map_error ~f:(fun _ ->
        Document_schema.Error.Invalid_field
          { path = [ "capture_fingerprint" ]; reason = "invalid SHA-256" }))
  in
  let%bind job_id =
    F.required json "job_id" (fun json -> P.Id.Job.of_json json |> F.protocol)
  in
  let%bind attempt = F.required json "attempt" (machine "attempt") in
  let%bind outcome =
    F.required json "outcome" (fun json ->
      let%bind value = F.string json in
      outcome_of_string value)
  in
  let%bind blob =
    F.required json "blob" (fun json -> P.Blob.Metadata.of_json json |> F.protocol)
  in
  if attempt <= 0
  then F.invalid "attempt" "developer result reference has no claimed attempt"
  else Ok { owner; capture_fingerprint; job_id; attempt; outcome; blob }
;;

let of_json json = F.store (decode json)
