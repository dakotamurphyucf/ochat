open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol

module Selection = struct
  type t =
    | Inline of P.Completion.t
    | Artifact of
        { reference : Developer_result_reference.t
        ; stage : Blob_stage_documents.t
        }
end

type value =
  { owner : P.Execution_owner.developer
  ; capture_fingerprint : Chatml_source.Digest.t
  ; job_id : P.Id.Job.t
  ; attempt : int
  ; selected_at : P.Timestamp.t
  ; after : P.Developer_job.State.t
  ; selection : Selection.t
  }

type t = value D.Extension_carrier.t

let value t = D.Extension_carrier.value t
let owner t = (value t).owner
let capture_fingerprint t = (value t).capture_fingerprint
let job_id t = (value t).job_id
let attempt t = (value t).attempt
let selected_at t = (value t).selected_at
let after t = (value t).after
let selection t = (value t).selection
let max_inline_bytes = 32 * 1024

let machine field json =
  let open Result.Let_syntax in
  let%bind value = F.decimal json in
  match Int64.to_int value with
  | Some value -> Ok value
  | None -> F.invalid field "integer exceeds machine range"
;;

let digest json =
  let open Result.Let_syntax in
  let%bind hex = F.string json in
  Chatml_source.Digest.of_hex hex
  |> Result.map_error ~f:(fun _ ->
    D.Error.Invalid_field { path = [ "capture_fingerprint" ]; reason = "invalid SHA-256" })
;;

let selection_to_json = function
  | Selection.Inline completion ->
    `Object [ "mode", `String "inline"; "completion", P.Completion.to_json completion ]
  | Selection.Artifact { reference; stage } ->
    `Object
      [ "mode", `String "artifact"
      ; "reference", Developer_result_reference.to_json reference
      ; "temporary_metadata_bytes", `String (Blob_stage_documents.temporary_bytes stage)
      ; "durable_metadata_bytes", `String (Blob_stage_documents.durable_bytes stage)
      ]
;;

let selection_of_json json =
  let open Result.Let_syntax in
  let%bind mode = F.required json "mode" F.string in
  match mode with
  | "inline" ->
    let%map completion =
      F.required json "completion" (fun json -> P.Completion.of_json json |> F.protocol)
    in
    Selection.Inline completion
  | "artifact" ->
    let%bind reference =
      F.required json "reference" (fun json ->
        Developer_result_reference.of_json json
        |> Result.map_error ~f:(fun _ ->
          D.Error.Invalid_field
            { path = [ "reference" ]; reason = "invalid developer result reference" }))
    in
    let%bind temporary_bytes = F.required json "temporary_metadata_bytes" F.string in
    let%bind durable_bytes = F.required json "durable_metadata_bytes" F.string in
    let%map stage =
      Blob_stage_documents.of_publications ~temporary_bytes ~durable_bytes
    in
    Selection.Artifact { reference; stage }
  | _ -> F.invalid "selection" "unknown developer result selection mode"
;;

let validates_selection (value : value) =
  let open Result.Let_syntax in
  let%bind () =
    if value.attempt > 0 && Int.equal value.after.attempt value.attempt
    then Ok ()
    else F.invalid "attempt" "selected result differs from its claimed attempt"
  in
  let%bind () =
    match value.after.status, value.after.completed_at with
    | (P.Job.Succeeded | Failed _ | Cancelled | Interrupted _), Some _ -> Ok ()
    | _ -> F.invalid "after" "selected result has no completed terminal"
  in
  match value.selection, value.after.result with
  | Selection.Inline completion, Some (P.Developer_job.Result_value.Inline selected)
    when P.Completion.equal completion selected ->
    let canonical = P.Completion.to_json completion |> Jsonaf.to_string in
    if String.length canonical <= max_inline_bytes
    then Ok ()
    else F.invalid "selection" "inline completion exceeds its fixed carrier"
  | ( Selection.Artifact { reference; stage }
    , Some (P.Developer_job.Result_value.Artifact result) ) ->
    let temporary = Blob_metadata_document.value (Blob_stage_documents.temporary stage) in
    let durable = Blob_metadata_document.value (Blob_stage_documents.durable stage) in
    let target =
      Blob_metadata.Target.Developer
        { host_id = value.owner.host_id
        ; run_id = value.owner.run_id
        ; principal_id = value.owner.principal_id
        }
    in
    if
      P.Execution_owner.equal_developer
        value.owner
        (Developer_result_reference.owner reference)
      && Chatml_source.Digest.equal
           value.capture_fingerprint
           (Developer_result_reference.capture_fingerprint reference)
      && P.Id.Job.equal value.job_id (Developer_result_reference.job_id reference)
      && Int.equal value.attempt (Developer_result_reference.attempt reference)
      && P.Stored_completion.equal_outcome
           result.outcome
           (Developer_result_reference.outcome reference)
      && Blob_metadata.blob_equal result.blob (Developer_result_reference.blob reference)
      && Blob_metadata.blob_equal temporary.blob result.blob
      && Blob_metadata.blob_equal durable.blob result.blob
      && Blob_metadata.Target.equal temporary.target target
      && Blob_metadata.Target.equal durable.target target
      && P.Id.Principal.equal temporary.creating_principal value.owner.principal_id
      && P.Id.Principal.equal durable.creating_principal value.owner.principal_id
      && String.equal
           temporary.allowed_use
           (Developer_result_reference.allowed_use reference)
      && String.equal
           durable.allowed_use
           (Developer_result_reference.allowed_use reference)
    then Ok ()
    else F.invalid "selection" "artifact reference differs from its private stage"
  | Selection.Inline _, _ | Selection.Artifact _, _ ->
    F.invalid "selection" "selected completion differs from terminal result"
;;

let decode json =
  let open Result.Let_syntax in
  let%bind owner = F.required json "owner" Developer_owner_document.of_json in
  let%bind capture_fingerprint = F.required json "capture_fingerprint" digest in
  let%bind job_id =
    F.required json "job_id" (fun json -> P.Id.Job.of_json json |> F.protocol)
  in
  let%bind attempt = F.required json "attempt" (machine "attempt") in
  let%bind selected_at =
    F.required json "selected_at" (fun json -> P.Timestamp.of_json json |> F.protocol)
  in
  let%bind after =
    F.required json "after" (fun json -> P.Developer_job.State.of_json json |> F.protocol)
  in
  let%bind selection = F.required json "selection" selection_of_json in
  let value =
    { owner; capture_fingerprint; job_id; attempt; selected_at; after; selection }
  in
  let%map () = validates_selection value in
  value
;;

let encode (value : value) =
  let open Result.Let_syntax in
  let%map () = validates_selection value in
  `Object
    [ "owner", Developer_owner_document.to_json value.owner
    ; ( "capture_fingerprint"
      , `String (Chatml_source.Digest.to_hex value.capture_fingerprint) )
    ; "job_id", P.Id.Job.to_json value.job_id
    ; "attempt", F.decimal_json (Int64.of_int value.attempt)
    ; "selected_at", P.Timestamp.to_json value.selected_at
    ; "after", P.Developer_job.State.to_json value.after
    ; "selection", selection_to_json value.selection
    ]
;;

let limits =
  F.limits ~max_bytes:(64 * 1024)
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let codec =
  D.Domain_codec.create
    ~limits
    ~kind:"store.developer_result_intent"
    ~version:1
    ~shape:
      (F.shape
         [ "owner", Developer_owner_document.shape
         ; "capture_fingerprint", D.Shape.value
         ; "job_id", D.Shape.value
         ; "attempt", D.Shape.value
         ; "selected_at", D.Shape.value
         ; "after", D.Shape.value
         ; "selection", D.Shape.value
         ])
    ~supported_semantics:[]
    ~decode
    ~encode
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind document = F.upgrade document ~limits ~kind:"store.developer_result_intent" in
  D.Domain_codec.decode codec document
;;

let to_document value = D.Domain_codec.encode codec value

let create ~owner ~capture_fingerprint ~job_id ~attempt ~selected_at ~after ~selection =
  let open Result.Let_syntax in
  let value =
    { owner; capture_fingerprint; job_id; attempt; selected_at; after; selection }
  in
  let%bind document = to_document (D.Extension_carrier.of_authored_value value) in
  of_document document
;;

module Stored_identity = struct
  type t =
    { owner : P.Execution_owner.developer
    ; job_id : P.Id.Job.t
    ; attempt : int
    }

  let owner t = t.owner
  let job_id t = t.job_id
  let attempt t = t.attempt
end

let stored_identity document =
  let open Result.Let_syntax in
  let%bind () =
    F.expect_versions document ~kind:"store.developer_result_intent" ~versions:[ 1 ]
  in
  let json = D.Document.payload document in
  let%bind owner = F.required json "owner" Developer_owner_document.of_json in
  let%bind job_id =
    F.required json "job_id" (fun json -> P.Id.Job.of_json json |> F.protocol)
  in
  let%map attempt = F.required json "attempt" (machine "attempt") in
  ({ owner; job_id; attempt } : Stored_identity.t)
;;
