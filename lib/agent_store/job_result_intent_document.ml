open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol

type value =
  { reference : P.Job_artifact.t
  ; stage : Blob_stage_documents.t
  }

type t = value D.Extension_carrier.t

let reference t = (D.Extension_carrier.value t).reference
let stage t = (D.Extension_carrier.value t).stage

let machine json =
  let open Result.Let_syntax in
  let%bind counter = F.decimal json in
  match Int64.to_int counter with
  | Some value -> Ok value
  | None -> F.invalid "counter" "counter exceeds machine integer"
;;

let reference_of_json json =
  let open Result.Let_syntax in
  let%bind session_id =
    F.required json "session_id" (fun json -> P.Id.Session.of_json json |> F.protocol)
  in
  let%bind job_id =
    F.required json "job_id" (fun json -> P.Id.Job.of_json json |> F.protocol)
  in
  let%bind generation = F.required json "generation" machine in
  let%bind attempt = F.required json "attempt" machine in
  let%bind blob = F.required json "blob" Blob_metadata_document.blob_of_json in
  P.Job_artifact.create ~session_id ~job_id ~generation ~attempt ~blob |> F.protocol
;;

let reference_to_json (value : P.Job_artifact.t) =
  `Object
    [ "session_id", P.Id.Session.to_json value.session_id
    ; "job_id", P.Id.Job.to_json value.job_id
    ; "generation", F.decimal_json (Int64.of_int value.generation)
    ; "attempt", F.decimal_json (Int64.of_int value.attempt)
    ; "blob", Blob_metadata_document.blob_to_json value.blob
    ]
;;

let decode json =
  let open Result.Let_syntax in
  let%bind reference = F.required json "reference" reference_of_json in
  let%bind temporary_bytes = F.required json "temporary_metadata_bytes" F.string in
  let%bind durable_bytes = F.required json "durable_metadata_bytes" F.string in
  let%bind stage = Blob_stage_documents.of_publications ~temporary_bytes ~durable_bytes in
  let metadata = Blob_metadata_document.value (Blob_stage_documents.temporary stage) in
  if
    (not
       (Option.exists
          metadata.target_session
          ~f:(P.Id.Session.equal reference.session_id)))
    || (not (String.equal metadata.allowed_use (P.Job_artifact.allowed_use reference)))
    || not (Blob_metadata.blob_equal metadata.blob reference.blob)
  then F.invalid "intent" "artifact reference differs from private stage ownership"
  else Ok { reference; stage }
;;

let encode value =
  Ok
    (`Object
        [ "reference", reference_to_json value.reference
        ; ( "temporary_metadata_bytes"
          , `String (Blob_stage_documents.temporary_bytes value.stage) )
        ; ( "durable_metadata_bytes"
          , `String (Blob_stage_documents.durable_bytes value.stage) )
        ])
;;

let limits =
  F.limits ~max_bytes:32768
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let reference_shape =
  F.shape
    [ "session_id", D.Shape.value
    ; "job_id", D.Shape.value
    ; "generation", D.Shape.value
    ; "attempt", D.Shape.value
    ; "blob", Blob_metadata_document.blob_shape
    ]
;;

let codec =
  D.Domain_codec.create
    ~limits
    ~kind:"store.job_result_intent"
    ~version:1
    ~shape:
      (F.shape
         [ "reference", reference_shape
         ; "temporary_metadata_bytes", D.Shape.value
         ; "durable_metadata_bytes", D.Shape.value
         ])
    ~supported_semantics:[]
    ~decode
    ~encode
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind document = F.upgrade document ~limits ~kind:"store.job_result_intent" in
  D.Domain_codec.decode codec document
;;

let to_document value = D.Domain_codec.encode codec value

let create ~reference ~stage =
  let open Result.Let_syntax in
  let%bind document =
    to_document (D.Extension_carrier.of_authored_value { reference; stage })
  in
  of_document document
;;

let stored_identity document =
  let open Result.Let_syntax in
  let%bind () =
    F.expect_versions document ~kind:"store.job_result_intent" ~versions:[ 1 ]
  in
  F.required (D.Document.payload document) "reference" (fun reference ->
    let%bind session =
      F.required reference "session_id" (fun json ->
        P.Id.Session.of_json json |> F.protocol)
    in
    let%map blob =
      F.required reference "blob" (fun blob ->
        F.required blob "id" (fun json -> P.Id.Blob.of_json json |> F.protocol))
    in
    session, blob)
;;
