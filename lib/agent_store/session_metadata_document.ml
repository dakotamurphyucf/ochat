open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol

type t = Session_metadata.t

let positive json =
  let open Result.Let_syntax in
  let%bind value = F.decimal json in
  match Int64.to_int value with
  | Some value when value > 0 -> Ok value
  | _ -> F.invalid "data_schema_version" "expected positive machine integer"
;;

let decode json =
  let open Result.Let_syntax in
  let%bind session = F.required json "session" Session_record_document.of_json in
  let%bind prompt_artifact = F.required json "prompt_artifact" F.string in
  let%bind workspace_identity = F.required json "workspace_identity" F.string in
  let%bind data_schema_version = F.required json "data_schema_version" positive in
  if String.is_empty prompt_artifact || String.is_empty workspace_identity
  then F.invalid "metadata" "artifact and workspace identity must be nonempty"
  else
    Ok
      Session_metadata.
        { schema_version = 1
        ; session
        ; prompt_artifact
        ; workspace_identity
        ; data_schema_version
        }
;;

let encode (value : t) =
  let open Result.Let_syntax in
  if value.schema_version <> 1
  then F.invalid "schema_version" "expected current metadata version"
  else if value.data_schema_version <= 0
  then F.invalid "data_schema_version" "must be positive"
  else (
    let%map session = Session_record_document.to_json value.session in
    `Object
      [ "session", session
      ; "prompt_artifact", `String value.prompt_artifact
      ; "workspace_identity", `String value.workspace_identity
      ; "data_schema_version", F.decimal_json (Int64.of_int value.data_schema_version)
      ])
;;

let limits =
  F.limits ~max_bytes:(16 * 1024 * 1024)
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let codec =
  D.Domain_codec.create
    ~limits
    ~kind:"store.session_metadata"
    ~version:1
    ~shape:
      (F.shape
         [ "session", Session_record_document.shape
         ; "prompt_artifact", D.Shape.value
         ; "workspace_identity", D.Shape.value
         ; "data_schema_version", D.Shape.value
         ])
    ~supported_semantics:[]
    ~decode
    ~encode
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind document = F.upgrade document ~limits ~kind:"store.session_metadata" in
  D.Domain_codec.decode codec document
;;

let to_document value = D.Domain_codec.encode codec value

let stored_session_id document =
  let open Result.Let_syntax in
  let%bind () =
    F.expect_versions document ~kind:"store.session_metadata" ~versions:[ 1 ]
  in
  F.required (D.Document.payload document) "session" Session_record_document.stored_id
;;
