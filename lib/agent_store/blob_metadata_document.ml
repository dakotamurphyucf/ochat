open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol

type t = Blob_metadata.t D.Extension_carrier.t

let optional json name decode =
  match D.Json.field json ~name with
  | Absent -> Ok None
  | Null -> F.invalid name "optional field must be omitted, not null"
  | Value value -> Result.map (decode value) ~f:Option.some
;;

let blob_of_json json =
  let open Result.Let_syntax in
  let%bind id = F.required json "id" (fun json -> P.Id.Blob.of_json json |> F.protocol) in
  let%bind kind = F.required json "kind" F.string in
  let%bind media_type = F.required json "media_type" F.string in
  let%bind byte_length = F.required json "byte_length" F.decimal in
  let%bind digest = F.required json "digest" F.digest in
  let%bind display_name = optional json "display_name" F.string in
  let known =
    `Object
      ([ "id", P.Id.Blob.to_json id
       ; "kind", `String kind
       ; "media_type", `String media_type
       ; "byte_length", `Number (Int64.to_string byte_length)
       ; "digest", `String digest
       ]
       @ Option.to_list
           (Option.map display_name ~f:(fun value -> "display_name", `String value)))
  in
  P.Blob.Metadata.of_json known |> F.protocol
;;

let blob_to_json (value : P.Blob.Metadata.t) =
  match P.Blob.Metadata.to_json value with
  | `Object fields ->
    `Object
      (List.Assoc.add
         fields
         ~equal:String.equal
         "byte_length"
         (F.decimal_json value.byte_length))
  | _ -> assert false
;;

let blob_shape =
  F.shape
    (List.map
       [ "id"; "kind"; "media_type"; "byte_length"; "digest"; "display_name" ]
       ~f:(fun key -> key, D.Shape.value))
;;

let decode json =
  let open Result.Let_syntax in
  let%bind blob = F.required json "blob" blob_of_json in
  let%bind creating_principal =
    F.required json "creating_principal" (fun json ->
      P.Id.Principal.of_json json |> F.protocol)
  in
  let%bind target_session =
    optional json "target_session" (fun json -> P.Id.Session.of_json json |> F.protocol)
  in
  let%bind allowed_use = F.required json "allowed_use" F.string in
  let%bind created_at =
    F.required json "created_at" (fun json -> P.Timestamp.of_json json |> F.protocol)
  in
  let%bind expires_at =
    optional json "expires_at" (fun json -> P.Timestamp.of_json json |> F.protocol)
  in
  let%bind durable = F.required json "durable" F.boolean in
  if String.is_empty allowed_use || String.mem allowed_use '\000'
  then F.invalid "allowed_use" "must be nonempty and contain no NUL"
  else if durable && Option.is_none target_session
  then F.invalid "target_session" "durable blob must have an owning session"
  else if Option.exists expires_at ~f:(fun at -> P.Timestamp.compare at created_at < 0)
  then F.invalid "expires_at" "expiry precedes creation"
  else
    Ok
      Blob_metadata.
        { blob
        ; creating_principal
        ; target_session
        ; allowed_use
        ; created_at
        ; expires_at
        ; durable
        }
;;

let encode (value : Blob_metadata.t) =
  Ok
    (`Object
        ([ "blob", blob_to_json value.blob
         ; "creating_principal", P.Id.Principal.to_json value.creating_principal
         ; "allowed_use", `String value.allowed_use
         ; "created_at", P.Timestamp.to_json value.created_at
         ; ("durable", if value.durable then `True else `False)
         ]
         @ Option.to_list
             (Option.map value.target_session ~f:(fun id ->
                "target_session", P.Id.Session.to_json id))
         @ Option.to_list
             (Option.map value.expires_at ~f:(fun at ->
                "expires_at", P.Timestamp.to_json at))))
;;

let limits =
  F.limits ~max_bytes:32768
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let codec =
  D.Domain_codec.create
    ~limits
    ~kind:"store.blob_metadata"
    ~version:1
    ~shape:
      (F.shape
         [ "blob", blob_shape
         ; "creating_principal", D.Shape.value
         ; "target_session", D.Shape.value
         ; "allowed_use", D.Shape.value
         ; "created_at", D.Shape.value
         ; "expires_at", D.Shape.value
         ; "durable", D.Shape.value
         ])
    ~supported_semantics:[]
    ~decode
    ~encode
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let value = D.Extension_carrier.value

let of_document document =
  let open Result.Let_syntax in
  let%bind document = F.upgrade document ~limits ~kind:"store.blob_metadata" in
  D.Domain_codec.decode codec document
;;

let to_document value = D.Domain_codec.encode codec value
let of_bytes bytes = Result.bind (D.Document.decode ~limits bytes) ~f:of_document
let to_bytes value = Result.map (to_document value) ~f:D.Document.to_string

let create value =
  let open Result.Let_syntax in
  let%bind document = to_document (D.Extension_carrier.of_authored_value value) in
  of_document document
;;

let with_value t value =
  let open Result.Let_syntax in
  let%bind document = to_document (D.Extension_carrier.with_value t value) in
  of_document document
;;

let stored_blob_id document =
  let open Result.Let_syntax in
  let%bind () = F.expect_versions document ~kind:"store.blob_metadata" ~versions:[ 1 ] in
  F.required (D.Document.payload document) "blob" (fun blob ->
    F.required blob "id" (fun id -> P.Id.Blob.of_json id |> F.protocol))
;;
