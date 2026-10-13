open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol
module B = P.Authoring_bundle.Bundle

let ( let* ) result f = Result.bind result ~f
let document_limit ~max_bytes = F.limits ~max_bytes |> F.store
let invalid field message = F.invalid field message

let exact_fields json allowed =
  match json with
  | `Object fields ->
    if
      List.length fields <> List.length allowed
      || List.exists fields ~f:(fun (name, _) ->
        not (List.mem allowed name ~equal:String.equal))
    then invalid "payload" "unexpected durable upload fields"
    else Ok ()
  | _ -> invalid "payload" "durable upload payload is not an object"
;;

let exact_envelope document =
  exact_fields
    (D.Document.json document)
    [ "format"; "schema_version"; "kind"; "payload" ]
;;

let encode ~kind ~max_bytes payload =
  let* limits = document_limit ~max_bytes in
  let* document = D.Document.create ~limits ~kind ~version:1 ~payload |> F.store in
  Ok (D.Document.to_string document)
;;

let decode ~kind ~max_bytes bytes =
  let* limits = document_limit ~max_bytes in
  let* document = D.Document.decode ~limits bytes |> F.store in
  let* () = exact_envelope document |> F.store in
  if not (String.equal (D.Document.kind document) kind)
  then Error (Store_error.Corrupt "wrong durable upload document kind")
  else if D.Document.version document <> 1
  then Error (Store_error.Corrupt "unsupported durable upload document version")
  else Ok (D.Document.payload document)
;;

let id decode json = F.protocol (decode json)
let timestamp json = id P.Timestamp.of_json json

let digest json =
  let* hex = F.digest json in
  match Chatml_source.Digest.of_hex hex with
  | Ok value -> Ok value
  | Error _ -> invalid "digest" "invalid source digest"
;;

let field json name decode = F.required json name decode

let optional json name decode =
  match D.Json.field json ~name with
  | Absent -> Ok None
  | Null -> invalid name "optional durable upload field must be absent"
  | Value value -> Result.map (decode value) ~f:Option.some
;;

let int json =
  let* value = F.decimal json in
  if Int64.(value < zero || value > of_int Int.max_value)
  then invalid "number" "durable upload integer outside range"
  else Ok (Int64.to_int_exn value)
;;

let int_json value = F.decimal_json (Int64.of_int value)

let optional_field name value encode =
  Option.to_list (Option.map value ~f:(fun x -> name, encode x))
;;

module Manifest = struct
  type t =
    { owner : B.Owner.t
    ; upload_id : P.Id.Authoring_upload.t
    ; begin_key : P.Idempotency_key.t
    ; manifest : B.Manifest.t
    ; parent_context : P.Authoring_context.Context.Expected.t option
    ; created_at : P.Timestamp.t
    ; stage_deadline : P.Timestamp.t
    }

  let kind = "store.authoring_upload.manifest"

  let to_payload t =
    `Object
      ([ "owner", B.Owner.to_json t.owner
       ; "upload_id", P.Id.Authoring_upload.to_json t.upload_id
       ; "begin_key", P.Idempotency_key.to_json t.begin_key
       ; "manifest", B.Manifest.to_json t.manifest
       ; "created_at", P.Timestamp.to_json t.created_at
       ; "stage_deadline", P.Timestamp.to_json t.stage_deadline
       ]
       @ optional_field
           "parent_context"
           t.parent_context
           P.Authoring_context.Context.Expected.to_json)
  ;;

  let to_bytes t ~max_bytes = encode ~kind ~max_bytes (to_payload t)

  let of_bytes bytes ~max_bytes =
    let* json = decode ~kind ~max_bytes bytes in
    let* () =
      (match json with
       | `Object fields ->
         let allowed =
           [ "owner"
           ; "upload_id"
           ; "begin_key"
           ; "manifest"
           ; "created_at"
           ; "stage_deadline"
           ; "parent_context"
           ]
         in
         if
           List.exists fields ~f:(fun (name, _) ->
             not (List.mem allowed name ~equal:String.equal))
         then invalid "payload" "unexpected durable upload manifest field"
         else Ok ()
       | _ -> invalid "payload" "durable upload manifest is not an object")
      |> F.store
    in
    let* owner = field json "owner" (id B.Owner.of_json) |> F.store in
    let* upload_id =
      field json "upload_id" (id P.Id.Authoring_upload.of_json) |> F.store
    in
    let* begin_key = field json "begin_key" (id P.Idempotency_key.of_json) |> F.store in
    let* manifest = field json "manifest" (id B.Manifest.of_json) |> F.store in
    let* parent_context =
      optional json "parent_context" (id P.Authoring_context.Context.Expected.of_json)
      |> F.store
    in
    let* created_at = field json "created_at" timestamp |> F.store in
    let* stage_deadline = field json "stage_deadline" timestamp |> F.store in
    if P.Timestamp.compare created_at stage_deadline >= 0
    then Error (Store_error.Corrupt "invalid upload stage deadline")
    else
      Ok
        { owner
        ; upload_id
        ; begin_key
        ; manifest
        ; parent_context
        ; created_at
        ; stage_deadline
        }
  ;;
end

module Phase = struct
  type outcome =
    | Open
    | Ready of
        { commit_key : P.Idempotency_key.t
        ; payload_digest : Chatml_source.Digest.t
        ; receipt_deadline : P.Timestamp.t
        }
    | Rejected of
        { commit_key : P.Idempotency_key.t
        ; reason : B.Rejection.t
        ; receipt_deadline : P.Timestamp.t
        }
    | Expired of { tombstone_deadline : P.Timestamp.t }

  type t =
    { upload_id : P.Id.Authoring_upload.t
    ; outcome : outcome
    }

  let kind = "store.authoring_upload.phase"

  let to_payload t =
    let fields = [ "upload_id", P.Id.Authoring_upload.to_json t.upload_id ] in
    let fields =
      match t.outcome with
      | Open -> ("kind", `String "open") :: fields
      | Ready { commit_key; payload_digest; receipt_deadline } ->
        [ "kind", `String "ready"
        ; "upload_id", P.Id.Authoring_upload.to_json t.upload_id
        ; "commit_key", P.Idempotency_key.to_json commit_key
        ; "payload_digest", `String (Chatml_source.Digest.to_hex payload_digest)
        ; "receipt_deadline", P.Timestamp.to_json receipt_deadline
        ]
      | Rejected { commit_key; reason; receipt_deadline } ->
        [ "kind", `String "rejected"
        ; "upload_id", P.Id.Authoring_upload.to_json t.upload_id
        ; "commit_key", P.Idempotency_key.to_json commit_key
        ; "reason", B.Rejection.to_json reason
        ; "receipt_deadline", P.Timestamp.to_json receipt_deadline
        ]
      | Expired { tombstone_deadline } ->
        [ "kind", `String "expired"
        ; "upload_id", P.Id.Authoring_upload.to_json t.upload_id
        ; "tombstone_deadline", P.Timestamp.to_json tombstone_deadline
        ]
    in
    `Object fields
  ;;

  let to_bytes t ~max_bytes = encode ~kind ~max_bytes (to_payload t)

  let of_bytes bytes ~max_bytes =
    let* json = decode ~kind ~max_bytes bytes in
    let* kind = field json "kind" F.string |> F.store in
    let allowed =
      match kind with
      | "open" -> [ "kind"; "upload_id" ]
      | "expired" -> [ "kind"; "upload_id"; "tombstone_deadline" ]
      | "ready" ->
        [ "kind"; "upload_id"; "commit_key"; "payload_digest"; "receipt_deadline" ]
      | "rejected" -> [ "kind"; "upload_id"; "commit_key"; "reason"; "receipt_deadline" ]
      | _ -> []
    in
    let* () =
      if List.is_empty allowed
      then Error (Store_error.Corrupt "unknown durable upload phase")
      else exact_fields json allowed |> F.store
    in
    let* upload_id =
      field json "upload_id" (id P.Id.Authoring_upload.of_json) |> F.store
    in
    let* outcome =
      match kind with
      | "open" -> Ok Open
      | "expired" ->
        let* tombstone_deadline = field json "tombstone_deadline" timestamp |> F.store in
        Ok (Expired { tombstone_deadline })
      | "ready" ->
        let* commit_key =
          field json "commit_key" (id P.Idempotency_key.of_json) |> F.store
        in
        let* payload_digest = field json "payload_digest" digest |> F.store in
        let* receipt_deadline = field json "receipt_deadline" timestamp |> F.store in
        Ok (Ready { commit_key; payload_digest; receipt_deadline })
      | "rejected" ->
        let* commit_key =
          field json "commit_key" (id P.Idempotency_key.of_json) |> F.store
        in
        let* reason = field json "reason" (id B.Rejection.of_json) |> F.store in
        let* receipt_deadline = field json "receipt_deadline" timestamp |> F.store in
        Ok (Rejected { commit_key; reason; receipt_deadline })
      | _ -> assert false
    in
    Ok { upload_id; outcome }
  ;;
end

module Progress = struct
  type t =
    { upload_id : P.Id.Authoring_upload.t
    ; member_index : int
    ; accepted_until_byte : int
    }

  let kind = "store.authoring_upload.progress"

  let to_bytes t ~max_bytes =
    encode
      ~kind
      ~max_bytes
      (`Object
          [ "upload_id", P.Id.Authoring_upload.to_json t.upload_id
          ; "member_index", int_json t.member_index
          ; "accepted_until_byte", int_json t.accepted_until_byte
          ])
  ;;

  let of_bytes bytes ~max_bytes =
    let* json = decode ~kind ~max_bytes bytes in
    let* () =
      exact_fields json [ "upload_id"; "member_index"; "accepted_until_byte" ] |> F.store
    in
    let* upload_id =
      field json "upload_id" (id P.Id.Authoring_upload.of_json) |> F.store
    in
    let* member_index = field json "member_index" int |> F.store in
    let* accepted_until_byte = field json "accepted_until_byte" int |> F.store in
    Ok { upload_id; member_index; accepted_until_byte }
  ;;
end
