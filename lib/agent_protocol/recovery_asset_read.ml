open! Core
module J = Json_codec
module W = Recovery_wire
module Citation = Recovery_window.Citation

let method_name = "session.recovery.read_asset"
let feature = "recovery.asset_read"
let max_original_bytes = 8 * 1024 * 1024
let max_chunk_bytes = 64 * 1024
let max_base64_bytes = 4 * ((max_chunk_bytes + 2) / 3)
let max_reply_json_bytes = max_base64_bytes + 2048
let max_envelope_bytes = max_reply_json_bytes + 512
let invalid message = Error (Protocol_error.invalid_request message)
let number n = `Number (Int.to_string n)

let valid_sha256 digest =
  String.length digest = 64
  && String.for_all digest ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let canonical citation =
  match Citation.version citation with
  | Canonical _ -> true
  | Effective _ -> false
;;

let part_number = J.bounded_int ~min:0 ~max:(History_text.max_parts - 1)
let offset_number = J.bounded_int ~min:0 ~max:max_original_bytes

type t =
  { citation : Citation.t
  ; part : int
  ; offset : int
  ; max_bytes : int
  ; expected_sha256 : string option
  }

let create citation ~part ~offset ?(max_bytes = 16 * 1024) ?expected_sha256 () =
  if
    (not (canonical citation))
    || part < 0
    || part >= History_text.max_parts
    || offset < 0
    || offset > max_original_bytes
    || max_bytes < 1
    || max_bytes > max_chunk_bytes
    || (offset > 0 && Option.is_none expected_sha256)
    || not (Option.for_all expected_sha256 ~f:valid_sha256)
  then invalid "invalid recovery asset coordinate, byte range or SHA-256"
  else Ok { citation; part; offset; max_bytes; expected_sha256 }
;;

let session t = Recovery_window.Scope.session (Citation.scope t.citation)

let to_json t =
  `Object
    ([ "citation", Citation.to_json t.citation
     ; "part", number t.part
     ; "offset", number t.offset
     ; "max_bytes", number t.max_bytes
     ]
     @ Option.to_list
         (Option.map t.expected_sha256 ~f:(fun digest ->
            "expected_sha256", `String digest)))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = W.admit_coordinate json in
  let%bind fields =
    W.fields json [ "citation"; "part"; "offset"; "max_bytes"; "expected_sha256" ]
  in
  let%bind citation = J.required_as fields "citation" Citation.of_json in
  let%bind part = J.required_as fields "part" part_number in
  let%bind offset = J.required_as fields "offset" offset_number in
  let%bind max_bytes =
    J.optional_as fields "max_bytes" (J.bounded_int ~min:1 ~max:max_chunk_bytes)
  in
  let%bind expected_sha256 = J.optional_as fields "expected_sha256" J.string in
  create citation ~part ~offset ?max_bytes ?expected_sha256 ()
;;

module Reply = struct
  type request = t

  type kind =
    | Image
    | Document
  [@@deriving equal]

  let kind_name = function
    | Image -> "image"
    | Document -> "document"
  ;;

  type t =
    { citation : Citation.t
    ; part : int
    ; kind : kind
    ; media_type : string
    ; byte_length : int
    ; sha256 : string
    ; offset : int
    ; bytes : string
    ; next_offset : int option
    }

  let valid_media kind media_type =
    Blob.is_media_type media_type
    &&
    match kind with
    | Document -> true
    | Image ->
      List.mem
        [ "image/png"; "image/jpeg"; "image/gif"; "image/webp" ]
        media_type
        ~equal:String.equal
  ;;

  let next ~byte_length ~finish = if finish < byte_length then Some finish else None

  let of_verified_original (request : request) ~kind ~media_type ~sha256 original =
    let byte_length = String.length original in
    if byte_length > max_original_bytes || request.offset > byte_length
    then invalid "recovery asset offset exceeds its original"
    else if not (valid_media kind media_type && valid_sha256 sha256)
    then invalid "invalid recovery asset metadata"
    else if not (Option.for_all request.expected_sha256 ~f:(String.equal sha256))
    then
      Error
        (Protocol_error.create
           Conflict
           ~message:"recovery asset SHA-256 differs"
           ~retryable:false
           ())
    else (
      let len = Int.min request.max_bytes (byte_length - request.offset) in
      let bytes = String.sub original ~pos:request.offset ~len in
      let next_offset = next ~byte_length ~finish:(request.offset + len) in
      Ok
        { citation = request.citation
        ; part = request.part
        ; kind
        ; media_type
        ; byte_length
        ; sha256
        ; offset = request.offset
        ; bytes
        ; next_offset
        })
  ;;

  let to_json t =
    `Object
      ([ "citation", Citation.to_json t.citation
       ; "part", number t.part
       ; "kind", `String (kind_name t.kind)
       ; "media_type", `String t.media_type
       ; "byte_length", number t.byte_length
       ; "sha256", `String t.sha256
       ; "offset", number t.offset
       ; "data_base64", `String (Base64.encode_exn t.bytes)
       ]
       @ Option.to_list (Option.map t.next_offset ~f:(fun n -> "next_offset", number n)))
  ;;

  let limits = W.limits ~max_bytes:max_reply_json_bytes ~max_fields:32 ~max_nodes:64

  let of_json json =
    let open Result.Let_syntax in
    let%bind _ = W.measure ~limits json in
    let%bind fields =
      W.fields
        json
        [ "citation"
        ; "part"
        ; "kind"
        ; "media_type"
        ; "byte_length"
        ; "sha256"
        ; "offset"
        ; "data_base64"
        ; "next_offset"
        ]
    in
    let%bind citation = J.required_as fields "citation" Citation.of_json in
    let%bind part = J.required_as fields "part" part_number in
    let%bind kind =
      J.required_as
        fields
        "kind"
        (J.enum ~name:"recovery asset kind" [ "image", Image; "document", Document ])
    in
    let%bind media_type = J.required_as fields "media_type" J.string in
    let%bind byte_length = J.required_as fields "byte_length" offset_number in
    let%bind sha256 = J.required_as fields "sha256" J.string in
    let%bind offset = J.required_as fields "offset" offset_number in
    let%bind data = J.required_as fields "data_base64" J.string in
    let%bind next_offset = J.optional_as fields "next_offset" offset_number in
    if
      String.length data > max_base64_bytes
      || not (canonical citation && valid_media kind media_type && valid_sha256 sha256)
    then invalid "invalid recovery asset response"
    else (
      match Base64.decode data with
      | Error _ -> invalid "invalid recovery asset chunk encoding"
      | Ok bytes ->
        let len = String.length bytes in
        let finish = offset + len in
        if
          len > max_chunk_bytes
          || finish > byte_length
          || (len = 0 && offset <> byte_length)
          || (not (String.equal data (Base64.encode_exn bytes)))
          || not (Option.equal Int.equal next_offset (next ~byte_length ~finish))
        then invalid "invalid recovery asset chunk position or encoding"
        else
          Ok
            { citation
            ; part
            ; kind
            ; media_type
            ; byte_length
            ; sha256
            ; offset
            ; bytes
            ; next_offset
            })
  ;;

  let validate t ~(request : request) =
    if
      Citation.equal t.citation request.citation
      && Int.equal t.part request.part
      && Int.equal t.offset request.offset
      && Int.equal
           (String.length t.bytes)
           (Int.min request.max_bytes (t.byte_length - t.offset))
      && Option.for_all request.expected_sha256 ~f:(String.equal t.sha256)
    then
      if
        t.offset = 0
        && Option.is_none t.next_offset
        && not (String.equal t.sha256 Digestif.SHA256.(digest_string t.bytes |> to_hex))
      then invalid "recovery asset bytes do not match their SHA-256"
      else Ok ()
    else invalid "recovery asset response does not match its request"
  ;;
end
