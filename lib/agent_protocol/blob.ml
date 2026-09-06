open Core

type kind =
  | File
  | Image
  | Audio
  | Binary
[@@deriving compare, equal, sexp]

let kind_to_string = function
  | File -> "file"
  | Image -> "image"
  | Audio -> "audio"
  | Binary -> "binary"
;;

let kind_of_json =
  Json_codec.enum
    ~name:"blob kind"
    [ "file", File; "image", Image; "audio", Audio; "binary", Binary ]
;;

let nonnegative_int64 = Json_codec.bounded_int64 ~min:Int64.zero ~max:Int64.max_value

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
;;

let validate_text name value =
  if String.is_empty value || String.mem value '\000'
  then Error (Protocol_error.invalid_request (name ^ " is invalid"))
  else Ok value
;;

let validate_common ~media_type ~byte_length ~digest ~display_name =
  let open Result.Let_syntax in
  let%bind media_type = validate_text "blob media type" media_type in
  let%bind digest = validate_text "blob digest" digest in
  let%bind display_name =
    match display_name with
    | None -> Ok None
    | Some value -> Result.map (validate_text "blob display name" value) ~f:Option.some
  in
  if Int64.(byte_length < zero)
  then Error (Protocol_error.invalid_request "blob byte length must be nonnegative")
  else Ok (media_type, digest, display_name)
;;

let common_fields ~kind ~media_type ~byte_length ~digest ~display_name =
  [ Some ("kind", `String (kind_to_string kind))
  ; Some ("media_type", `String media_type)
  ; Some ("byte_length", `Number (Int64.to_string byte_length))
  ; Some ("digest", `String digest)
  ; optional_field "display_name" display_name (fun value -> `String value)
  ]
  |> List.filter_opt
;;

let decode_common fields =
  let open Result.Let_syntax in
  let%bind kind = Json_codec.required_as fields "kind" kind_of_json in
  let%bind media_type = Json_codec.required_as fields "media_type" Json_codec.string in
  let%bind byte_length = Json_codec.required_as fields "byte_length" nonnegative_int64 in
  let%bind digest = Json_codec.required_as fields "digest" Json_codec.string in
  let%bind display_name =
    Json_codec.optional_as fields "display_name" Json_codec.string
  in
  let%map media_type, digest, display_name =
    validate_common ~media_type ~byte_length ~digest ~display_name
  in
  kind, media_type, byte_length, digest, display_name
;;

module Metadata = struct
  type t =
    { id : Id.Blob.t
    ; kind : kind
    ; media_type : string
    ; byte_length : int64
    ; digest : string
    ; display_name : string option
    }
  [@@deriving sexp]

  let create ~id ~kind ~media_type ~byte_length ~digest ?display_name () =
    let open Result.Let_syntax in
    let%map media_type, digest, display_name =
      validate_common ~media_type ~byte_length ~digest ~display_name
    in
    { id; kind; media_type; byte_length; digest; display_name }
  ;;

  let to_json t =
    `Object
      (("id", Id.Blob.to_json t.id)
       :: common_fields
            ~kind:t.kind
            ~media_type:t.media_type
            ~byte_length:t.byte_length
            ~digest:t.digest
            ~display_name:t.display_name)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind id = Json_codec.required_as fields "id" Id.Blob.of_json in
    let%bind kind, media_type, byte_length, digest, display_name = decode_common fields in
    create ~id ~kind ~media_type ~byte_length ~digest ?display_name ()
  ;;
end

module Input = struct
  type source =
    | Stored of Id.Blob.t
    | Inline_base64 of string
  [@@deriving sexp]

  type t =
    { kind : kind
    ; media_type : string
    ; byte_length : int64
    ; digest : string
    ; display_name : string option
    ; source : source
    }
  [@@deriving sexp]

  let source_fields = function
    | Stored id -> [ "source", `String "stored"; "blob_id", Id.Blob.to_json id ]
    | Inline_base64 data ->
      [ "source", `String "inline_base64"; "inline_base64", `String data ]
  ;;

  let to_json t =
    `Object
      (common_fields
         ~kind:t.kind
         ~media_type:t.media_type
         ~byte_length:t.byte_length
         ~digest:t.digest
         ~display_name:t.display_name
       @ source_fields t.source)
  ;;

  let decode_source fields =
    let open Result.Let_syntax in
    let%bind encoded = Json_codec.required_as fields "source" Json_codec.string in
    match encoded with
    | "stored" ->
      Result.map (Json_codec.required_as fields "blob_id" Id.Blob.of_json) ~f:(fun id ->
        Stored id)
    | "inline_base64" ->
      Result.map
        (Json_codec.required_as fields "inline_base64" Json_codec.string)
        ~f:(fun data -> Inline_base64 data)
    | _ -> Error (Protocol_error.invalid_request "unknown blob input source")
  ;;

  let validate_inline byte_length = function
    | Stored _ -> Ok ()
    | Inline_base64 data ->
      (match Base64.decode data with
       | Ok decoded when Int64.equal (Int64.of_int (String.length decoded)) byte_length ->
         Ok ()
       | Ok _ -> Error (Protocol_error.invalid_request "inline blob byte length differs")
       | Error _ ->
         Error (Protocol_error.invalid_request "inline blob is not valid base64"))
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind kind, media_type, byte_length, digest, display_name = decode_common fields in
    let%bind source = decode_source fields in
    let%map () = validate_inline byte_length source in
    { kind; media_type; byte_length; digest; display_name; source }
  ;;
end

module Read_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; blob_id : Id.Blob.t
    ; offset : int64
    ; max_bytes : int
    }
  [@@deriving sexp]

  let max_chunk_bytes = 1024 * 1024

  let to_json t =
    `Object
      [ "session_id", Id.Session.to_json t.session_id
      ; "attachment_id", Id.Attachment.to_json t.attachment_id
      ; "blob_id", Id.Blob.to_json t.blob_id
      ; "offset", `Number (Int64.to_string t.offset)
      ; "max_bytes", `Number (Int.to_string t.max_bytes)
      ]
  ;;

  let validate t =
    if Int64.(t.offset < zero)
    then Error (Protocol_error.invalid_request "blob read offset must be nonnegative")
    else if t.max_bytes <= 0 || t.max_bytes > max_chunk_bytes
    then
      Error
        (Protocol_error.invalid_request
           (sprintf "blob read max_bytes must be between 1 and %d" max_chunk_bytes))
    else Ok t
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind attachment_id =
      Json_codec.required_as fields "attachment_id" Id.Attachment.of_json
    in
    let%bind blob_id = Json_codec.required_as fields "blob_id" Id.Blob.of_json in
    let%bind offset = Json_codec.required_as fields "offset" nonnegative_int64 in
    let%bind max_bytes =
      Json_codec.required_as
        fields
        "max_bytes"
        (Json_codec.bounded_int ~min:1 ~max:max_chunk_bytes)
    in
    validate { session_id; attachment_id; blob_id; offset; max_bytes }
  ;;
end

module Chunk = struct
  type t =
    { blob : Metadata.t
    ; offset : int64
    ; next_offset : int64
    ; data_base64 : string
    ; eof : bool
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "blob", Metadata.to_json t.blob
      ; "offset", `Number (Int64.to_string t.offset)
      ; "next_offset", `Number (Int64.to_string t.next_offset)
      ; "data_base64", `String t.data_base64
      ; ("eof", if t.eof then `True else `False)
      ]
  ;;

  let validate t =
    let open Result.Let_syntax in
    let%bind decoded =
      Base64.decode t.data_base64
      |> Result.map_error ~f:(fun _ ->
        Protocol_error.invalid_request "blob chunk data is not valid base64")
    in
    let expected = Int64.(t.next_offset - t.offset) in
    if Int64.(t.offset < zero || t.next_offset < t.offset)
    then Error (Protocol_error.invalid_request "blob chunk offsets are invalid")
    else if not (Int64.equal expected (Int64.of_int (String.length decoded)))
    then Error (Protocol_error.invalid_request "blob chunk length does not match offsets")
    else if Int64.(t.next_offset > t.blob.byte_length)
    then Error (Protocol_error.invalid_request "blob chunk exceeds advertised length")
    else if Bool.(t.eof <> Int64.equal t.next_offset t.blob.byte_length)
    then Error (Protocol_error.invalid_request "blob chunk eof flag is inconsistent")
    else Ok t
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind blob = Json_codec.required_as fields "blob" Metadata.of_json in
    let%bind offset = Json_codec.required_as fields "offset" nonnegative_int64 in
    let%bind next_offset =
      Json_codec.required_as fields "next_offset" nonnegative_int64
    in
    let%bind data_base64 =
      Json_codec.required_as fields "data_base64" Json_codec.string
    in
    let%bind eof = Json_codec.required_as fields "eof" Json_codec.bool in
    validate { blob; offset; next_offset; data_base64; eof }
  ;;
end
