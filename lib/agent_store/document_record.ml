open! Core

module Error = struct
  type t =
    | Frame of Frame.error
    | Incomplete_frame
    | Trailing_bytes
    | Digest_mismatch of
        { expected : string
        ; actual : string
        }
    | Invalid_digest of string
    | Document of Document_schema.Error.t
  [@@deriving sexp]
end

type t =
  { stored_bytes : string
  ; stored_digest : string
  ; document : Document_schema.Document.t
  ; flags : int
  }

let digest bytes = Digestif.SHA256.(digest_string bytes |> to_hex)
let stored_bytes t = t.stored_bytes
let stored_digest t = t.stored_digest
let document t = t.document
let flags t = t.flags

let check_digest bytes = function
  | None -> Ok ()
  | Some expected ->
    if
      String.length expected <> 64
      || not
           (String.for_all expected ~f:(fun ch ->
              Char.is_digit ch || Char.(ch >= 'a' && ch <= 'f')))
    then Error (Error.Invalid_digest expected)
    else (
      let actual = digest bytes in
      if String.equal expected actual
      then Ok ()
      else Error (Error.Digest_mismatch { expected; actual }))
;;

let of_frame frame ~limits ~expected_digest =
  let open Result.Let_syntax in
  let stored_bytes = Frame.payload frame in
  let%bind () = check_digest stored_bytes expected_digest in
  let%map document =
    Document_schema.Document.decode ~limits stored_bytes
    |> Result.map_error ~f:(fun error -> Error.Document error)
  in
  { stored_bytes
  ; stored_digest = digest stored_bytes
  ; document
  ; flags = Frame.flags frame
  }
;;

let of_document document ~limits =
  let open Result.Let_syntax in
  let%map () =
    Document_schema.Document.validate document ~limits
    |> Result.map_error ~f:(fun error -> Error.Document error)
  in
  let stored_bytes = Document_schema.Document.to_string document in
  { stored_bytes; stored_digest = digest stored_bytes; document; flags = 0 }
;;

let decode_frame ~limits ~contents ~offset ~expected_digest =
  let open Result.Let_syntax in
  let%bind decoded =
    Frame.decode
      ~max_payload_length:(Document_schema.Limits.max_bytes limits)
      ~contents
      ~offset
    |> Result.map_error ~f:(fun error -> Error.Frame error)
  in
  match decoded with
  | Incomplete_tail _ -> Error Error.Incomplete_frame
  | Complete { frame; next_offset } ->
    let%map record = of_frame frame ~limits ~expected_digest in
    record, next_offset
;;

let decode_file ~limits ~expected_digest contents =
  let open Result.Let_syntax in
  let%bind record, next_offset =
    decode_frame ~limits ~contents ~offset:0 ~expected_digest
  in
  if next_offset <> String.length contents then Error Error.Trailing_bytes else Ok record
;;

let upgrade t ~conversion = Document_schema.Conversion.upgrade conversion t.document

let encode document ~limits ~flags =
  let open Result.Let_syntax in
  let%bind () =
    Document_schema.Document.validate document ~limits
    |> Result.map_error ~f:(fun error -> Error.Document error)
  in
  Frame.encode
    ~max_payload_length:(Document_schema.Limits.max_bytes limits)
    ~flags
    (Document_schema.Document.to_string document)
  |> Result.map_error ~f:(fun error -> Error.Frame error)
;;
