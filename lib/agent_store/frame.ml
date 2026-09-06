open Core

let magic = "OCHATJNL"
let current_version = 1
let header_length = 20
let checksum_length = 32

type t =
  { flags : int
  ; payload : string
  ; checksum_raw : string
  }

type decoded =
  | Complete of
      { frame : t
      ; next_offset : int
      }
  | Incomplete_tail of { offset : int }

type error =
  | Invalid_offset of int
  | Invalid_magic
  | Unsupported_version of int
  | Invalid_flags of int
  | Payload_too_large of int64
  | Checksum_mismatch
[@@deriving sexp]

let flags t = t.flags
let payload t = t.payload
let checksum_raw t = t.checksum_raw
let checksum_hex t = Digestif.SHA256.(of_raw_string (checksum_raw t) |> to_hex)

let set_uint16_be bytes offset value =
  Bytes.set bytes offset (Char.of_int_exn (value lsr 8));
  Bytes.set bytes (offset + 1) (Char.of_int_exn (value land 0xff))
;;

let get_uint16_be contents offset =
  (Char.to_int contents.[offset] lsl 8) lor Char.to_int contents.[offset + 1]
;;

let set_int64_be bytes offset value =
  for index = 0 to 7 do
    let shift = (7 - index) * 8 in
    let byte = Int64.(to_int_exn ((value lsr shift) land 0xffL)) in
    Bytes.set bytes (offset + index) (Char.of_int_exn byte)
  done
;;

let get_int64_be contents offset =
  let value = ref Int64.zero in
  for index = 0 to 7 do
    let byte = Char.to_int contents.[offset + index] in
    value := Int64.((!value lsl 8) lor of_int byte)
  done;
  !value
;;

let validate_flags flags =
  if flags < 0 || flags > 0xffff then Error (Invalid_flags flags) else Ok ()
;;

let validate_payload_length ~max_payload_length length =
  if length < 0 || length > max_payload_length
  then Error (Payload_too_large (Int64.of_int length))
  else Ok ()
;;

let encode ~max_payload_length ~flags payload =
  let open Result.Let_syntax in
  let%bind () = validate_flags flags in
  let payload_length = String.length payload in
  let%map () = validate_payload_length ~max_payload_length payload_length in
  let framed_length = header_length + payload_length + checksum_length in
  let bytes = Bytes.create framed_length in
  Stdlib.Bytes.blit_string magic 0 bytes 0 (String.length magic);
  set_uint16_be bytes 8 current_version;
  set_uint16_be bytes 10 flags;
  set_int64_be bytes 12 (Int64.of_int payload_length);
  Stdlib.Bytes.blit_string payload 0 bytes header_length payload_length;
  let checksum_offset = header_length + payload_length in
  let body = Stdlib.Bytes.sub_string bytes 0 checksum_offset in
  let checksum = Digestif.SHA256.(digest_string body |> to_raw_string) in
  Stdlib.Bytes.blit_string checksum 0 bytes checksum_offset checksum_length;
  Bytes.to_string bytes
;;

let decode_length ~max_payload_length contents offset =
  let length = get_int64_be contents (offset + 12) in
  if Int64.(length < zero) || Int64.(length > of_int max_payload_length)
  then Error (Payload_too_large length)
  else Ok (Int64.to_int_exn length)
;;

let validate_header contents offset =
  if not (String.equal magic (String.sub contents ~pos:offset ~len:(String.length magic)))
  then Error Invalid_magic
  else (
    let version = get_uint16_be contents (offset + 8) in
    if version <> current_version then Error (Unsupported_version version) else Ok ())
;;

let decode_complete ~contents ~offset ~payload_length =
  let payload_offset = offset + header_length in
  let checksum_offset = payload_offset + payload_length in
  let body = String.sub contents ~pos:offset ~len:(header_length + payload_length) in
  let expected = Digestif.SHA256.(digest_string body |> to_raw_string) in
  let actual = String.sub contents ~pos:checksum_offset ~len:checksum_length in
  if not (String.equal expected actual)
  then Error Checksum_mismatch
  else
    Ok
      (Complete
         { frame =
             { flags = get_uint16_be contents (offset + 10)
             ; payload = String.sub contents ~pos:payload_offset ~len:payload_length
             ; checksum_raw = actual
             }
         ; next_offset = checksum_offset + checksum_length
         })
;;

let decode ~max_payload_length ~contents ~offset =
  let contents_length = String.length contents in
  if offset < 0 || offset > contents_length
  then Error (Invalid_offset offset)
  else if contents_length - offset < header_length
  then Ok (Incomplete_tail { offset })
  else
    let open Result.Let_syntax in
    let%bind () = validate_header contents offset in
    let%bind payload_length = decode_length ~max_payload_length contents offset in
    let frame_length = header_length + payload_length + checksum_length in
    if contents_length - offset < frame_length
    then Ok (Incomplete_tail { offset })
    else decode_complete ~contents ~offset ~payload_length
;;
