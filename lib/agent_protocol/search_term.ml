open! Core

type t = string [@@deriving equal, sexp_of]

let create text =
  if String.is_empty text || String.length text > 256
  then
    Error (Protocol_error.invalid_request "search term must contain 1 to 256 UTF-8 bytes")
  else if not (Stdlib.String.is_valid_utf_8 text)
  then Error (Protocol_error.invalid_request "search term is not valid UTF-8")
  else Ok text
;;

let text t = t
let to_json t = `String t
let of_json json = Result.bind (Json_codec.string json) ~f:create
