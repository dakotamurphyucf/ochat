open! Core
module Error = Protocol_error

type t = string [@@deriving compare, equal, sexp_of]

let of_string value =
  if String.is_empty value || String.length value > 256
  then Error (Error.invalid_request "command receipt ID must be 1..256 bytes")
  else Ok value
;;

let to_string t = t
let to_json t = `String t

let of_json = function
  | `String value -> of_string value
  | `Null | `True | `False | `Number _ | `Array _ | `Object _ ->
    Error (Error.invalid_request "command receipt ID must be a string")
;;
