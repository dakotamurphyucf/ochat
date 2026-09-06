open Core

type t = string [@@deriving compare, equal, hash, sexp]

let maximum_length = 256

let is_allowed_character = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | ':' | '/' -> true
  | _ -> false
;;

let of_string encoded =
  if String.is_empty encoded
  then Error (Protocol_error.invalid_request "idempotency key must be nonempty")
  else if String.length encoded > maximum_length
  then Error (Protocol_error.invalid_request "idempotency key exceeds maximum length")
  else if not (String.for_all encoded ~f:is_allowed_character)
  then
    Error (Protocol_error.invalid_request "idempotency key contains invalid characters")
  else Ok encoded
;;

let to_string t = t
let to_json t = `String t

let of_json = function
  | `String encoded -> of_string encoded
  | _ -> Error (Protocol_error.invalid_request "idempotency key must be a JSON string")
;;
