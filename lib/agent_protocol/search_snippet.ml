open! Core
module J = Json_codec

type t =
  { text : string
  ; highlight_start : int
  ; highlight_length : int
  ; truncated_before : bool
  ; truncated_after : bool
  }
[@@deriving equal, sexp_of]

let is_boundary text offset =
  if offset = String.length text
  then true
  else (
    let byte = Char.to_int text.[offset] in
    byte < 128 || byte >= 192)
;;

let create ~text ~highlight_start ~highlight_length ~truncated_before ~truncated_after =
  let bytes = String.length text in
  if bytes > 2048 || not (Stdlib.String.is_valid_utf_8 text)
  then Error (Protocol_error.invalid_request "invalid or oversized UTF-8 search snippet")
  else if
    highlight_start < 0
    || highlight_start > bytes
    || highlight_length <= 0
    || highlight_length > bytes - highlight_start
  then Error (Protocol_error.invalid_request "search highlight lies outside snippet")
  else if
    (not (is_boundary text highlight_start))
    || not (is_boundary text (highlight_start + highlight_length))
  then Error (Protocol_error.invalid_request "search highlight splits a UTF-8 scalar")
  else Ok { text; highlight_start; highlight_length; truncated_before; truncated_after }
;;

let text t = t.text
let highlight_start t = t.highlight_start
let highlight_length t = t.highlight_length
let truncated_before t = t.truncated_before
let truncated_after t = t.truncated_after

let to_json t =
  `Object
    [ "text", `String t.text
    ; "highlight_start", `Number (Int.to_string t.highlight_start)
    ; "highlight_length", `Number (Int.to_string t.highlight_length)
    ; ("truncated_before", if t.truncated_before then `True else `False)
    ; ("truncated_after", if t.truncated_after then `True else `False)
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind text = J.required_as fields "text" J.string in
  let%bind highlight_start =
    J.required_as fields "highlight_start" (J.bounded_int ~min:0 ~max:2048)
  in
  let%bind highlight_length =
    J.required_as fields "highlight_length" (J.bounded_int ~min:1 ~max:2048)
  in
  let%bind truncated_before = J.required_as fields "truncated_before" J.bool in
  let%bind truncated_after = J.required_as fields "truncated_after" J.bool in
  create ~text ~highlight_start ~highlight_length ~truncated_before ~truncated_after
;;
