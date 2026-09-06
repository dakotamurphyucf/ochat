open Core

type t = Time_ns.t [@@deriving compare, equal]

let now = Time_ns.now
let of_time_ns time = time
let to_time_ns t = t

let has_rfc3339_utc_shape encoded =
  let length = String.length encoded in
  length >= 20
  && Char.equal encoded.[4] '-'
  && Char.equal encoded.[7] '-'
  && Char.equal encoded.[10] 'T'
  && Char.equal encoded.[13] ':'
  && Char.equal encoded.[16] ':'
  && Char.equal encoded.[length - 1] 'Z'
;;

let of_string encoded =
  if not (has_rfc3339_utc_shape encoded)
  then Error (Protocol_error.invalid_request "timestamp must be RFC 3339 UTC")
  else (
    match Or_error.try_with (fun () -> Time_ns.of_string_with_utc_offset encoded) with
    | Ok timestamp -> Ok timestamp
    | Error _ -> Error (Protocol_error.invalid_request "timestamp is invalid"))
;;

let to_string t =
  Time_ns.to_string_utc t |> String.substr_replace_first ~pattern:" " ~with_:"T"
;;

let sexp_of_t t = Sexp.Atom (to_string t)

let t_of_sexp = function
  | Sexp.Atom encoded ->
    (match of_string encoded with
     | Ok timestamp -> timestamp
     | Error error -> failwith error.message)
  | sexp -> Sexplib.Conv.of_sexp_error "Timestamp.t must be an RFC 3339 atom" sexp
;;

let to_json t = `String (to_string t)

let of_json = function
  | `String encoded -> of_string encoded
  | _ -> Error (Protocol_error.invalid_request "timestamp must be a JSON string")
;;
