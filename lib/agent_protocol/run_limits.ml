open! Core

let max_occurrences = 4096
let max_document_bytes = 1024 * 1024
let max_depth = 64

let check_count count =
  if count > max_occurrences
  then Error (Protocol_error.invalid_request "run occurrence bound exceeded")
  else Ok ()
;;

let list decode = function
  | `Array values ->
    let open Result.Let_syntax in
    let%bind () = check_count (List.length values) in
    Result.all (List.map values ~f:decode)
  | _ -> Error (Protocol_error.invalid_request "run occurrence list must be array")
;;
