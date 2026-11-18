open! Core
module Error = Protocol_error
module J = Json_codec

let counter = function
  | `String value ->
    (match Int64.of_string_opt value with
     | Some counter
       when Int64.(counter >= 0L) && String.equal (Int64.to_string counter) value ->
       Ok counter
     | Some _ | None -> Error (Error.invalid_request "invalid native Goal counter"))
  | _ -> Error (Error.invalid_request "native Goal counters must be decimal strings")
;;

let fields json ~names =
  let open Result.Let_syntax in
  let%bind () = J.validate_limits ~max_bytes:32768 ~max_depth:16 json in
  let%bind fields = J.fields json in
  let%map () =
    if
      List.for_all (J.to_alist fields) ~f:(fun (name, _) ->
        List.mem names name ~equal:String.equal)
    then Ok ()
    else Error (Error.invalid_request "unknown native Goal request field")
  in
  fields
;;
