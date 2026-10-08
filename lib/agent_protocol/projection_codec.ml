open Core

let limits = Transcript.Admission.default

let validate json =
  Document_schema.Json.validate ~limits json
  |> Result.map_error ~f:(fun _ ->
    Protocol_error.invalid_request "invalid or oversized transcript envelope")
;;

let string_result result = Result.map_error result ~f:Protocol_error.invalid_request

let optional name value encode =
  Option.to_list (Option.map value ~f:(fun value -> name, encode value))
;;
