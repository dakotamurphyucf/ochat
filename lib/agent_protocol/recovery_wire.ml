open! Core

let limits ~max_bytes ~max_fields ~max_nodes =
  Document_schema.Limits.create ~max_bytes ~max_depth:16 ~max_fields ~max_nodes
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let coordinate_limits = limits ~max_bytes:32768 ~max_fields:32 ~max_nodes:64

let measure ~limits json =
  Document_schema.Json.validate_and_measure ~limits json
  |> Result.map_error ~f:(fun _ ->
    Protocol_error.invalid_request "invalid or oversized recovery JSON")
;;

let admit_coordinate json = Result.map (measure ~limits:coordinate_limits json) ~f:ignore

let fields json names =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%map () = Extension_codec.closed fields names in
  fields
;;

let revision_to_json n = `String (Int64.to_string n)

let revision_of_json json =
  let open Result.Let_syntax in
  let%bind encoded = Json_codec.string json in
  match Int64.of_string_opt encoded with
  | Some n when Int64.(n >= 0L) && String.equal encoded (Int64.to_string n) -> Ok n
  | Some _ | None ->
    Error (Protocol_error.invalid_request "invalid decimal recovery revision")
;;
