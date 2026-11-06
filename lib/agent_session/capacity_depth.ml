open! Core
module P = Agent_protocol
module D = Document_schema

let domain error = P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error))

let measure raw ~limits =
  let%map.Result () = D.Json.validate raw ~limits |> Result.map_error ~f:domain in
  (* The shared validator has bounded depth and total nodes before this walk. *)
  let rec depth = function
    | `Object fields ->
      List.fold fields ~init:1 ~f:(fun maximum (_, value) ->
        Int.max maximum (1 + depth value))
    | `Array values ->
      List.fold values ~init:1 ~f:(fun maximum value -> Int.max maximum (1 + depth value))
    | `Null | `True | `False | `Number _ | `String _ -> 1
  in
  depth raw
;;

let validate depth ~ancestors ~limits =
  if depth <= 0 || ancestors < 0
  then Error (P.Error.invalid_request "invalid JSON depth coordinates")
  else if
    ancestors >= D.Limits.max_depth limits
    || depth > D.Limits.max_depth limits - ancestors
  then Error (domain (D.Error.Limit_exceeded "depth"))
  else Ok ()
;;
