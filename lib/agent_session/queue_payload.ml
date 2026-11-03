open! Core
module P = Agent_protocol
module S = Session.Snapshot
module D = Document_schema

let invalid message = Error (P.Error.invalid_request message)
let max_rows = 256
let max_nodes = 200000
let max_bytes = 8 * 1024 * 1024

let limits =
  D.Limits.create ~max_bytes ~max_depth:256 ~max_fields:max_nodes ~max_nodes
  |> function
  | Ok t -> t
  | Error e -> raise_s [%sexp (e : D.Error.t)]
;;

let digests queue =
  let open Result.Let_syntax in
  let nodes = ref 0 in
  let bytes = ref 0 in
  let add count =
    if count > max_bytes - !bytes
    then invalid "queued payload byte limit"
    else (
      bytes := !bytes + count;
      Ok ())
  in
  let text value = add (String.length value) in
  let rec visit depth value =
    if depth > 80 || !nodes >= max_nodes
    then invalid "queued payload structural limit"
    else (
      incr nodes;
      match value with
      | S.Int _ | Bool _ | Unit -> add 8
      | Float value ->
        if Float.is_finite value then add 32 else invalid "nonfinite queued payload"
      | String value -> text value
      | Array values -> children depth values
      | Variant (tag, values) ->
        let%bind () = text tag in
        children depth values
      | Tuple (first, second, rest) ->
        let%bind () = visit (depth + 1) first in
        let%bind () = visit (depth + 1) second in
        children depth rest
      | Record fields ->
        List.fold_result fields ~init:() ~f:(fun () (name, value) ->
          let%bind () = text name in
          visit (depth + 1) value))
  and children depth values =
    List.fold_result values ~init:() ~f:(fun () value -> visit (depth + 1) value)
  in
  let rec bounded_rows remaining = function
    | [] -> Ok ()
    | _ :: _ when remaining = 0 -> invalid "queued callback row limit"
    | _ :: rest -> bounded_rows (remaining - 1) rest
  in
  let%bind () = bounded_rows max_rows queue in
  let%bind () = List.fold_result queue ~init:() ~f:(fun () value -> visit 0 value) in
  let json = `Array (List.map queue ~f:S.to_jsonaf) in
  let%bind () =
    D.Json.validate ~limits json
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
  in
  List.map queue ~f:(fun value ->
    let%bind () = S.validate value |> Result.map_error ~f:P.Error.invalid_request in
    P.Json_codec.canonical_string (S.to_jsonaf value)
    |> Result.map ~f:(fun bytes ->
      Digestif.SHA256.digest_string bytes |> Digestif.SHA256.to_hex))
  |> Result.all
;;
