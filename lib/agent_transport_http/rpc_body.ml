open! Core

type t =
  | Single of Agent_protocol.Envelope.t
  | Batch of Agent_protocol.Envelope.t list

let protocol_error code message =
  Agent_protocol.Error.create code ~message ~retryable:false ()
;;

let valid_utf8 value =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String value) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar _ -> loop ()
    | `End -> true
    | `Malformed _ -> false
    | `Await -> assert false
  in
  loop ()
;;

let decode_envelope json =
  let open Result.Let_syntax in
  let%bind canonical = Agent_protocol.Json_codec.canonical json in
  Agent_protocol.Envelope.of_json canonical
;;

let rec within_depth remaining = function
  | `Object fields when remaining > 0 ->
    List.for_all fields ~f:(fun (_, value) -> within_depth (remaining - 1) value)
  | `Array values when remaining > 0 ->
    List.for_all values ~f:(within_depth (remaining - 1))
  | `Object _ | `Array _ -> false
  | `Null | `True | `False | `String _ | `Number _ -> true
;;

let decode_json ~max_batch_size = function
  | `Array [] -> Error (protocol_error Invalid_request "RPC batch must be nonempty")
  | `Array values when List.length values > max_batch_size ->
    Error (protocol_error Resource_limit "RPC batch exceeds the configured limit")
  | `Array values ->
    Result.map
      (Result.all (List.map values ~f:decode_envelope))
      ~f:(fun values -> Batch values)
  | json -> Result.map (decode_envelope json) ~f:(fun envelope -> Single envelope)
;;

let parse ~max_batch_size body =
  if max_batch_size <= 0
  then Error (protocol_error Internal_error "RPC batch limit must be positive")
  else if not (valid_utf8 body)
  then Error (protocol_error Invalid_request "JSON request body must be valid UTF-8")
  else (
    match Result.try_with (fun () -> Jsonaf.of_string body) with
    | Error exn ->
      Error
        (protocol_error
           Invalid_request
           ("invalid JSON request body: " ^ Exn.to_string exn))
    | Ok json when not (within_depth 64 json) ->
      Error (protocol_error Resource_limit "JSON request body exceeds nesting limit")
    | Ok json -> decode_json ~max_batch_size json)
;;
