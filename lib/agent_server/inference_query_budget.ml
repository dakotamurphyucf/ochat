open! Core

let ceiling = 16 * 1024 * 1024

let exceeded () =
  Agent_protocol.Error.create
    Resource_limit
    ~message:"inference query response exceeds its byte budget"
    ~retryable:false
    ()
;;

let limits max_bytes =
  Transcript.Admission.limits ~max_bytes |> Result.map_error ~f:(fun _ -> exceeded ())
;;

let measure ~max_bytes json =
  let open Result.Let_syntax in
  let%bind limits = limits max_bytes in
  Document_schema.Json.validate_and_measure ~limits json
  |> Result.map_error ~f:(fun _ -> exceeded ())
;;

module Policy = struct
  type t = { max_envelope_bytes : int }

  let create ~max_envelope_bytes =
    if max_envelope_bytes <= 0 || max_envelope_bytes > ceiling
    then
      Error
        (Agent_protocol.Error.invalid_request
           "inference response budget must be between 1 byte and 16 MiB")
    else Ok { max_envelope_bytes }
  ;;

  let default = { max_envelope_bytes = ceiling }
end

type t = { max_result_bytes : int }

let for_request (policy : Policy.t) request_id =
  let open Result.Let_syntax in
  let%bind bytes =
    Agent_protocol.Envelope.success ~id:request_id `Null
    |> Agent_protocol.Envelope.to_json
    |> measure ~max_bytes:policy.max_envelope_bytes
  in
  let max_result_bytes = policy.max_envelope_bytes - (bytes - 4) in
  if max_result_bytes <= 0 then Error (exceeded ()) else Ok { max_result_bytes }
;;

let for_embedded ~max_result_bytes =
  if max_result_bytes <= 0 || max_result_bytes > ceiling
  then
    Error
      (Agent_protocol.Error.invalid_request
         "inference result budget must be between 1 byte and 16 MiB")
  else Ok { max_result_bytes }
;;

let max_result_bytes t = t.max_result_bytes

let validate_result t result =
  Agent_protocol.Public.Result.to_json result
  |> measure ~max_bytes:t.max_result_bytes
  |> Result.map ~f:ignore
;;

let validate_envelope (policy : Policy.t) envelope =
  Agent_protocol.Envelope.to_json envelope
  |> measure ~max_bytes:policy.max_envelope_bytes
  |> Result.map ~f:ignore
;;
