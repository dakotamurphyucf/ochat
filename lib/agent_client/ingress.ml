open Core
module P = Agent_protocol

let submit connection (request : P.Ingress.Submit_request.t) =
  match Connection.request connection (Ingress_submit request) with
  | Ok (Ingress_submit response)
    when P.Id.Session.equal request.session_id response.session_id
         && P.Id.Capability.equal request.registration_id response.registration_id
         && P.Idempotency_key.equal request.idempotency_key response.idempotency_key ->
    Ok response
  | Ok _ -> Error (P.Error.invalid_request "unexpected ingress acknowledgement")
  | Error _ as failure -> failure
;;
