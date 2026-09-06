open Core

type t = { handler : Command_handler.t }

let create handler = { handler }

let dispatch_command t ~context command =
  Command_handler.handle t.handler ~context command
;;

let decode method_ params = Agent_protocol.Command.of_method_and_params ~method_ ~params

let request t context (request : Agent_protocol.Envelope.request) =
  match decode request.Agent_protocol.Envelope.method_ request.params with
  | Error error -> Ok (Some (Agent_protocol.Envelope.failure ~id:request.id error))
  | Ok command ->
    let response =
      match dispatch_command t ~context command with
      | Ok result ->
        Agent_protocol.Method_result.to_json result
        |> Agent_protocol.Envelope.success ~id:request.id
      | Error error -> Agent_protocol.Envelope.failure ~id:request.id error
    in
    Ok (Some response)
;;

let notification_safe = function
  | Agent_protocol.Command.Protocol_ping _ -> true
  | _ -> false
;;

let notification t context (notification : Agent_protocol.Envelope.notification) =
  let open Result.Let_syntax in
  let%bind command =
    decode notification.Agent_protocol.Envelope.method_ notification.params
  in
  if notification_safe command
  then Result.map (dispatch_command t ~context command) ~f:(fun _ -> None)
  else
    Error
      (Agent_protocol.Error.create
         Invalid_request
         ~message:"method requires a request identifier"
         ~retryable:false
         ())
;;

let dispatch_envelope t ~context = function
  | Agent_protocol.Envelope.Request value -> request t context value
  | Notification value -> notification t context value
  | Response _ ->
    Error
      (Agent_protocol.Error.create
         Invalid_request
         ~message:"server dispatcher does not accept response envelopes"
         ~retryable:false
         ())
;;
