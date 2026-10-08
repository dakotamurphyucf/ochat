open Core

type t =
  { handler : Command_handler.t
  ; admit : Agent_protocol.Command.t -> (unit, Agent_protocol.Error.t) result
  ; inference_response_policy : Inference_query_budget.Policy.t
  }

let create
      ?(admit = fun _ -> Ok ())
      ?(inference_response_policy = Inference_query_budget.Policy.default)
      handler
  =
  { handler; admit; inference_response_policy }
;;

let dispatch_command t ~context command =
  let open Result.Let_syntax in
  let%bind () = t.admit command in
  let%bind inference_budget =
    Inference_query_budget.for_embedded ~max_result_bytes:(16 * 1024 * 1024)
  in
  Command_handler.handle t.handler ~context ~inference_budget command
;;

let decode method_ params = Agent_protocol.Command.of_method_and_params ~method_ ~params

let inference_method method_ =
  String.equal method_ "session.inference_summary"
  || String.equal method_ "session.inference_observations"
;;

let request t context (request : Agent_protocol.Envelope.request) =
  let query = inference_method request.method_ in
  let outcome =
    let open Result.Let_syntax in
    let%bind command = decode request.method_ request.params in
    let%bind () = t.admit command in
    if query
    then (
      let%bind inference_budget =
        Inference_query_budget.for_request t.inference_response_policy request.id
      in
      Command_handler.handle t.handler ~context ~inference_budget command)
    else dispatch_command t ~context command
  in
  let response =
    match outcome with
    | Ok result ->
      Agent_protocol.Public.Result.to_json result
      |> Agent_protocol.Envelope.success ~id:request.id
    | Error error -> Agent_protocol.Envelope.failure ~id:request.id error
  in
  if query
  then
    (* A correlated error may itself exceed the policy for a huge request ID.
       Refuse publication through the transport error path in that case. *)
    Inference_query_budget.validate_envelope t.inference_response_policy response
    |> Result.map ~f:(fun () -> Some response)
  else Ok (Some response)
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
