open Core

module Developer_installation = struct
  type t =
    { endpoint : Developer_endpoint.t
    ; admit : unit -> (unit, Agent_protocol.Error.t) result
    }

  let create ~endpoint ~admit = { endpoint; admit }
end

type t =
  { handler : Command_handler.t
  ; admit : Agent_protocol.Command.t -> (unit, Agent_protocol.Error.t) result
  ; inference_response_policy : Inference_query_budget.Policy.t
  ; developer : Developer_installation.t option
  }

let create
      ?(admit = fun _ -> Ok ())
      ?(inference_response_policy = Inference_query_budget.Policy.default)
      ?developer
      handler
  =
  { handler; admit; inference_response_policy; developer }
;;

let developer_method method_ =
  List.mem
    Agent_protocol.Developer_wire.Request.supported_methods
    method_
    ~equal:String.equal
;;

let developer_repl_method method_ =
  List.mem
    Agent_protocol.Developer_repl_wire.Request.supported_methods
    method_
    ~equal:String.equal
;;

let developer t =
  Result.of_option
    t.developer
    ~error:
      (Agent_protocol.Error.create
         Method_not_found
         ~message:"Developer method is not installed"
         ~retryable:false
         ())
;;

let dispatch_developer t ?actor ~context request =
  let open Result.Let_syntax in
  let%bind installed = developer t in
  let%bind () = installed.admit () in
  Developer_endpoint.handle installed.endpoint ?actor ~connection:context request
;;

let dispatch_developer_repl t ?actor ~context request =
  let open Result.Let_syntax in
  let%bind installed = developer t in
  let%bind () = installed.admit () in
  Developer_endpoint.handle_repl installed.endpoint ?actor ~connection:context request
;;

let dispatch_command t ?actor ~context command =
  let open Result.Let_syntax in
  let%bind () = t.admit command in
  let%bind inference_budget =
    Inference_query_budget.for_embedded ~max_result_bytes:(16 * 1024 * 1024)
  in
  Command_handler.handle t.handler ?actor ~context ~inference_budget command
;;

let decode method_ params = Agent_protocol.Command.of_method_and_params ~method_ ~params

let inference_method method_ =
  String.equal method_ "session.inference_summary"
  || String.equal method_ "session.inference_observations"
;;

let request t ?actor context (request : Agent_protocol.Envelope.request) =
  if developer_repl_method request.method_
  then (
    let outcome =
      let open Result.Let_syntax in
      let%bind installed = developer t in
      let%bind () = installed.admit () in
      let%bind limits = Developer_endpoint.repl_limits installed.endpoint in
      let%bind decoded =
        Agent_protocol.Developer_repl_wire.Request.of_method_and_params
          ~limits
          ~method_:request.method_
          ~params:request.params
      in
      let%bind response =
        Developer_endpoint.handle_repl
          installed.endpoint
          ?actor
          ~connection:context
          decoded
      in
      if
        not
          (String.equal
             request.method_
             (Agent_protocol.Developer_repl_wire.Response.method_name response))
      then
        Error
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"REPL result has a different method"
             ~retryable:false
             ())
      else Developer_endpoint.encode_repl_response installed.endpoint response
    in
    let response =
      match outcome with
      | Ok result -> Agent_protocol.Envelope.success ~id:request.id result
      | Error error -> Agent_protocol.Envelope.failure ~id:request.id error
    in
    Ok (Some response))
  else if developer_method request.method_
  then (
    let outcome =
      let open Result.Let_syntax in
      let%bind installed = developer t in
      let%bind () = installed.admit () in
      let%bind decoded =
        Agent_protocol.Developer_wire.Request.of_method_and_params
          ~limits:(Developer_endpoint.limits installed.endpoint)
          ~method_:request.method_
          ~params:request.params
      in
      let%bind response =
        Developer_endpoint.handle installed.endpoint ?actor ~connection:context decoded
      in
      if
        not
          (String.equal
             request.method_
             (Agent_protocol.Developer_wire.Response.method_name response))
      then
        Error
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"Developer result has a different method"
             ~retryable:false
             ())
      else Developer_endpoint.encode_response installed.endpoint response
    in
    let response =
      match outcome with
      | Ok result -> Agent_protocol.Envelope.success ~id:request.id result
      | Error error -> Agent_protocol.Envelope.failure ~id:request.id error
    in
    Ok (Some response))
  else (
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
        Command_handler.handle t.handler ?actor ~context ~inference_budget command)
      else dispatch_command t ?actor ~context command
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
    else Ok (Some response))
;;

let notification_safe = function
  | Agent_protocol.Command.Protocol_ping _ -> true
  | _ -> false
;;

let notification t ?actor context (notification : Agent_protocol.Envelope.notification) =
  if developer_method notification.method_ || developer_repl_method notification.method_
  then
    Error
      (Agent_protocol.Error.create
         Invalid_request
         ~message:"Developer method requires a request identifier"
         ~retryable:false
         ())
  else
    let open Result.Let_syntax in
    let%bind command =
      decode notification.Agent_protocol.Envelope.method_ notification.params
    in
    if notification_safe command
    then Result.map (dispatch_command t ?actor ~context command) ~f:(fun _ -> None)
    else
      Error
        (Agent_protocol.Error.create
           Invalid_request
           ~message:"method requires a request identifier"
           ~retryable:false
           ())
;;

let close_connection_and_wait t context =
  Eio.Cancel.protect (fun () ->
    let hub_outcome =
      try
        Ok
          (match t.developer with
           | None -> Ok ()
           | Some installed ->
             Developer_endpoint.close_connection_and_wait installed.endpoint context)
      with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    let finish original_failure =
      let existing_outcome =
        try Ok (Command_handler.close_connection t.handler context) with
        | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
      in
      match original_failure, existing_outcome with
      | Some (exn, backtrace), _ -> Stdlib.Printexc.raise_with_backtrace exn backtrace
      | None, Error (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
      | None, Ok result -> result
    in
    match hub_outcome with
    | Ok (Error error) ->
      (* The Hub's typed error is a preflight domain/self-join rejection:
         its pump remains live, so shared Authoring cleanup cannot start. *)
      Error error
    | Ok (Ok ()) -> finish None
    | Error failure -> finish (Some failure))
;;

let close_and_wait t =
  match t.developer with
  | None -> Ok ()
  | Some installed -> Developer_endpoint.close_and_wait installed.endpoint
;;

let dispatch_envelope t ?actor ~context = function
  | Agent_protocol.Envelope.Request value -> request t ?actor context value
  | Notification value -> notification t ?actor context value
  | Response _ ->
    Error
      (Agent_protocol.Error.create
         Invalid_request
         ~message:"server dispatcher does not accept response envelopes"
         ~retryable:false
         ())
;;
