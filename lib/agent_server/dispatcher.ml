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

let developer_trace_method method_ =
  List.mem
    Agent_protocol.Developer_trace_wire.Request.supported_methods
    method_
    ~equal:String.equal
;;

let trace_envelope_limits inner =
  let module D = Document_schema in
  let add left right =
    if left > Int.max_value - right then None else Some (left + right)
  in
  let open Option.Let_syntax in
  let%bind max_bytes = add (D.Limits.max_bytes inner) 512 in
  let%bind max_nodes = add (D.Limits.max_nodes inner) 8 in
  let%bind max_fields = add (D.Limits.max_fields inner) 8 in
  let%bind max_depth = add (D.Limits.max_depth inner) 2 in
  D.Limits.create ~max_bytes ~max_nodes ~max_fields ~max_depth |> Result.ok
;;

let trace_envelope ~limits envelope =
  let error =
    Agent_protocol.Error.create
      Resource_limit
      ~message:"trace JSON-RPC envelope exceeds installed bound"
      ~retryable:false
      ()
  in
  match trace_envelope_limits limits with
  | None -> Error error
  | Some limits ->
    Document_schema.Json.validate_and_measure
      ~limits
      (Agent_protocol.Envelope.to_json envelope)
    |> Result.map ~f:ignore
    |> Result.map_error ~f:(fun _ -> error)
;;

let trace_request_id request_id =
  let error =
    Agent_protocol.Error.create
      Resource_limit
      ~message:"trace request ID exceeds installed bound"
      ~retryable:false
      ()
  in
  let open Result.Let_syntax in
  let%bind limits =
    Document_schema.Limits.create ~max_bytes:256 ~max_depth:2 ~max_fields:1 ~max_nodes:2
    |> Result.map_error ~f:(fun _ -> error)
  in
  Document_schema.Json.validate_and_measure
    ~limits
    (Agent_protocol.Envelope.Request_id.to_json request_id)
  |> Result.map ~f:ignore
  |> Result.map_error ~f:(fun _ -> error)
;;

let trace_uninstalled_response_limits =
  (* An unavailable opt-in has no installed trace policy to borrow. Its fixed
     error and bounded request ID still deserve a correlated reply. *)
  Document_schema.Limits.create ~max_bytes:1_024 ~max_depth:8 ~max_fields:32 ~max_nodes:48
  |> Result.ok
  |> Option.value_exn
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

let dispatch_developer_trace t ?actor ~context request =
  let open Result.Let_syntax in
  let%bind installed = developer t in
  let%bind () = installed.admit () in
  Developer_endpoint.handle_trace installed.endpoint ?actor ~connection:context request
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
  if developer_trace_method request.method_
  then
    let open Result.Let_syntax in
    let outcome =
      let%bind installed = developer t in
      let%bind () = installed.admit () in
      let%bind limits = Developer_endpoint.trace_limits installed.endpoint in
      let%bind () = trace_request_id request.id in
      let%bind () =
        trace_envelope
          ~limits:(Agent_protocol.Developer_trace_wire.Limits.request limits)
          (Agent_protocol.Envelope.Request request)
      in
      let%bind decoded =
        Agent_protocol.Developer_trace_wire.Request.of_method_and_params
          ~limits
          ~method_:request.method_
          ~params:request.params
      in
      let%bind response =
        Developer_endpoint.handle_trace
          installed.endpoint
          ?actor
          ~connection:context
          decoded
      in
      if
        not
          (String.equal
             request.method_
             (Agent_protocol.Developer_trace_wire.Response.method_name response))
      then
        Error
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"trace result has a different method"
             ~retryable:false
             ())
      else Developer_endpoint.encode_trace_response installed.endpoint response
    in
    let response =
      match outcome with
      | Ok result -> Agent_protocol.Envelope.success ~id:request.id result
      | Error error -> Agent_protocol.Envelope.failure ~id:request.id error
    in
    let response_limits =
      match t.developer with
      | None -> trace_uninstalled_response_limits
      | Some installed ->
        (match Developer_endpoint.trace_limits installed.endpoint with
         | Ok limits -> Agent_protocol.Developer_trace_wire.Limits.response limits
         | Error _ -> trace_uninstalled_response_limits)
    in
    let%bind () = trace_envelope ~limits:response_limits response in
    Ok (Some response)
  else if developer_repl_method request.method_
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
  if
    developer_method notification.method_
    || developer_repl_method notification.method_
    || developer_trace_method notification.method_
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

type dispatch_schedule =
  | Immediate
  | Repl_wait
  | Repl_cancel

let dispatch_schedule t = function
  | Agent_protocol.Envelope.Request request ->
    (match t.developer with
     | None -> Immediate
     | Some installed ->
       let module R = Agent_protocol.Developer_repl_wire in
       (match Developer_endpoint.repl_limits installed.endpoint with
        | Error _ -> Immediate
        | Ok limits ->
          let open R.Request in
          (match
             R.Request.of_method_and_params
               ~limits
               ~method_:request.method_
               ~params:request.params
           with
           | Ok (Cancel_submission _ | Task_cancel_and_wait _) -> Repl_cancel
           | Ok (Submit _ | Task_await_terminal _) -> Repl_wait
           | Ok
               ( Capabilities
               | Create _
               | Observe _
               | Inspect_type _
               | Task_references_page _
               | Task_submit _
               | Task_observe _
               | Reset_and_wait _
               | Close_and_wait _
               | Watch _
               | Unwatch _ )
           | Error _ -> Immediate)))
  | Notification _ | Response _ -> Immediate
;;
