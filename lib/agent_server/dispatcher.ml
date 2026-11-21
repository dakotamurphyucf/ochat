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

let query_envelope_limits ~max_depth ~max_fields ~max_nodes max_bytes =
  Document_schema.Limits.create ~max_bytes ~max_depth ~max_fields ~max_nodes
  |> Result.ok
  |> Option.value_exn
;;

let goal_query_limits =
  query_envelope_limits
    ~max_depth:74
    ~max_fields:16_384
    ~max_nodes:32_768
    Agent_protocol.Goal_query.max_envelope_bytes
;;

let recurrence_query_limits =
  (* Up to 512 retained occurrences with eight attempts each, plus bounded
     audit ranges. The complete correlated envelope shares the 4 MiB cap. *)
  query_envelope_limits
    ~max_depth:34
    ~max_fields:131_072
    ~max_nodes:262_144
    Agent_protocol.Recurrence_query.max_envelope_bytes
;;

let recovery_method name =
  String.equal name Agent_protocol.Recovery_query.Enable.method_name
  || String.equal name Agent_protocol.Recovery_query.List_windows.method_name
  || String.equal name Agent_protocol.Recovery_history_request.method_name
  || String.equal name Agent_protocol.Recovery_text_range.method_name
  || String.equal name Agent_protocol.Recovery_asset_read.method_name
;;

let recovery_control_method name =
  let module R = Agent_protocol.Recovery_rollover_control in
  String.equal name R.Inspect.method_name || String.equal name R.Request.method_name
;;

let recovery_control_limits =
  query_envelope_limits ~max_depth:16 ~max_fields:108 ~max_nodes:220
    Agent_protocol.Recovery_rollover_control.max_envelope_bytes
;;

let recovery_envelope_limits =
  query_envelope_limits ~max_depth:18 ~max_fields:516 ~max_nodes:1032
    (Agent_protocol.Recovery_query.max_page_bytes + 512)
;;

let history_envelope_limits =
  query_envelope_limits ~max_depth:18 ~max_fields:2052 ~max_nodes:65544
    (Agent_protocol.Recovery_query.max_page_bytes + 512)
;;

(* A UTF-8 byte can expand to six JSON bytes when escaped. *)
let text_range_limits =
  query_envelope_limits ~max_depth:18 ~max_fields:64 ~max_nodes:128 ((6 * Agent_protocol.History_text.max_slice_bytes) + 512)
;;

let asset_read_limits =
  query_envelope_limits
    ~max_depth:18
    ~max_fields:40
    ~max_nodes:80
    Agent_protocol.Recovery_asset_read.max_envelope_bytes
;;

let operator_query_limits method_ =
  if List.mem Agent_protocol.Goal_query.Request.methods method_ ~equal:String.equal
  then Some ("Goal", goal_query_limits)
  else if String.equal method_ Agent_protocol.Recovery_history_request.method_name
  then Some ("Recovery", history_envelope_limits)
  else if String.equal method_ Agent_protocol.Recovery_text_range.method_name
  then Some ("Recovery", text_range_limits)
  else if String.equal method_ Agent_protocol.Recovery_asset_read.method_name
  then Some ("Recovery", asset_read_limits)
  else if recovery_control_method method_
  then Some ("Recovery", recovery_control_limits)
  else if recovery_method method_
  then Some ("Recovery", recovery_envelope_limits)
  else if
    List.mem Agent_protocol.Recurrence_query.Request.methods method_ ~equal:String.equal
  then Some ("Recurrence", recurrence_query_limits)
  else None
;;

let validate_query_envelope name limits envelope =
  Document_schema.Json.validate_and_measure
    ~limits
    (Agent_protocol.Envelope.to_json envelope)
  |> Result.map ~f:ignore
  |> Result.map_error ~f:(fun _ ->
    Agent_protocol.Error.create
      Resource_limit
      ~message:(name ^ " JSON-RPC envelope exceeds its response bound")
      ~retryable:false
      ())
;;

let bounded_request_id ~family request_id =
  let error =
    Agent_protocol.Error.create
      Resource_limit
      ~message:(family ^ " request ID exceeds installed bound")
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

let run_command t ?actor ?on_release ~context command =
  let open Result.Let_syntax in
  let%bind () = t.admit command in
  let%bind inference_budget =
    Inference_query_budget.for_embedded ~max_result_bytes:(16 * 1024 * 1024)
  in
  Command_handler.handle t.handler ?actor ?on_release ~context ~inference_budget command
;;

let dispatch_command t ?actor ~context command =
  run_command t ?actor ~context command
;;

let decode method_ params = Agent_protocol.Command.of_method_and_params ~method_ ~params

let inference_method method_ =
  String.equal method_ "session.inference_summary"
  || String.equal method_ "session.inference_observations"
;;

let request t ?actor ~on_release context (request : Agent_protocol.Envelope.request) =
  if developer_trace_method request.method_
  then
    let open Result.Let_syntax in
    let outcome =
      let%bind installed = developer t in
      let%bind () = installed.admit () in
      let%bind limits = Developer_endpoint.trace_limits installed.endpoint in
      let%bind () = bounded_request_id ~family:"trace" request.id in
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
      else run_command t ?actor ~on_release ~context command
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
    else (
      match operator_query_limits request.method_ with
      | None -> Ok (Some response)
      | Some (name, limits) ->
        (match validate_query_envelope name limits response with
         | Ok () -> Ok (Some response)
         | Error failure ->
           let response = Agent_protocol.Envelope.failure ~id:request.id failure in
           validate_query_envelope name limits response
           |> Result.map ~f:(fun () -> Some response))))
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

module Output = struct
  type t =
    | Immediate of Agent_protocol.Envelope.t
    | Checked of
        { id : Agent_protocol.Envelope.Request_id.t
        ; envelope : Agent_protocol.Envelope.t
        ; check : unit -> (unit, Agent_protocol.Error.t) result
        }

  let immediate envelope = Immediate envelope

  let requires_check = function
    | Immediate _ -> false
    | Checked _ -> true
  ;;

  let materialize = function
    | Immediate envelope -> envelope
    | Checked { id; envelope; check } ->
      (match check () with
       | Ok () -> envelope
       | Error _ ->
         Agent_protocol.Envelope.failure ~id
           (Agent_protocol.Error.create Permission_denied
              ~message:"recovery result is no longer available" ~retryable:false ()))
  ;;
end

let dispatch_output t ?actor ~context = function
  | Agent_protocol.Envelope.Request value ->
    let%bind.Result () =
      if recovery_method value.method_
      then bounded_request_id ~family:"recovery" value.id
      else Ok ()
    in
    let release = ref None in
    let on_release check = release := Some check in
    let%map.Result response = request t ?actor ~on_release context value in
    Option.map response ~f:(fun envelope ->
      match !release, envelope with
      | Some check, Agent_protocol.Envelope.Response { outcome = Ok _; id } ->
        Output.Checked { id; envelope; check }
      | _ -> Output.immediate envelope)
  | Notification value ->
    notification t ?actor context value |> Result.map ~f:(Option.map ~f:Output.immediate)
  | Response _ ->
    Error
      (Agent_protocol.Error.create
         Invalid_request
         ~message:"server dispatcher does not accept response envelopes"
         ~retryable:false
         ())
;;

let dispatch_envelope t ?actor ~context envelope =
  dispatch_output t ?actor ~context envelope
  |> Result.map ~f:(Option.map ~f:Output.materialize)
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
               | Complete _
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
