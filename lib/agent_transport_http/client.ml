open! Core
module P = Piaf

type t =
  { sw : Eio.Switch.t
  ; lifetime : Client_lifetime.t
  ; env : Eio_unix.Stdenv.base
  ; rpc_client : P.Client.t
  ; control_rpc_client : P.Client.t
  ; event_client : P.Client.t
  ; rpc_path : string
  ; event_path : string
  ; close_path : string
  ; bearer_token : string option
  ; notifications : Agent_protocol.Envelope.t Agent_session.Mailbox.t
  ; mutex : Eio.Mutex.t
  ; control_mutex : Eio.Mutex.t
  ; mutable connection_id : string option
  ; mutable next_request_id : int64
  ; mutable event_reader_started : bool
  ; mutable failed : bool
  ; mutable closed : bool
  }

let connection_header = "ochat-connection-id"
let protocol_version_header = "ochat-protocol-version"

let interrupted message =
  Agent_protocol.Error.create Interrupted ~message ~retryable:true ()
;;

let invalid_response message =
  Agent_protocol.Error.create Interrupted ~message ~retryable:false ()
;;

let base_headers t =
  Option.value_map t.bearer_token ~default:[] ~f:(fun token ->
    [ "authorization", "Bearer " ^ token ])
;;

let connection_headers t =
  base_headers t
  @ Option.value_map t.connection_id ~default:[] ~f:(fun id -> [ connection_header, id ])
;;

let rpc_headers t =
  [ "content-type", "application/json"
  ; "accept", "application/json"
  ; protocol_version_header, "2.0"
  ]
  @ connection_headers t
;;

let parse_json body =
  try Ok (Jsonaf.of_string body) with
  | exn -> Error (invalid_response ("invalid HTTP response JSON: " ^ Exn.to_string exn))
;;

let response_error response body =
  match parse_json body |> Result.bind ~f:Agent_protocol.Error.of_json with
  | Ok error -> error
  | Error _ ->
    interrupted
      (sprintf
         "HTTP request failed with status %d"
         (P.Status.to_code response.P.Response.status))
;;

let read_body response =
  P.Body.to_string response.P.Response.body
  |> Result.map_error ~f:(fun failure -> interrupted (P.Error.to_string failure))
;;

exception Response_body_too_large

let read_body_bounded response ~max_bytes =
  let body = Buffer.create (Int.min max_bytes 4096) in
  match
    P.Body.iter response.P.Response.body ~f:(fun { buffer; off; len } ->
      if len > max_bytes - Buffer.length body
      then raise Response_body_too_large
      else
        Buffer.add_string body (Cstruct.to_string (Cstruct.of_bigarray ~off ~len buffer)))
  with
  | Ok () -> Ok (Buffer.contents body)
  | Error failure -> Error (interrupted (P.Error.to_string failure))
  | exception Response_body_too_large ->
    Error (interrupted "Developer HTTP response exceeds its negotiated body limit")
;;

let capture_connection_id t response =
  match P.Headers.get response.P.Response.headers connection_header with
  | None -> Ok false
  | Some id ->
    (match t.connection_id with
     | None ->
       t.connection_id <- Some id;
       Ok true
     | Some current when String.equal current id -> Ok false
     | Some _ -> Error (invalid_response "HTTP connection ID changed unexpectedly"))
;;

let validate_control_connection_id t response =
  match t.connection_id, P.Headers.get response.P.Response.headers connection_header with
  | Some expected, Some actual when String.equal expected actual -> Ok false
  | Some _, Some _ | Some _, None | None, _ ->
    Error (invalid_response "REPL control changed logical HTTP connection identity")
;;

let next_request_id t =
  if Int64.equal t.next_request_id Int64.max_value
  then Error (interrupted "HTTP request identifier space is exhausted")
  else (
    let id = t.next_request_id in
    t.next_request_id <- Int64.(id + 1L);
    Agent_protocol.Envelope.Request_id.of_json (`Number (Int64.to_string id)))
;;

let response_result command request_id envelope =
  match envelope with
  | Agent_protocol.Envelope.Response response
    when Agent_protocol.Envelope.Request_id.compare response.id request_id = 0 ->
    Result.bind response.outcome ~f:(fun json ->
      Agent_protocol.Public.Result.of_json
        ~method_:(Agent_protocol.Command.method_name command)
        json
      |> Result.map_error ~f:(fun failure ->
        interrupted
          ("HTTP success response could not be validated: "
           ^ failure.Agent_protocol.Error.message)))
  | Response _ -> Error (invalid_response "HTTP response identifier does not match")
  | Notification _ | Request _ ->
    Error (invalid_response "HTTP RPC returned a non-response envelope")
;;

let fail_connection t =
  t.failed <- true;
  Agent_session.Mailbox.close t.notifications
;;

let retire_connection t =
  Eio.Cancel.protect (fun () ->
    fail_connection t;
    t.closed <- true;
    Client_lifetime.close t.lifetime)
;;

let push_notification t envelope =
  if not (Agent_session.Mailbox.try_push t.notifications ~priority:Normal envelope)
  then fail_connection t
;;

let parse_sse_data t lines =
  let data =
    List.filter_map lines ~f:(fun line ->
      match String.chop_prefix line ~prefix:"data:" with
      | None -> None
      | Some value -> Some (String.lstrip value))
    |> String.concat ~sep:"\n"
  in
  if not (String.is_empty data)
  then (
    match parse_json data |> Result.bind ~f:Agent_protocol.Envelope.of_json with
    | Ok (Notification _ as envelope) -> push_notification t envelope
    | Ok (Request _ | Response _) | Error _ -> fail_connection t)
;;

let rec read_sse_lines t reader lines =
  if not t.failed
  then (
    match Eio.Buf_read.line reader with
    | "" ->
      parse_sse_data t (List.rev lines);
      read_sse_lines t reader []
    | line -> read_sse_lines t reader (line :: lines)
    | exception End_of_file -> parse_sse_data t (List.rev lines))
;;

let read_sse_body t body =
  Eio.Switch.run (fun sw ->
    let read_flow, write_flow = Eio_unix.pipe sw in
    Eio.Fiber.both
      (fun () ->
         Exn.protect
           ~f:(fun () ->
             match
               P.Body.iter body ~f:(fun { buffer; off; len } ->
                 Eio.Flow.write write_flow [ Cstruct.of_bigarray ~off ~len buffer ])
             with
             | Ok () -> ()
             | Error _ -> fail_connection t)
           ~finally:(fun () -> Eio.Flow.close write_flow))
      (fun () ->
         let reader = Eio.Buf_read.of_flow read_flow ~max_size:(16 * 1024 * 1024) in
         Exn.protect
           ~f:(fun () -> read_sse_lines t reader [])
           ~finally:(fun () -> Eio.Flow.close read_flow)))
;;

let event_headers t = ("accept", "text/event-stream") :: connection_headers t

let read_events t =
  if not t.closed
  then (
    match P.Client.get t.event_client ~headers:(event_headers t) t.event_path with
    | Ok response when P.Status.to_code response.status = 200 ->
      read_sse_body t response.body
    | Ok response ->
      fail_connection t;
      ignore (P.Body.drain response.body : (unit, P.Error.t) result)
    | Error _ -> ())
;;

let event_loop t =
  Exn.protect
    ~f:(fun () ->
      try read_events t with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | _ -> ())
    ~finally:(fun () -> fail_connection t)
;;

let start_event_reader t =
  if not t.event_reader_started
  then (
    t.event_reader_started <- true;
    Eio.Fiber.fork ~sw:t.sw (fun () -> event_loop t))
;;

let request_locked
      t
      ~rpc_client
      ~control
      ~method_
      ~params
      ~max_payload_bytes
      ~max_envelope_bytes
      ~read_body
      ~validate_json
      ~decode
  =
  let open Result.Let_syntax in
  if t.closed || t.failed
  then Error (interrupted "HTTP connection is closed")
  else if control && Option.is_none t.connection_id
  then Error (invalid_response "REPL control requires an initialized HTTP connection")
  else (
    let%bind request_id = next_request_id t in
    let envelope = Agent_protocol.Envelope.request ~id:request_id ~method_ ~params () in
    let encoded = Agent_protocol.Envelope.to_json envelope |> Jsonaf.to_string in
    let%bind () =
      match max_payload_bytes, max_envelope_bytes with
      | None, None -> Ok ()
      | Some payload_maximum, Some envelope_maximum ->
        let payload_bytes = String.length (Jsonaf.to_string params) in
        let envelope_bytes = String.length encoded in
        if
          payload_bytes > payload_maximum
          || payload_bytes > envelope_bytes
          || envelope_maximum < envelope_bytes - payload_bytes
          || payload_bytes > envelope_maximum - (envelope_bytes - payload_bytes)
        then Error (invalid_response "REPL HTTP request exceeds its envelope bound")
        else Ok ()
      | None, Some _ | Some _, None ->
        Error (invalid_response "REPL HTTP request bounds are incomplete")
    in
    match
      P.Client.post
        rpc_client
        ~headers:(rpc_headers t)
        ~body:(P.Body.of_string encoded)
        t.rpc_path
    with
    | Error failure ->
      fail_connection t;
      Error (interrupted (P.Error.to_string failure))
    | Ok response ->
      let%bind newly_connected =
        if control
        then (
          match validate_control_connection_id t response with
          | Ok value -> Ok value
          | Error failure ->
            retire_connection t;
            Error failure)
        else capture_connection_id t response
      in
      let%bind body =
        match read_body request_id response with
        | Ok body -> Ok body
        | Error error ->
          fail_connection t;
          Error error
      in
      if P.Status.to_code response.status < 200 || P.Status.to_code response.status >= 300
      then Error (response_error response body)
      else (
        if newly_connected then start_event_reader t;
        let%bind json = parse_json body in
        let%bind () = validate_json json in
        let%bind envelope =
          Agent_protocol.Envelope.of_json json
          |> Result.map_error ~f:(fun failure ->
            interrupted failure.Agent_protocol.Error.message)
        in
        decode request_id envelope))
;;

let request_command_locked t command =
  request_locked
    t
    ~rpc_client:t.rpc_client
    ~control:false
    ~method_:(Agent_protocol.Command.method_name command)
    ~params:(Agent_protocol.Command.params command)
    ~max_payload_bytes:None
    ~max_envelope_bytes:None
    ~read_body:(fun _ response -> read_body response)
    ~validate_json:(fun _ -> Ok ())
    ~decode:(response_result command)
;;

let closed_response_result ~name ~error_within_bound request_id envelope =
  match envelope with
  | Agent_protocol.Envelope.Response response
    when Agent_protocol.Envelope.Request_id.compare response.id request_id = 0 ->
    (match response.outcome with
     | Error failure when not (error_within_bound failure) ->
       Error (invalid_response (name ^ " HTTP error response exceeds its bound"))
     | outcome -> outcome)
  | Response _ -> Error (invalid_response "HTTP response identifier does not match")
  | Notification _ | Request _ ->
    Error (invalid_response "HTTP RPC returned a non-response envelope")
;;

let validate_closed_payload_json t ~name ~max_result_bytes ~max_error_bytes = function
  | `Object fields ->
    let result = List.Assoc.find fields "result" ~equal:String.equal in
    let error = List.Assoc.find fields "error" ~equal:String.equal in
    let too_large =
      match result, error with
      | Some result, _ -> String.length (Jsonaf.to_string result) > max_result_bytes
      | None, Some error -> String.length (Jsonaf.to_string error) > max_error_bytes
      | None, None -> false
    in
    if too_large
    then (
      fail_connection t;
      Error (invalid_response (name ^ " HTTP response payload exceeds its bound")))
    else Ok ()
  | _ -> Ok ()
;;

let read_closed_body ~name ~max_result_bytes ~max_error_bytes id response =
  (* The actual generated request ID determines the response envelope framing.
     The body is counted while Piaf streams it, before JSON parsing. *)
  if
    P.Status.to_code response.P.Response.status < 200
    || P.Status.to_code response.P.Response.status >= 300
  then read_body_bounded response ~max_bytes:max_error_bytes
  else (
    let success_framing =
      Agent_protocol.Envelope.success ~id `Null
      |> Agent_protocol.Envelope.to_json
      |> Jsonaf.to_string
      |> String.length
      |> fun length -> length - 4
    in
    let empty_error = Agent_protocol.Error.invalid_request "" in
    let error_json_bytes =
      Agent_protocol.Error.to_json empty_error |> Jsonaf.to_string |> String.length
    in
    let error_framing =
      Agent_protocol.Envelope.failure ~id empty_error
      |> Agent_protocol.Envelope.to_json
      |> Jsonaf.to_string
      |> String.length
      |> fun length -> length - error_json_bytes
    in
    if
      max_result_bytes <= 0
      || max_result_bytes > Int.max_value - success_framing
      || max_error_bytes > Int.max_value - error_framing
    then Error (invalid_response (name ^ " response bound is invalid"))
    else
      read_body_bounded
        response
        ~max_bytes:
          (Int.max (max_result_bytes + success_framing) (max_error_bytes + error_framing)))
;;

let request_developer_locked t ~bound request =
  let max_result_bytes =
    Agent_client.Transport.Developer_response_bound.max_result_bytes bound
  in
  let max_error_bytes = Agent_client.Transport.Developer_response_bound.max_error_bytes in
  request_locked
    t
    ~rpc_client:t.rpc_client
    ~control:false
    ~method_:(Agent_protocol.Developer_wire.Request.method_name request)
    ~params:(Agent_protocol.Developer_wire.Request.params request)
    ~max_payload_bytes:None
    ~max_envelope_bytes:None
    ~read_body:(read_closed_body ~name:"Developer" ~max_result_bytes ~max_error_bytes)
    ~validate_json:
      (validate_closed_payload_json
         t
         ~name:"Developer"
         ~max_result_bytes
         ~max_error_bytes)
    ~decode:
      (closed_response_result
         ~name:"Developer"
         ~error_within_bound:
           Agent_client.Transport.Developer_response_bound.error_within_bound)
;;

let request_repl_locked t ~rpc_client ~control ~max_request_bytes ~bound request =
  let max_payload_bytes =
    match bound with
    | Agent_client.Transport.Repl_response_bound.Bootstrap -> 64 * 1024
    | Installed limits ->
      Document_schema.Limits.max_bytes
        (Agent_protocol.Developer_repl_wire.Limits.request limits)
  in
  let max_result_bytes =
    Agent_client.Transport.Repl_response_bound.max_result_bytes bound
  in
  let max_error_bytes = Agent_client.Transport.Repl_response_bound.max_error_bytes in
  request_locked
    t
    ~rpc_client
    ~control
    ~method_:(Agent_protocol.Developer_repl_wire.Request.method_name request)
    ~params:(Agent_protocol.Developer_repl_wire.Request.params request)
    ~max_payload_bytes:(Some max_payload_bytes)
    ~max_envelope_bytes:(Some max_request_bytes)
    ~read_body:(read_closed_body ~name:"REPL" ~max_result_bytes ~max_error_bytes)
    ~validate_json:
      (validate_closed_payload_json t ~name:"REPL" ~max_result_bytes ~max_error_bytes)
    ~decode:
      (closed_response_result
         ~name:"REPL"
         ~error_within_bound:Agent_client.Transport.Repl_response_bound.error_within_bound)
;;

let next_notification t = Agent_session.Mailbox.pop t.notifications

let close_locked t =
  if not t.closed
  then (
    t.closed <- true;
    Agent_session.Mailbox.close t.notifications;
    Exn.protect
      ~finally:(fun () -> Client_lifetime.close t.lifetime)
      ~f:(fun () ->
        if not t.failed
        then
          Option.iter t.connection_id ~f:(fun _ ->
            ignore
              (Eio.Time.with_timeout (Eio.Stdenv.clock t.env) 1. (fun () ->
                 Eio.Mutex.use_rw ~protect:false t.control_mutex (fun () ->
                   match
                     P.Client.delete
                       t.control_rpc_client
                       ~headers:(connection_headers t)
                       t.close_path
                   with
                   | Ok response ->
                     ignore (P.Body.drain response.body : (unit, P.Error.t) result)
                   | Error _ -> ());
                 Ok ())
               : (unit, [ `Timeout ]) result))))
;;

let close t = Eio.Cancel.protect (fun () -> close_locked t)

let with_request_lock t f =
  match
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
      match f () with
      | result -> Ok result
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
        (* Piaf owns the writer and response monitor on the connection switch.
           Abandoning the caller cannot establish whether the request was sent. *)
        Eio.Cancel.protect (fun () ->
          fail_connection t;
          ignore (Result.try_with (fun () -> close_locked t) : (unit, exn) result));
        Error (exn, backtrace))
  with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let request t command = with_request_lock t (fun () -> request_command_locked t command)

let request_developer t ~bound request =
  with_request_lock t (fun () -> request_developer_locked t ~bound request)
;;

let request_repl t ~max_request_bytes ~bound request =
  match request with
  | Agent_protocol.Developer_repl_wire.Request.Cancel_submission _
  | Agent_protocol.Developer_repl_wire.Request.Task_cancel_and_wait _ ->
    if not (Eio.Mutex.try_lock t.control_mutex)
    then
      Error
        (Agent_protocol.Error.create
           Resource_limit
           ~message:"HTTP REPL control lane is busy"
           ~retryable:true
           ())
    else
      Exn.protect
        ~finally:(fun () -> Eio.Mutex.unlock t.control_mutex)
        ~f:(fun () ->
          match
            request_repl_locked
              t
              ~rpc_client:t.control_rpc_client
              ~control:true
              ~max_request_bytes
              ~bound
              request
          with
          | value -> value
          | exception (Eio.Cancel.Cancelled _ as exn) ->
            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
            ignore (Result.try_with (fun () -> retire_connection t) : (unit, exn) result);
            Exn.raise_with_original_backtrace exn backtrace)
  | _ ->
    with_request_lock t (fun () ->
      request_repl_locked
        t
        ~rpc_client:t.rpc_client
        ~control:false
        ~max_request_bytes
        ~bound
        request)
;;

let normalize_prefix uri =
  let path = Uri.path uri |> String.rstrip ~drop:(Char.equal '/') in
  if String.equal path "/" then "" else path
;;

let connect_owned lifetime ~env ~uri ~bearer_token ~notification_capacity =
  let sw = Client_lifetime.switch lifetime in
  let base_uri = Uri.with_path uri "" |> Fn.flip Uri.with_query [] in
  match
    ( P.Client.create ~sw env base_uri
    , P.Client.create ~sw env base_uri
    , P.Client.create ~sw env base_uri )
  with
  | Error failure, _, _ | _, Error failure, _ | _, _, Error failure ->
    Error (interrupted (P.Error.to_string failure))
  | Ok rpc_client, Ok control_rpc_client, Ok event_client ->
    let prefix = normalize_prefix uri in
    let t =
      { sw
      ; lifetime
      ; env
      ; rpc_client
      ; control_rpc_client
      ; event_client
      ; rpc_path = prefix ^ "/v1/rpc"
      ; event_path = prefix ^ "/v1/events"
      ; close_path = prefix ^ "/v1/connection"
      ; bearer_token
      ; notifications = Agent_session.Mailbox.create ~capacity:notification_capacity
      ; mutex = Eio.Mutex.create ()
      ; control_mutex = Eio.Mutex.create ()
      ; connection_id = None
      ; next_request_id = 1L
      ; event_reader_started = false
      ; failed = false
      ; closed = false
      }
    in
    Agent_client.Transport.create_with_developer_repl
      ~request:(request t)
      ~request_developer:(request_developer t)
      ~request_repl:(request_repl t)
      ~next_notification:(fun () -> next_notification t)
      ~close:(fun () -> close t)
    |> Agent_client.Connection.create
    |> Result.return
;;

let connect ~sw ~env ~uri ~bearer_token ~notification_capacity =
  if notification_capacity <= 0
  then Error (invalid_response "notification capacity must be positive")
  else
    Client_lifetime.start ~sw (fun lifetime ->
      connect_owned lifetime ~env ~uri ~bearer_token ~notification_capacity)
;;
