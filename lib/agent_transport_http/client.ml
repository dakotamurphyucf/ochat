open! Core
module P = Piaf

type t =
  { sw : Eio.Switch.t
  ; lifetime : Client_lifetime.t
  ; env : Eio_unix.Stdenv.base
  ; rpc_client : P.Client.t
  ; event_client : P.Client.t
  ; rpc_path : string
  ; event_path : string
  ; close_path : string
  ; bearer_token : string option
  ; notifications : Agent_protocol.Envelope.t Agent_session.Mailbox.t
  ; mutex : Eio.Mutex.t
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
  Agent_protocol.Error.create Invalid_request ~message ~retryable:false ()
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
  ; protocol_version_header, "1.0"
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
      Agent_protocol.Method_result.of_json
        ~method_:(Agent_protocol.Command.method_name command)
        json)
  | Response _ -> Error (invalid_response "HTTP response identifier does not match")
  | Notification _ | Request _ ->
    Error (invalid_response "HTTP RPC returned a non-response envelope")
;;

let fail_connection t =
  t.failed <- true;
  Agent_session.Mailbox.close t.notifications
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

let request_locked t command =
  let open Result.Let_syntax in
  if t.closed || t.failed
  then Error (interrupted "HTTP connection is closed")
  else (
    let%bind request_id = next_request_id t in
    let envelope =
      Agent_protocol.Envelope.request
        ~id:request_id
        ~method_:(Agent_protocol.Command.method_name command)
        ~params:(Agent_protocol.Command.params command)
        ()
    in
    match
      P.Client.post
        t.rpc_client
        ~headers:(rpc_headers t)
        ~body:
          (P.Body.of_string
             (Agent_protocol.Envelope.to_json envelope |> Jsonaf.to_string))
        t.rpc_path
    with
    | Error failure ->
      fail_connection t;
      Error (interrupted (P.Error.to_string failure))
    | Ok response ->
      let%bind newly_connected = capture_connection_id t response in
      let%bind body = read_body response in
      if P.Status.to_code response.status < 200 || P.Status.to_code response.status >= 300
      then Error (response_error response body)
      else (
        if newly_connected then start_event_reader t;
        let%bind json = parse_json body in
        let%bind envelope = Agent_protocol.Envelope.of_json json in
        response_result command request_id envelope))
;;

let request t command =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> request_locked t command)
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
            match
              P.Client.delete t.rpc_client ~headers:(connection_headers t) t.close_path
            with
            | Ok response ->
              ignore (P.Body.drain response.body : (unit, P.Error.t) result)
            | Error _ -> ())))
;;

let close t = Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> close_locked t)

let normalize_prefix uri =
  let path = Uri.path uri |> String.rstrip ~drop:(Char.equal '/') in
  if String.equal path "/" then "" else path
;;

let connect_owned lifetime ~env ~uri ~bearer_token ~notification_capacity =
  let sw = Client_lifetime.switch lifetime in
  let base_uri = Uri.with_path uri "" |> Fn.flip Uri.with_query [] in
  match P.Client.create ~sw env base_uri, P.Client.create ~sw env base_uri with
  | Error failure, _ | _, Error failure -> Error (interrupted (P.Error.to_string failure))
  | Ok rpc_client, Ok event_client ->
    let prefix = normalize_prefix uri in
    let t =
      { sw
      ; lifetime
      ; env
      ; rpc_client
      ; event_client
      ; rpc_path = prefix ^ "/v1/rpc"
      ; event_path = prefix ^ "/v1/events"
      ; close_path = prefix ^ "/v1/connection"
      ; bearer_token
      ; notifications = Agent_session.Mailbox.create ~capacity:notification_capacity
      ; mutex = Eio.Mutex.create ()
      ; connection_id = None
      ; next_request_id = 1L
      ; event_reader_started = false
      ; failed = false
      ; closed = false
      }
    in
    Agent_client.Transport.create
      ~request:(request t)
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
