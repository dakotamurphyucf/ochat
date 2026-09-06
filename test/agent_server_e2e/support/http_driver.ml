open Core
module P = Piaf

type t =
  { env : Eio_unix.Stdenv.base
  ; port : int
  ; client : P.Client.t
  ; token : string option
  ; mutable connection_id : string option
  ; mutable next_request_id : int64
  }

type response =
  { status : int
  ; headers : P.Headers.t
  ; body : string
  }

type rpc_response =
  { result : Agent_protocol.Method_result.t
  ; response : response
  }

module Sse = struct
  type event =
    { id : string option
    ; event : string option
    ; data : string
    }
  [@@deriving sexp]

  type item =
    | Event of event
    | Failure of string
    | Closed

  type t =
    { flow : [ Eio.Net.socket_ty | Eio.Flow.two_way_ty | `Stream ] Eio.Resource.t
    ; items : item Eio.Stream.t
    ; done_ : unit Eio.Promise.t
    ; closing : unit Eio.Promise.t
    ; closing_resolver : unit Eio.Promise.u
    ; mutable closed : bool
    }

  let next t ~clock ~timeout_seconds =
    match
      Eio.Time.with_timeout clock timeout_seconds (fun () -> Ok (Eio.Stream.take t.items))
    with
    | Error `Timeout -> Error "timed out waiting for an SSE event"
    | Ok (Event event) -> Ok event
    | Ok (Failure message) -> Error message
    | Ok Closed -> Error "SSE stream closed"
  ;;

  let close t =
    if not t.closed
    then (
      t.closed <- true;
      ignore (Eio.Promise.try_resolve t.closing_resolver ());
      ignore
        (Result.try_with (fun () -> Eio.Flow.shutdown t.flow `All) : (unit, exn) result);
      Eio.Flow.close t.flow;
      Eio.Promise.await t.done_)
  ;;
end

let client_config =
  { P.Config.default with allow_insecure = true; flush_headers_immediately = true }
;;

let create_client ~sw ~env ~port =
  let uri = Uri.of_string (sprintf "http://127.0.0.1:%d" port) in
  P.Client.create ~config:client_config ~sw env uri
  |> Result.map_error ~f:P.Error.to_string
;;

let create ~sw ~env ~port ~token =
  Result.map (create_client ~sw ~env ~port) ~f:(fun client ->
    { env; port; client; token; connection_id = None; next_request_id = 1L })
;;

let connection_id t = t.connection_id
let set_connection_id t value = t.connection_id <- value

let authorization_headers t =
  Option.value_map t.token ~default:[] ~f:(fun token ->
    [ "authorization", "Bearer " ^ token ])
;;

let connection_headers t =
  Option.value_map t.connection_id ~default:[] ~f:(fun id ->
    [ "ochat-connection-id", id ])
;;

let read_response response =
  P.Body.to_string response.P.Response.body
  |> Result.map_error ~f:P.Error.to_string
  |> Result.map ~f:(fun body ->
    { status = P.Status.to_code response.status; headers = response.headers; body })
;;

let request_raw t ?(headers = []) ?(body = "") ~meth ~path () =
  P.Client.request t.client ~headers ~body:(P.Body.of_string body) ~meth path
  |> Result.map_error ~f:P.Error.to_string
  |> Result.bind ~f:read_response
;;

let rpc_headers t =
  [ "content-type", "application/json"
  ; "accept", "application/json"
  ; "ochat-protocol-version", "1.0"
  ]
  @ authorization_headers t
  @ connection_headers t
;;

let capture_connection_id t response =
  match P.Headers.get response.headers "ochat-connection-id" with
  | None -> ()
  | Some value -> t.connection_id <- Some value
;;

let rpc_raw t ?(headers = []) body =
  let headers = headers @ rpc_headers t in
  request_raw t ~headers ~body ~meth:`POST ~path:"/v1/rpc" ()
  |> Result.map ~f:(fun response ->
    capture_connection_id t response;
    response)
;;

let protocol_error message = Agent_protocol.Error.invalid_request message

let parse_json body =
  Result.try_with (fun () -> Jsonaf.of_string body)
  |> Result.map_error ~f:(fun exn ->
    protocol_error ("invalid HTTP JSON: " ^ Exn.to_string exn))
;;

let decode_protocol_error response =
  match parse_json response.body |> Result.bind ~f:Agent_protocol.Error.of_json with
  | Ok error -> error
  | Error _ ->
    protocol_error (sprintf "HTTP request failed with status %d" response.status)
;;

let next_id t =
  let id = t.next_request_id in
  t.next_request_id <- Int64.(id + 1L);
  Agent_protocol.Envelope.Request_id.of_json (`Number (Int64.to_string id))
;;

let request t command =
  let open Result.Let_syntax in
  let%bind id = next_id t in
  let envelope =
    Agent_protocol.Envelope.request
      ~id
      ~method_:(Agent_protocol.Command.method_name command)
      ~params:(Agent_protocol.Command.params command)
      ()
  in
  let%bind response =
    Agent_protocol.Envelope.to_json envelope
    |> Jsonaf.to_string
    |> rpc_raw t
    |> Result.map_error ~f:protocol_error
  in
  if response.status < 200 || response.status >= 300
  then Error (decode_protocol_error response)
  else (
    let%bind json = parse_json response.body in
    let%bind envelope = Agent_protocol.Envelope.of_json json in
    match envelope with
    | Agent_protocol.Envelope.Response rpc
      when Agent_protocol.Envelope.Request_id.compare rpc.id id = 0 ->
      let%map result =
        Result.bind rpc.outcome ~f:(fun json ->
          Agent_protocol.Method_result.of_json
            ~method_:(Agent_protocol.Command.method_name command)
            json)
      in
      { result; response }
    | Response _ -> Error (protocol_error "HTTP response identifier differs")
    | Notification _ | Request _ -> Error (protocol_error "HTTP returned a non-response"))
;;

let initialize_request () =
  let open Result.Let_syntax in
  let%bind implementation =
    Agent_protocol.Initialize.Implementation.create
      ~name:"agent-server-e2e-http"
      ~version:"dev"
  in
  Agent_protocol.Initialize.Request.create
    ~implementation
    ~protocol_min:Agent_protocol.Version.initial
    ~protocol_max:Agent_protocol.Version.initial
    ~features:[]
    ~event_encodings:[ Json ]
    ~max_inbound_event_bytes:(16 * 1024 * 1024)
    ()
;;

let initialize t =
  let open Result.Let_syntax in
  let%bind initialize_request = initialize_request () in
  let%bind rpc = request t (Protocol_initialize initialize_request) in
  match rpc.result with
  | Protocol_initialize response -> Ok (response, rpc.response)
  | _ -> Error (protocol_error "initialize returned the wrong result")
;;

let notify t command =
  let envelope =
    Agent_protocol.Envelope.notification
      ~method_:(Agent_protocol.Command.method_name command)
      ~params:(Agent_protocol.Command.params command)
      ()
  in
  Agent_protocol.Envelope.to_json envelope |> Jsonaf.to_string |> rpc_raw t
;;

let close_connection t =
  request_raw
    t
    ~headers:(authorization_headers t @ connection_headers t)
    ~meth:`DELETE
    ~path:"/v1/connection"
    ()
;;

let field_value prefix lines =
  List.filter_map lines ~f:(fun line ->
    String.chop_prefix line ~prefix |> Option.map ~f:String.lstrip)
;;

let decode_sse lines =
  let id = field_value "id:" lines |> List.hd in
  let event = field_value "event:" lines |> List.hd in
  let data = field_value "data:" lines |> String.concat ~sep:"\n" in
  if Option.is_none id && Option.is_none event && String.is_empty data
  then None
  else Some Sse.{ id; event; data }
;;

let enqueue_sse closing items item =
  Eio.Fiber.first
    (fun () ->
       Eio.Stream.add items item;
       true)
    (fun () ->
       Eio.Promise.await closing;
       false)
;;

let rec read_sse closing reader items lines =
  match Eio.Buf_read.line reader with
  | "" ->
    let continue =
      Option.value_map
        (decode_sse (List.rev lines))
        ~default:true
        ~f:(fun event -> enqueue_sse closing items (Sse.Event event))
    in
    if continue then read_sse closing reader items []
  | line -> read_sse closing reader items (line :: lines)
  | exception End_of_file -> ignore (enqueue_sse closing items Sse.Closed : bool)
;;

let trim_http_line = String.rstrip ~drop:(fun char -> Char.equal char '\r')

let rec read_headers reader headers =
  match Eio.Buf_read.line reader |> trim_http_line with
  | "" -> List.rev headers
  | line ->
    (match String.lsplit2 line ~on:':' with
     | None -> read_headers reader headers
     | Some (name, value) ->
       read_headers reader ((String.lowercase name, String.lstrip value) :: headers))
;;

let status_code line =
  match String.split (trim_http_line line) ~on:' ' with
  | _version :: code :: _ -> Int.of_string_opt code
  | _ -> None
;;

let chunk_length line =
  let encoded =
    String.lsplit2 (trim_http_line line) ~on:';'
    |> Option.value_map ~default:(trim_http_line line) ~f:fst
  in
  Int.of_string_opt ("0x" ^ encoded)
;;

let rec drain_trailers reader =
  if not (String.is_empty (Eio.Buf_read.line reader |> trim_http_line))
  then drain_trailers reader
;;

let rec copy_chunks reader sink =
  match Eio.Buf_read.line reader |> chunk_length with
  | None -> failwith "invalid HTTP chunk length"
  | Some 0 -> drain_trailers reader
  | Some length ->
    Eio.Flow.copy_string (Eio.Buf_read.take length reader) sink;
    ignore (Eio.Buf_read.line reader : string);
    copy_chunks reader sink
;;

let read_chunked_body ~sw reader items closing done_resolver =
  let source, sink = Eio_unix.pipe sw in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Exn.protect
      ~f:(fun () ->
        read_sse
          closing
          (Eio.Buf_read.of_flow source ~max_size:(16 * 1024 * 1024))
          items
          [])
      ~finally:(fun () -> Eio.Flow.close source);
    `Stop_daemon);
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Exn.protect
      ~f:(fun () ->
        try copy_chunks reader sink with
        | End_of_file -> ()
        | exn ->
          ignore (enqueue_sse closing items (Sse.Failure (Exn.to_string exn)) : bool))
      ~finally:(fun () ->
        Eio.Flow.close sink;
        ignore (Eio.Promise.try_resolve done_resolver ()));
    `Stop_daemon)
;;

let http_request headers path =
  let lines =
    [ "GET " ^ path ^ " HTTP/1.1"; "host: 127.0.0.1"; "connection: close" ]
    @ List.map headers ~f:(fun (name, value) -> name ^ ": " ^ value)
  in
  String.concat ~sep:"\r\n" lines ^ "\r\n\r\n"
;;

let connect_sse t ~sw headers path =
  let flow =
    (Eio.Net.connect
       ~sw
       (Eio.Stdenv.net t.env)
       (`Tcp (Eio.Net.Ipaddr.V4.loopback, t.port))
      :> [ Eio.Net.socket_ty | Eio.Flow.two_way_ty | `Stream ] Eio.Resource.t)
  in
  Eio.Flow.copy_string (http_request headers path) flow;
  let reader = Eio.Buf_read.of_flow flow ~max_size:(16 * 1024 * 1024) in
  let status = Eio.Buf_read.line reader |> status_code |> Option.value_exn in
  let response_headers = read_headers reader [] |> P.Headers.of_list in
  if not (Int.equal status 200)
  then (
    Eio.Flow.close flow;
    failwith (sprintf "SSE request returned HTTP %d" status));
  flow, reader, status, response_headers
;;

let open_sse t ~sw ~headers ~path ~buffer_capacity =
  Result.try_with (fun () ->
    let flow, reader, status, response_headers = connect_sse t ~sw headers path in
    let items = Eio.Stream.create buffer_capacity in
    let done_, done_resolver = Eio.Promise.create () in
    let closing, closing_resolver = Eio.Promise.create () in
    read_chunked_body ~sw reader items closing done_resolver;
    ( Sse.{ flow; items; done_; closing; closing_resolver; closed = false }
    , { status; headers = response_headers; body = "" } ))
  |> Result.map_error ~f:Exn.to_string
;;

let open_connection_events t ~sw =
  let headers =
    [ "accept", "text/event-stream" ] @ authorization_headers t @ connection_headers t
  in
  open_sse t ~sw ~headers ~path:"/v1/events" ~buffer_capacity:256
;;

let session_events_path session_id after_sequence =
  let query =
    Option.value_map after_sequence ~default:"" ~f:(fun sequence ->
      "?after_sequence=" ^ Int64.to_string sequence)
  in
  sprintf
    "/v1/sessions/%s/events%s"
    (Agent_protocol.Id.Session.to_string session_id)
    query
;;

let session_events_headers t last_event_id =
  [ "accept", "text/event-stream" ]
  @ authorization_headers t
  @ Option.value_map last_event_id ~default:[] ~f:(fun sequence ->
    [ "last-event-id", Int64.to_string sequence ])
;;

let open_session_events
      t
      ~sw
      ~session_id
      ?(buffer_capacity = 256)
      ?after_sequence
      ?last_event_id
      ()
  =
  let path = session_events_path session_id after_sequence in
  let headers = session_events_headers t last_event_id in
  open_sse t ~sw ~headers ~path ~buffer_capacity
;;

let get_snapshot t session_id =
  let path =
    sprintf "/v1/sessions/%s/snapshot" (Agent_protocol.Id.Session.to_string session_id)
  in
  request_raw t ~headers:(authorization_headers t) ~meth:`GET ~path ()
  |> Result.bind ~f:(fun response ->
    if response.status <> 200
    then Error (sprintf "snapshot returned HTTP %d: %s" response.status response.body)
    else
      Result.try_with (fun () -> Jsonaf.of_string response.body)
      |> Result.map_error ~f:Exn.to_string
      |> Result.bind ~f:(fun json ->
        Agent_protocol.Snapshot.of_json json
        |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message))
      |> Result.map ~f:(fun snapshot -> snapshot, response))
;;

let shutdown t = P.Client.shutdown t.client
