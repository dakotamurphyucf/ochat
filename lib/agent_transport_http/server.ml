open! Core
module P = Piaf

type connection =
  { context : Agent_server.Connection_context.t
  ; principal_id : Agent_protocol.Id.Principal.t
  ; outgoing : Agent_protocol.Envelope.t Agent_session.Mailbox.t
  ; mutable last_activity : float
  ; mutable active_streams : int
  }

type t =
  { sw : Eio.Switch.t
  ; env : Eio_unix.Stdenv.base
  ; dispatcher : Agent_server.Dispatcher.t
  ; registry : Agent_server.Session_registry.t
  ; blob_store : Agent_store.Blob_store.t
  ; health : include_details:bool -> Agent_protocol.Health.Response.t
  ; close_connection : Agent_server.Connection_context.t -> unit
  ; authenticate :
      Agent_server.Authenticator.Request_identity.t
      -> string option
      -> (Agent_protocol.Principal.t, Agent_protocol.Error.t) result
  ; max_body_bytes : int
  ; max_batch_size : int
  ; batch_concurrency : int
  ; outgoing_capacity : int
  ; max_connections : int
  ; max_attachments : int
  ; idle_connection_timeout : float
  ; mutex : Eio.Mutex.t
  ; mutable connections : (string, connection) Map.Poly.t
  }

let connection_header = Request_contract.connection_header
let protocol_version_header = Request_contract.protocol_version_header
let json_headers = P.Headers.of_list [ "content-type", "application/json" ]

let json_response ?(status = `OK) ?connection_id json =
  let headers =
    Option.value_map connection_id ~default:json_headers ~f:(fun id ->
      P.Headers.add json_headers connection_header id)
  in
  P.Response.create ~headers ~body:(P.Body.of_string (Jsonaf.to_string json)) status
;;

let error_response ?(status = `Bad_request) error =
  json_response ~status (Agent_protocol.Error.to_json error)
;;

let protocol_error code message =
  Agent_protocol.Error.create code ~message ~retryable:false ()
;;

let require_json_content_type = Request_contract.require_json_content_type
let require_protocol_version = Request_contract.require_protocol_version

let not_found () =
  error_response
    ~status:`Not_found
    (protocol_error Method_not_found "HTTP route was not found")
;;

let method_not_allowed allow =
  let error =
    protocol_error Method_not_found "HTTP method is not supported for this route"
  in
  let headers =
    P.Headers.of_list
      [ "content-type", "application/json"; "allow", String.concat ~sep:", " allow ]
  in
  P.Response.create
    ~headers
    ~body:(P.Body.of_string (Agent_protocol.Error.to_json error |> Jsonaf.to_string))
    `Method_not_allowed
;;

let bearer_error () =
  P.Response.create
    ~headers:
      (P.Headers.of_list
         [ "content-type", "application/json"; "www-authenticate", "Bearer" ])
    ~body:
      (P.Body.of_string
         (Agent_protocol.Error.to_json
            (protocol_error Unauthenticated "authentication is required")
          |> Jsonaf.to_string))
    `Unauthorized
;;

let bearer_token = Request_contract.bearer_token

let principal_matches connection principal =
  let bound = Agent_server.Connection_context.principal connection.context in
  Agent_protocol.Id.Principal.compare bound.id principal.Agent_protocol.Principal.id = 0
  && String.equal bound.authentication_kind principal.authentication_kind
  && Agent_protocol.Scope.Set.equal bound.scopes principal.scopes
  && Poly.equal bound.attributes principal.attributes
;;

let close_connection_record t connection =
  Agent_session.Mailbox.close connection.outgoing;
  t.close_connection connection.context
;;

let remove_connection t id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let connection = Map.find t.connections id in
    t.connections <- Map.remove t.connections id;
    connection)
;;

let close_connection_id t id =
  Option.iter (remove_connection t id) ~f:(close_connection_record t)
;;

let now t = Eio.Time.now (Eio.Stdenv.clock t.env)
let touch connection now = connection.last_activity <- now

let find_connection t request principal =
  match P.Headers.get (P.Request.headers request) connection_header with
  | None -> Error (protocol_error Invalid_request "HTTP connection ID is required")
  | Some id ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match Map.find t.connections id with
      | None -> Error (protocol_error Invalid_request "HTTP connection ID is unknown")
      | Some connection when principal_matches connection principal ->
        touch connection (now t);
        Ok (id, connection)
      | Some _ ->
        Error (protocol_error Permission_denied "HTTP connection authority differs"))
;;

let create_connection t principal =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if Map.length t.connections >= t.max_connections
    then Error (protocol_error Resource_limit "HTTP connection limit reached")
    else (
      let id =
        Agent_protocol.Id.Attachment.create () |> Agent_protocol.Id.Attachment.to_string
      in
      let outgoing = Agent_session.Mailbox.create ~capacity:t.outgoing_capacity in
      let publish_notification envelope =
        if not (Agent_session.Mailbox.try_push outgoing ~priority:Normal envelope)
        then Eio.Fiber.fork ~sw:t.sw (fun () -> close_connection_id t id)
      in
      let context =
        Agent_server.Connection_context.create
          ~connection_id:id
          ~principal
          ~transport:Http
          ~publish_notification
          ~max_attachments:t.max_attachments
      in
      let connection =
        { context
        ; principal_id = principal.id
        ; outgoing
        ; last_activity = now t
        ; active_streams = 0
        }
      in
      t.connections <- Map.set t.connections ~key:id ~data:connection;
      Ok (id, connection)))
;;

let initializes = function
  | Agent_protocol.Envelope.Request { method_ = "protocol.initialize"; _ } -> true
  | Notification _ | Response _ | Request _ -> false
;;

let first_envelope = function
  | Rpc_body.Single envelope -> envelope
  | Batch (envelope :: _) -> envelope
  | Batch [] -> assert false
;;

let resolve_rpc_connection t request principal body =
  match P.Headers.get (P.Request.headers request) connection_header with
  | Some _ ->
    Result.map (find_connection t request principal) ~f:(fun value -> value, false)
  | None ->
    (match first_envelope body with
     | envelope when initializes envelope ->
       Result.map (create_connection t principal) ~f:(fun connection -> connection, true)
     | _ ->
       Error (protocol_error Incompatible_protocol "initialize a HTTP connection first"))
;;

let request_body t request =
  Request_contract.request_body ~max_body_bytes:t.max_body_bytes request
;;

let dispatch t connection envelope =
  Agent_server.Dispatcher.dispatch_envelope
    t.dispatcher
    ~context:connection.context
    envelope
;;

let dispatch_batch t connection envelopes =
  Eio.Fiber.List.map
    ~max_fibers:t.batch_concurrency
    (fun envelope -> dispatch t connection envelope)
    envelopes
;;

let dispatch_rpc t connection ~created = function
  | Rpc_body.Single envelope ->
    Result.map (dispatch t connection envelope) ~f:Option.to_list
  | Batch (first :: rest) when created ->
    let open Result.Let_syntax in
    let%bind first_response = dispatch t connection first in
    let results = dispatch_batch t connection rest in
    let%map responses = Result.all results in
    Option.to_list first_response @ List.filter_opt responses
  | Batch envelopes ->
    Result.map (Result.all (dispatch_batch t connection envelopes)) ~f:List.filter_opt
;;

let rpc_response ~connection_id responses =
  match responses with
  | [] ->
    P.Response.create
      ~headers:(P.Headers.of_list [ connection_header, connection_id ])
      `No_content
  | [ response ] ->
    Agent_protocol.Envelope.to_json response |> json_response ~connection_id
  | responses ->
    List.map responses ~f:Agent_protocol.Envelope.to_json
    |> fun values -> json_response ~connection_id (`Array values)
;;

let handle_rpc t request principal =
  let open Result.Let_syntax in
  match
    let%bind () = require_json_content_type request in
    let%bind () = require_protocol_version request in
    let%bind body = request_body t request in
    let%bind body = Rpc_body.parse ~max_batch_size:t.max_batch_size body in
    let%bind (connection_id, connection), created =
      resolve_rpc_connection t request principal body
    in
    let%map responses = dispatch_rpc t connection ~created body in
    connection_id, responses
  with
  | Error failure -> error_response failure
  | Ok (connection_id, responses) -> rpc_response ~connection_id responses
;;

let sse_payload envelope =
  "data: " ^ (Agent_protocol.Envelope.to_json envelope |> Jsonaf.to_string) ^ "\n\n"
;;

let sse_connected = ": connected\n\n"

let stream_notifications connection push =
  let rec loop () =
    match Agent_session.Mailbox.pop connection.outgoing with
    | None -> push None
    | Some envelope ->
      push (Some (sse_payload envelope));
      loop ()
  in
  try loop () with
  | _ -> ()
;;

let begin_stream t connection =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    connection.active_streams <- connection.active_streams + 1;
    touch connection (now t))
;;

let end_stream t connection =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    connection.active_streams <- Int.max 0 (connection.active_streams - 1);
    touch connection (now t))
;;

let handle_events t ~request_sw request principal =
  match find_connection t request principal with
  | Error failure -> error_response failure
  | Ok (_, connection) ->
    begin_stream t connection;
    let stream, push = P.Stream.create t.outgoing_capacity in
    push (Some sse_connected);
    Eio.Fiber.fork ~sw:request_sw (fun () ->
      Exn.protect
        ~f:(fun () -> stream_notifications connection push)
        ~finally:(fun () -> end_stream t connection));
    let body = P.Body.of_string_stream ~length:`Chunked stream in
    P.Response.create
      ~headers:
        (P.Headers.of_list
           [ "content-type", "text/event-stream"
           ; "cache-control", "no-cache"
           ; "x-accel-buffering", "no"
           ])
      ~body
      `OK
;;

let handle_close t request principal =
  match find_connection t request principal with
  | Error failure -> error_response failure
  | Ok (id, _) ->
    close_connection_id t id;
    json_response (`Object [ "closed", `True ])
;;

let has_scope principal scope = Agent_protocol.Principal.has_scope principal scope

let can_read_session principal state =
  has_scope principal Agent_protocol.Scope.View_session_transcript
  &&
  match state.Agent_session.Session_state.identity.creating_principal with
  | None -> true
  | Some creator ->
    Agent_protocol.Id.Principal.compare creator principal.Agent_protocol.Principal.id = 0
    || has_scope principal Administer_configuration
;;

let authorized_entry t principal session_id =
  match Agent_server.Session_registry.load t.registry session_id with
  | Error _ as failure -> failure
  | Ok entry ->
    (match Agent_session.Session_actor.state entry.actor with
     | Error _ as failure -> failure
     | Ok state when can_read_session principal state -> Ok (entry, state)
     | Ok _ -> Error (protocol_error Permission_denied "session is not visible"))
;;

let session_id encoded = Agent_protocol.Id.Session.of_string encoded
let blob_id encoded = Agent_protocol.Id.Blob.of_string encoded
let parse_cursor = Request_contract.event_cursor

let sse_durable event =
  sprintf
    "id: %Ld\nevent: session.event\ndata: %s\n\n"
    event.Agent_protocol.Event.Durable.sequence
    (Agent_protocol.Event.Durable.to_json event |> Jsonaf.to_string)
;;

let sse_recoverable event =
  "event: session.live_event\ndata: "
  ^ (Agent_protocol.Event.Recoverable.to_json event |> Jsonaf.to_string)
  ^ "\n\n"
;;

let sse_snapshot_required error =
  "event: snapshot.required\ndata: "
  ^ (Agent_protocol.Error.to_json error |> Jsonaf.to_string)
  ^ "\n\n"
;;

let subscriber_item t subscriber ~heartbeat_interval =
  Eio.Fiber.first
    (fun () -> `Item (Agent_session.Subscriber.take subscriber))
    (fun () ->
       Eio.Time.sleep (Eio.Stdenv.clock t.env) heartbeat_interval;
       `Keep_alive)
;;

type session_sse_state =
  { mutable connected : bool
  ; mutable replay : Agent_protocol.Event.Durable.t list
  ; mutable last_pull : float
  ; mutable closed : bool
  }

let close_session_sse entry (attachment : Agent_protocol.Session.Attachment.t) state =
  if not state.closed
  then (
    state.closed <- true;
    ignore
      (Agent_session.Session_actor.detach
         entry.Agent_server.Session_registry.actor
         attachment.id
       : (unit, Agent_protocol.Error.t) result))
;;

let next_session_sse t principal entry attachment subscriber state heartbeat_interval =
  let sse_durable event =
    sse_durable (Agent_server.Principal_projection.durable principal event)
  in
  state.last_pull <- now t;
  match state.connected, state.replay with
  | false, _ ->
    state.connected <- true;
    Some sse_connected
  | true, event :: replay ->
    state.replay <- replay;
    Some (sse_durable event)
  | true, [] ->
    (match subscriber_item t subscriber ~heartbeat_interval with
     | `Keep_alive -> Some ": keep-alive\n\n"
     | `Item None ->
       close_session_sse entry attachment state;
       None
     | `Item (Some (Ok (Durable event))) -> Some (sse_durable event)
     | `Item (Some (Ok (Recoverable event))) ->
       Some
         (Option.value_map
            (Agent_server.Principal_projection.recoverable principal event)
            ~default:": filtered\n\n"
            ~f:sse_recoverable)
     | `Item (Some (Error error)) ->
       close_session_sse entry attachment state;
       Some (sse_snapshot_required error))
;;

let watch_session_sse t entry attachment state stream heartbeat_interval =
  let rec loop () =
    Eio.Time.sleep (Eio.Stdenv.clock t.env) heartbeat_interval;
    if state.closed
    then P.Stream.close stream
    else if Float.(now t -. state.last_pull >= heartbeat_interval *. 3.)
    then (
      close_session_sse entry attachment state;
      P.Stream.close stream)
    else loop ()
  in
  loop ()
;;

let handle_session_events t request principal encoded_session_id =
  let open Result.Let_syntax in
  match
    let%bind session_id = session_id encoded_session_id in
    let%bind cursor = parse_cursor request in
    let%bind entry, _ = authorized_entry t principal session_id in
    let%bind attachment, subscriber, snapshot, _ =
      Agent_session.Session_actor.attach_with_snapshot
        entry.actor
        ~principal_id:(Some principal.id)
        ~reclaim_token:None
        ~mode:Read_only
        ~subscribe:true
    in
    let subscriber = Option.value_exn subscriber in
    let after_sequence = Option.value cursor ~default:snapshot.latest_event_sequence in
    match
      Agent_session.Durable_event_log.replay
        entry.durable_events
        ~after_sequence
        ~through_sequence:snapshot.latest_event_sequence
    with
    | Snapshot_required ->
      ignore
        (Agent_session.Session_actor.detach entry.actor attachment.id
         : (unit, Agent_protocol.Error.t) result);
      Error
        (protocol_error Snapshot_required "requested event cursor is no longer retained")
    | Available replay -> Ok (entry, attachment, subscriber, replay)
  with
  | Error error -> error_response ~status:`Conflict error
  | Ok (entry, attachment, subscriber, replay) ->
    let heartbeat_interval =
      Float.min 15. (Float.max 0.1 (t.idle_connection_timeout /. 2.))
    in
    let state = { connected = false; replay; last_pull = now t; closed = false } in
    let stream =
      P.Stream.from ~f:(fun () ->
        next_session_sse t principal entry attachment subscriber state heartbeat_interval)
    in
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      watch_session_sse t entry attachment state stream heartbeat_interval);
    P.Response.create
      ~headers:
        (P.Headers.of_list
           [ "content-type", "text/event-stream"
           ; "cache-control", "no-cache"
           ; "x-accel-buffering", "no"
           ])
      ~body:(P.Body.of_string_stream ~length:`Chunked stream)
      `OK
;;

let snapshot_etag snapshot =
  sprintf
    "\"%Ld-%Ld\""
    snapshot.Agent_protocol.Snapshot.revision
    snapshot.latest_event_sequence
;;

let handle_snapshot t request principal encoded_session_id =
  let open Result.Let_syntax in
  match
    let%bind session_id = session_id encoded_session_id in
    let%bind entry, _ = authorized_entry t principal session_id in
    let%map snapshot = Agent_session.Session_actor.snapshot entry.actor in
    Agent_server.Principal_projection.snapshot principal snapshot
  with
  | Error error -> error_response error
  | Ok snapshot ->
    let etag =
      "\""
      ^ Agent_server.Principal_projection.scope_identity principal
      ^ ":"
      ^ (snapshot_etag snapshot |> String.filter ~f:(fun ch -> not (Char.equal ch '"')))
      ^ ":"
      ^ Digestif.SHA256.(
          digest_string (Agent_protocol.Snapshot.to_json snapshot |> Jsonaf.to_string)
          |> to_hex)
      ^ "\""
    in
    if
      P.Headers.get (P.Request.headers request) "if-none-match"
      |> Option.exists ~f:(String.equal etag)
    then P.Response.create ~headers:(P.Headers.of_list [ "etag", etag ]) `Not_modified
    else
      P.Response.create
        ~headers:(P.Headers.of_list [ "content-type", "application/json"; "etag", etag ])
        ~body:
          (P.Body.of_string
             (Agent_protocol.Snapshot.to_json snapshot |> Jsonaf.to_string))
        `OK
;;

let handle_health t principal =
  let include_details = has_scope principal Agent_protocol.Scope.Diagnostics in
  t.health ~include_details |> Agent_protocol.Health.Response.to_json |> json_response
;;

let blob_kind = function
  | "file" -> Ok Agent_protocol.Blob.File
  | "image" -> Ok Image
  | "audio" -> Ok Audio
  | "binary" -> Ok Binary
  | _ -> Error (protocol_error Invalid_request "ochat-blob-kind is invalid")
;;

let upload_metadata_json handle =
  let metadata = Agent_store.Blob_store.Handle.metadata handle in
  `Object
    [ "blob", Agent_protocol.Blob.Metadata.to_json metadata.blob
    ; "allowed_use", `String metadata.allowed_use
    ; ( "expires_at"
      , Option.value_map
          metadata.expires_at
          ~default:`Null
          ~f:Agent_protocol.Timestamp.to_json )
    ]
;;

let upload_parameters request =
  let open Result.Let_syntax in
  let headers = P.Request.headers request in
  let%bind media_type =
    match P.Headers.get headers "content-type" with
    | Some value when not (String.is_empty value) -> Ok value
    | _ -> Error (protocol_error Invalid_request "blob content-type is required")
  in
  let%bind kind =
    P.Headers.get headers "ochat-blob-kind"
    |> Option.value ~default:"binary"
    |> String.lowercase
    |> blob_kind
  in
  let%bind target_session =
    match P.Headers.get headers "ochat-target-session" with
    | None -> Ok None
    | Some encoded -> Result.map (session_id encoded) ~f:Option.some
  in
  Ok
    ( media_type
    , kind
    , target_session
    , P.Headers.get headers "ochat-display-name"
    , Option.value (P.Headers.get headers "ochat-allowed-use") ~default:"message_input"
    , P.Headers.get headers "ochat-sha256" )
;;

let write_upload_body request upload =
  let failure = ref None in
  let result =
    P.Body.iter
      ~f:(fun { buffer; off; len } ->
        if Option.is_none !failure
        then (
          let chunk = Cstruct.of_bigarray ~off ~len buffer |> Cstruct.to_string in
          match Agent_store.Blob_store.write_string upload chunk with
          | Ok () -> ()
          | Error error -> failure := Some error))
      (P.Request.body request)
  in
  match !failure, result with
  | Some error, _ -> Error error
  | None, Error error ->
    Error
      (Agent_store.Store_error.Io
         { operation = "receive blob upload"
         ; path = "HTTP request body"
         ; message = P.Error.to_string error
         })
  | None, Ok () -> Ok ()
;;

let upload_expiration now =
  Agent_protocol.Timestamp.to_time_ns now
  |> Fn.flip Time_ns.add (Time_ns.Span.of_hr 1.)
  |> Agent_protocol.Timestamp.of_time_ns
;;

let handle_blob_upload t request principal =
  if not (has_scope principal Agent_protocol.Scope.Send_messages)
  then
    error_response
      ~status:`Forbidden
      (protocol_error Permission_denied "blob upload is not allowed")
  else (
    match upload_parameters request with
    | Error error -> error_response error
    | Ok (media_type, kind, target_session, display_name, allowed_use, expected_digest) ->
      (match
         Option.value_map target_session ~default:(Ok ()) ~f:(fun session_id ->
           Result.map (authorized_entry t principal session_id) ~f:(fun _ -> ()))
       with
       | Error error -> error_response ~status:`Forbidden error
       | Ok () ->
         let now =
           Eio.Time.now (Eio.Stdenv.clock t.env)
           |> Time_ns.Span.of_sec
           |> Time_ns.of_span_since_epoch
           |> Agent_protocol.Timestamp.of_time_ns
         in
         let id = Agent_protocol.Id.Blob.create () in
         (match
            Agent_store.Blob_store.begin_upload
              t.blob_store
              ~sw:t.sw
              ~id
              ~creating_principal:principal.id
              ~target_session
              ~kind
              ~media_type
              ~display_name
              ~allowed_use
              ~created_at:now
              ~expires_at:(Some (upload_expiration now))
          with
          | Error error ->
            error_response (Agent_store.Store_error.to_protocol_error error)
          | Ok upload ->
            (match write_upload_body request upload with
             | Error error ->
               Agent_store.Blob_store.abort upload;
               error_response (Agent_store.Store_error.to_protocol_error error)
             | Ok () ->
               (match Agent_store.Blob_store.finish upload ~expected_digest with
                | Error error ->
                  error_response (Agent_store.Store_error.to_protocol_error error)
                | Ok handle ->
                  json_response ~status:`Created (upload_metadata_json handle))))))
;;

let can_download_blob principal metadata =
  Agent_protocol.Id.Principal.compare
    metadata.Agent_store.Blob_store.Metadata.creating_principal
    principal.Agent_protocol.Principal.id
  = 0
  || has_scope principal Agent_protocol.Scope.Administer_configuration
;;

let open_durable_blob t id =
  let open Agent_protocol in
  match Agent_server.Session_registry.load_all t.registry with
  | Error _ as failure -> failure
  | Ok entries ->
    entries
    |> List.filter_map ~f:(fun entry -> entry.store_handle)
    |> List.find_map ~f:(fun session ->
      Agent_store.Blob_store.open_session t.blob_store session id |> Result.ok)
    |> Result.of_option
         ~error:
           (Error.create
              Blob_unavailable
              ~message:"blob was not found"
              ~retryable:false
              ())
;;

let open_blob t id =
  match Agent_store.Blob_store.open_temporary t.blob_store id with
  | Ok handle -> Ok handle
  | Error (Agent_store.Store_error.Missing _) -> open_durable_blob t id
  | Error failure -> Error (Agent_store.Store_error.to_protocol_error failure)
;;

let handle_blob_download t principal encoded_blob_id =
  match
    let open Result.Let_syntax in
    let%bind id = blob_id encoded_blob_id in
    let%bind handle = open_blob t id in
    let metadata = Agent_store.Blob_store.Handle.metadata handle in
    if
      can_download_blob principal metadata
      && Agent_server.Principal_projection.can_read_blob principal metadata
    then Ok handle
    else Error (protocol_error Permission_denied "blob is not visible")
  with
  | Error error when Agent_protocol.Error.equal_code error.code Permission_denied ->
    error_response ~status:`Forbidden error
  | Error error -> error_response error
  | Ok handle ->
    let metadata = Agent_store.Blob_store.Handle.metadata handle in
    let stream, push = P.Stream.create t.outgoing_capacity in
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      Exn.protect
        ~f:(fun () ->
          ignore
            (Agent_store.Blob_store.iter_chunks
               t.blob_store
               ~sw:t.sw
               handle
               ~chunk_size:65536
               ~f:(fun chunk -> push (Some chunk))
             : (unit, Agent_store.Store_error.t) result))
        ~finally:(fun () -> push None));
    P.Response.create
      ~headers:
        (P.Headers.of_list
           [ "content-type", metadata.blob.media_type
           ; "content-length", Int64.to_string metadata.blob.byte_length
           ; "etag", "\"sha256:" ^ metadata.blob.digest ^ "\""
           ])
      ~body:(P.Body.of_string_stream ~length:(`Fixed metadata.blob.byte_length) stream)
      `OK
;;

let path_segments request =
  Uri.path (P.Request.uri request)
  |> String.split ~on:'/'
  |> List.filter ~f:(Fn.non String.is_empty)
;;

let handle_authenticated t ~request_sw request principal =
  match P.Request.meth request, path_segments request with
  | `POST, [ "v1"; "rpc" ] -> handle_rpc t request principal
  | _, [ "v1"; "rpc" ] -> method_not_allowed [ "POST" ]
  | `POST, [ "v1"; "blobs" ] -> handle_blob_upload t request principal
  | _, [ "v1"; "blobs" ] -> method_not_allowed [ "POST" ]
  | `GET, [ "v1"; "blobs"; id ] -> handle_blob_download t principal id
  | _, [ "v1"; "blobs"; _ ] -> method_not_allowed [ "GET" ]
  | `GET, [ "v1"; "sessions"; id; "events" ] ->
    handle_session_events t request principal id
  | _, [ "v1"; "sessions"; _; "events" ] -> method_not_allowed [ "GET" ]
  | `GET, [ "v1"; "sessions"; id; "snapshot" ] -> handle_snapshot t request principal id
  | _, [ "v1"; "sessions"; _; "snapshot" ] -> method_not_allowed [ "GET" ]
  | `GET, [ "v1"; "health" ] -> handle_health t principal
  | _, [ "v1"; "health" ] -> method_not_allowed [ "GET" ]
  | `GET, [ "v1"; "events" ] -> handle_events t ~request_sw request principal
  | `DELETE, [ "v1"; "connection" ] -> handle_close t request principal
  | _, [ "v1"; "events" ] -> method_not_allowed [ "GET" ]
  | _, [ "v1"; "connection" ] -> method_not_allowed [ "DELETE" ]
  | _ -> not_found ()
;;

let handler t ({ P.Server.request; ctx = request_info } : P.Request_info.t P.Server.ctx) =
  match bearer_token request with
  | Error () -> bearer_error ()
  | Ok token ->
    let identity =
      Agent_server.Authenticator.Request_identity.
        { client_address = request_info.client_address
        ; headers = P.Headers.to_list (P.Request.headers request)
        }
    in
    (match t.authenticate identity token with
     | Error _ -> bearer_error ()
     | Ok principal ->
       handle_authenticated t ~request_sw:request_info.sw request principal)
;;

let close_all t =
  let connections =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let connections = Map.data t.connections in
      t.connections <- Map.Poly.empty;
      connections)
  in
  List.iter connections ~f:(fun connection -> close_connection_record t connection)
;;

let expired_connections t =
  let cutoff = now t -. t.idle_connection_timeout in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let expired, active =
      Map.partition_tf t.connections ~f:(fun connection ->
        connection.active_streams = 0 && Float.(connection.last_activity <= cutoff))
    in
    t.connections <- active;
    Map.data expired)
;;

let reap_connections t = expired_connections t |> List.iter ~f:(close_connection_record t)

let rec reaper_loop t interval =
  Eio.Time.sleep (Eio.Stdenv.clock t.env) interval;
  reap_connections t;
  reaper_loop t interval
;;

let server_error_handler
      on_error
      _client_address
      ?request:_
      ~respond
      (failure : P.Error.server)
  =
  on_error (Failure ("HTTP server error: " ^ P.Error.to_string (failure :> P.Error.t)));
  respond ~headers:(P.Headers.of_list [ "connection", "close" ]) P.Body.empty
;;

let run
      ~sw
      ~env
      ~address
      ~dispatcher
      ~registry
      ~blob_store
      ~health
      ~close_connection
      ~authenticate
      ~max_body_bytes
      ~max_batch_size
      ~batch_concurrency
      ~outgoing_capacity
      ~max_connections
      ~max_attachments
      ~idle_connection_timeout
      ~on_error
  =
  let t =
    { sw
    ; env
    ; dispatcher
    ; registry
    ; blob_store
    ; health
    ; close_connection
    ; authenticate
    ; max_body_bytes
    ; max_batch_size
    ; batch_concurrency
    ; outgoing_capacity
    ; max_connections
    ; max_attachments
    ; idle_connection_timeout
    ; mutex = Eio.Mutex.create ()
    ; connections = Map.Poly.empty
    }
  in
  let config = P.Server.Config.create address in
  let server =
    P.Server.create ~config ~error_handler:(server_error_handler on_error) (handler t)
  in
  Exn.protect
    ~f:(fun () ->
      let reaper_interval = Float.min 5. (idle_connection_timeout /. 2.) in
      Eio.Fiber.fork ~sw (fun () -> reaper_loop t reaper_interval);
      ignore (P.Server.Command.start ~sw env server : P.Server.Command.t);
      let forever, _ = Eio.Promise.create () in
      Eio.Promise.await forever)
    ~finally:(fun () -> close_all t)
;;
