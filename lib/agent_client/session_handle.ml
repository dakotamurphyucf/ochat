open! Core

type t =
  { sleep : float -> unit
  ; now : unit -> Time_ns.t
  ; connection : Connection.t
  ; session_id : Agent_protocol.Id.Session.t
  ; mutex : Eio.Mutex.t
  ; on_update : (Projection.t -> unit) option
  ; on_error : (Agent_protocol.Error.t -> unit) option
  ; mutable attachment : Agent_protocol.Session.Attachment.t
  ; reclaim_token : string option
  ; mutable projection : Projection.t
  ; mutable last_error : Agent_protocol.Error.t option
  ; mutable closed : bool
  ; closed_signal : unit Eio.Promise.t
  ; closed_resolver : unit Eio.Promise.u
  }

let key () =
  Agent_protocol.Id.Transaction.create ()
  |> Agent_protocol.Id.Transaction.to_string
  |> Agent_protocol.Idempotency_key.of_string
;;

let interrupted message =
  Agent_protocol.Error.create Interrupted ~message ~retryable:true ()
;;

let initialize connection ~implementation_name ~implementation_version =
  let open Result.Let_syntax in
  let%bind implementation =
    Agent_protocol.Initialize.Implementation.create
      ~name:implementation_name
      ~version:implementation_version
  in
  let%bind request =
    Agent_protocol.Initialize.Request.create
      ~implementation
      ~protocol_min:Agent_protocol.Version.initial
      ~protocol_max:Agent_protocol.Version.initial
      ~features:[]
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(16 * 1024 * 1024)
      ()
  in
  match Connection.request connection (Protocol_initialize request) with
  | Ok (Protocol_initialize response) -> Ok response
  | Ok _ -> Error (Agent_protocol.Error.invalid_request "unexpected initialize result")
  | Error _ as failure -> failure
;;

let initial_projection connection session_id replay previous_projection =
  let open Result.Let_syntax in
  match replay with
  | Agent_protocol.Method_result.Attach.Snapshot snapshot ->
    Ok (Projection.install_snapshot snapshot)
  | Current ->
    (match previous_projection with
     | Some projection -> Ok projection
     | None ->
       (match
          Connection.request connection (Session_get { session_id; history = None })
        with
        | Ok (Session_get snapshot) -> Ok (Projection.install_snapshot snapshot)
        | Ok _ ->
          Error (Agent_protocol.Error.invalid_request "unexpected session.get result")
        | Error _ as failure -> failure))
  | Events events ->
    (match previous_projection with
     | Some projection ->
       List.fold_result events ~init:projection ~f:Projection.apply_event
     | None ->
       Error
         (Agent_protocol.Error.create
            Snapshot_required
            ~message:"event replay requires the projection that produced its cursor"
            ~retryable:true
            ()))
;;

let notify t projection =
  Option.iter t.on_update ~f:(fun callback ->
    try callback projection with
    | _ -> ())
;;

let install_projection t result =
  let projection =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match result with
      | Error failure ->
        t.last_error <- Some failure;
        None
      | Ok projection ->
        t.projection <- projection;
        Some projection)
  in
  match result, projection with
  | Error failure, _ -> Option.iter t.on_error ~f:(fun callback -> callback failure)
  | Ok _, Some projection -> notify t projection
  | Ok _, None -> ()
;;

let apply_notification t = function
  | Agent_protocol.Envelope.Notification { method_ = "session.event"; params } ->
    (match Agent_protocol.Event.Durable.of_json params with
     | Error failure -> install_projection t (Error failure)
     | Ok event ->
       if Agent_protocol.Id.Session.compare event.session_id t.session_id = 0
       then
         Eio.Mutex.use_ro t.mutex (fun () -> Projection.apply_event t.projection event)
         |> install_projection t)
  | Notification { method_ = "session.live_event"; params } ->
    (match Agent_protocol.Event.Recoverable.of_json params with
     | Error failure -> install_projection t (Error failure)
     | Ok event ->
       if Agent_protocol.Id.Session.compare event.session_id t.session_id = 0
       then
         Eio.Mutex.use_ro t.mutex (fun () ->
           Projection.apply_live_event t.projection event)
         |> install_projection t)
  | Notification _ | Request _ | Response _ -> ()
;;

let mark_closed t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if not t.closed
    then (
      t.closed <- true;
      Eio.Promise.resolve t.closed_resolver ()))
;;

let next_notification t =
  Eio.Fiber.first
    (fun () -> Connection.next_notification t.connection)
    (fun () ->
       Eio.Promise.await t.closed_signal;
       None)
;;

let rec read_notifications t =
  if not (Eio.Mutex.use_ro t.mutex (fun () -> t.closed))
  then (
    match next_notification t with
    | None ->
      install_projection t (Error (interrupted "session notification stream closed"));
      mark_closed t
    | Some envelope ->
      apply_notification t envelope;
      read_notifications t)
;;

let renew_delay t lease =
  let now = t.now () in
  let expires =
    Agent_protocol.Timestamp.to_time_ns
      lease.Agent_protocol.Session.Owner_lease.expires_at
  in
  Float.max 0.1 (Time_ns.Span.to_sec (Time_ns.diff expires now) /. 2.)
;;

let rec renew_owner t lease =
  let closed =
    Eio.Fiber.first
      (fun () ->
         t.sleep (renew_delay t lease);
         false)
      (fun () ->
         Eio.Promise.await t.closed_signal;
         true)
  in
  if not closed
  then (
    match key () with
    | Error failure -> install_projection t (Error failure)
    | Ok idempotency_key ->
      let request =
        Agent_protocol.Session.Renew_owner_request.
          { session_id = t.session_id
          ; attachment_id = t.attachment.id
          ; lease_generation = lease.generation
          ; idempotency_key
          }
      in
      (match Connection.request t.connection (Session_renew_owner request) with
       | Ok (Session_renew_owner (lease, _)) ->
         Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
           t.attachment <- { t.attachment with owner_lease = Some lease });
         renew_owner t lease
       | Ok _ ->
         install_projection
           t
           (Error (Agent_protocol.Error.invalid_request "unexpected owner renewal result"))
       | Error failure -> install_projection t (Error failure)))
;;

let make_handle
      ~sw
      ~clock
      ~connection
      ~session_id
      ~attachment
      ~reclaim_token
      ~replay
      ~subscribe
      ?previous_projection
      ?on_update
      ?on_error
      ()
  =
  Result.map
    (initial_projection connection session_id replay previous_projection)
    ~f:(fun projection ->
      let closed_signal, closed_resolver = Eio.Promise.create () in
      let t =
        { sleep = Eio.Time.sleep clock
        ; now =
            (fun () ->
              Eio.Time.now clock |> Time_ns.Span.of_sec |> Time_ns.of_span_since_epoch)
        ; connection
        ; session_id
        ; mutex = Eio.Mutex.create ()
        ; on_update
        ; on_error
        ; attachment
        ; reclaim_token
        ; projection
        ; last_error = None
        ; closed = false
        ; closed_signal
        ; closed_resolver
        }
      in
      if subscribe then Eio.Fiber.fork ~sw (fun () -> read_notifications t);
      Option.iter attachment.owner_lease ~f:(fun lease ->
        Eio.Fiber.fork ~sw (fun () -> renew_owner t lease));
      t)
;;

let attach
      ~sw
      ~clock
      ~connection
      ~session_id
      ~mode
      ?(subscribe = true)
      ?after_sequence
      ?reclaim_token
      ?previous_projection
      ?on_update
      ?on_error
      ()
  =
  let open Result.Let_syntax in
  let%bind idempotency_key = key () in
  let request =
    Agent_protocol.Session.Attach_request.
      { session_id
      ; requested_mode = mode
      ; subscribe
      ; after_sequence
      ; reclaim_token
      ; idempotency_key
      }
  in
  match Connection.request connection (Session_attach request) with
  | Error _ as failure -> failure
  | Ok (Session_attach response) ->
    make_handle
      ~sw
      ~clock
      ~connection
      ~session_id
      ~attachment:response.attachment
      ~reclaim_token:response.reclaim_token
      ~replay:response.replay
      ~subscribe
      ?previous_projection
      ?on_update
      ?on_error
      ()
  | Ok _ ->
    Error (Agent_protocol.Error.invalid_request "unexpected session.attach result")
;;

let create ~sw ~clock ~connection ~spec ~mode ?on_update ?on_error () =
  let open Result.Let_syntax in
  let%bind idempotency_key = key () in
  let request =
    Agent_protocol.Session.Create_request.
      { spec; requested_mode = Some mode; subscribe = true; idempotency_key }
  in
  match Connection.request connection (Session_create request) with
  | Error _ as failure -> failure
  | Ok (Session_create { session; attachment = Some response; _ }) ->
    make_handle
      ~sw
      ~clock
      ~connection
      ~session_id:session.id
      ~attachment:response.attachment
      ~reclaim_token:response.reclaim_token
      ~replay:response.replay
      ~subscribe:true
      ?on_update
      ?on_error
      ()
  | Ok (Session_create { attachment = None; _ }) ->
    Error (Agent_protocol.Error.invalid_request "session.create omitted its attachment")
  | Ok _ ->
    Error (Agent_protocol.Error.invalid_request "unexpected session.create result")
;;

let session_id t = t.session_id
let attachment t = Eio.Mutex.use_ro t.mutex (fun () -> t.attachment)
let reclaim_token t = t.reclaim_token
let projection t = Eio.Mutex.use_ro t.mutex (fun () -> t.projection)
let last_error t = Eio.Mutex.use_ro t.mutex (fun () -> t.last_error)
let await_closed t = Eio.Promise.await t.closed_signal
let is_closed t = Eio.Mutex.use_ro t.mutex (fun () -> t.closed)

let mutation_command t make extract =
  let open Result.Let_syntax in
  let%bind idempotency_key = key () in
  match Connection.request t.connection (make idempotency_key) with
  | Ok result -> extract result
  | Error _ as failure -> failure
;;

let start t ~queue_if_limited =
  mutation_command
    t
    (fun idempotency_key ->
       Session_start
         { session_id = t.session_id
         ; attachment_id = t.attachment.id
         ; queue_if_limited
         ; idempotency_key
         })
    (function
      | Session_start result -> Ok result.session
      | _ ->
        Error (Agent_protocol.Error.invalid_request "unexpected session.start result"))
;;

let stop t ~mode =
  mutation_command
    t
    (fun idempotency_key ->
       Session_stop
         { session_id = t.session_id
         ; attachment_id = t.attachment.id
         ; mode
         ; idempotency_key
         })
    (function
      | Session_stop result -> Ok result.session
      | _ -> Error (Agent_protocol.Error.invalid_request "unexpected session.stop result"))
;;

let send_message t content =
  mutation_command
    t
    (fun idempotency_key ->
       Session_send_message
         { session_id = t.session_id
         ; attachment_id = t.attachment.id
         ; content
         ; idempotency_key
         })
    (function
      | Session_send_message result -> Ok result
      | _ -> Error (Agent_protocol.Error.invalid_request "unexpected send result"))
;;

let delete_history t ~expected_revision history_id =
  mutation_command
    t
    (fun idempotency_key ->
       Session_delete_history
         { session_id = t.session_id
         ; attachment_id = t.attachment.id
         ; expected_revision
         ; history_id
         ; idempotency_key
         })
    (function
      | Session_delete_history result -> Ok result.session
      | _ ->
        Error (Agent_protocol.Error.invalid_request "unexpected history deletion result"))
;;

let compact t ~expected_revision =
  mutation_command
    t
    (fun idempotency_key ->
       Session_compact
         { session_id = t.session_id
         ; attachment_id = t.attachment.id
         ; expected_revision
         ; idempotency_key
         })
    (function
      | Session_compact result -> Ok result.session
      | _ -> Error (Agent_protocol.Error.invalid_request "unexpected compact result"))
;;

let cancel_operation t operation_id =
  mutation_command
    t
    (fun idempotency_key ->
       Session_cancel_operation
         { session_id = t.session_id
         ; attachment_id = t.attachment.id
         ; operation_id
         ; idempotency_key
         })
    (function
      | Session_cancel_operation result -> Ok result.session
      | _ -> Error (Agent_protocol.Error.invalid_request "unexpected cancel result"))
;;

let respond_permission t ~permission_id ~permission_generation ~choice ~reason =
  mutation_command
    t
    (fun idempotency_key ->
       Permission_respond
         { session_id = t.session_id
         ; attachment_id = t.attachment.id
         ; permission_id
         ; permission_generation
         ; choice
         ; reason
         ; idempotency_key
         })
    (function
      | Permission_respond result -> Ok result.permission
      | _ -> Error (Agent_protocol.Error.invalid_request "unexpected permission result"))
;;

let revoke_grant t ~grant_id ~reason =
  mutation_command
    t
    (fun idempotency_key ->
       Grant_revoke
         { grant_id
         ; session_id = t.session_id
         ; attachment_id = t.attachment.id
         ; reason
         ; idempotency_key
         })
    (function
      | Grant_revoke result -> Ok result.grant
      | _ -> Error (Agent_protocol.Error.invalid_request "unexpected grant result"))
;;

let read_audit (t : t) ~limit =
  let open Result.Let_syntax in
  let%bind page = Agent_protocol.Page.Request.create ~limit () in
  let request =
    Agent_protocol.Audit.Read_request.
      { page
      ; session_id = Some t.session_id
      ; principal_id = None
      ; minimum_level = None
      ; name_prefix = None
      }
  in
  match Connection.request t.connection (Audit_read request) with
  | Ok (Audit_read page) -> Ok page
  | Ok _ -> Error (Agent_protocol.Error.invalid_request "unexpected audit result")
  | Error _ as failure -> failure
;;

let reset
      t
      ~expected_revision
      ~keep_history
      ~keep_tasks
      ~keep_cache
      ~keep_workspace
      ~keep_grants
      ~keep_labels
  =
  mutation_command
    t
    (fun idempotency_key ->
       Session_reset
         { session_id = t.session_id
         ; attachment_id = t.attachment.id
         ; expected_revision
         ; keep_history
         ; keep_tasks
         ; keep_cache
         ; keep_workspace
         ; keep_grants
         ; keep_labels
         ; idempotency_key
         })
    (function
      | Session_reset result -> Ok result.session
      | _ -> Error (Agent_protocol.Error.invalid_request "unexpected reset result"))
;;

let rebuild t ~expected_revision ~prompt_choice =
  mutation_command
    t
    (fun idempotency_key ->
       Session_rebuild
         { session_id = t.session_id
         ; attachment_id = t.attachment.id
         ; expected_revision
         ; prompt_choice
         ; idempotency_key
         })
    (function
      | Session_rebuild result -> Ok result.session
      | _ -> Error (Agent_protocol.Error.invalid_request "unexpected rebuild result"))
;;

let export t ~format ~revision =
  match
    Connection.request
      t.connection
      (Session_export
         { session_id = t.session_id
         ; attachment_id = t.attachment.id
         ; format
         ; revision
         ; history = None
         })
  with
  | Ok (Session_export result) -> Ok result
  | Ok _ -> Error (Agent_protocol.Error.invalid_request "unexpected export result")
  | Error _ as failure -> failure
;;

let download_blob t ~blob ~output =
  Blob_download.download
    ~connection:t.connection
    ~session_id:t.session_id
    ~attachment_id:t.attachment.id
    ~blob
    ~output
;;

let delete t ~expected_revision ~policy ~confirmation =
  mutation_command
    t
    (fun idempotency_key ->
       Session_delete
         { session_id = t.session_id
         ; attachment_id = t.attachment.id
         ; expected_revision
         ; policy
         ; confirmation
         ; idempotency_key
         })
    (function
      | Session_delete result -> Ok result
      | _ -> Error (Agent_protocol.Error.invalid_request "unexpected delete result"))
;;

let detach t =
  mutation_command
    t
    (fun idempotency_key ->
       Session_detach
         { session_id = t.session_id; attachment_id = t.attachment.id; idempotency_key })
    (function
      | Session_detach _ ->
        mark_closed t;
        Ok ()
      | _ -> Error (Agent_protocol.Error.invalid_request "unexpected detach result"))
;;

let close t =
  if not (Eio.Mutex.use_ro t.mutex (fun () -> t.closed))
  then (
    ignore (detach t : (unit, Agent_protocol.Error.t) result);
    mark_closed t)
;;
