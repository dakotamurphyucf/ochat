open! Core

type transport =
  | In_memory
  | Unix_socket
  | Stdio
  | Http
[@@deriving compare, equal, sexp]

type t =
  { connection_id : string
  ; principal : Agent_protocol.Principal.t
  ; transport : transport
  ; publish_notification : Agent_protocol.Envelope.t -> unit
  ; max_attachments : int
  ; mutex : Eio.Mutex.t
  ; mutable initialized : bool
  ; mutable protocol_version : Agent_protocol.Version.t option
  ; mutable reserved_attachments : int
  ; mutable attachments :
      (Agent_protocol.Id.Attachment.t, Agent_protocol.Session.Attachment.t) Map.Poly.t
  }

let create ~connection_id ~principal ~transport ~publish_notification ~max_attachments =
  if max_attachments <= 0 then invalid_arg "max_attachments must be positive";
  { connection_id
  ; principal
  ; transport
  ; publish_notification
  ; max_attachments
  ; mutex = Eio.Mutex.create ()
  ; initialized = false
  ; protocol_version = None
  ; reserved_attachments = 0
  ; attachments = Map.Poly.empty
  }
;;

let principal t = t.principal
let initialized t = Eio.Mutex.use_ro t.mutex (fun () -> t.initialized)
let protocol_version t = Eio.Mutex.use_ro t.mutex (fun () -> t.protocol_version)

let mark_initialized ?(version = Agent_protocol.Version.initial) t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.protocol_version <- Some version;
    t.initialized <- true)
;;

let attachment_limit_error () =
  Agent_protocol.Error.create
    Resource_limit
    ~message:"connection attachment limit reached"
    ~retryable:true
    ()
;;

let reserve_attachment t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if Map.length t.attachments + t.reserved_attachments >= t.max_attachments
    then Error (attachment_limit_error ())
    else (
      t.reserved_attachments <- t.reserved_attachments + 1;
      Ok ()))
;;

let release_attachment_reservation t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if t.reserved_attachments <= 0
    then invalid_arg "connection attachment reservation underflow"
    else t.reserved_attachments <- t.reserved_attachments - 1)
;;

let register_reserved_attachment t attachment =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if t.reserved_attachments <= 0
    then invalid_arg "connection attachment registration has no reservation";
    t.reserved_attachments <- t.reserved_attachments - 1;
    t.attachments
    <- Map.set
         t.attachments
         ~key:attachment.Agent_protocol.Session.Attachment.id
         ~data:attachment)
;;

let remove_attachment t attachment_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.attachments <- Map.remove t.attachments attachment_id)
;;

let remove_session_attachments t session_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.attachments
    <- Map.filter t.attachments ~f:(fun attachment ->
         Agent_protocol.Id.Session.compare attachment.session_id session_id <> 0))
;;

let owns_attachment t ~session_id ~attachment_id =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Map.find t.attachments attachment_id
    |> Option.exists ~f:(fun attachment ->
      Agent_protocol.Id.Session.compare attachment.session_id session_id = 0))
;;

let attachments t = Eio.Mutex.use_ro t.mutex (fun () -> Map.data t.attachments)
let publish_notification t envelope = t.publish_notification envelope
