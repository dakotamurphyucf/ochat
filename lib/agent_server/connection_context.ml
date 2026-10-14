open! Core

type transport =
  | In_memory
  | Unix_socket
  | Stdio
  | Http
[@@deriving compare, equal, sexp]

module Authoring_negotiation = struct
  type t =
    { token : unit ref
    ; owner : unit ref
    ; revoked : bool Atomic.t
    ; lease_label : string
    ; enabled_features : string list
    }

  let lease_label t = t.lease_label
  let selected t feature = List.mem t.enabled_features feature ~equal:String.equal
end

type t =
  { connection_id : string
  ; principal : Agent_protocol.Principal.t
  ; actor : Operator_authorization.t
  ; transport : transport
  ; publish_notification : Agent_protocol.Envelope.t -> unit
  ; max_attachments : int
  ; mutex : Eio.Mutex.t
  ; authoring_owner : unit ref
  ; mutable initialized : bool
  ; mutable protocol_version : Agent_protocol.Version.t option
  ; mutable authoring_negotiation : Authoring_negotiation.t option
  ; mutable authoring_closed : bool
  ; mutable reserved_attachments : int
  ; mutable attachments :
      (Agent_protocol.Id.Attachment.t, Agent_protocol.Session.Attachment.t) Map.Poly.t
  }

let create_authenticated
      ~actor
      ~connection_id
      ~transport
      ~publish_notification
      ~max_attachments
  =
  let principal = Operator_authorization.principal actor in
  if max_attachments <= 0 then invalid_arg "max_attachments must be positive";
  { connection_id
  ; principal
  ; actor
  ; transport
  ; publish_notification
  ; max_attachments
  ; mutex = Eio.Mutex.create ()
  ; authoring_owner = ref ()
  ; initialized = false
  ; protocol_version = None
  ; authoring_negotiation = None
  ; authoring_closed = false
  ; reserved_attachments = 0
  ; attachments = Map.Poly.empty
  }
;;

let create ~connection_id ~principal ~transport ~publish_notification ~max_attachments =
  create_authenticated
    ~actor:(Operator_authorization.guarded ~principal ~is_current:(fun () -> false))
    ~connection_id
    ~transport
    ~publish_notification
    ~max_attachments
;;

let principal t = t.principal
let actor t = t.actor

let request_actor t actor =
  let actor = Option.value actor ~default:t.actor in
  let principal = Operator_authorization.principal actor in
  if
    Agent_protocol.Id.Principal.equal principal.id t.principal.id
    && String.equal principal.authentication_kind t.principal.authentication_kind
    && Agent_protocol.Scope.Set.equal principal.scopes t.principal.scopes
    && List.equal
         (fun (left_name, left_value) (right_name, right_value) ->
            String.equal left_name right_name && String.equal left_value right_value)
         principal.attributes
         t.principal.attributes
  then Ok actor
  else
    Error
      (Agent_protocol.Error.create
         Permission_denied
         ~message:"request authentication differs from connection authority"
         ~retryable:false
         ())
;;

let initialized t = Eio.Mutex.use_ro t.mutex (fun () -> t.initialized)
let protocol_version t = Eio.Mutex.use_ro t.mutex (fun () -> t.protocol_version)
let authoring_negotiation t = Eio.Mutex.use_ro t.mutex (fun () -> t.authoring_negotiation)
let authoring_closed t = Eio.Mutex.use_ro t.mutex (fun () -> t.authoring_closed)

let close_authoring t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Option.iter t.authoring_negotiation ~f:(fun negotiation ->
      Atomic.set negotiation.Authoring_negotiation.revoked true);
    t.authoring_closed <- true;
    t.authoring_negotiation <- None)
;;

let current_authoring_negotiation t negotiation =
  phys_equal t.authoring_owner negotiation.Authoring_negotiation.owner
  && not (Atomic.get negotiation.Authoring_negotiation.revoked)
;;

let try_mark_initialized
      ?(version = Agent_protocol.Version.current)
      ?(enabled_features = [])
      t
  =
  let negotiation =
    { Authoring_negotiation.token = ref ()
    ; owner = t.authoring_owner
    ; revoked = Atomic.make false
    ; lease_label = Agent_protocol.Id.Transaction.(create () |> to_string)
    ; enabled_features
    }
  in
  let next_version = Some version in
  let next_negotiation = Some negotiation in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if t.authoring_closed
    then false
    else (
      Option.iter t.authoring_negotiation ~f:(fun previous ->
        Atomic.set previous.Authoring_negotiation.revoked true);
      t.protocol_version <- next_version;
      t.authoring_negotiation <- next_negotiation;
      t.initialized <- true;
      true))
;;

let mark_initialized ?version ?enabled_features t =
  if not (try_mark_initialized ?version ?enabled_features t)
  then invalid_arg "authoring connection is closed"
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
