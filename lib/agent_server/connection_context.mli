open! Core

type transport =
  | In_memory
  | Unix_socket
  | Stdio
  | Http
[@@deriving compare, equal, sexp]

type t

val create
  :  connection_id:string
  -> principal:Agent_protocol.Principal.t
  -> transport:transport
  -> publish_notification:(Agent_protocol.Envelope.t -> unit)
  -> max_attachments:int
  -> t

val principal : t -> Agent_protocol.Principal.t
val initialized : t -> bool
val mark_initialized : t -> unit
val reserve_attachment : t -> (unit, Agent_protocol.Error.t) result
val release_attachment_reservation : t -> unit
val register_reserved_attachment : t -> Agent_protocol.Session.Attachment.t -> unit
val remove_attachment : t -> Agent_protocol.Id.Attachment.t -> unit
val remove_session_attachments : t -> Agent_protocol.Id.Session.t -> unit

val owns_attachment
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> bool

val attachments : t -> Agent_protocol.Session.Attachment.t list
val publish_notification : t -> Agent_protocol.Envelope.t -> unit
