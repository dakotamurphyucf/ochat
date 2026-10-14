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

(** Principal-only [create] above does not grant provider operator authority.
    Real transports and trusted local hosts use this proof-bearing constructor. *)
val create_authenticated
  :  actor:Operator_authorization.t
  -> connection_id:string
  -> transport:transport
  -> publish_notification:(Agent_protocol.Envelope.t -> unit)
  -> max_attachments:int
  -> t

val actor : t -> Operator_authorization.t

val request_actor
  :  t
  -> Operator_authorization.t option
  -> (Operator_authorization.t, Agent_protocol.Error.t) result

val principal : t -> Agent_protocol.Principal.t
val initialized : t -> bool

module Authoring_negotiation : sig
  type t

  (** Host-minted comparison lease for one successful initialize on this
      physical connection. It is not an authorization capability. *)
  val lease_label : t -> string

  val selected : t -> string -> bool
end

val authoring_negotiation : t -> Authoring_negotiation.t option

(** Nonyielding physical-connection/lease comparison. Reinitialization and
    terminal close revoke an earlier lease atomically; this check grants no
    actor or method authority. *)
val current_authoring_negotiation : t -> Authoring_negotiation.t -> bool

val authoring_closed : t -> bool

(** Terminal for this physical connection: invalidates its authoring lease and
    prevents a later initialize from reopening it. The service calls this when
    joining transport close; unrelated connection attachment state is intact. *)
val close_authoring : t -> unit

(** Mint a new authoring incarnation atomically with protocol-version and
    selected-feature replacement. The installed authoring handler must call
    this only through its joined reinitialization lifecycle transition. *)
val mark_initialized
  :  ?version:Agent_protocol.Version.t
  -> ?enabled_features:string list
  -> t
  -> unit

(** Same atomic initialization transition, with a typed terminal-close
    outcome for the joined authoring lifecycle. [false] leaves the tuple of
    version, lease and initialized status unchanged. *)
val try_mark_initialized
  :  ?version:Agent_protocol.Version.t
  -> ?enabled_features:string list
  -> t
  -> bool

val protocol_version : t -> Agent_protocol.Version.t option
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
