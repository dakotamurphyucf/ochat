open! Core

(** Attached daemon-session client for the TUI controller. *)

type update =
  | Projection of Agent_projection.t
  | Connection_changed of Connection_status.t
  | Connection_failed of Agent_protocol.Error.t

type t

type create_options =
  { prompt : string
  ; workspace : string
  ; liveness : Agent_protocol.Session.liveness
  ; permission_profile : string option
  ; display_name : string option
  ; labels : (string * string) list
  ; mode : Agent_protocol.Session.attachment_mode
  }

val attach
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> connection:Agent_client.Connection.t
  -> ?reconnect:
       (unit -> (Agent_client.Connection.t, Agent_protocol.Error.t) result) option
  -> session_id:Agent_protocol.Id.Session.t
  -> mode:Agent_protocol.Session.attachment_mode
  -> unit
  -> (t, Agent_protocol.Error.t) result

val create
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> connection:Agent_client.Connection.t
  -> ?reconnect:
       (unit -> (Agent_client.Connection.t, Agent_protocol.Error.t) result) option
  -> create_options
  -> (t, Agent_protocol.Error.t) result

(** Create and attach through the existing reconnect owner using an explicit
    validated specification. Unlike the named daemon convenience [create], this
    preserves the caller's execution host, persistence and start intent. Exactly
    one subscribed handle owns notifications; failed/uncertain admission is not
    retried with a new command identity. The caller owns the enclosing switch. *)
val create_with_spec
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> connection:Agent_client.Connection.t
  -> ?reconnect:
       (unit -> (Agent_client.Connection.t, Agent_protocol.Error.t) result) option
  -> spec:Agent_protocol.Session.Spec.t
  -> mode:Agent_protocol.Session.attachment_mode
  -> unit
  -> (t, Agent_protocol.Error.t) result

val projection : t -> Agent_projection.t
val status : t -> Connection_status.t
val next_update : t -> update
val take_update_nonblocking : t -> update option

val send_text
  :  t
  -> string
  -> (Agent_protocol.Method_result.Send_message.t, Agent_protocol.Error.t) result

val send_content
  :  t
  -> Agent_protocol.Session.Message_content.t
  -> (Agent_protocol.Method_result.Send_message.t, Agent_protocol.Error.t) result

val compact : t -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Requests authoritative deletion at the client's current revision. Does not
    optimistically mutate the local projection. *)
val delete_history
  :  t
  -> History_entry.Id.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val cancel_active_operation
  :  t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val start
  :  t
  -> queue_if_limited:bool
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val stop
  :  t
  -> mode:Agent_protocol.Session.stop_mode
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val respond_permission
  :  t
  -> permission_id:Agent_protocol.Id.Permission.t
  -> permission_generation:int
  -> choice:Agent_protocol.Permission.choice
  -> reason:string option
  -> (Agent_protocol.Permission.t, Agent_protocol.Error.t) result

val revoke_grant
  :  t
  -> grant_id:Agent_protocol.Id.Grant.t
  -> reason:string
  -> (Agent_protocol.Grant.t, Agent_protocol.Error.t) result

val read_audit
  :  t
  -> limit:int
  -> (Agent_protocol.Audit.t Agent_protocol.Page.t, Agent_protocol.Error.t) result

val detach : t -> (unit, Agent_protocol.Error.t) result
val close : t -> unit

(** Complete authorized canonical history at the exact requested revision,
    using the current reconnect owner's attachment. Does not render the UI's
    possibly incomplete or transformed projection, or retry after a conflict. *)
val read_canonical
  :  t
  -> revision:int64
  -> limits:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Agent_client.Canonical_export.t, Agent_protocol.Error.t) Result.t

(** [attachment t] exposes current attachment identity/mode, not credentials. *)
val attachment : t -> Agent_protocol.Session.Attachment.t option
