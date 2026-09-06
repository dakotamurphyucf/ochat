open! Core

(** Connection-loss recovery for an attached agent session. The manager owns
    the current connection and session handle, preserves the last durable
    projection, and reattaches with its replay cursor after transport loss. *)

type status =
  | Connected
  | Reconnecting of { attempt : int }
  | Disconnected
  | Failed of Agent_protocol.Error.t
[@@deriving sexp]

type policy =
  { initial_delay : Time_ns.Span.t
  ; maximum_delay : Time_ns.Span.t
  ; multiplier : float
  ; jitter_ratio : float
  ; maximum_attempts : int option
  }

type t

val default_policy : policy

val attach
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> connection:Connection.t
  -> reconnect:(unit -> (Connection.t, Agent_protocol.Error.t) result) option
  -> session_id:Agent_protocol.Id.Session.t
  -> mode:Agent_protocol.Session.attachment_mode
  -> ?policy:policy
  -> ?on_update:(Projection.t -> unit)
  -> ?on_status:(status -> unit)
  -> ?on_error:(Agent_protocol.Error.t -> unit)
  -> unit
  -> (t, Agent_protocol.Error.t) result

val create
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> connection:Connection.t
  -> reconnect:(unit -> (Connection.t, Agent_protocol.Error.t) result) option
  -> spec:Agent_protocol.Session.Spec.t
  -> mode:Agent_protocol.Session.attachment_mode
  -> ?policy:policy
  -> ?on_update:(Projection.t -> unit)
  -> ?on_status:(status -> unit)
  -> ?on_error:(Agent_protocol.Error.t -> unit)
  -> unit
  -> (t, Agent_protocol.Error.t) result

val session_id : t -> Agent_protocol.Id.Session.t
val attachment : t -> Agent_protocol.Session.Attachment.t option
val reclaim_token : t -> string option
val projection : t -> Projection.t
val status : t -> status

val start
  :  t
  -> queue_if_limited:bool
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val stop
  :  t
  -> mode:Agent_protocol.Session.stop_mode
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val send_message
  :  t
  -> Agent_protocol.Session.Message_content.t
  -> (Agent_protocol.Method_result.Send_message.t, Agent_protocol.Error.t) result

val delete_history
  :  t
  -> expected_revision:int64
  -> Agent_protocol.History.Id.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val compact
  :  t
  -> expected_revision:int64 option
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val cancel_operation
  :  t
  -> Agent_protocol.Id.Operation.t
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
