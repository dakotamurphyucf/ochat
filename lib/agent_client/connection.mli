(** Typed request connection with one serialized transport owner. *)

type t

val create : Transport.t -> t

val request
  :  t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result

(** Accepts only the public Non_history response whitelist. Excludes the inline
    snapshot/history containers in get/create/attach, not authority or disclosure:
    export can still reference a history artifact. *)
val request_without_history
  :  t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result

(** The last validated successful initialize response on this transport. Failed
    initialization and unrelated responses do not establish support; closed
    connections expose None. Capability selection does not grant authority. *)
val initialization : t -> Agent_protocol.Initialize.Response.t option

(** Fixed-bound bootstrap for the actual installed Developer handler inventory.
    A successful Initialize selecting developer.run is required; no peer-supplied
    limits govern decoding of this first response. *)
val developer_capabilities
  :  t
  -> (Agent_protocol.Developer_common.Capabilities.t, Agent_protocol.Error.t) result

(** Same serialized request lane and physical initialization as [request].
    [Capabilities] uses [developer_capabilities]; all other closed Developer
    requests are redecoded and their method-affine responses validated against
    the effective limits from that same connection. This call may block: a
    same-connection Cancel cannot overtake Await_terminal. *)
val request_developer
  :  t
  -> limits:Agent_protocol.Developer_common.Limits.t
  -> Agent_protocol.Developer_wire.Request.t
  -> (Agent_protocol.Developer_wire.Response.t, Agent_protocol.Error.t) result

(** Exclusive asynchronous consumer, claimed before session admission. *)
type notification_lease

val claim_notifications : t -> (notification_lease, Agent_protocol.Error.t) result

val next_owned_notification
  :  notification_lease
  -> (Agent_protocol.Envelope.t option, Agent_protocol.Error.t) result

val release_notifications : notification_lease -> unit

(** Legacy unowned drain returns None while an exclusive consumer is claimed. *)
val next_notification : t -> Agent_protocol.Envelope.t option

val close : t -> unit

(** Uncertain original command identity, retained across lost replies. It may
    contain private attachment data and is never a serializable public profile. *)
type pending_command

module Pending_identity : sig
  (** Non-authority recovery coordinates from the retained original request.
      No params, attachment credentials or submitted content are exposed. *)
  type t =
    { method_name : string
    ; idempotency_key : Agent_protocol.Idempotency_key.t option
    }
end

(** Constant-time immutable projection, captured during the original bounded
    parameter admission; does not rebuild or serialize private request data. *)
val pending_identity : pending_command -> Pending_identity.t

val pending_commands : t -> pending_command list

(** Transfer in-memory unresolved intents only after confirming the same host and
    principal. No request is executed. *)
val adopt_pending : t -> pending_command list -> (unit, Agent_protocol.Error.t) result

(** Read the existing receipt. Missing/expired/pending remains unresolved; no
    new-key admission or automatic replay follows. *)
val reconcile
  :  t
  -> pending_command
  -> (Agent_protocol.Command_receipt.t, Agent_protocol.Error.t) result

(** Explicit operator choice to relinquish this client's uncertainty guard.
    Does not cancel or undo admitted work, nor establish whether it committed.
    Call only after presenting that uncertainty to the operator. *)
val abandon_pending : t -> pending_command -> unit
