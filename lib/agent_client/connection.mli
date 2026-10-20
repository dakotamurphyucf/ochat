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

(** Fixed-bound bootstrap for the actual installed REPL handler inventory.
    Requires this physical Initialize to select [developer.repl]. The cached
    Capabilities expire when that Initialize identity changes or on close. *)
val repl_capabilities
  :  t
  -> (Agent_protocol.Developer_repl_wire.Capabilities.t, Agent_protocol.Error.t) result

(** Ordinary REPL requests share the serialized request lane and current Initialize with
    ordinary Developer calls. Except for Capabilities, this revalidates the
    closed request, selected feature, advertised method and exact installed
    limits, then decodes a method-affine bounded response. It owns no mutation
    recovery state: callers retain uncertain Create/Submit/Task/Reset/Close
    requests before calling it. The caller owns the one notification lease. *)
val request_repl
  :  t
  -> limits:Agent_protocol.Developer_repl_wire.Limits.t
  -> Agent_protocol.Developer_repl_wire.Request.t
  -> (Agent_protocol.Developer_repl_wire.Response.t, Agent_protocol.Error.t) result

module Repl_control : sig
  (** The only REPL operations permitted to overtake a blocked ordinary request.
      Neither carries a new idempotency key or grants authority. *)
  type t =
    | Cancel_submission of Agent_protocol.Developer_repl_wire.Lookup.t
    | Task_cancel_and_wait of Agent_protocol.Developer_common.Lookup.t
end

(** Bounded same-connection control lane. [initialized_as] must be the exact
    successful physical Initialize captured by the upper client. Before and
    after a yield, check that generation, selected feature, advertised method,
    installed limits and close state. Response decoding remains method-affine
    and bounded; the server still performs current actor/lease authorization.
    A busy control lane fails with Resource_limit without queueing a waiter. *)
val request_repl_control
  :  t
  -> initialized_as:Agent_protocol.Initialize.Response.t
  -> limits:Agent_protocol.Developer_repl_wire.Limits.t
  -> Repl_control.t
  -> (Agent_protocol.Developer_repl_wire.Response.t, Agent_protocol.Error.t) result

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
