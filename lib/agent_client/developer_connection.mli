(** Closed portable Developer client over one already initialized physical
    connection. The cached Capabilities are actual installed handlers, not
    authority. Every call checks the same Initialize identity and selected
    feature, advertised method and effective wire limits. The server still
    reauthorizes source, command, approval and control operations. *)

type t

val create : Connection.t -> (t, Agent_protocol.Error.t) result
val capabilities : t -> Agent_protocol.Developer_common.Capabilities.t

val send
  :  t
  -> Agent_protocol.Developer_wire.Request.t
  -> (Agent_protocol.Developer_wire.Response.t, Agent_protocol.Error.t) result

(** Successful Watch registers future invalidation hints only. Observe of the
    same exact key immediately afterward establishes the authoritative
    baseline; after reconnect or resync, repeat both. This client does not
    claim a second notification reader. The caller owns the physical Connection
    lease and can pass each envelope to [change_of_notification]. A decoded
    hint is not proof of a current subscription or authorization; the caller
    reobserves only its exact keys. *)
val watch_and_observe
  :  t
  -> Agent_protocol.Developer_common.Lookup.t
  -> (Agent_protocol.Developer_common.View.t option, Agent_protocol.Error.t) result

val change_of_notification
  :  t
  -> Agent_protocol.Envelope.t
  -> (Agent_protocol.Developer_wire.Change.t option, Agent_protocol.Error.t) result

(** The exact bounded original Submit is retained after every post-handoff
    failure, including local response decoding or identity rejection: an error
    code alone cannot prove the server did not execute. It is private data,
    never a transferable capability. Missing Observe does not prove
    nonexecution; no retry occurs automatically. *)
type pending_submit

module Pending_identity : sig
  type t =
    { run_id : Agent_protocol.Id.Developer_run.t
    ; key : Agent_protocol.Idempotency_key.t
    }
end

val pending_identity : pending_submit -> Pending_identity.t
val pending_submits : t -> pending_submit list

(** Adopt only onto a freshly initialized physical connection with the same
    host ID, principal ID and authentication kind, under finite retention
    bounds. No request is sent. *)
val adopt_pending : t -> pending_submit list -> (unit, Agent_protocol.Error.t) result

(** Authorized historical read of the exact key. A View with matching IDs is
    not sufficient to clear a lost initial Submit, because this client lacks
    the Host's independently authenticated request fingerprint. *)
val observe_pending
  :  t
  -> pending_submit
  -> (Agent_protocol.Developer_common.View.t option, Agent_protocol.Error.t) result

(** Explicit operator choice to retry the retained identical request and key.
    Host compares its original action/payload under the serialized historical
    lookup before any source recapture; if no row exists, this can start work.
    A successful method-affine Submit clears this pending object. *)
val retry_pending
  :  t
  -> pending_submit
  -> (Agent_protocol.Developer_common.View.t, Agent_protocol.Error.t) result

val abandon_pending : t -> pending_submit -> unit
