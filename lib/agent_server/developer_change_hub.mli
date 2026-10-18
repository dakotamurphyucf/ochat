open! Core
module P = Agent_protocol

(** Bounded process-local invalidation for authenticated Developer command
    watchers. Store rows and current approvals remain authoritative. No durable
    cursor, output stream, polling producer or second Store owner lives here. *)
type t

module Limits : sig
  type t

  (** Bounds retained physical connections, per-connection exact-key watches,
      queued hints and publication work. Dirty/resync state is one bounded
      slot per watch, independent of queued-hint capacity. The installed wire
      event bound must fit a worst-case key-scoped resync hint. All fields are
      positive and their aggregate product is checked. *)
  val create
    :  max_connections:int
    -> max_watches_per_connection:int
    -> max_pending_hints_per_connection:int
    -> (t, P.Error.t) result
end

val create
  :  sw:Eio.Switch.t
  -> limits:Limits.t
  -> wire_limits:P.Developer_common.Limits.t
  -> (t, P.Error.t) result

(** Nonyielding same-domain post-transition enqueue. Both command and
    approval changes carry the exact owning command key; Task rows never
    enter this family. A full per-connection queue marks that exact watch
    dirty in bounded per-watch state, superseding its dropped hints with a
    resync obligation. The pump fairly prioritizes dirty watches so an active
    connected watcher eventually receives Resync_required for every affected
    key; unrelated watch obligations remain intact. A new transition during
    an in-flight resync retains a later obligation. Closed hubs discard hints.
    This function never publishes to a transport or awaits a subscriber. *)
val enqueue
  :  t
  -> run_id:P.Id.Developer_run.t
  -> key:P.Idempotency_key.t
  -> kind:P.Developer_wire.Change.kind
  -> unit

(** [authorize_key] first observes an existing non-Task command under the
    current actor, method and read policy; an absent/foreign key cannot
    register. [actor] is the exact request_actor result, retained only for
    final currentness comparison, not as a new grant. A repeat of the same
    physical connection/run/key refreshes that proof and both callbacks in
    place, invalidating older in-flight delivery generations without adding
    a second watch. [authorize_change]
    rechecks Host disclosure after any yielding read for this exact key or
    current approval owner. The pump then performs a final nonyielding actor,
    feature-lease and watch-generation check immediately before handing the
    bounded hint to Connection_context.publish_notification. This handoff is
    the disclosure linearization point; already handed-off bytes need not be
    retracted. A connection-owned pump alone makes that possibly blocking
    call, outside all Store/approval locks. Reinitialize revokes the old
    feature lease and watch generation. The Watch acknowledgement establishes
    only future hint registration; a transition between the yielding initial
    authorization and insertion may be missed. Clients must Observe this key
    after every successful Watch (and on reconnect) to establish an
    authoritative command baseline. Approval_list is a separate optional
    refresh only when that feature and its current read policy authorize it;
    Check-only watchers require no approval scope. *)
val watch
  :  t
  -> connection:Connection_context.t
  -> actor:Operator_authorization.t
  -> run_id:P.Id.Developer_run.t
  -> key:P.Idempotency_key.t
  -> authorize_key:(unit -> (unit, P.Error.t) result)
  -> authorize_change:(P.Developer_wire.Change.t -> (unit, P.Error.t) result)
  -> (unit, P.Error.t) result

val unwatch
  :  t
  -> connection:Connection_context.t
  -> run_id:P.Id.Developer_run.t
  -> key:P.Idempotency_key.t
  -> (unit, P.Error.t) result

(** Terminal connection close first revokes its physical Authoring feature
    lease, including when no watch entry exists, then cancels and joins the
    exact pump. Replacing a stale lease during Watch uses private entry
    retirement and does not close a newly initialized Authoring lease;
    Unwatch likewise leaves that physical lease alone. Concurrent Unwatch,
    Watch and terminal close share the in-flight retirement outcome instead
    of bypassing a blocked publication. Exceptions and cancellation retain
    their identity. *)
val close_connection_and_wait : t -> Connection_context.t -> (unit, P.Error.t) result

val close_and_wait : t -> (unit, P.Error.t) result
