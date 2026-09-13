(** Pure host cancellation plan for source-owned subscriptions and timers.
    Graceful stop preserves durable work. Cancel preserves terminal winners and
    already-claimed callbacks, cancels active schedules, and invalidates enqueued
    callbacks without mutating the manager's borrowed queue. Commit this plan with
    the actor's job, permission and lifecycle changes before cancelling workers. *)
type t

val prepare
  :  state:Session_state.t
  -> mode:Agent_protocol.Session.stop_mode
  -> now:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

val is_empty : t -> bool
val deltas : t -> Session_delta.t list
val payloads : t -> Agent_protocol.Event.Durable.Payload.t list
