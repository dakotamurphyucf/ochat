open! Core

(** Presentation wakeups for one real attached-session reader. This owner never
    acquires a connection, attachment, notification lease, actor or command key.
    Creation, observation and awaiting are confined to the creating domain. *)
type t

val create : Agent_protocol.Id.Session.t -> t

(** Supply as the existing [Session_handle.attach] on_update callback. Only
    validate session affiliation and wake the waiter; retain no projection log
    and perform no I/O or user callback. The actual handle remains the owner of
    the current Projection and bounded live state. Foreign observations become
    a retained typed failure, not an exception swallowed by the reader callback. *)
val observe : t -> Projection.t -> unit

(** Await an operation actually admitted by the caller's existing command. [after]
    is that command's returned durable mutation position, so an earlier operation
    or idle snapshot cannot be mistaken for the requested terminal. At most one
    waiter is allowed per receiver; another waiter returns Conflict.

    Read authoritative state from [handle], racing its actual [await_closed].
    Reader failure returns the retained [last_error] or stale projection error;
    normal closure before completion returns Interrupted. Neither requires an
    on_update notification. Every race fiber is cancelled/joined before return.

    [on_view] runs in the waiting caller, outside the notification reader. It sees
    existing bounded Draft/activity state, including honest gaps/replacements;
    it does not receive fabricated provider deltas. Callback exceptions and caller
    cancellation propagate unchanged. Recheck cancellation, handle failure and
    current state after the callback and immediately before result delivery.

    Success returns an actual terminal Projection for [operation], after [after],
    while that Projection is Current and no different operation has superseded
    it. Completed/Failed/Cancelled/Interrupted remain the actual typed operation
    state; success here means terminal observation, not successful model execution.
    At/after the command position, a requested active operation keeps waiting;
    a different operation conflicts. No active operation and no retained terminal
    returns Snapshot_required; an idle/stopped replacement never fabricates completion.

    This operation issues no activation/send/seed/export command. It does not
    detach or close the borrowed handle; its caller retains that responsibility.
    Cross-domain misuse raises Invalid_argument before touching receiver state. *)
val await_terminal
  :  t
  -> handle:Session_handle.t
  -> operation:Agent_protocol.Id.Operation.t
  -> after:Agent_protocol.Mutation_result.t
  -> on_view:(Projection.t -> unit)
  -> (Projection.t, Agent_protocol.Error.t) Result.t
