(** Pure rendering-neutral client projection reducer. *)

type t

val install_snapshot : Agent_protocol.Snapshot.t -> t
val snapshot : t -> Agent_protocol.Snapshot.t

val apply_event
  :  t
  -> Agent_protocol.Event.Durable.t
  -> (t, Agent_protocol.Error.t) result

val apply_live_event
  :  t
  -> Agent_protocol.Event.Recoverable.t
  -> (t, Agent_protocol.Error.t) result

val live_events : t -> Agent_protocol.Event.Recoverable.t list

(** Last observed durable terminal operation, retained across projection
    coalescing until the next operation starts. Snapshot installation clears
    this transient observation; it is not invented from an idle snapshot. *)
val terminal_operation : t -> Agent_protocol.Operation.t option
