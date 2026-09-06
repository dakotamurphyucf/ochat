(** Bounded per-attachment event delivery. Durable overflow requires a fresh
    snapshot; recoverable live overflow may be dropped. *)

type item =
  | Durable of Agent_protocol.Event.Durable.t
  | Recoverable of Agent_protocol.Event.Recoverable.t

type t

val create : capacity:int -> t
val publish_durable : t -> Agent_protocol.Event.Durable.t -> unit
val publish_recoverable : t -> Agent_protocol.Event.Recoverable.t -> unit
val take : t -> (item, Agent_protocol.Error.t) result option
val close : t -> unit
val needs_snapshot : t -> bool
