open! Core

(** Daemon-owned Eio timer service for actor-persisted permission deadlines. *)

type t

val start : sw:Eio.Switch.t -> clock:_ Eio.Time.clock -> registry:Session_registry.t -> t
val close : t -> unit
val is_running : t -> bool

(** [process registry now] expires every loaded permission whose persisted
    deadline is at or before [now]. Actor compare-and-set semantics resolve
    races with clients and local fallback timers. *)
val process : Session_registry.t -> Agent_protocol.Timestamp.t -> unit
