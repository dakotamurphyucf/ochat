open! Core

(** Supervises accepted queued session starts under the daemon switch. *)

type t

val start
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> registry:Session_registry.t
  -> queue:Agent_session.Start_queue.t
  -> t

val close : t -> unit
val is_running : t -> bool

(** [seed_recovered registry queue] reconstructs queue tickets from durable
    queued lifecycle state. *)
val seed_recovered
  :  registry:Session_registry.t
  -> queue:Agent_session.Start_queue.t
  -> (unit, Agent_protocol.Error.t) result
