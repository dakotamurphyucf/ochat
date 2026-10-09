open! Core

(** Supervises accepted queued session starts under the daemon switch. *)

type t

(** Internal composition: false constructs a stopped service and starts no fiber. *)
val start_controlled
  :  enabled:bool
  -> sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> registry:Session_registry.t
  -> queue:Agent_session.Start_queue.t
  -> resume_initial_starts:(unit -> unit)
  -> t

val start
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> registry:Session_registry.t
  -> queue:Agent_session.Start_queue.t
  -> resume_initial_starts:(unit -> unit)
  -> t

val close : t -> unit

(** Close admission, cancel this service's actual owned fiber, and wait for its
    callback/finalizers to finish before retiring registry actors. Disabled services
    finish immediately. Joining is cancellation-protected; callback cancellation
    remains cancellation and must never become a permanent startup failure. Existing
    protected startup sections must finish before the callback can acknowledge
    cancellation; this join does not impose an independent timeout on them. *)
val close_and_wait : t -> unit

val is_running : t -> bool

(** [seed_recovered registry queue] reconstructs queue tickets from durable
    queued lifecycle state. *)
val seed_recovered
  :  registry:Session_registry.t
  -> queue:Agent_session.Start_queue.t
  -> (unit, Agent_protocol.Error.t) result
