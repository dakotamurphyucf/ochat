open! Core

(** Daemon-owned timer service for durable one-shot ChatML schedules. Session
    actors remain authoritative for every claim and terminal transition. *)

type t

(** [reconcile_recovered] expires overdue subscriptions before applying configured
    misfire policy to schedules that were already overdue at daemon startup.
    Propagate actor/persistence failures
    rather than allowing startup to clear an incomplete recovery marker. *)
val reconcile_recovered
  :  registry:Session_registry.t
  -> startup_time:Agent_protocol.Timestamp.t
  -> (unit, Agent_protocol.Error.t) result

(** Each pass also sweeps subscription deadlines through the actor, independently
    of any earlier callback holding this entry's runtime. *)
val start : sw:Eio.Switch.t -> clock:_ Eio.Time.Mono.t -> registry:Session_registry.t -> t

val close : t -> unit
val is_running : t -> bool
