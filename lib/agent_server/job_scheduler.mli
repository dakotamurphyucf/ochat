open! Core

(** Daemon-owned dispatcher for actor-persisted background model jobs. *)

type t

(** [reconcile_recovered ~registry] durably interrupts recovered running jobs.
    Return the first actor/persistence failure so startup cannot clear an
    incomplete index-recovery marker. *)
val reconcile_recovered
  :  registry:Session_registry.t
  -> (unit, Agent_protocol.Error.t) result

val start
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> registry:Session_registry.t
  -> capacity:Job_capacity.t
  -> t

(** [cancel t job_id] cooperatively cancels the running Eio worker, if any.
    Actor state remains authoritative for whether cancellation was accepted. *)
val cancel : t -> Agent_protocol.Id.Job.t -> unit

val close : t -> unit
val is_running : t -> bool
val running_count : t -> int
