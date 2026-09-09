(** Actor-local provisional launches. Records become runnable only when their
    owner saves them with its outcome/checkpoint and calls commit. This registry
    performs no caller authorization; Session_actor owns live-scope validation. *)
type capacity =
  { publish : unit -> unit
  ; abort : unit -> unit
  }

type t

val create : unit -> t

val stage
  :  t
  -> job:Agent_protocol.Job.t
  -> capacity:capacity
  -> (unit, Agent_protocol.Error.t) result

val contains
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Job.t
  -> bool

val find
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Job.t
  -> (Agent_protocol.Job.t option, Agent_protocol.Error.t) result

(** Retain a cancelled ticket for its owner's eventual acknowledgement while
    immediately releasing capacity. A committed cancelled ticket never publishes
    its reservation. Missing IDs return None for the durable-job lookup. *)
val cancel
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Job.t
  -> now:Agent_protocol.Timestamp.t
  -> (Agent_protocol.Job.t option, Agent_protocol.Error.t) result

val select
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> ids:Agent_protocol.Id.Job.t list
  -> (unit, Agent_protocol.Error.t) result

val selected : t -> owner:Agent_protocol.Job.launch_owner -> Agent_protocol.Job.t list

(** Callbacks must be infallible and only publish/release capacity. Commit follows
    successful durable owner/job persistence, never before it. Unselected starts
    are aborted; catch rollback can abort a provisional start earlier. *)
val commit : t -> owner:Agent_protocol.Job.launch_owner -> unit

val abort_owner : t -> owner:Agent_protocol.Job.launch_owner -> unit
val abort_all : t -> unit

val abort
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Job.t
  -> (unit, Agent_protocol.Error.t) result
