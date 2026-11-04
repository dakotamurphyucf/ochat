(** Actor-local provisional launches. Records become runnable only when their
    owner saves them with its outcome/checkpoint and calls commit. This registry
    performs no caller authorization; Session_actor owns live-scope validation. *)
type capacity =
  { publish : unit -> unit
  ; abort : unit -> unit
  }

type 'payload t

val create : unit -> 'payload t
val is_empty : 'payload t -> bool

val stage
  :  'payload t
  -> job:Agent_protocol.Job.t
  -> payload:'payload
  -> capacity:capacity
  -> (unit, Agent_protocol.Error.t) result

val contains
  :  'payload t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Job.t
  -> bool

val find
  :  'payload t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Job.t
  -> (Agent_protocol.Job.t option, Agent_protocol.Error.t) result

(** Retain a cancelled ticket for its owner's eventual acknowledgement while
    immediately releasing capacity. A committed cancelled ticket never publishes
    its reservation. Missing IDs return None for the durable-job lookup. *)
val cancel
  :  'payload t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Job.t
  -> now:Agent_protocol.Timestamp.t
  -> (Agent_protocol.Job.t option, Agent_protocol.Error.t) result

val select
  :  'payload t
  -> owner:Agent_protocol.Job.launch_owner
  -> ids:Agent_protocol.Id.Job.t list
  -> (unit, Agent_protocol.Error.t) result

val selected
  :  'payload t
  -> owner:Agent_protocol.Job.launch_owner
  -> Agent_protocol.Job.t list

(** Selected entries and their original opaque process-local payloads, in the
    same order as [selected]. The registry does not interpret payload authority;
    the actor validates and installs selected custody before publishing capacity. *)
val selected_payloads
  :  'payload t
  -> owner:Agent_protocol.Job.launch_owner
  -> (Agent_protocol.Job.t * 'payload) list

(** Callbacks must only publish/release capacity and are expected to be
    infallible. Commit follows successful durable owner/job persistence, never
    before it. Unselected starts are aborted; catch rollback can abort a
    provisional start earlier. After detaching the entries, all prescribed
    actions run under cancellation protection even if a callback unexpectedly
    raises. The first exception and original backtrace re-raise after draining;
    a failed acknowledged publish is never replaced by an abort. *)
val commit : 'payload t -> owner:Agent_protocol.Job.launch_owner -> unit

val abort_owner : 'payload t -> owner:Agent_protocol.Job.launch_owner -> unit
val abort_all : 'payload t -> unit

val abort
  :  'payload t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Job.t
  -> (unit, Agent_protocol.Error.t) result
