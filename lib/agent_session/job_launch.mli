(** Derive immutable launch ancestry from retained actor records. Live callback
    admission remains the actor's responsibility; these functions grant no tool
    authority and never reserve or execute work. Invocation wrappers do not reset
    depth; only crossing into a parent job adds a level. *)
val derive
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> invocations:Agent_protocol.Invocation.t list
  -> events:Agent_protocol.Moderator_execution.t list
  -> jobs:Agent_protocol.Job.t list
  -> owner:Agent_protocol.Job.launch_owner
  -> (Agent_protocol.Job.launch, Agent_protocol.Error.t) result

(** Derive a NEW direct Model Job birth from the exact current immediate Job
    attempt. This validates immutable ancestry/depth, not live producer authority.
    The optional source is copied exactly from that immediate parent's launch,
    including absence. The Actor separately proves the running scope and original. *)
val derive_from_job
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> jobs:Agent_protocol.Job.t list
  -> job_id:Agent_protocol.Id.Job.t
  -> attempt:int
  -> (Agent_protocol.Job.launch, Agent_protocol.Error.t) result

(** Validate persisted owner, ancestry and depth. Historical parent attempts may
    since have advanced; a stored direct Job or event attempt stays frozen.
    Self/cycles, exact optional source and exact depth remain strict. *)
val validate
  :  invocations:Agent_protocol.Invocation.t list
  -> events:Agent_protocol.Moderator_execution.t list
  -> jobs:Agent_protocol.Job.t list
  -> Agent_protocol.Job.t
  -> (unit, Agent_protocol.Error.t) result

(** Validate a subscription's optional retained creating job attempt against its
    invocation/event ancestry. Historical parent attempts may have advanced. *)
val validate_subscription
  :  invocations:Agent_protocol.Invocation.t list
  -> events:Agent_protocol.Moderator_execution.t list
  -> jobs:Agent_protocol.Job.t list
  -> Agent_protocol.Subscription.t
  -> (unit, Agent_protocol.Error.t) result
