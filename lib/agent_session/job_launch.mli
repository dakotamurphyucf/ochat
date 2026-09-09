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

(** Validate persisted owner, ancestry and depth. Parent attempts may since have
    advanced; an event's recorded attempt must still match exactly. *)
val validate
  :  invocations:Agent_protocol.Invocation.t list
  -> events:Agent_protocol.Moderator_execution.t list
  -> jobs:Agent_protocol.Job.t list
  -> Agent_protocol.Job.t
  -> (unit, Agent_protocol.Error.t) result
