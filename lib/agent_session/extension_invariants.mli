(** Cross-record validation shared by recovery and actor transactions. *)
val invocation_event_owner
  :  events:Agent_protocol.Moderator_execution.t list
  -> Agent_protocol.Invocation.t
  -> (unit, Agent_protocol.Error.t) result

val owner
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> Agent_protocol.Id.Session.t
  -> int
  -> (unit, Agent_protocol.Error.t) result

val delivery_ready
  :  invocations:Agent_protocol.Invocation.t list
  -> Agent_protocol.Delivery.t
  -> (unit, Agent_protocol.Error.t) result

val validate
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> invocations:Agent_protocol.Invocation.t list
  -> subscriptions:Agent_protocol.Subscription.t list
  -> deliveries:Agent_protocol.Delivery.t list
  -> jobs:Agent_protocol.Job.t list
  -> schedules:Agent_protocol.Schedule.t list
  -> (unit, Agent_protocol.Error.t) result
