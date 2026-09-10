(** Cross-record validation of source-bound schedules. Legacy schedules retain
    their original contract and never acquire current-source authority. *)
val validate
  :  invocations:Agent_protocol.Invocation.t list
  -> events:Agent_protocol.Moderator_execution.t list
  -> subscriptions:Agent_protocol.Subscription.t list
  -> Agent_protocol.Schedule.t
  -> (unit, Agent_protocol.Error.t) result
