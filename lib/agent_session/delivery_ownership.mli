(** Validate retained source/creator references and source-bound subscription
    correlation. Active caller and disclosure checks belong to actor admission;
    historical records retain their original generation and source. *)
val validate
  :  invocations:Agent_protocol.Invocation.t list
  -> events:Agent_protocol.Moderator_execution.t list
  -> subscriptions:Agent_protocol.Subscription.t list
  -> Agent_protocol.Delivery.t
  -> (unit, Agent_protocol.Error.t) result
