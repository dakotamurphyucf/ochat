(** Route only actual startup/owned operation/job callbacks to one retained run.
    Observation and generic unrelated internal events do not acquire authority.
    This selection grants no native scope: the actor separately checks its live
    borrow, current host authorization and exact source installation. *)
val select
  :  Session_state.t
  -> executing:Agent_protocol.Moderator_execution.t
  -> (Agent_protocol.Run.t option, Agent_protocol.Error.t) result
