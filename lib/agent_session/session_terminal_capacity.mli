(** Shared numeric frontiers for known live Run obligations. Rechecking every
    candidate prevents unrelated updates from spending terminal/recovery credits.
    [additional_steps] and [additional_events] reserve nonnegative credits for
    independent owners, additively with Run obligations even when no Run exists.
    No unbounded future workflow or unrelated host lifecycle is promised. *)
val validate
  :  index:Run_state.t option
  -> jobs:Agent_protocol.Job.t list
  -> permissions:Agent_protocol.Permission.t list
  -> schedules:Agent_protocol.Schedule.t list
  -> additional_steps:int
  -> additional_events:int
  -> revision:int64
  -> transaction_sequence:int64
  -> event_sequence:int64
  -> (unit, Agent_protocol.Error.t) result
