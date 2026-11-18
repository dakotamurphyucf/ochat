(** Factual recovery of already retained work. These operations do not dispatch
    new jobs or schedules and apply equally to selected and metadata owners.
    Cancellation and persistence failures propagate unchanged. *)
val jobs
  :  Session_registry.entry
  -> max_count:int
  -> max_total_bytes:int
  -> (unit, Agent_protocol.Error.t) result

val schedules
  :  Session_registry.entry
  -> startup_time:Agent_protocol.Timestamp.t
  -> (unit, Agent_protocol.Error.t) result
