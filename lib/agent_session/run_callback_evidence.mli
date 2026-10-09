(** Capture an owned callback's actual prospective terminal receipt. The actor
    supplies the completed receipt it has just validated for this same borrow,
    before committing both receipt and run evidence atomically. Identity, source,
    generation and the full context must match; an existing proof cannot change.
    A callback that is not owned by this run adds no evidence. No effects run. *)
val capture
  :  Agent_protocol.Run.t
  -> executing:Agent_protocol.Moderator_execution.t
  -> completed:Agent_protocol.Moderator_execution.t
  -> (Agent_protocol.Run_work.Terminal.t list, Agent_protocol.Error.t) result
