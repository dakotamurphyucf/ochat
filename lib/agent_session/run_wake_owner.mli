(** Validate an exact retained wake occurrence against its actual owner. Historical
    unbound deliveries never acquire the current subscription epoch or source.
    Validation does not consume the wake, run a callback or change custody. *)
val validate
  :  Session_state.t
  -> run:Agent_protocol.Run.t
  -> wake:Agent_protocol.Run_wake.t
  -> (unit, Agent_protocol.Error.t) result

(** Match only job/timer private queue frames retained in an actual Internal_event
    execution receipt. This checks identity/source/generation and immutable frame
    provenance only. The actor must first run its existing delivery retirement
    guard (including current expiry and duplicate claims), and recheck retained
    run authority before committing the wake with the new execution receipt.
    Subscription deliveries instead use [Run_transition.admit_turn], which binds
    their actual delivery to the admitted operation and consumes once. *)
val claim_matches
  :  Session_state.t
  -> run:Agent_protocol.Run.t
  -> wake:Agent_protocol.Run_wake.t
  -> executing:Agent_protocol.Moderator_execution.t
  -> (bool, Agent_protocol.Error.t) result
