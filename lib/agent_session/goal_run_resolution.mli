(** Pure actual-evidence outcome selection shared by source/reset/recovery and
    normal terminal convergence. Supply the actual prospective Run index after
    retirement. Each changed Goal advances once; all share one catalog revision.
    No user receipt, scheduling or execution authority is constructed. *)
val plan_catalog
  :  Goal_catalog.t
  -> runs:Run_state.t option
  -> hold_for_review:bool
  -> (Goal_catalog.t option, Agent_protocol.Error.t) result
