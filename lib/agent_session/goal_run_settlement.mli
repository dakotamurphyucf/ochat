(** Pure final-candidate convergence, after source retirement/holds and before
    persistence. Actual Run terminal evidence wins; no worker or scheduler is
    created. Closed abandoned goals remain immutable. Exact replay is a no-op. *)
val prepare
  :  Session_state.t
  -> delta:Session_delta.t
  -> (Session_delta.t, Agent_protocol.Error.t) result
