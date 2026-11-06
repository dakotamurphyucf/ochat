(** The session-owned inputs to the pure ordered descendant validator. This
    adapter supplies actual state fields; it does not reconstruct host authority. *)
val of_state : Session_state.t -> Goal_descendants_ordered.Projection.t

val begin_
  :  Session_state.t
  -> (Goal_descendants_ordered.t, Agent_protocol.Error.t) result
