(** Capture source installation from the actual validated checkpoint. This pure
    result contains durable facts only: no Run, receipt or execution permission.
    The caller must own checkpoint admission and commit the index atomically with
    its operation. Matching installation preserves its epoch. *)
val capture
  :  Session_state.t
  -> (Run_state.t * Agent_protocol.Run_source.t, Agent_protocol.Error.t) result
