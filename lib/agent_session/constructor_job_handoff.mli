(** Factual constructor checkpoint planning after the actual owner has selected
    ordinary source retirement. Validates complete queue bounds separately from
    known snapshot/resource equality, preserving the full original checkpoint.
    The optional index contains source installation only, never a new Run or
    execution permission. Caller still owns its exact constructor subset and ACK. *)
val prepare
  :  Session_state.t
  -> manager:Chat_response.Moderator_manager.t option
  -> (Run_state.t option * Job_custody.Moderator_binding.t, Agent_protocol.Error.t) result
