(** Reapply current transcript/security disclosure to retained public results.
    A cached Full body may become Visible/Redacted; a narrow body is never
    reconstructed into canonical input or upgraded from unavailable evidence. *)
val view
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Pending_query.View.t
  -> (Agent_protocol.Pending_query.View.t, Agent_protocol.Error.t) result

val outcome
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Pending_query.Outcome.t
  -> (Agent_protocol.Pending_query.Outcome.t, Agent_protocol.Error.t) result

val control
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Pending_control.Result.t
  -> (Agent_protocol.Pending_control.Result.t, Agent_protocol.Error.t) result
