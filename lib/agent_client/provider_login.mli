(** Shared operator login calls on the connected runtime host. Credentials and
    provider exchanges stay on that host; this client owns no credential store,
    background polling or notification consumer. Ordinary mutations use the
    Connection original-command uncertainty and reconciliation discipline. *)

val status
  :  Connection.t
  -> Agent_protocol.Provider_operator.Status_request.t
  -> (Agent_protocol.Provider_operator.Status_result.t, Agent_protocol.Error.t) result

val begin_login
  :  Connection.t
  -> Agent_protocol.Provider_operator.Login_request.t
  -> (Agent_protocol.Provider_operator.Flow_ref.t, Agent_protocol.Error.t) result

(** Explicit private consumer for an owner-authorized, expiring live challenge.
    Display using Private_challenge accessors; never persist it in ordinary
    session state, logs or receipts. The host rechecks ownership and expiry. *)
val challenge
  :  Connection.t
  -> Agent_protocol.Provider_operator.Challenge_request.t
  -> (Agent_protocol.Provider_operator.Private_challenge.t, Agent_protocol.Error.t) result

val cancel
  :  Connection.t
  -> Agent_protocol.Provider_operator.Cancel_request.t
  -> (Agent_protocol.Provider_operator.Flow_result.t, Agent_protocol.Error.t) result

val logout
  :  Connection.t
  -> Agent_protocol.Provider_operator.Logout_request.t
  -> (Agent_protocol.Provider_operator.Logout_result.t, Agent_protocol.Error.t) result
