(** Logical group CRUD; no session activation, workspace configuration or
    membership. Current scopes and principal ownership apply on every request. *)
val handles : Agent_protocol.Command.t -> bool

val handle
  :  Agent_store.Organization_store.t
  -> server_id:Agent_protocol.Id.Server.t
  -> principal:Agent_protocol.Principal.t
  -> now:Agent_protocol.Timestamp.t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result

(** Read-only reconciliation rechecks current mutation scope and target ownership. *)
val receipt
  :  Agent_store.Organization_store.t
  -> server_id:Agent_protocol.Id.Server.t
  -> principal:Agent_protocol.Principal.t
  -> now:Agent_protocol.Timestamp.t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Command_receipt.t, Agent_protocol.Error.t) result
