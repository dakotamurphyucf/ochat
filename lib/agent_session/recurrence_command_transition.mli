(** Pure metadata planning after the actual actor has established operator,
    source and lifecycle authority. Scalars here cannot establish that authority.
    A retained receipt grants no new work. Old epochs must be reconciled before
    accepting a fresh capture/enable; no ordinary edit resurrects an old birth. *)
type decision =
  | Retained of Recurrence_command_receipt.t
  | Commit of
      { catalog : Recurrence_catalog.t
      ; receipt : Recurrence_command_receipt.t
      }

val prepare
  :  Recurrence_catalog.t
  -> request:Recurrence_command.t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> session:Agent_protocol.Session_ref.t
  -> generation:int
  -> current_source:Agent_protocol.Run_source.t option
  -> current_lifecycle_revision:Agent_protocol.Session_lifecycle.Revision.t option
  -> now:Agent_protocol.Timestamp.t
  -> (decision, Agent_protocol.Error.t) result
