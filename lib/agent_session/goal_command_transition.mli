(** Pure metadata planning, not authenticated actor authority. Only the actor
    supplies the actual principal, host session reference and captured source. *)
type decision =
  | Retained of Goal_command_receipt.t
  | Commit of
      { catalog : Goal_catalog.t
      ; receipt : Goal_command_receipt.t
      }

val prepare
  :  Goal_catalog.t
  -> request:Goal_command.t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> session:Agent_protocol.Session_ref.t
  -> generation:int
  -> current_source:Agent_protocol.Run_source.t option
  -> now:Agent_protocol.Timestamp.t
  -> (decision, Agent_protocol.Error.t) result
