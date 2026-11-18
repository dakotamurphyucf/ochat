(** Invalid maximum-size witnesses only; never decoded as receipt/ACK evidence. *)
val capture
  :  Goal_policy_contributions_data.t
  -> current:Document_schema.Document.t
  -> (Capacity_growth.t * int, Agent_protocol.Error.t) result
