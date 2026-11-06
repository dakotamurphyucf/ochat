type t

val capture
  :  Goal_descendants_data.t
  -> current:Document_schema.Document.t
  -> (t, Agent_protocol.Error.t) result

val growth : t -> Capacity_growth.t
val required_depth : t -> int

val reserved_growth
  :  Goal_descendants_data.t
  -> current:Document_schema.Document.t
  -> (Capacity_growth.t, Agent_protocol.Error.t) result
