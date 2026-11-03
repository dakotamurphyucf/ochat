(** A bounded universal document of stored accounting claims. Decoding grants no
    inference Handle or authority. Resolve only against the exact canonical ledger.
    Complete unknown fields survive edits; protected binding removal is ineligible. *)
type t

val create
  :  Agent_protocol.Goal.t
  -> ledger:Inference_ledger.t
  -> (t, Agent_protocol.Error.t) result

val stored : t -> Goal_accounting.Stored.t

val resolve
  :  t
  -> goal:Agent_protocol.Goal.t
  -> ledger:Inference_ledger.t
  -> (Goal_accounting.t, Agent_protocol.Error.t) result

val of_document : Document_schema.Document.t -> (t, Agent_protocol.Error.t) result
val to_document : t -> (Document_schema.Document.t, Agent_protocol.Error.t) result
val apply : t -> Goal_accounting.Plan.t -> (t, Agent_protocol.Error.t) result
val adopt : t -> incoming:t -> (t, Agent_protocol.Error.t) result
val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result
val can_retire : t -> ordinal:int64 -> (bool, Agent_protocol.Error.t) result
val limits : Document_schema.Limits.t
