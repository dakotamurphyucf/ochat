(** One preserved whole-canonical-ledger partition of an actual delegated child.
    No foreign inference Handle, Goal grant or permission is encoded here. *)
type t

val reference : t -> Agent_store.Delegation_store.Reference.t

val create
  :  reference:Agent_store.Delegation_store.Reference.t
  -> ledger:Inference_ledger.t
  -> (t, Agent_protocol.Error.t) result

val validate : t -> ledger:Inference_ledger.t -> (unit, Agent_protocol.Error.t) result

val reconcile
  :  t
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> (t, Agent_protocol.Error.t) result

val usage
  :  t
  -> ledger:Inference_ledger.t
  -> (Goal_usage.t, Agent_protocol.Error.t) result

val of_document : Document_schema.Document.t -> (t, Agent_protocol.Error.t) result
val to_document : t -> (Document_schema.Document.t, Agent_protocol.Error.t) result
val adopt : t -> incoming:t -> (t, Agent_protocol.Error.t) result

val validate_transition
  :  t
  -> incoming:t
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> (unit, Agent_protocol.Error.t) result

(** Known aggregate growth is reserved once in the complete carrier. Future
    data that blocks such changes makes canonical retirement ineligible before
    the ledger chooses a row. No row identity is duplicated in this partition. *)
val can_retire : t -> (bool, Agent_protocol.Error.t) result

val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result
val limits : Document_schema.Limits.t
