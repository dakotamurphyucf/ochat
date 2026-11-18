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

(** An immutable complete document and retirement-capacity result prepared from
    the same child. Capturing includes the intrinsic capacity checks of
    [to_document]; it grants no accounting or inference authority. *)
module Export : sig
  type source = t
  type t

  val prepare : source -> (t, Agent_protocol.Error.t) result
  val document : t -> Document_schema.Document.t
  val reserved_growth : t -> Capacity_growth.t
end

val of_document : Document_schema.Document.t -> (t, Agent_protocol.Error.t) result
val to_document : t -> (Document_schema.Document.t, Agent_protocol.Error.t) result
val apply : t -> Goal_accounting.Plan.t -> (t, Agent_protocol.Error.t) result
val adopt : t -> incoming:t -> (t, Agent_protocol.Error.t) result
val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result
val can_retire : t -> ordinal:int64 -> (bool, Agent_protocol.Error.t) result
val limits : Document_schema.Limits.t
