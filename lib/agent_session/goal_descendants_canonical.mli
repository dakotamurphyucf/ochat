type t =
  | Attempt of Inference_ledger.Handle.t * Inference_ledger.tracking
  | Host_turn of
      Agent_protocol.Operation.t
      * Inference_ledger.Turn_handle.t
      * Inference_ledger.tracking

module Fact : sig
  type t

  val occurrence : t -> Goal_descendant_occurrence.t
  val tracking : t -> Inference_ledger.tracking
  val operation : t -> Agent_protocol.Operation.t option
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  val shape : Document_schema.Shape.t
end

val check
  :  before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> canonical:t
  -> occurrence:Goal_descendant_occurrence.t
  -> (Fact.t, Agent_protocol.Error.t) result

val validate_fact
  :  before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> fact:Fact.t
  -> (unit, Agent_protocol.Error.t) result
