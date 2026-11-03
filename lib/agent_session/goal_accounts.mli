(** At most sixteen stable accounting partitions, with at most 256 canonical
    bindings in total and 4 MiB complete encoded custody. This is storage, not
    attribution authority. Missing historical accounting remains absent. *)
type t

val empty : t
val documents : t -> Goal_accounting_document.t list
val find : t -> Agent_protocol.Id.Goal.t -> Goal_accounting_document.t option

val bootstrap
  :  t
  -> goal:Agent_protocol.Goal.t
  -> ledger:Inference_ledger.t
  -> (t, Agent_protocol.Error.t) result

val apply : t -> Goal_accounting.Plan.t -> (t, Agent_protocol.Error.t) result

val validate
  :  t
  -> catalog:Goal_catalog.t option
  -> ledger:Inference_ledger.t
  -> runs:Run_state.t option
  -> (unit, Agent_protocol.Error.t) result

val adopt : t -> incoming:t -> (t, Agent_protocol.Error.t) result
val can_retire : t -> ordinal:int64 -> (bool, Agent_protocol.Error.t) result
val to_jsonaf : t -> (Jsonaf.t, Agent_protocol.Error.t) result
val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
val limits : Document_schema.Limits.t
val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result
val validate_encoded_capacity : t -> Jsonaf.t -> (unit, Agent_protocol.Error.t) result

(** Final ordered-batch evidence check, independent of actor authorization.
    New partitions require a pre-episode Goal and exact before-ledger bootstrap;
    existing settled changes must derive from actual removed canonical rows. *)
val validate_transition
  :  previous:t option
  -> incoming:t option
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> previous_catalog:Goal_catalog.t option
  -> catalog:Goal_catalog.t option
  -> (unit, Agent_protocol.Error.t) result
