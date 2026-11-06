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

(** Ordered validation DATA only; no actor/source authority. *)
module Ordered_bootstrap : sig
  type accounts = t
  type t

  (** Capture immutable transaction-origin accounting and before-ledger. The
      catalog is the actual previously accepted prefix, not a supplied Goal.
      Does not bootstrap any partition or validate a historical snapshot anew. *)
  val begin_
    :  previous:accounts option
    -> before:Inference_ledger.t
    -> catalog:Goal_catalog.t option
    -> t

  type step =
    | Catalog of Goal_catalog.t
    | Accounts of accounts
    | Replacement of
        { catalog : Goal_catalog.t option
        ; accounts : accounts option
        }

  (** Consume actual ordered leaf projections. Catalog edges retain the existing
      exact Goal_catalog validation. Accounts records a genuinely new partition
      only against that then-current catalog and Child.create's exact immutable
      before-ledger bootstrap. Replacement is one endpoint edge, never inferred
      sequential creation, and does not reset already captured evidence.
      Existing partitions retain original-to-final Stored validation. *)
  val advance : t -> step -> (t, Agent_protocol.Error.t) result

  (** Same final checks as validate_transition, with the narrow additional
      captured creation Goal for each genuinely new partition. Incoming new
      Stored accounting must still equal the original before-ledger bootstrap;
      all original global admission, old Stored/retirement and adoption checks
      remain. Never falls back to a fabricated Held projection. *)
  val check
    :  t
    -> incoming:accounts option
    -> after:Inference_ledger.t
    -> catalog:Goal_catalog.t option
    -> (unit, Agent_protocol.Error.t) result
end
