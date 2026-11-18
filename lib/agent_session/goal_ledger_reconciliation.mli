open! Core

(** Pure atomic accounting candidate for one actual canonical ledger transition.
    All retained partitions, including closed goals, reconcile removed rows from
    the same final before-ledger contribution. This grants no attribution authority. *)
type t

val prepare
  :  accounts:Goal_accounts.t option
  -> catalog:Goal_catalog.t option
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> admission:(Agent_protocol.Id.Goal.t * Goal_accounting.Admission.t) option
  -> (t, Agent_protocol.Error.t) result

val accounts : t -> Goal_accounts.t option
val changed : t -> bool

(** Bounded immutable deny-set for the existing canonical retirement owner.
    Inspect only actually bound ended rows (at most 256). Protected child data
    cannot disappear through a canonical retirement. Other errors propagate. *)
val protected_ordinals
  :  Goal_accounts.t option
  -> ledger:Inference_ledger.t
  -> (Int64.Set.t, Agent_protocol.Error.t) result
