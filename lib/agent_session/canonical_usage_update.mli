(** One pure candidate over the actual same-session canonical edge. Used by
    both inference and host-turn writers; no provider evidence is copied. *)
type t

val prepare
  :  Inference_ledger.t
  -> after:Inference_ledger.t
  -> accounts:Goal_accounts.t option
  -> catalog:Goal_catalog.t option
  -> delegated_usage:Delegated_usage.t option
  -> admission:(Agent_protocol.Id.Goal.t * Goal_accounting.Admission.t) option
  -> (t, Agent_protocol.Error.t) result

val ledger : t -> Inference_ledger.t
val accounts : t -> Goal_accounts.t option
val accounts_changed : t -> bool
val delegated_usage : t -> Delegated_usage.t option

(** Union of actual Goal and delegated-partition carrier retention deny-sets.
    Only actually ended rows are added; live work remains owned by the ledger. *)
val protected_ordinals
  :  Inference_ledger.t
  -> accounts:Goal_accounts.t option
  -> delegated_usage:Delegated_usage.t option
  -> (Core.Int64.Set.t, Agent_protocol.Error.t) result
