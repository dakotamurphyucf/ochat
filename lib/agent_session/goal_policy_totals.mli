(** Combined truthful per-Goal reads/fresh policy. Pure DATA only; never apply
    these gates again to a canonical attempt that already owns its ACK guard. *)
module P = Agent_protocol

module PC = Policy_canonical_usage

(* Pure composition under actual State-prefix projections. No call creates an
   Admission, Current, Manager, Run, controller, Handle or lower Reference. *)
module Context : sig
  type t

  val create
    :  owner:P.Session_ref.t
    -> catalog:Goal_catalog.t option
    -> accounts:Goal_accounts.t option
    -> ledger:Inference_ledger.t
    -> descendants:Goal_descendants.t option
    -> children:Child_contributions.t option
    -> causal:Goal_policy_contributions.t option
    -> canonical:PC.t option
    -> session_revision:int64
    -> (t, P.Error.t) result
end

module View : sig
  type t

  val local_usage : t -> Goal_usage.t
  val physical_usage : t -> Goal_usage.t
  val causal_usage : t -> Goal_usage.t
  val total_usage : t -> Goal_usage.t
  val historical_causal_unknown : t -> bool
  val physical_unknown : t -> bool
  val pending : t -> dimension:Goal_admission_policy.dimension -> int64
  val committed : t -> dimension:Goal_admission_policy.dimension -> int64
end

val capture : Context.t -> goal_id:P.Id.Goal.t -> (View.t, P.Error.t) result

val available
  :  View.t
  -> context:Goal_policy_contributions.Admission_context.t
  -> goal:P.Goal.t
  -> dimension:Goal_admission_policy.dimension
  -> (Goal_admission_policy.Count_availability.t, P.Error.t) result

val check
  :  Context.t
  -> goal:P.Goal.t
  -> context:Goal_policy_contributions.Admission_context.t
  -> now:P.Timestamp.t
  -> dimension:Goal_admission_policy.dimension
  -> (Goal_admission_policy.scoped_decision, P.Error.t) result

val check_activation
  :  Context.t
  -> goal:P.Goal.t
  -> context:Goal_policy_contributions.Admission_context.t
  -> now:P.Timestamp.t
  -> (Goal_admission_policy.decision, P.Error.t) result
(* Integrity first; same existing controls/deadline/selected-token rules.
   Ordinary_existing masks ONLY historical NEW causal absence, never physical
   uncertainty, retained known unknown metrics, malformed data or pending.
   Context is an internal closed actual route choice, not a user option.
   Count/control functions are never used again after actual H admission ACK. *)

(** Advisory fresh-activation policy for the same already captured accounting
    view, treating an open held Goal as prospectively enabled. No state is
    installed and no execution authority, receipt or reservation is created. *)
val observe_activation
  :  View.t
  -> now:P.Timestamp.t
  -> (Goal_admission_policy.decision, P.Error.t) result
