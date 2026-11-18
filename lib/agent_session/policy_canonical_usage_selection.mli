(** Private per-H native association and prospective ordered coverage claim. *)
type t = Policy_canonical_usage_carrier.t

module Choice = Policy_canonical_usage_data.Choice

module Local_attempt : sig
  type t
end

module Basis : sig
  type t = private
    { accounts : string option
    ; descendants : string option
    ; catalog : string option
    }

  val equal : t -> t -> bool
  val capture : Policy_cause_data.Context.t -> (t, Agent_protocol.Error.t) result
  val to_json : t -> Jsonaf.t
end

module Selection : sig
  type accounting = t

  type t = private
    { cause : Policy_cause_data.Key.t
    ; work : Policy_cause_data.Allocation_work.t
    ; fact : Goal_descendants.Canonical.Fact.t
    ; local_attempt : Local_attempt.t option
    ; basis : Basis.t
    ; choices : Choice.t list
    }

  val check_work
    :  Policy_canonical_usage_data.t
    -> cause:Policy_cause_data.t
    -> work:Policy_cause_data.Allocation_work.t
    -> fact:Goal_descendants.Canonical.Fact.t
    -> Policy_cause_data.Context.t
    -> (unit, Agent_protocol.Error.t) result

  val verify
    :  t
    -> accounting
    -> live_prefix:Policy_cause_data.Context.t
    -> coverage_after:Policy_cause_data.Context.t
    -> before_ledger:Inference_ledger.t
    -> after_ledger:Inference_ledger.t
    -> (unit, Agent_protocol.Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  val shape : Document_schema.Shape.t

  val prepare
    :  accounting
    -> cause:Policy_cause_data.Key.t
    -> work:Policy_cause_data.Allocation_work.t
    -> live_prefix:Policy_cause_data.Context.t
    -> coverage_after:Policy_cause_data.Context.t
    -> native_admission:(Agent_protocol.Id.Goal.t * Goal_accounting.Admission.t) option
    -> before_ledger:Inference_ledger.t
    -> after_ledger:Inference_ledger.t
    -> canonical:Goal_descendants.Canonical.t
    -> occurrence:Goal_descendant_occurrence.t
    -> (t, Agent_protocol.Error.t) result

  (** Actual one-H check, not a source-wide classification. Native association
      presence is biconditional with the unique actual native Attempt account
      advance. Tracked requires the actual same-H native binding; Untracked
      requires exact native +1 Unknown settlement/full canonical fold. Missing
      association with an actual native +1 rejects, rather than charging overlay.
      Remaining targets require same-H Included or enter the sparse overlay. *)
  val choices : t -> Choice.t list

  (** Exact immutable selected subject/known coverage. DATA only: neither
      comparison authenticates source work or admits a canonical occurrence. *)
  val matches_subject
    :  t
    -> cause:Policy_cause_data.Key.t
    -> work:Policy_cause_data.Allocation_work.t
    -> occurrence:Goal_descendant_occurrence.t
    -> bool

  val equal : t -> t -> bool
end

module Admission : sig
  type accounting = t
  type t

  val create
    :  cause:Policy_cause_data.Key.t
    -> work:Policy_cause_data.Allocation_work.t
    -> live_prefix:Policy_cause_data.Context.t
    -> descendants_after:Goal_descendants.t option
    -> canonical:Goal_descendants.Canonical.t
    -> occurrence:Goal_descendant_occurrence.t
    -> (t, Agent_protocol.Error.t) result

  (** [accounts] is the actual native plan computed first by
      Canonical_usage_update.prepare with the SAME native admission tuple.
      This prospective claim is checked again against real ordered native leaves;
      it cannot select a covered Boolean, fabricate an account, or grant work. *)
  val select
    :  t
    -> accounting
    -> accounts:Goal_accounts.t option
    -> native_admission:(Agent_protocol.Id.Goal.t * Goal_accounting.Admission.t) option
    -> before:Inference_ledger.t
    -> after:Inference_ledger.t
    -> (Selection.t, Agent_protocol.Error.t) result
end
