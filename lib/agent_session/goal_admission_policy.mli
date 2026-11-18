(** Fresh admission policy over the exact stable account and current canonical
    evidence. No permission, mutation or provider submission is performed. An
    already admitted attempt uses its own final guard, not this check again. *)
type dimension =
  | Attempt
  | Host_turn
[@@deriving equal, sexp_of]

type decision =
  | Allow
  | Hold of Agent_protocol.Goal.Hold.t
[@@deriving equal, sexp_of]

(** Count ceilings apply before increment. Missing/contradictory account evidence
    is an error, including for policies without a token ceiling. Selected token
    uncertainty holds new work; no-token policies preserve labelled uncertainty
    without imposing such a hold. Existing holds are returned unchanged. *)
val check
  :  Agent_protocol.Goal.t
  -> accounting:Goal_accounting.t
  -> ledger:Inference_ledger.t
  -> now:Agent_protocol.Timestamp.t
  -> dimension:dimension
  -> (decision, Agent_protocol.Error.t) result

(** Global new-activation eligibility over actual stable accounting/canonical
    usage: Enabled control, deadline and selected-token policy. No attempt or
    host-turn count ceiling is tested or incremented. Those belong only to
    [check] at their actual fresh admission. Never reuse this to validate an
    already-admitted allocation or continuation. *)
val check_activation
  :  Agent_protocol.Goal.t
  -> accounting:Goal_accounting.t
  -> ledger:Inference_ledger.t
  -> now:Agent_protocol.Timestamp.t
  -> (decision, Agent_protocol.Error.t) result

(** Compare the immutable attribution and configured budget/objective policy of
    two revisions of the same Goal for an already admitted allocation. Lifecycle
    state is deliberately separate: the actor distinguishes an automatic
    admission hold from an explicit operator state change. No live allocation
    is authenticated or revoked by this pure comparison. *)
val same_allocation_policy : Agent_protocol.Goal.t -> Agent_protocol.Goal.t -> bool

(** Read-only disjoint descendant subtotal and its explicit historical/unobserved
    coverage. These projections grant no authority or provenance. The actual
    State adapter obtains them from accepted root-scoped child contributions. *)
module Descendants : sig
  type t =
    { usage : Goal_usage.t
    ; coverage_unknown : bool
    }
end

module Count_availability : sig
  type t =
    | Available
    | Reserved
    | Exhausted
    | Unknown
  [@@deriving equal, sexp_of]
end

module Deferred : sig
  type t =
    | Reserved
    | Unknown
  [@@deriving equal, sexp_of]
end

type scoped_decision =
  | Decided of decision
  | Deferred of Deferred.t

(** SAME account/ledger validation, control and deadline precede count policy;
    selected combined observed usage follows it. Only known Exhausted creates a
    count Hold. Pending/unknown count capacity defers without any durable Hold.
    A known selected subtotal at ceiling holds even with incomplete coverage;
    below ceiling, selected token uncertainty also holds. No-token policies do
    not turn coverage uncertainty into a token gate. *)
val check_scoped
  :  Agent_protocol.Goal.t
  -> accounting:Goal_accounting.t
  -> ledger:Inference_ledger.t
  -> descendants:Descendants.t
  -> availability:Count_availability.t
  -> now:Agent_protocol.Timestamp.t
  -> dimension:dimension
  -> (scoped_decision, Agent_protocol.Error.t) result

(** NEW activation eligibility including accepted descendants, without any
    dimension count check. Existing [check_activation] retains its own-only
    model contract. Already-admitted work uses neither activation API. *)
val check_scoped_activation
  :  Agent_protocol.Goal.t
  -> accounting:Goal_accounting.t
  -> ledger:Inference_ledger.t
  -> descendants:Descendants.t
  -> now:Agent_protocol.Timestamp.t
  -> (decision, Agent_protocol.Error.t) result
