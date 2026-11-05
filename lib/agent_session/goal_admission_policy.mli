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
