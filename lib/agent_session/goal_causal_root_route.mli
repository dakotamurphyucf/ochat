(** Dune-private pure association validation beneath actual Actor causal route
    issuance. No value here grants permission, currentness, reservation or ACK.
    Actor authenticates SAME Preparation/selection/source-owner/controller and
    optional genuine callee ancestry before capture, resolves references outside
    its mailbox, and repeats its exact basis before consuming these results. *)
module P = Agent_protocol

module D = Agent_store.Delegation_store
module Cause = Policy_cause_data

module Evidence : sig
  type t =
    | Caller_local
    | Inherited_scope of Agent_store.Delegation_goal_origin.Scope.t
    | Forwarded of Cause.Forwarding.t
end

module Target : sig
  type t

  val target : t -> Cause.Goal_target.t
  val coverage : t -> Cause.Coverage.t
  val evidence : t -> Evidence.t
end

type t

(** Derive original full References from EVERY forwarding frame and Cause path,
    then actual callee ancestry. None only matches callee_delegation=None; Some
    must start with that exact actual Reference. The actual Actor independently
    authenticates the Checked_ancestry issuer before supplying its record list.
    Selection must cover exactly the complete Cause target vector. Bounds are
    those already captured by Cause; missing edges never become synthetic refs. *)
val capture
  :  Cause.t
  -> selection:Policy_canonical_usage.Selection.t
  -> callee_delegation:D.Reference.t option
  -> callee_ancestry:D.record list option
  -> (t, P.Error.t) result

val references : t -> D.Reference.t list
val targets : t -> Target.t list

(** Exact ordered resolved records, full pinned references, Linked/unrevoked,
    immutable suffix identity and inherited Scope propagation. No I/O or lookup
    by IDs; opaque unknown/raw children are neither moved nor rewritten. *)
val validate_records : t -> records:D.record list -> (unit, P.Error.t) result

(** Validate all selected targets belonging to the actual root session, including
    inline/covered choices. Existing root Goal/source/generation and strict GD
    branch/child-origin association remain required. Frozen episode need not be
    current/live. No remote-route issuance or budget admission occurs here. *)
val select
  :  t
  -> records:D.record list
  -> session:P.Session_ref.t
  -> generation:int
  -> delegation:D.Reference.t option
  -> catalog:Goal_catalog.t option
  -> descendants:Goal_descendants.t option
  -> children:Child_contributions.t option
  -> (Target.t list, P.Error.t) result
