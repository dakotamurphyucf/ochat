(** Immutable Actor-issued association below Caller and State in the dependency
    graph. Dune-private: no decoder, permission, user callback or runtime owner.
    A failed capture is retained in Caller as Error, never this unrelated case. *)
open! Core

module P = Agent_protocol
module Cause = Policy_cause_data

module Work : sig
  type t =
    | Root_operation of P.Id.Operation.t
    | Member of Cause.Allocation_work.t
  [@@deriving equal]
end

module Registered : sig
  type t =
    { owner : P.Session_ref.t
    ; source : Cause.Caller_source.t
    ; source_owner : Runtime_source_owner.Identity.t
    ; work : Work.t
    ; origin : P.Inference_origin.t
    ; local : Cause.Goal_target.t option
    ; inherited : Agent_store.Delegation_goal_origin.t option
    ; incoming : Cause.Forwarding.t option
    }

  (** Full immutable source/association and complete raw forwarding equality.
      The actual Run/Unbound origin is retained even without a Goal. Missing
      historical classification rejects; it is never changed to Unbound.
      None inherited retains unknown historical origin. Root unrelated origin
      is explicit Some unrelated, issued only from its actual no-delegation Spec.
      None incoming means the strict native ancestry lookup positively found no
      cause. Neither absence grants permission or substitutes an ambient owner. *)
  val equal : t -> t -> bool
end

type t =
  | Standalone_unrelated of Policy_standalone_source.t
  | Registered of Registered.t

(** Standalone is issued only for a real controller-free, nondelegated Actor
    without local Goal, inherited scopes or causal component. The payload retains
    exact subject, Run/Unbound and actual source facts. No server identity is
    fabricated. Same-source Book ACKs do not change these immutable facts. *)
val equal : t -> t -> bool
