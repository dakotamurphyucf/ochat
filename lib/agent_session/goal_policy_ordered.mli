(** Private ordered DATA validation. Never authorizes a caller, proves a writer
    ACK or reconstructs a live original. Actual whole-delta selection is external. *)
module P = Agent_protocol

module Book = Goal_policy_contributions
module PC = Policy_canonical_usage

module Projection : sig
  type t

  val create
    :  session_id:P.Id.Session.t
    -> session_revision:int64
    -> catalog:Goal_catalog.t option
    -> accounts:Goal_accounts.t option
    -> ledger:Inference_ledger.t
    -> descendants:Goal_descendants.t option
    -> children:Child_contributions.t option
    -> causal:Book.t option
    -> canonical:PC.t option
    -> (t, P.Error.t) result

  (** Consensus of all actual full-owner facts, none when genuinely absent.
      No future delta, first-fact override or default server identity. *)
  val owner : t -> P.Session_ref.t option
end

type t

val begin_ : Projection.t -> (t, P.Error.t) result

type step =
  | Catalog of Goal_catalog.t option
  | Accounts of Goal_accounts.t option
  | Canonical of
      { before : Inference_ledger.t
      ; after : Inference_ledger.t
      ; policy : PC.Update.t option
      }
  | Descendants of Goal_descendants.Update.t
  | Children of Child_contributions.t option
  | Canonical_policy of PC.Update.t
  | Update of Book.Update.t
  | Replacement of Projection.t

val advance : t -> step -> (t, P.Error.t) result

(** Actual accepted Catalog/Accounts creation is required for initialization.
    Canonical captures its preceding count view. Native Accounts+1 must prove the
    exact same edge using existing Stored validation; later policy edits cannot
    substitute another view. New quota decisions seal an unassociated edge.
    Factual corrections/imports/retirement never perform new admission. Existing
    native, GD and PC validators remain mandatory alongside this supplement. *)
val check : t -> Projection.t -> (unit, P.Error.t) result

(** Expected publication of this actual ordered cursor, derived only from its
    retained original revision with checked exhaustion. Not supplied metadata.
    Use only after [check] proves the candidate's exact accepted components. *)
val publication_revision : t -> (int64, P.Error.t) result
