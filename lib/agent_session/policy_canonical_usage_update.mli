(** Private closed reason, actual birth/fold/retirement and preserved update. *)
type t = Policy_canonical_usage_carrier.t

module Selection = Policy_canonical_usage_selection.Selection

module Update : sig
  type accounting = t

  module Reason : sig
    type t =
      | Register_event of Policy_cause_data.t
      | Register_work of
          { cause : Policy_cause_data.Key.t
          ; member : Policy_cause_data.Member.t
          }
      | Reconcile of Selection.t option
      | Prune_work
  end

  type t = private
    { candidate : accounting
    ; reason : Reason.t
    }

  val candidate : t -> accounting
  val to_json : t -> (Jsonaf.t, Agent_protocol.Error.t) result
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

  (** Closed reason plus complete candidate. A decoded reason is a claim;
      actual ordered birth/canonical/native coverage evidence must reproduce it. *)
  val shape : Document_schema.Shape.t
end

val register_event
  :  t option
  -> before:Policy_cause_data.Context.t
  -> after:Policy_cause_data.Context.t
  -> ledger:Inference_ledger.t
  -> cause:Policy_cause_data.t
  -> (Update.t, Agent_protocol.Error.t) result

(** Registration requires actual missing-to-present original work and exact
    previously caused parent. Existing source IDs cannot acquire a cause later.
    Only a new delegated seed initializes a component, at the actual canonical
    future frontier. This never infers historical unrelatedness. *)
val register_work
  :  t
  -> before:Policy_cause_data.Context.t
  -> after:Policy_cause_data.Context.t
  -> cause:Policy_cause_data.Key.t
  -> member:Policy_cause_data.Member.t
  -> (Update.t, Agent_protocol.Error.t) result

(** Every actual canonical edge, including factual corrections/retirement/reset/
    host turns. [None] preserves old choices and requires no current old work;
    removed ended selected rows fold once. [Some] is one actual new Attempt.
    Complete State must pair this proposal with exact ordered native evidence. *)
val reconcile
  :  t
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> selection:Selection.t option
  -> (Update.t, Agent_protocol.Error.t) result

(** Recompute only factual lifecycle reconciliation from the actual canonical
    before/after ledgers. All four canonical admission marks must be unchanged.
    Retained rows may settle/retire and generation may advance under the ledger's
    existing validator. No cause, membership, route, stream or Attempt is added.
    Compatible preserved children survive; protected retirement still rejects. *)
val reconcile_factual
  :  t
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> (t, Agent_protocol.Error.t) result

(** Optional pruning only after actual work absence, with no retained H/latest
    head/ancestor dependency. Unknown protected children can reject the proposal.
    Streams and their cumulative settled history are never removed or reused. *)
val prune_work
  :  t
  -> context:Policy_cause_data.Context.t
  -> (Update.t, Agent_protocol.Error.t) result
