(** Canonical host-turn tracking for actual Operation_started/terminal payloads.
    The returned delta retains real Turn_handle admission facts and reconciles
    all stable Goal partitions with the same before/after canonical ledger.
    This is evidence planning, not authorization or a worker-start capability.
    Compaction never admits a host turn. *)
module Inherited : sig
  (** Pure DATA input, not a root reservation ACK or execution permission. *)
  type t

  (** Requires a Host_turn occurrence and nonempty strictly root-ordered
      receipts naming exactly that occurrence. The actual owner and final State
      guard validate the frozen origin, genuine Handle and publication frame. *)
  val create
    :  occurrence:Goal_descendant_occurrence.t
    -> receipts:Goal_descendant_included.t list
    -> (t, Agent_protocol.Error.t) result
end

module Inherited_edge : sig
  (** Exact immediate canonical edge selected by the real Host_turn_tracking
      owner while consuming one Inherited input. Retains the genuine operation,
      Turn_handle and tracking returned by Inference_ledger.admit_turn, plus its
      SAME immediate ledger pair. Earlier/later canonical leaves in the same
      transition do not change this pair. No constructor or decoder.

      This is pure transition planning evidence. It proves no persistence ACK,
      root reservation, installed State, runtime custody or effect permission. *)
  type t

  val before : t -> Inference_ledger.t
  val after : t -> Inference_ledger.t

  (** Always Host_turn of the actual admitted operation/Turn_handle/tracking.
      Never Attempt, reconstructed from durable Fact, or looked up after planning. *)
  val canonical : t -> Goal_descendants.Canonical.t
end

(** Optional input must be consumed by exactly one matching actual Turn start.
    The real Handle owner records the inherited receipts at that same canonical
    edge before final State validation. Compaction, unused input or reuse reject.
    Omission preserves ordinary behavior; positive inherited admission without
    receipts still fails the existing final descendant guard. *)
val apply
  :  ?inherited_turn:Inherited.t
  -> previous:Session_state.t
  -> Session_state.t
  -> Session_delta.t
  -> Agent_protocol.Event.Durable.Payload.t list
  -> (Session_state.t * Session_delta.t, Agent_protocol.Error.t) result

(** Same complete tracking path as apply, requiring exactly one matching actual
    inherited Turn admission. Returns its exact edge with the ordinary state and
    delta. Unused/mismatched/duplicate input and compaction reject as before.
    A successful result remains planning, not a child canonical writer ACK. *)
val apply_inherited
  :  inherited_turn:Inherited.t
  -> previous:Session_state.t
  -> Session_state.t
  -> Session_delta.t
  -> Agent_protocol.Event.Durable.Payload.t list
  -> (Session_state.t * Session_delta.t * Inherited_edge.t, Agent_protocol.Error.t) result
