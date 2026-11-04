(** Host-checked source/startup authority captured from the actual installed or
    prepared runtime. This process-local scope is never reconstructed from a
    request or a durable receipt. Current authorization must be non-yielding and
    rechecked inside the actor's final admission and callback commit boundaries. *)
module Scope : sig
  type t

  val create
    :  principal_id:Agent_protocol.Id.Principal.t
    -> observer:Agent_protocol.Invocation.observer
    -> startup_pending:(unit -> bool)
    -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
    -> (t, Agent_protocol.Error.t) result

  (** Genuine proof-bearing counterpart; derives its principal from the original
      object and repeats its currentness before the supplied host policy. *)
  val create_authenticated
    :  actor:Operator_authorization.t
    -> observer:Agent_protocol.Invocation.observer
    -> startup_pending:(unit -> bool)
    -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
    -> (t, Agent_protocol.Error.t) result

  val principal_id : t -> Agent_protocol.Id.Principal.t

  (** Exact original proof when supplied by genuine authenticated admission.
      Absence stays unavailable; a principal or retained receipt supplies none. *)
  val original_actor : t -> Operator_authorization.t option

  val observer : t -> Agent_protocol.Invocation.observer
  val authorize : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
  val startup_pending : t -> bool
end

type t

(** New admission constructs only durable facts. The actor supplies its actual
    allocated operation for user input and persists the matching history entry
    with this delta before launching the existing worker. Authored_start owns the
    already-pending actual startup callback; initialized runtime replay rejects.
    Retained receipt retries are reconciled before allocating or preparing work. *)
val prepare
  :  Session_state.t
  -> scope:Scope.t
  -> request:Agent_protocol.Run_start.t
  -> session:Agent_protocol.Session_ref.t
  -> run_id:Agent_protocol.Id.Run.t
  -> operation:Agent_protocol.Operation.t option
  -> request_sha256:string
  -> now:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

(** Normalized actor-internal request. Headless callers still require exact
    actor-issued preparation and genuine command authorization. *)
val prepare_request
  :  Session_state.t
  -> scope:Scope.t
  -> request:Run_admission_request.t
  -> session:Agent_protocol.Session_ref.t
  -> run_id:Agent_protocol.Id.Run.t
  -> operation:Agent_protocol.Operation.t option
  -> request_sha256:string
  -> now:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

(** Only the exact actor's checked ephemeral preparation permits acknowledged
    owned constructor/history advances. The original request and digest come
    directly from custody; no public raw revision override or replacement input. *)
val prepare_from_preparation
  :  Session_state.t
  -> preparation:Run_preparation.t
  -> owner:Run_preparation.Owner.t
  -> scope:Scope.t
  -> session:Agent_protocol.Session_ref.t
  -> run_id:Agent_protocol.Id.Run.t
  -> operation:Agent_protocol.Operation.t option
  -> now:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

val run : t -> Agent_protocol.Run.t
val receipt : t -> Agent_protocol.Run_receipt.t
val index : t -> Run_state.t

module Held_plan : sig
  (** Validated inert admission DATA, not a runtime, source authorization or
      live held owner. The index precedes addition of the Run/receipt so its
      actual owner can commit an exact reciprocal origin in the same candidate. *)
  type t

  val source_index : t -> Run_state.t
  val run : t -> Agent_protocol.Run.t
  val receipt : t -> Agent_protocol.Run_receipt.t
end

(** Callback-free facts from the exact preparation's original request and a
    captured source selection. Checks only preparation custody, source/request
    invariants and existing Run constraints; original authentication and policy
    belong to the actual actor before persistence. Installs no scope or authority,
    allocates no operation/history, and executes no native/compiler/startup work.
    Legacy and extension sources carry actual source identity; neither a legacy
    identity nor this plan creates extension invocation capability. No-moderator
    sources reject, as in ordinary Run admission's orchestration requirement.
    Actual initialized startup eligibility remains an activation-owner check. *)
val prepare_held_from_preparation
  :  Session_state.t
  -> preparation:Run_preparation.t
  -> owner:Run_preparation.Owner.t
  -> source:Runtime_source_preparation.t
  -> session:Agent_protocol.Session_ref.t
  -> run_id:Agent_protocol.Id.Run.t
  -> now:Agent_protocol.Timestamp.t
  -> (Held_plan.t, Agent_protocol.Error.t) result
