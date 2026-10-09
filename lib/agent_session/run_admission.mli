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

  val principal_id : t -> Agent_protocol.Id.Principal.t
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
