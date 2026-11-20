open! Core

(** Actor-owned process-local root request lifetime. No durable authority is
    stored here. The actor serializes every mutation after its ownership checks. *)
type t

module Capacity_basis : sig
  type t

  val capture : Session_state.t -> selection:Session_context_capacity.Selection.t -> t
end

module Capacity_observation : sig
  type t
end

module Token : sig
  type t

  val operation_id : t -> Agent_protocol.Id.Operation.t
end

val create : unit -> t

val begin_capture
  :  ?capacity:Capacity_basis.t
  -> ?recovery_scope:Agent_protocol.Recovery_window.Scope.t
  -> capture_recovery:bool
  -> t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> generation:int
  -> revision:int64
  -> target:Inference.Request.Target.t
  -> policy:Configuration_policy.t
  -> Token.t

val check_recovery
  :  t
  -> Token.t
  -> scope:Agent_protocol.Recovery_window.Scope.t
  -> (unit, Agent_protocol.Error.t) Result.t

val mark
  :  t
  -> Token.t
  -> Inference.Observation.Configuration.t
  -> (unit, Agent_protocol.Error.t) Result.t

val mark_capacity
  :  t
  -> Token.t
  -> current:Capacity_basis.t option
  -> Capacity_observation.t
  -> unit

val capacity_status
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> current:Capacity_basis.t option
  -> Session_context_capacity.status

(** Window of the actual finalized root preparation for this operation. A
    retained same-operation observation still requires live Actor checks. *)
module Native_window : sig
  type t

  val scope : t -> Agent_protocol.Recovery_window.Scope.t
  val same_preparation : t -> t -> bool
end

val native_window
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> configuration_revision:int64
  -> target:Inference.Request.Target.t
  -> Native_window.t option

val finish : t -> Token.t -> success:bool -> unit

val view
  :  t
  -> generation:int
  -> revision:int64
  -> selected:Inference.Selection.t
  -> (Agent_protocol.Session_configuration.t, Agent_protocol.Error.t) Result.t

(** Resource bracket outside the actor mailbox. Begin/mark/finish callbacks
    reenter the actor and must enforce live operation/admission ownership.
    Resolution and final preparation are pinned to one immutable token. Failure
    or cancellation clears preparing ownership under cancellation protection;
    successful completion retains a historical projection, never an active one. *)
val root_port
  :  begin_capture:(unit -> (Token.t, Agent_protocol.Error.t) Result.t)
  -> publish_capacity:
       (Token.t -> Capacity_observation.t -> (unit, Agent_protocol.Error.t) Result.t)
  -> publish_recovery:
       (Token.t
        -> Inference_runtime.Recovery_input.t
        -> binding_is_current:(unit -> bool)
        -> (unit, Agent_protocol.Error.t) Result.t)
  -> mark:
       (Token.t
        -> Inference.Observation.Configuration.t
        -> (unit, Agent_protocol.Error.t) Result.t)
  -> finish:(Token.t -> success:bool -> (unit, Agent_protocol.Error.t) Result.t)
  -> Chat_response.Root_context.t
