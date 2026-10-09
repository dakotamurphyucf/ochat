open! Core

(** Actor-owned process-local root request lifetime. No durable authority is
    stored here. The actor serializes every mutation after its ownership checks. *)
type t

module Token : sig
  type t

  val operation_id : t -> Agent_protocol.Id.Operation.t
end

val create : unit -> t

val begin_capture
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> generation:int
  -> revision:int64
  -> target:Inference.Request.Target.t
  -> policy:Configuration_policy.t
  -> Token.t

val mark
  :  t
  -> Token.t
  -> Inference.Observation.Configuration.t
  -> (unit, Agent_protocol.Error.t) Result.t

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
  -> mark:
       (Token.t
        -> Inference.Observation.Configuration.t
        -> (unit, Agent_protocol.Error.t) Result.t)
  -> finish:(Token.t -> success:bool -> (unit, Agent_protocol.Error.t) Result.t)
  -> Chat_response.Root_context.t
