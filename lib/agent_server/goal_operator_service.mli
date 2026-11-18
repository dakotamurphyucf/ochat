(** RPC write adapter over the actual registered Actor and Goal Run service.
    The supplied Registry and Factory are the canonical owners of this host.
    [authorize_write] is the nonyielding host policy for an original operator. *)
type t

val create
  :  registry:Session_registry.t
  -> factory:Session_factory.t
  -> server_id:Agent_protocol.Id.Server.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> authorize_write:
       (Operator_authorization.t
        -> Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> t

(** Verifies current original request proof, local host, session visibility and
    same-connection writer attachment. Actor/Factory owners apply current writer
    and domain fences before retained disclosure or fresh command admission.
    The attachment belongs to this RPC. Durable Goal intent and a continued
    Workflow carry their own independently authenticated evidence. *)
val execute
  :  t
  -> actor:Operator_authorization.t
  -> context:Connection_context.t
  -> Agent_protocol.Goal_control.Request.t
  -> (Agent_protocol.Goal_control.Result.t, Agent_protocol.Error.t) result
