(** Original-operator recurrence inspection through immutable Registry reads.
    [authorize_write] is the nonyielding shared policy for advisory writer actions. *)
type t

val create
  :  registry:Session_registry.t
  -> factory:Session_factory.t
  -> server_id:Agent_protocol.Id.Server.t
  -> execution_enabled:bool
  -> authorize_write:
       (Operator_authorization.t
        -> Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> t

(** Reads indexed or loaded controller data, then reads each relevant target
    under the same current original principal. A second controller read
    verifies basis and binding before disclosure. No runtime is loaded or started. *)
val query
  :  t
  -> actor:Operator_authorization.t
  -> context:Connection_context.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> Agent_protocol.Recurrence_query.Request.t
  -> (Agent_protocol.Recurrence_query.Result.t, Agent_protocol.Error.t) result
