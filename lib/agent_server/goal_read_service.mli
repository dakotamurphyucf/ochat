(** Bounded Goal metadata over the same immutable Registry reader used for Runs.
    Reads do not select an execution owner. Writer and execution observations
    describe only the snapshot that produced the result. *)
type t

val create
  :  registry:Session_registry.t
  -> server_id:Agent_protocol.Id.Server.t
  -> execution_enabled:bool
  -> authorize_write:
       (Operator_authorization.t
        -> Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> t

val query
  :  t
  -> actor:Operator_authorization.t
  -> context:Connection_context.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> Agent_protocol.Goal_query.Request.t
  -> (Agent_protocol.Goal_query.Result.t, Agent_protocol.Error.t) result
