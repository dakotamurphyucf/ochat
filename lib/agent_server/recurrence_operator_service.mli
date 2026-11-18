(** Execute durable controller metadata commands for the original connected
    writer. Checking a retained receipt does not recapture a target or launch
    an occurrence. *)
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

val execute
  :  t
  -> actor:Operator_authorization.t
  -> context:Connection_context.t
  -> Agent_protocol.Recurrence_control.Request.t
  -> (Agent_protocol.Recurrence_control.Result.t, Agent_protocol.Error.t) result

(** Revalidate the original session writer after the response audit yields and
    before disclosing an accepted receipt. A new command is never submitted. *)
val check_disclosure
  :  t
  -> actor:Operator_authorization.t
  -> context:Connection_context.t
  -> Agent_protocol.Recurrence_control.Request.t
  -> (unit, Agent_protocol.Error.t) result
