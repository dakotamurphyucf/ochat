(** Select the private obligation from the authorized active window. Its owner
    and physical operation identifiers never cross the public boundary. *)
val execute
  :  Agent_session.Session_actor.t
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> on_release:((unit -> (unit, Agent_protocol.Error.t) result) -> unit)
  -> Agent_protocol.Recovery_followup.t
  -> (Agent_protocol.Recovery_followup.Reply.t, Agent_protocol.Error.t) result
