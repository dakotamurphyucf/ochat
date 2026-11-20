(** Shared recovery operations for an initialized daemon connection. Callers
    can inspect without attaching. Activation requires the current owner's
    attachment and the generation they intend to enable. *)
val enable
  :  Connection.t
  -> Agent_protocol.Recovery_query.Enable.t
  -> (Agent_protocol.Recovery_window.Scope.t, Agent_protocol.Error.t) result

val list
  :  Connection.t
  -> Agent_protocol.Recovery_query.List_windows.t
  -> ( Agent_protocol.Recovery_query.Summary.t Agent_protocol.Page.t
       , Agent_protocol.Error.t )
       result
