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

(** List or search visible items. Continue on the same host with the returned
    opaque cursor, including when the current page contains no rows. *)
val query
  :  Connection.t
  -> Agent_protocol.Recovery_history_request.t
  -> (Agent_protocol.Recovery_history_page.t, Agent_protocol.Error.t) result
