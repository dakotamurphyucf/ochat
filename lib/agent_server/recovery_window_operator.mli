(** One negotiated operator request on its original connection. Activation
    requires the current owner writer. A successful result supplies a
    nonyielding check for deferred transport publication. *)
type t

type release = unit -> (unit, Agent_protocol.Error.t) result

val create
  :  Session_factory.t option
  -> context:Connection_context.t
  -> actor:Operator_authorization.t
  -> registry:Session_registry.t
  -> server_id:Agent_protocol.Id.Server.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> (t, Agent_protocol.Error.t) result

val enable
  :  t
  -> Session_registry.entry
  -> on_release:(release -> unit)
  -> Agent_protocol.Recovery_query.Enable.t
  -> (Agent_protocol.Recovery_window.Scope.t, Agent_protocol.Error.t) result

val list
  :  t
  -> on_release:(release -> unit)
  -> Agent_protocol.Recovery_query.List_windows.t
  -> ( Agent_protocol.Recovery_query.Summary.t Agent_protocol.Page.t
       , Agent_protocol.Error.t )
       result
