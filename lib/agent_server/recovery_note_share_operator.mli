(** Connected, source-owner-only management of explicit note grants. *)

val dispatch
  :  Session_factory.t option
  -> context:Connection_context.t
  -> actor:Operator_authorization.t
  -> registry:Session_registry.t
  -> server_id:Agent_protocol.Id.Server.t
  -> on_release:((unit -> (unit, Agent_protocol.Error.t) result) -> unit)
  -> Agent_protocol.Recovery_note_share_control.Request.t
  -> ( Agent_protocol.Recovery_note_share_control.Response.t
       , Agent_protocol.Error.t )
       result
