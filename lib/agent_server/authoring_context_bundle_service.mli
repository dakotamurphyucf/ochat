open! Core
module W = Agent_protocol.Authoring.Wire
module Service = Authoring_context_service_core

(** The four bundle methods borrow the existing connection lifecycle and the
    sole upload host. No source context or core feature is needed. The final
    protocol installer decides whether to route and advertise this handler. *)
val handle_bundle
  :  Service.t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> W.Request.t
  -> (W.Response.t, Service.Error.t) result
