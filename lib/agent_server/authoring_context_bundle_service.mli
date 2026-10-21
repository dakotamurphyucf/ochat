open! Core
module W = Agent_protocol.Authoring.Wire
module Service = Authoring_context_service_core

(** The four bundle methods borrow the existing connection lifecycle and sole
    upload host. Parentless operations need no source context or Core feature.
    A parent-bearing Begin borrows a live physical Core Current for the same
    connection and persisted Expected. Commit first claims the exact durable
    upload; Ready, Published and Rejected outcomes all recheck that persisted
    parent under a fresh same-connection Core request before any success reply.
    Ready keeps its original sealed claim and uses the existing sole publisher;
    an uncertain or revoked parent retains exact-key retry custody. Status is
    write-authorized observation, not a parent publication grant. The final
    protocol installer decides whether to route and advertise this handler. *)
val handle_bundle
  :  ?script_selection:Authoring_script_selection.Policy.t
  -> Service.t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> W.Request.t
  -> (W.Response.t, Service.Error.t) result
