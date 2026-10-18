(** Transport-neutral envelope dispatcher. *)

type t

module Developer_installation : sig
  type t

  (** One closed installed service. [admit] is the daemon's nonyielding
      execution-availability check; the endpoint separately authenticates the
      request actor, negotiated method, source and current Host authority.
      Neither component can be installed alone. *)
  val create
    :  endpoint:Developer_endpoint.t
    -> admit:(unit -> (unit, Agent_protocol.Error.t) result)
    -> t
end

(** Explicit per-envelope response policy for inference queries only. Default
    16 MiB; unrelated responses and HTTP batch aggregate behavior are unchanged. *)
val create
  :  ?admit:(Agent_protocol.Command.t -> (unit, Agent_protocol.Error.t) result)
  -> ?inference_response_policy:Inference_query_budget.Policy.t
  -> ?developer:Developer_installation.t
  -> Command_handler.t
  -> t

(** Typed local path through the same installed endpoint, actor, negotiated
    feature and daemon admission checks as a Developer request envelope.
    The returned closed Response constructor must match Request.method_name;
    there is no generic Command/Command_receipt fallback. *)
val dispatch_developer
  :  t
  -> ?actor:Operator_authorization.t
  -> context:Connection_context.t
  -> Agent_protocol.Developer_wire.Request.t
  -> (Agent_protocol.Developer_wire.Response.t, Agent_protocol.Error.t) result

val dispatch_command
  :  t
  -> ?actor:Operator_authorization.t
  -> context:Connection_context.t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result

(** Request envelopes produce one response unless an inference query cannot fit
    even its correlated error under the configured envelope policy, in which case
    a transport-level error is returned without publication. Notification-safe calls
    produce no response; all other input is rejected. *)
val dispatch_envelope
  :  t
  -> ?actor:Operator_authorization.t
  -> context:Connection_context.t
  -> Agent_protocol.Envelope.t
  -> (Agent_protocol.Envelope.t option, Agent_protocol.Error.t) result

(** Join Developer watch pumps before the existing Authoring/attachment
    connection closes, retaining cleanup custody on a failed join. *)
val close_connection_and_wait
  :  t
  -> Connection_context.t
  -> (unit, Agent_protocol.Error.t) result

(** Join the installed Developer event hub before the daemon closes its
    shared Host, Authoring and Store owners. A missing service is a no-op. *)
val close_and_wait : t -> (unit, Agent_protocol.Error.t) result
