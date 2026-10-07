(** Transport-neutral envelope dispatcher. *)

type t

(** Explicit per-envelope response policy for inference queries only. Default
    16 MiB; unrelated responses and HTTP batch aggregate behavior are unchanged. *)
val create
  :  ?inference_response_policy:Inference_query_budget.Policy.t
  -> Command_handler.t
  -> t

val dispatch_command
  :  t
  -> context:Connection_context.t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result

(** Request envelopes produce one response unless an inference query cannot fit
    even its correlated error under the configured envelope policy, in which case
    a transport-level error is returned without publication. Notification-safe calls
    produce no response; all other input is rejected. *)
val dispatch_envelope
  :  t
  -> context:Connection_context.t
  -> Agent_protocol.Envelope.t
  -> (Agent_protocol.Envelope.t option, Agent_protocol.Error.t) result
