(** Transport-neutral envelope dispatcher. *)

type t

val create : Command_handler.t -> t

val dispatch_command
  :  t
  -> context:Connection_context.t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result

(** Request envelopes always produce one response. Notification-safe calls
    produce no response; all other input is rejected. *)
val dispatch_envelope
  :  t
  -> context:Connection_context.t
  -> Agent_protocol.Envelope.t
  -> (Agent_protocol.Envelope.t option, Agent_protocol.Error.t) result
