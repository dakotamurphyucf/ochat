(** Typed request connection with one serialized transport owner. *)

type t

val create : Transport.t -> t

val request
  :  t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result

val next_notification : t -> Agent_protocol.Envelope.t option
val close : t -> unit
