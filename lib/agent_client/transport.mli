(** Full-duplex typed client transport boundary. *)

type t

val create
  :  request:
       (Agent_protocol.Command.t
        -> (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result)
  -> next_notification:(unit -> Agent_protocol.Envelope.t option)
  -> close:(unit -> unit)
  -> t

val request
  :  t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result

val next_notification : t -> Agent_protocol.Envelope.t option
val close : t -> unit
