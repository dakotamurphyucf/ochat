(** Full-duplex typed client transport boundary. *)

type t

module Developer_response_bound : sig
  type t =
    | Bootstrap
    | Installed of Agent_protocol.Developer_common.Limits.t

  (** Upper bound on success result JSON only. Errors use an independent finite
      JSON bound. Socket/HTTP add the exact generated Envelope Request_id/framing
      length and enforce their physical line/body limits before parsing.
      Bootstrap uses the protocol's fixed Capabilities decoder bound; Installed
      is supplied only after Connection has checked the same physical
      negotiated Capabilities. *)
  val max_result_bytes : t -> int

  val max_error_bytes : int
  val error_within_bound : Agent_protocol.Error.t -> bool
end

val create
  :  request:
       (Agent_protocol.Command.t
        -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result)
  -> next_notification:(unit -> Agent_protocol.Envelope.t option)
  -> close:(unit -> unit)
  -> t

(** Install the separate closed Developer family on this same physical
    transport. The callback returns only the bounded, envelope-correlated raw
    success JSON; [Connection] applies the exact Developer decoder and current
    negotiated limits. No Developer method is routed through [Command]. *)
val create_with_developer
  :  request:
       (Agent_protocol.Command.t
        -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result)
  -> request_developer:
       (bound:Developer_response_bound.t
        -> Agent_protocol.Developer_wire.Request.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> next_notification:(unit -> Agent_protocol.Envelope.t option)
  -> close:(unit -> unit)
  -> t

val request
  :  t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result

val request_developer
  :  t
  -> bound:Developer_response_bound.t
  -> Agent_protocol.Developer_wire.Request.t
  -> (Jsonaf.t, Agent_protocol.Error.t) result

val next_notification : t -> Agent_protocol.Envelope.t option
val close : t -> unit
