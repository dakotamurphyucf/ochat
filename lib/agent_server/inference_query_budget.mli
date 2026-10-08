(** Bounds only inference-query responses. The default is 16 MiB per RPC
    envelope; HTTP batch aggregate and all unrelated RPC limits are unchanged.
    Input/event limits are not reused as response policy. *)
module Policy : sig
  type t

  val create : max_envelope_bytes:int -> (t, Agent_protocol.Error.t) result
  val default : t
end

type t

(** Measures the actual correlated success-envelope wrapper, including escaped
    request ID, before allocating serialized output. *)
val for_request
  :  Policy.t
  -> Agent_protocol.Envelope.Request_id.t
  -> (t, Agent_protocol.Error.t) result

(** Explicit result-only budget for typed embedded dispatch; no fabricated RPC
    identifier or envelope. *)
val for_embedded : max_result_bytes:int -> (t, Agent_protocol.Error.t) result

val max_result_bytes : t -> int

val validate_result
  :  t
  -> Agent_protocol.Public.Result.t
  -> (unit, Agent_protocol.Error.t) result

val validate_envelope
  :  Policy.t
  -> Agent_protocol.Envelope.t
  -> (unit, Agent_protocol.Error.t) result
