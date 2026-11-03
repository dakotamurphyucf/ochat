(** Focused part of the shared terminal journal validator, not another owner.
    Preserve the complete opaque selection/unknowns, use the actual instruction
    delta, typed final session.updated/status event, durable event serializer
    and Transaction encoder for the actual internal native Clear route
    (no external server-request audit), and shared4096/64/128 receipt/counter
    reserve. Each future frame must fit the actual writer profile. Any future
    audited entry point must preflight its genuine additional audit envelope. *)
val validate
  :  Session_state.t
  -> limits:Document_schema.Limits.t
  -> max_payload_length:int
  -> (unit, Agent_protocol.Error.t) result
