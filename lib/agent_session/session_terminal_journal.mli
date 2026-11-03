(** Pure future terminal/recovery journal admission under the actual persistence
    owner's fixed profiles. No IO, evidence construction or writer authority.
    Unknown future worker history/errors need ordinary admission; if too large,
    the known truthful Run recovery disposition still fits these profiles. *)
val validate
  :  Session_state.t
  -> terminal:Run_terminal_document.t
  -> limits:Document_schema.Limits.t
  -> max_payload_length:int
  -> (unit, Agent_protocol.Error.t) result
