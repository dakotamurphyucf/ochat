(** Pure future terminal/recovery journal admission under the actual persistence
    owner's fixed profiles. No IO, evidence construction or writer authority.
    Recurrence control joins the same Run/Goal frame and is checked even with no
    Run. The persistence owner supplies its exact admitted recurrence witness;
    omitting a live control obligation fails. Unknown future worker history/errors
    need ordinary admission; if too large, the known truthful Run recovery
    disposition still fits these profiles. *)
val validate
  :  Session_state.t
  -> terminal:Run_terminal_document.t
  -> recurrence:Recurrence_control_capacity.t option
  -> limits:Document_schema.Limits.t
  -> max_payload_length:int
  -> (unit, Agent_protocol.Error.t) result
