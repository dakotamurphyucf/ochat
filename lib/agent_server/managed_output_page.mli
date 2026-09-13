(** Bounded, non-consuming output projection for an already authorized child.
    Ordinary records contain full history/provenance and submission/operation
    correlation. Oversized records use UTF-8-safe JSON-text fragments; concatenate
    fragments of the same entry before decoding the output record. *)
val read
  :  Managed_output_cursor.t
  -> state:Agent_session.Session_state.t
  -> receipt_id:Agent_protocol.History.Id.t option
  -> cursor:Agent_protocol.Page.Cursor.t option
  -> history_epoch:Agent_session.Durable_event_log.history_epoch
  -> limit:int
  -> max_bytes:int
  -> (Jsonaf.t, Agent_protocol.Error.t) result

(** Full assistant text for one successfully completed, already authorized
    submission. Uses the same retained output selection as [read]; missing,
    redacted or malformed output fails instead of disclosing or silently losing
    data. The caller must recheck relationship and authority before disclosure. *)
val completed_answer
  :  state:Agent_session.Session_state.t
  -> receipt_id:Agent_protocol.History.Id.t
  -> (string, Agent_protocol.Error.t) result
