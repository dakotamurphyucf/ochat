val limits : Document_schema.Limits.t

(** Clear adds only a bounded fresh receipt; captures/pool remain and active
    references shrink. Includes decimal growth for selection and host counters.
    This is document capacity only: the actual activation owner must separately
    preflight the complete transaction/journal frame, including audit and hash
    envelope. No transaction or journal capacity is reserved here. *)
val validate
  :  active:bool
  -> Jsonaf.t
  -> limits:Document_schema.Limits.t
  -> (unit, Agent_protocol.Error.t) result

(** Subtract the same reserve before another carrier charges its own reserve. *)
val remaining_limits
  :  active:bool
  -> Document_schema.Limits.t
  -> (Document_schema.Limits.t, Document_schema.Error.t) result
