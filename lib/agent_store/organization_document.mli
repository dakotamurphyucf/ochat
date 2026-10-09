(** Version 1 authoritative host organization document. Complete envelope cap
    16 MiB with shared structural bounds. Keyed group entries retain tombstones
    and unknown fields forever; only explicitly expired receipts may retire. *)
val limits : Document_schema.Limits.t

val restore
  :  Document_schema.Document.t
  -> (Organization_state.t Document_schema.Extension_carrier.t, Store_error.t) result

val encode
  :  Organization_state.t Document_schema.Extension_carrier.t
  -> (Document_schema.Document.t, Store_error.t) result

(** Retire expired receipts absent from the next validated state or renewed under
    the same key with a fresh lifetime. Their old extensions never transfer into
    the fresh receipt; unchanged receipts retain their extensions exactly. *)
val with_state
  :  Organization_state.t Document_schema.Extension_carrier.t
  -> Organization_state.t
  -> now:Agent_protocol.Timestamp.t
  -> (Organization_state.t Document_schema.Extension_carrier.t, Store_error.t) result
