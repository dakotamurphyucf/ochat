(** Rehydrates private subcarriers from the original validated whole-state
    document. Ordered known projections must agree exactly with the decoded
    state; original raw bytes supply retained custody, never inferred data. *)
val rehydrate
  :  Session_state.t
  -> document:Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> (Session_state.t, Document_schema.Error.t) result

(** Private transaction-prefix admission: bounded nested carrier decoders and
    exact ordered known/raw agreement, deferring whole-state relationships until
    Session_state_document.Transaction.finish. Never a persistence entry point. *)
val rehydrate_transaction_prefix
  :  Session_state.t
  -> document:Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> (Session_state.t, Document_schema.Error.t) result
