(** Pure ordered admission of a durable mutation into its sole whole-state
    preservation carrier. Each sanctioned custody transfer is validated against
    the exact intermediate basis, including split pending-input adoption. The final
    candidate must match the complete validated native storage encoding of the
    ordered delta result, including opaque inference selection and ledger values.
    Only owner-stamped time and commit counters may differ. *)
val admit
  :  Session_state_document.t
  -> delta:Session_delta.t
  -> next:Session_state.t
  -> limits:Document_schema.Limits.t
  -> (Session_state_document.t, Document_schema.Error.t) result

(** Same ordered admission, retaining its already validated exact final encoding.
    Persistence may decode this immutable encoding once instead of encoding the
    same candidate again. Existing [admit] preserves native carrier semantics. *)
val admit_encoded
  :  Session_state_document.t
  -> delta:Session_delta.t
  -> next:Session_state.t
  -> limits:Document_schema.Limits.t
  -> (Session_state_document.Admitted.t, Document_schema.Error.t) result
