(** Private immutable storage custody of the exact pending wrapper except its
    entry, which moves to canonical history. No public pending DTO includes it. *)
type t

val of_pending
  :  Pending_input_document.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val of_jsonaf
  :  Jsonaf.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val to_jsonaf
  :  t
  -> limits:Document_schema.Limits.t
  -> (Jsonaf.t, Document_schema.Error.t) result

val history_id : t -> Agent_protocol.History.Id.t
val generation : t -> int
val shape : Document_schema.Shape.t
val owner : t -> Pending_input_document.Owner.t

(** Known semantic projection for the owning document codec. Retained unknowns
    remain in the original whole-state template; this does not retire custody. *)
val known_jsonaf
  :  t
  -> limits:Document_schema.Limits.t
  -> (Jsonaf.t, Document_schema.Error.t) result
