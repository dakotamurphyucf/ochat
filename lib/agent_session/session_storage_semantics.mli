(** Compose the two explicit owned state/delta feature fences. Existing queue
    semantics and marker ordering are preserved. No Store outer envelope or
    execution authority is added. *)
val supported : string list

val require_state
  :  Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> (Document_schema.Document.t, Document_schema.Error.t) result

val check_state : Document_schema.Document.t -> (unit, Document_schema.Error.t) result

val require_delta
  :  Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> (Document_schema.Document.t, Document_schema.Error.t) result

val check_delta : Document_schema.Document.t -> (unit, Document_schema.Error.t) result
