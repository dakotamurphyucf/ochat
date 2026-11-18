(** Pure structural conversion of an OLD Invocation record. The caller supplies
    the bounded raw record only at its declared old schema boundary. This moves
    the complete old Resume DATA into the canonical action envelope; it never
    decodes an intent, issues custody or changes document metadata/hash evidence. *)
val invocation_record : Jsonaf.t -> (Jsonaf.t, Document_schema.Error.t) result

(** Current readers call this before admitting a known record. Even an otherwise
    ignored old alias rejects, so missing canonical DATA cannot silently erase a
    retained command. Unknown unrelated fields remain the carrier's concern. *)
val reject_old_field : Jsonaf.t -> (unit, Document_schema.Error.t) result
