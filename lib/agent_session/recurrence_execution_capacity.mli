(** Private storage-only witnesses for the execution owner's complete child
    profiles. No witness is decoded, persisted or treated as domain evidence. *)

(** Conservatively exceeds each dimension of the complete profile, including a
    maximum-depth branch. The actual enclosing encoder checks all dimensions at
    the witness's real future field path. Construction is bounded by the fixed
    domain profiles supplied by this owner, never untrusted wire values. *)
val profile : Document_schema.Limits.t -> Jsonaf.t
