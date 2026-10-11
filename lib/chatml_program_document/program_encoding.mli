open Core

(** Exact encoded-byte preflight before hex/JSON expansion. Ordinary JSON scalar
    sizes are measured by Document_schema, not a parallel escaping implementation. *)
val document
  :  Chatml_prepared_program.t
  -> limits:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Document_schema.Document.t, Document_schema.Error.t) Result.t
