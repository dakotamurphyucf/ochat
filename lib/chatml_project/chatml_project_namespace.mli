open Core

(** Shared source-free root schema and namespace rules for bootstrap/full decode. *)
val of_json
  :  Jsonaf.t
  -> checkpoint:(unit -> unit)
  -> (string, Document_schema.Error.t) Result.t

val of_contents
  :  string
  -> limits:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (string, Document_schema.Error.t) Result.t
