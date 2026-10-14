(** Canonical private-document projection of developer owner coordinates.
    Decoding validates shape and IDs but does not authenticate a physical Store
    or principal. Result reference and intent documents share these bytes. *)
open! Core

val to_json : Agent_protocol.Execution_owner.developer -> Jsonaf.t

val of_json
  :  Jsonaf.t
  -> (Agent_protocol.Execution_owner.developer, Document_schema.Error.t) result

val shape : Document_schema.Shape.t
