open! Core

(** Old persisted-state readers ignored these newly owned optional children.
    Positive facts therefore require an outer semantics fence. Public Schedule
    receipts already have their independent v4 wire fence. *)
val name : string

val supported : string list
val state_requires : Jsonaf.t -> bool
val delta_requires : Jsonaf.t -> bool

(** Add only a missing marker, preserving all existing markers and their order.
    Complete document bounds apply to the resulting envelope. *)
val require
  :  Document_schema.Document.t
  -> required:bool
  -> limits:Document_schema.Limits.t
  -> (Document_schema.Document.t, Document_schema.Error.t) result

val check
  :  Document_schema.Document.t
  -> required:bool
  -> (unit, Document_schema.Error.t) result
