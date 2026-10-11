open Core

(** Two-pass exact custody schema preflight. Ordinary scalar escaping and supplied
    JSON subdocuments are measured by Document_schema. No source body enters JSON. *)
val payload
  :  Program_inventory.t
  -> limits:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Jsonaf.t, Document_schema.Error.t) Result.t

val shape : unit -> Document_schema.Shape.t
val source_key : Chatml_source.Id.t -> checkpoint:(unit -> unit) -> string
