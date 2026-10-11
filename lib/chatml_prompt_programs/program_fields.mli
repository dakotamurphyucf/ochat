open Core

(** Strict named-field primitives for the custody schema. Input JSON has already
    passed Document_schema admission; hex validation precedes decoded allocation. *)
val invalid : string -> ('a, Document_schema.Error.t) Result.t

val field : Jsonaf.t -> string -> (Jsonaf.t, Document_schema.Error.t) Result.t
val string : Jsonaf.t -> (string, Document_schema.Error.t) Result.t
val int : Jsonaf.t -> (int, Document_schema.Error.t) Result.t
val digest : Jsonaf.t -> (Chatml_source.Digest.t, Document_schema.Error.t) Result.t

val hex
  :  Jsonaf.t
  -> checkpoint:(unit -> unit)
  -> (string, Document_schema.Error.t) Result.t

val id
  :  Jsonaf.t
  -> checkpoint:(unit -> unit)
  -> (Chatml_source.Id.t, Document_schema.Error.t) Result.t

val array
  :  Jsonaf.t
  -> maximum:int
  -> checkpoint:(unit -> unit)
  -> (Jsonaf.t list, Document_schema.Error.t) Result.t
