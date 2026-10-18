(** Strict finite JSON primitives for the Developer wire family only. *)

val invalid : string -> ('a, Error.t) result
val limit : string -> ('a, Error.t) result
val validate : limits:Document_schema.Limits.t -> Jsonaf.t -> (unit, Error.t) result
val measure : limits:Document_schema.Limits.t -> Jsonaf.t -> (int, Error.t) result
val fields : allowed:string list -> Jsonaf.t -> (Json_codec.fields, Error.t) result
val required : Json_codec.fields -> string -> (Jsonaf.t, Error.t) result
val optional : Json_codec.fields -> string -> (Jsonaf.t option, Error.t) result
val string : max_bytes:int -> Jsonaf.t -> (string, Error.t) result
val nonnegative : Jsonaf.t -> (int, Error.t) result
val positive : Jsonaf.t -> (int, Error.t) result
val finite_positive_float : Jsonaf.t -> (float, Error.t) result
val boolean : Jsonaf.t -> (bool, Error.t) result

val array
  :  max_items:int
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> Jsonaf.t
  -> ('a list, Error.t) result

val integer_json : int -> Jsonaf.t
val optional_field : string -> 'a option -> ('a -> Jsonaf.t) -> (string * Jsonaf.t) list
val digest : Jsonaf.t -> (Chatml_source.Digest.t, Error.t) result
val digest_json : Chatml_source.Digest.t -> Jsonaf.t
val source_id : Jsonaf.t -> (Chatml_source.Id.t, Error.t) result
val source_id_json : Chatml_source.Id.t -> Jsonaf.t
