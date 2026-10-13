(** Internal strict, bounded primitives shared by the authoring DTO codecs. *)

(** Largest exact integer shared with common JSON clients. Constructors and
    decoders keep all revision/generation counters within this bound. *)
val maximum_wire_integer : int

val invalid : string -> ('a, Error.t) result
val fields : allowed:string list -> Jsonaf.t -> (Json_codec.fields, Error.t) result
val required : Json_codec.fields -> string -> (Jsonaf.t, Error.t) result

val required_as
  :  Json_codec.fields
  -> string
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> ('a, Error.t) result

val optional_as
  :  Json_codec.fields
  -> string
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> ('a option, Error.t) result

val string : max_bytes:int -> Jsonaf.t -> (string, Error.t) result
val nonnegative : Jsonaf.t -> (int, Error.t) result
val positive : Jsonaf.t -> (int, Error.t) result
val integer : int -> Jsonaf.t
val boolean : bool -> Jsonaf.t

val list
  :  max_items:int
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> Jsonaf.t
  -> ('a list, Error.t) result

val array : ('a -> Jsonaf.t) -> 'a list -> Jsonaf.t
val optional_field : string -> 'a option -> ('a -> Jsonaf.t) -> (string * Jsonaf.t) list

(** Source IDs encode the arbitrary-byte package namespace as bounded base64;
    the portable relative member remains a UTF-8 JSON string. *)
val source_id : Jsonaf.t -> (Chatml_source.Id.t, Error.t) result

val validate_source_id : Chatml_source.Id.t -> (unit, Error.t) result
val source_id_json : Chatml_source.Id.t -> Jsonaf.t
val digest : Jsonaf.t -> (Chatml_source.Digest.t, Error.t) result
val digest_json : Chatml_source.Digest.t -> Jsonaf.t
val portable_member : Jsonaf.t -> (string, Error.t) result
val portable_member_string : string -> (string, Error.t) result
val utf8_text : max_bytes:int -> Jsonaf.t -> (string, Error.t) result
val bytes_base64 : max_bytes:int -> Jsonaf.t -> (string, Error.t) result
val bytes_base64_json : string -> Jsonaf.t
