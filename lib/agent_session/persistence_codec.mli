open! Core

val integer : Jsonaf.t -> (int, Agent_protocol.Error.t) Result.t
val signed_integer : Jsonaf.t -> (int, Agent_protocol.Error.t) Result.t
val integer_json : int -> Jsonaf.t
val int64_json : int64 -> Jsonaf.t
val int64 : Jsonaf.t -> (int64, Agent_protocol.Error.t) Result.t
val nonnegative_int64 : Jsonaf.t -> (int64, Agent_protocol.Error.t) Result.t
val nullable : (Jsonaf.t -> ('a, 'e) Result.t) -> Jsonaf.t -> ('a option, 'e) Result.t
val option_json : ('a -> Jsonaf.t) -> 'a option -> Jsonaf.t
val list_json : ('a -> Jsonaf.t) -> 'a list -> Jsonaf.t
val raw : 'a -> ('a, 'e) Result.t
val text_json : string -> Jsonaf.t
val bool_json : bool -> Jsonaf.t

val required
  :  Agent_protocol.Json_codec.fields
  -> string
  -> (Jsonaf.t -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

val object_
  :  Jsonaf.t
  -> (Agent_protocol.Json_codec.fields, Agent_protocol.Error.t) Result.t

val list
  :  (Jsonaf.t -> ('a, Agent_protocol.Error.t) Result.t)
  -> Jsonaf.t
  -> ('a list, Agent_protocol.Error.t) Result.t

val protocol_error : Agent_protocol.Error.t -> Document_schema.Error.t

val document_result
  :  ('a, Agent_protocol.Error.t) Result.t
  -> ('a, Document_schema.Error.t) Result.t

val shape_exn : (string * Document_schema.Shape.t) list -> Document_schema.Shape.t

val array_shape_exn
  :  ?allow_empty_identity:bool
  -> ?identity_field:string
  -> Document_schema.Shape.t
  -> Document_schema.Shape.t

val fields_shape : string list -> Document_schema.Shape.t
val nullable_shape : Document_schema.Shape.t -> Document_schema.Shape.t
val document_shape : Document_schema.Shape.t -> Document_schema.Shape.t

val codec_exn
  :  limits:Document_schema.Limits.t
  -> kind:string
  -> shape:Document_schema.Shape.t
  -> decode:(Jsonaf.t -> ('a, Agent_protocol.Error.t) Result.t)
  -> encode:('a -> (Jsonaf.t, Agent_protocol.Error.t) Result.t)
  -> 'a Document_schema.Domain_codec.t

val inspect : Jsonaf.t -> (Document_schema.Document.t, Document_schema.Error.t) Result.t
val document_json : Document_schema.Document.t -> Jsonaf.t

(** The shared [Agent_store.Document_fields.limits] durable profile, with the
    owner's configured complete-document byte budget. *)
val limits : max_bytes:int -> (Document_schema.Limits.t, Document_schema.Error.t) result

val validate_document
  :  Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> kind:string
  -> (unit, Document_schema.Error.t) result

val tagged_shape_exn
  :  discriminator:string
  -> (string * Document_schema.Shape.t) list
  -> Document_schema.Shape.t

val host_counter_to_json : int -> Jsonaf.t
val host_counter_of_json : Jsonaf.t -> (int, Agent_protocol.Error.t) result

val upgrade
  :  Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> kind:string
  -> (Document_schema.Document.t, Document_schema.Error.t) result

val moderator_of_jsonaf : Jsonaf.t -> (Jsonaf.t, Agent_protocol.Error.t) result
