(** Private building blocks for the named-field standalone and moderator codecs.
    Options are required nullable fields. Wide integers are canonical decimal
    strings. Missing fields require an explicit document conversion. *)
open! Core

val object_ : (string * Document_schema.Shape.t) list -> Document_schema.Shape.t
val array : ?identity:string -> Document_schema.Shape.t -> Document_schema.Shape.t
val nullable : Document_schema.Shape.t -> Document_schema.Shape.t
val value : Document_schema.Shape.t
val fields : Jsonaf.t -> ((string * Jsonaf.t) list, string) Result.t

val field
  :  (string * Jsonaf.t) list
  -> string
  -> (Jsonaf.t -> ('a, string) Result.t)
  -> ('a, string) Result.t

val string : Jsonaf.t -> (string, string) Result.t
val bool : Jsonaf.t -> (bool, string) Result.t
val int : Jsonaf.t -> (int, string) Result.t
val int64 : Jsonaf.t -> (int64, string) Result.t

val option
  :  (Jsonaf.t -> ('a, string) Result.t)
  -> Jsonaf.t
  -> ('a option, string) Result.t

val list : (Jsonaf.t -> ('a, string) Result.t) -> Jsonaf.t -> ('a list, string) Result.t
val encode_string : string -> Jsonaf.t
val encode_bool : bool -> Jsonaf.t
val encode_int : int -> Jsonaf.t
val encode_int64 : int64 -> Jsonaf.t
val encode_option : ('a -> Jsonaf.t) -> 'a option -> Jsonaf.t
val encode_list : ('a -> Jsonaf.t) -> 'a list -> Jsonaf.t

val named_values
  :  (Jsonaf.t -> ('a, string) Result.t)
  -> Jsonaf.t
  -> ((string * 'a) list, string) Result.t

val encode_named_values : ('a -> Jsonaf.t) -> (string * 'a) list -> Jsonaf.t
val named_values_shape : Document_schema.Shape.t -> Document_schema.Shape.t
val bounded : (Jsonaf.t -> ('a, string) Result.t) -> Jsonaf.t -> ('a, string) Result.t
val unique : ?allow_empty:bool -> string list -> (unit, string) Result.t
