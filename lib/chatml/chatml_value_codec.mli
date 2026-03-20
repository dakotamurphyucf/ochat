(** Shared codecs between ChatML runtime values and host-facing data
    representations.

    The current focus of this module is conversion to and from {!Jsonaf.t},
    plus a few small validation helpers commonly needed by host runtimes. *)

open! Core
open Chatml_lang

(** Convert a {!Jsonaf.t} tree into the builtin ChatML [json] runtime
    representation. *)
val jsonaf_to_value : Jsonaf.t -> value

(** Convert a ChatML value representing builtin JSON into {!Jsonaf.t},
    returning a descriptive error on shape mismatch. *)
val value_to_jsonaf_result : value -> (Jsonaf.t, string) result

(** Exception-raising wrapper around {!value_to_jsonaf_result}. *)
val value_to_jsonaf_exn : value -> Jsonaf.t

(** Read a named field from a record-like value map, returning a labelled
    error when the field is absent. *)
val expect_record_field : string -> value String.Map.t -> string -> (value, string) result

(** Validate that a value is a string. *)
val expect_string : string -> value -> (string, string) result

(** Validate that a value is an integer. *)
val expect_int : string -> value -> (int, string) result
