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

module Snapshot : sig
  (** Data-only ChatML snapshots suitable for persisted moderator state
      and queued internal events.

      This type intentionally excludes runtime-only values such as refs,
      closures, modules, builtins, and tasks. *)
  type t =
    | Int of int
    | Float of float
    | Bool of bool
    | String of string
    | Unit
    | Array of t list
    | Record of (string * t) list
    | Variant of string * t list
  [@@deriving sexp, compare, bin_io]

  (** [of_value value] converts [value] into a durable snapshot.

      Record fields preserve sorted key order. The conversion fails with
      a descriptive error when [value] contains runtime-only data. *)
  val of_value : value -> (t, string) result

  (** Exception-raising wrapper around {!of_value}. *)
  val of_value_exn : value -> t

  (** [to_value snapshot] rehydrates a runtime value from [snapshot].

      Record field names must be unique. *)
  val to_value : t -> (value, string) result

  (** Exception-raising wrapper around {!to_value}. *)
  val to_value_exn : t -> value
end

(** Read a named field from a record-like value map, returning a labelled
    error when the field is absent. *)
val expect_record_field : string -> value String.Map.t -> string -> (value, string) result

(** Validate that a value is a string. *)
val expect_string : string -> value -> (string, string) result

(** Validate that a value is an integer. *)
val expect_int : string -> value -> (int, string) result
