open! Core

(** Small independent syntax/value model. No production AST, extraction, type or
    frame operations determine expected leaves, ordering or slot kinds. *)
type value =
  | Number of int
  | Flag of bool
  | Text of string
  | Product of value list
[@@deriving sexp]

type t =
  | Bind of string * value
  | Ignore of value
  | Group of t list

val examples : t list
val pattern : t -> string
val value : t -> value
val leaves : t -> (string * value) list
val source : value -> string
val annotation : value -> string
val slot : value -> string
