open! Core

(** Independent ground basis: scalar leaves, flat arities2/3 and nested pairs,
    depth at most two. No parser/inference/runtime/codec constructors here. *)
type t =
  | Number of int
  | Flag of bool
  | Text of string
  | Product of t list
[@@deriving sexp]

val products : t list
val source : t -> string
val annotation : t -> string
val display : t -> string
