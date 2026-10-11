open Core

type t = member list

and member =
  | Value of string
  | Module of string * t
[@@deriving sexp]
