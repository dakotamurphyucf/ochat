open Core

type t =
  | Record
  | Module
[@@deriving equal, sexp_of]

let name = function
  | Record -> "record"
  | Module -> "module"
;;
