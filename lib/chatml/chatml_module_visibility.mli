open Core

(** Runtime export selection produced by successful interface checking.
    Names are unique within each scope. A nested selection restricts the nested
    module itself, while values retain their underlying identity and closures.
    The evaluator rejects missing members or non-module nested selections.
    This data carries no execution authority. *)
type t = member list

and member =
  | Value of string
  | Module of string * t
[@@deriving sexp]
