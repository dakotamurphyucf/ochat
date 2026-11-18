open Core

type t =
  | Invalid of string
  | Out_of_range of string
  | Out_of_coverage
  | Zone_mismatch
[@@deriving equal, sexp]
