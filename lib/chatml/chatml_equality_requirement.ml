open Core

type t =
  | Value
  | Row of String.Set.t
[@@deriving equal, sexp]

module Conflict = struct
  type t = Value_and_row [@@deriving equal, sexp]
end

let combine t other =
  match t, other with
  | Value, Value -> Ok Value
  | Row left, Row right -> Ok (Row (Set.inter left right))
  | Value, Row _ | Row _, Value -> Error Conflict.Value_and_row
;;
