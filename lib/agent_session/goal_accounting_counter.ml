open! Core

type t = int64 [@@deriving equal, sexp_of]

type error =
  | Negative
  | Exhausted
[@@deriving equal, sexp_of]

let zero = 0L
let of_int64 value = if Int64.(value < 0L) then Error Negative else Ok value
let to_int64 t = t

let add t other =
  if Int64.(other > max_value - t) then Error Exhausted else Ok Int64.(t + other)
;;

let succ t = add t 1L
