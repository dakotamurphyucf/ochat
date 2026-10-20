open! Core

let max_checkpoints_between_yields = 64

type t =
  { check : unit -> unit
  ; yield : unit -> unit
  ; mutable remaining : int
  }

let create ~check ~yield = { check; yield; remaining = 0 }

let run t =
  t.check ();
  if t.remaining = 0
  then (
    t.yield ();
    t.check ();
    t.remaining <- max_checkpoints_between_yields - 1)
  else t.remaining <- t.remaining - 1
;;
