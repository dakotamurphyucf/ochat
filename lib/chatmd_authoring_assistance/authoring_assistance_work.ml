open! Core

exception Exhausted

type t =
  { mutable remaining : int
  ; checkpoint : unit -> unit
  }

let create ~remaining ~checkpoint = { remaining; checkpoint }
let remaining t = t.remaining

let charge t n =
  if n < 0 || n > t.remaining then raise Exhausted;
  t.checkpoint ();
  t.remaining <- t.remaining - n
;;

let tick t () = charge t 1

let list_length_bounded t ~maximum items =
  let rec loop count = function
    | [] -> count
    | _ :: rest ->
      charge t 1;
      if count >= maximum then raise Exhausted;
      loop (count + 1) rest
  in
  loop 0 items
;;
