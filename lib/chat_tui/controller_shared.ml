(* Shared helper functions used by both Insert- and Normal-mode handlers.  We
   keep them in a tiny stand-alone module so that [controller.ml] and
   [controller_normal.ml] can `open` it without creating cyclic
   dependencies. *)

open Core

let line_bounds (s : string) (pos : int) : int * int =
  (* Return [start, end_] byte indices (end is exclusive) of the line that
     contains [pos].  The implementation is identical to the previous
     copies that lived in each controller file. *)
  let len = String.length s in
  let rec find_start i =
    if i <= 0
    then 0
    else if Char.equal (String.get s (i - 1)) '\n'
    then i
    else find_start (i - 1)
  in
  let rec find_end i =
    if i >= len
    then len
    else if Char.equal (String.get s i) '\n'
    then i
    else find_end (i + 1)
  in
  let start_idx = find_start pos in
  let end_idx = find_end pos in
  start_idx, end_idx
;;
