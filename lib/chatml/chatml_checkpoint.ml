open Core

type t = unit -> unit

let protect checkpoint ~f =
  (* A generative exception keeps nested boundaries from unwrapping each
     other's callbacks. No state or exception identity escapes compilation. *)
  let exception Raised of exn * Stdlib.Printexc.raw_backtrace in
  let guarded_checkpoint () =
    try checkpoint () with
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      raise (Raised (exn, backtrace))
  in
  try f guarded_checkpoint with
  | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;
