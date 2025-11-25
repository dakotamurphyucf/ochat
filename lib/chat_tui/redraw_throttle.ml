open Core

type t =
  { fps : float
  ; enqueue_redraw : unit -> unit
  ; mutable dirty : bool
  ; mutable pending : bool
  }

let create ~fps ~enqueue_redraw =
  let fps = if Float.is_finite fps && Float.(fps > 0.) then fps else 30. in
  { fps; enqueue_redraw; dirty = false; pending = false }
;;

let request_redraw t = t.dirty <- true
let on_redraw_handled t = t.pending <- false

let redraw_immediate t ~draw =
  t.pending <- false;
  t.dirty <- false;
  draw ()
;;

let tick t =
  if t.dirty && not t.pending
  then (
    t.dirty <- false;
    t.pending <- true;
    t.enqueue_redraw ())
;;

let spawn t ~sw ~sleep =
  let open Eio.Std in
  Fiber.fork_daemon ~sw (fun () ->
    let interval = 1. /. t.fps in
    let rec loop () =
      sleep interval;
      tick t;
      loop ()
    in
    loop ())
;;
