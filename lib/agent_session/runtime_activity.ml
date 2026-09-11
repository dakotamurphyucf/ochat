open Core

exception Closed

type t =
  { switch : Eio.Switch.t
  ; closed : bool Atomic.t
  }

let create ~sw =
  let closed = Atomic.make false in
  Eio.Switch.on_release sw (fun () -> Atomic.set closed true);
  { switch = sw; closed }
;;

let run t f =
  Eio.Fiber.check ();
  if Atomic.get t.closed then raise Closed;
  (* Fork from the invoking fiber, preserving its scope bindings, but register
     the activity under the runtime's switch. Either owner can cancel it. *)
  let cancel_requested = Atomic.make false in
  let context = Atomic.make None in
  let activity =
    Eio.Fiber.fork_promise ~sw:t.switch (fun () ->
      Eio.Cancel.sub (fun current ->
        Atomic.set context (Some current);
        Exn.protect
          ~finally:(fun () -> Atomic.set context None)
          ~f:(fun () ->
            (match Atomic.get cancel_requested with
             | false -> ()
             | true -> Eio.Cancel.cancel current Exit);
            Eio.Fiber.check ();
            match f () with
            | result -> Ok result
            | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))))
  in
  match Eio.Promise.await_exn activity with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    Eio.Cancel.protect (fun () ->
      Atomic.set cancel_requested true;
      Option.iter (Atomic.get context) ~f:(fun current -> Eio.Cancel.cancel current Exit);
      ignore (Eio.Promise.await activity : (_, exn) result));
    (match exn with
     | Eio.Cancel.Cancelled _ when Eio.Fiber.is_cancelled () ->
       Exn.raise_with_original_backtrace exn backtrace
     | _ when Atomic.get t.closed -> raise Closed
     | _ -> Exn.raise_with_original_backtrace exn backtrace)
;;

let with_switch t f = run t (fun () -> Eio.Switch.run (fun sw -> f ~sw))
