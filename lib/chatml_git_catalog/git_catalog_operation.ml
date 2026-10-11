open Core

type pending =
  { admitted : unit Eio.Promise.t
  ; mutable completed : unit Eio.Promise.or_exn option
  ; mutable context : Eio.Cancel.t option
  ; mutable requested : exn option
  }

type t =
  { switch : Eio.Switch.t
  ; mutable pending : pending option
  ; mutable closed : bool
  }

type error =
  | Busy
  | Closed

exception Owner_closed

let create switch = { switch; pending = None; closed = false }
let is_active t = Option.is_some t.pending

let stop pending reason =
  Eio.Cancel.protect (fun () ->
    pending.requested <- Some reason;
    Option.iter pending.context ~f:(fun context -> Eio.Cancel.cancel context reason);
    Eio.Promise.await pending.admitted;
    Option.iter pending.completed ~f:(fun completed ->
      ignore (Eio.Promise.await completed : (unit, exn) Result.t)))
;;

let clear t pending =
  match t.pending with
  | Some current when phys_equal pending current -> t.pending <- None
  | None | Some _ -> ()
;;

let run t ~f =
  if t.closed
  then Error Closed
  else if is_active t
  then Error Busy
  else (
    Eio.Switch.check t.switch;
    let admitted, resolve_admitted = Eio.Promise.create () in
    let result, resolve_result = Eio.Promise.create () in
    let pending = { admitted; completed = None; context = None; requested = None } in
    t.pending <- Some pending;
    let completed =
      try
        let completed =
          Eio.Fiber.fork_promise ~sw:t.switch (fun () ->
            let outcome =
              try
                Ok
                  (Fun.protect
                     ~finally:(fun () -> pending.context <- None)
                     (fun () ->
                        Eio.Cancel.sub (fun context ->
                          pending.context <- Some context;
                          Option.iter pending.requested ~f:(Eio.Cancel.cancel context);
                          Eio.Fiber.check ();
                          f ())))
              with
              | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
            in
            Eio.Promise.resolve resolve_result outcome)
        in
        pending.completed <- Some completed;
        Eio.Promise.resolve resolve_admitted ();
        completed
      with
      | exn ->
        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
        Eio.Promise.resolve resolve_admitted ();
        clear t pending;
        Exn.raise_with_original_backtrace exn backtrace
    in
    try
      (* The worker promise settles even when switch admission rejects before
         entering our callback. A callback-owned completion promise does not. *)
      Eio.Promise.await_exn completed;
      match Eio.Promise.await result with
      | Ok value ->
        clear t pending;
        Ok value
      | Error (exn, backtrace) ->
        clear t pending;
        Exn.raise_with_original_backtrace exn backtrace
    with
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      stop pending exn;
      clear t pending;
      Exn.raise_with_original_backtrace exn backtrace)
;;

let close t =
  t.closed <- true;
  Option.iter t.pending ~f:(fun pending ->
    stop pending Owner_closed;
    clear t pending)
;;
