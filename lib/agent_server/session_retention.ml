open! Core

type attempted =
  | Completed
  | Deferred

type terminal =
  | Returned
  | Raised of exn * Stdlib.Printexc.raw_backtrace

type t =
  { domain : int
  ; changed : Eio.Condition.t
  ; mutable requested : unit ref option
  ; mutable stopped : terminal Eio.Promise.t option
  ; mutable closed : bool
  ; mutable failure_observed : bool
  ; mutable last_error : Agent_protocol.Error.t option
  }

let create () =
  { domain = (Stdlib.Domain.self () :> int)
  ; changed = Eio.Condition.create ()
  ; requested = None
  ; stopped = None
  ; closed = false
  ; failure_observed = false
  ; last_error = None
  }
;;

let check_domain t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "session retention belongs to another Eio domain"
;;

let request t =
  check_domain t;
  if not t.closed
  then (
    t.requested <- Some (ref ());
    Eio.Condition.broadcast t.changed)
;;

let last_error t =
  check_domain t;
  t.last_error
;;

let rec await_closed t =
  if not t.closed
  then (
    Eio.Condition.await_no_mutex t.changed;
    await_closed t)
;;

let backoff t clock =
  if not t.closed
  then Eio.Fiber.first (fun () -> Eio.Time.Mono.sleep clock 1.) (fun () -> await_closed t)
;;

let rec run t ~clock ~attempt =
  if not t.closed
  then (
    (match t.requested with
     | None -> Eio.Condition.await_no_mutex t.changed
     | Some generation ->
       (match attempt () with
        | Ok Completed ->
          t.last_error <- None;
          if Option.exists t.requested ~f:(phys_equal generation) then t.requested <- None
        | Ok Deferred -> backoff t clock
        | Error error ->
          t.last_error <- Some error;
          backoff t clock));
    run t ~clock ~attempt)
;;

let bind_exn t ~sw ~clock ~attempt =
  check_domain t;
  if t.closed || Option.is_some t.stopped
  then invalid_arg "session retention has already been bound or closed";
  let stopped, finish = Eio.Promise.create () in
  t.stopped <- Some stopped;
  let finish_once outcome =
    if not (Eio.Promise.is_resolved stopped) then Eio.Promise.resolve finish outcome
  in
  try
    Eio.Fiber.fork ~sw (fun () ->
      let terminal =
        match run t ~clock ~attempt with
        | () -> Returned
        | exception exn -> Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
      in
      t.closed <- true;
      finish_once terminal)
  with
  | exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    t.closed <- true;
    finish_once (Raised (exn, backtrace));
    (* Binding's caller already observes this failure. Owner cleanup must still
       join the actual terminal worker without reporting the same failure twice. *)
    t.failure_observed <- true;
    Exn.raise_with_original_backtrace exn backtrace
;;

let close_and_wait t =
  check_domain t;
  Eio.Cancel.protect (fun () ->
    t.closed <- true;
    Eio.Condition.broadcast t.changed;
    match Option.map t.stopped ~f:Eio.Promise.await with
    | None | Some Returned -> ()
    | Some (Raised (exn, backtrace)) ->
      if not t.failure_observed
      then (
        t.failure_observed <- true;
        Exn.raise_with_original_backtrace exn backtrace))
;;
