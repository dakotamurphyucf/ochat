open! Core

type snapshot =
  { identity : string
  ; epoch : int
  ; generation : int
  ; draft : string
  ; cursor : int
  ; input : Type_ahead_provider.input
  }

type event =
  | Ready of snapshot
  | Completed of snapshot * Type_ahead_provider.outcome

type job =
  | Debounce of snapshot * float
  | Request of snapshot

type t =
  { wake : unit Eio.Stream.t
  ; mutable pending : job option
  ; mutable active : Eio.Switch.t option
  ; mutable closed : bool
  ; finished : unit Eio.Promise.t
  }

exception Superseded

let wake t = if Eio.Stream.is_empty t.wake then Eio.Stream.add t.wake ()

let cancel t =
  t.pending <- None;
  let active = t.active in
  t.active <- None;
  Option.iter active ~f:(fun sw -> Eio.Switch.fail sw Superseded)
;;

let enqueue t job =
  if not t.closed
  then (
    cancel t;
    t.pending <- Some job;
    wake t)
;;

let perform ~sleep ~complete ~emit sw = function
  | Debounce (snapshot, delay) ->
    sleep delay;
    emit (Ready snapshot)
  | Request snapshot ->
    let outcome = complete ~sw snapshot.input in
    emit (Completed (snapshot, outcome))
;;

let rec loop t ~sleep ~complete ~emit =
  Eio.Stream.take t.wake;
  if not t.closed
  then (
    let job = t.pending in
    t.pending <- None;
    Option.iter job ~f:(fun job ->
      (match
         Eio.Switch.run (fun sw ->
           t.active <- Some sw;
           perform ~sleep ~complete ~emit sw job)
       with
       | () -> ()
       | exception Superseded -> ());
      t.active <- None);
    loop t ~sleep ~complete ~emit)
;;

let create ~sw ~sleep ~complete ~emit =
  let finished, resolver = Eio.Promise.create () in
  let t =
    { wake = Eio.Stream.create 1
    ; pending = None
    ; active = None
    ; closed = false
    ; finished
    }
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Fun.protect
      ~finally:(fun () -> Eio.Promise.resolve resolver ())
      (fun () -> loop t ~sleep ~complete ~emit);
    `Stop_daemon);
  t
;;

let schedule t snapshot ~delay = enqueue t (Debounce (snapshot, delay))
let request t snapshot = enqueue t (Request snapshot)

let close t =
  t.closed <- true;
  cancel t;
  wake t;
  Eio.Promise.await t.finished
;;
