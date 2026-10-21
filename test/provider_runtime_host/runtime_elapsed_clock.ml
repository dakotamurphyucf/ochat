open! Core

type t =
  { real : Eio.Time.Mono.ty Eio.Resource.t
  ; offset : Mtime.span Atomic.t
  ; owner : int
  ; mutable changed : unit Eio.Promise.t * unit Eio.Promise.u
  }

let create real =
  { real
  ; offset = Atomic.make Mtime.Span.zero
  ; owner = (Stdlib.Domain.self () :> int)
  ; changed = Eio.Promise.create ()
  }
;;

let require_owner t =
  if not (Int.equal t.owner (Stdlib.Domain.self () :> int))
  then invalid_arg "elapsed clock sleeps and advances require the creating domain"
;;

let now t =
  Mtime.add_span (Eio.Time.Mono.now t.real) (Atomic.get t.offset) |> Option.value_exn
;;

let rec sleep_until t deadline =
  require_owner t;
  let changed, _ = t.changed in
  let current = now t in
  if Mtime.compare current deadline >= 0
  then Eio.Fiber.yield ()
  else (
    Eio.Fiber.first
      (fun () -> Eio.Time.Mono.sleep_span t.real (Mtime.span current deadline))
      (fun () -> Eio.Promise.await changed);
    sleep_until t deadline)
;;

let resource t =
  let module Provider = struct
    type nonrec t = t
    type time = Mtime.t

    let now = now
    let sleep_until = sleep_until
  end
  in
  Eio.Resource.T (t, Eio.Time.Pi.clock (module Provider))
;;

let advance t ~seconds =
  require_owner t;
  let additional =
    Mtime.Span.of_float_ns (seconds *. 1e9)
    |> Option.value_exn ~message:"invalid elapsed clock advance"
  in
  let previous = Atomic.get t.offset in
  let offset = Mtime.Span.add previous additional in
  if
    Mtime.Span.compare offset previous < 0
    || Option.is_none (Mtime.add_span (Eio.Time.Mono.now t.real) offset)
  then invalid_arg "elapsed clock advance overflow";
  Atomic.set t.offset offset;
  let _, resolver = t.changed in
  t.changed <- Eio.Promise.create ();
  Eio.Promise.resolve resolver ()
;;
