open Core

module Clock = struct
  type t =
    { nanoseconds : int64 Atomic.t
    ; mutable changed : unit Eio.Promise.t * unit Eio.Promise.u
    }

  let create () = { nanoseconds = Atomic.make 0L; changed = Eio.Promise.create () }
  let now t = Mtime.of_uint64_ns (Atomic.get t.nanoseconds)

  let advance t ~seconds =
    let nanos = Int64.of_float (seconds *. 1e9) in
    Atomic.set t.nanoseconds Int64.(Atomic.get t.nanoseconds + nanos);
    let _, resolver = t.changed in
    t.changed <- Eio.Promise.create ();
    Eio.Promise.resolve resolver ()
  ;;

  let rec sleep_until t deadline =
    if Mtime.compare (now t) deadline >= 0
    then Eio.Fiber.yield ()
    else (
      let changed, _ = t.changed in
      Eio.Promise.await changed;
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
end

module Observed_manager = struct
  type t =
    { actual : Eio.Domain_manager.ty Eio.Resource.t
    ; on_start : unit -> unit
    ; completed : int Atomic.t
    }

  let run t f =
    t.on_start ();
    let (Eio.Resource.T (data, ops)) = t.actual in
    let module Native = (val Eio.Resource.get ops Eio.Domain_manager.Pi.Mgr) in
    Native.run data (fun ~cancelled ->
      Exn.protect ~finally:(fun () -> Atomic.incr t.completed) ~f:(fun () -> f ~cancelled))
  ;;

  let run_raw t f = Eio.Domain_manager.run_raw t.actual f
end

let environment clock env ~on_domain_start ~completed : Eio_unix.Stdenv.base =
  let observed =
    Observed_manager.
      { actual = Eio.Stdenv.domain_mgr env; on_start = on_domain_start; completed }
  in
  let manager =
    Eio.Resource.T (observed, Eio.Domain_manager.Pi.mgr (module Observed_manager))
  in
  object
    method domain_mgr = manager
    method mono_clock = Clock.resource clock
    method clock = env#clock
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method process_mgr = env#process_mgr
    method fs = env#fs
    method cwd = env#cwd
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;
