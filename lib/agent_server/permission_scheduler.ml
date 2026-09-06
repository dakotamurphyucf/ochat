open! Core

type t = { closed : bool Atomic.t }

let timestamp clock =
  Eio.Time.now clock
  |> Time_ns.Span.of_sec
  |> Time_ns.of_span_since_epoch
  |> Agent_protocol.Timestamp.of_time_ns
;;

let process registry now =
  Session_registry.entries registry
  |> List.iter ~f:(fun entry -> entry.Session_registry.expire_permissions ~now)
;;

let rec run t clock registry =
  if not (Atomic.get t.closed)
  then (
    process registry (timestamp clock);
    Eio.Time.sleep clock 0.05;
    run t clock registry)
;;

let start ~sw ~clock ~registry =
  let t = { closed = Atomic.make false } in
  Eio.Fiber.fork ~sw (fun () -> run t clock registry);
  t
;;

let close t = Atomic.set t.closed true
let is_running t = not (Atomic.get t.closed)
