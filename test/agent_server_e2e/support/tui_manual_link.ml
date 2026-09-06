open Core

type t =
  { mutable is_available : bool
  ; mutable generation : int
  ; mutable next_id : int
  ; connections : (int, unit -> unit) Hashtbl.t
  }

let is_available t = t.is_available
let connection_count t = Hashtbl.length t.connections

let shutdown flow =
  try Eio.Flow.shutdown flow `All with
  | Eio.Io _ -> ()
;;

let cut t =
  t.is_available <- false;
  t.generation <- t.generation + 1;
  Hashtbl.data t.connections |> List.iter ~f:(fun close -> close ())
;;

let resume t = t.is_available <- true

let serve t env upstream downstream _address =
  if not t.is_available
  then shutdown downstream
  else (
    let id = t.next_id in
    let generation = t.generation in
    t.next_id <- id + 1;
    Hashtbl.set t.connections ~key:id ~data:(fun () -> shutdown downstream);
    Exn.protect
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let remote = Eio.Net.connect ~sw (Eio.Stdenv.net env) upstream in
          if t.is_available && Int.equal generation t.generation
          then (
            Hashtbl.set t.connections ~key:id ~data:(fun () ->
              shutdown downstream;
              shutdown remote);
            Eio.Fiber.first
              (fun () -> Eio.Flow.copy downstream remote)
              (fun () -> Eio.Flow.copy remote downstream))))
      ~finally:(fun () -> Hashtbl.remove t.connections id))
;;

let serve_listener ~sw ~env ~upstream listener =
  let t =
    { is_available = true
    ; generation = 0
    ; next_id = 0
    ; connections = Hashtbl.create (module Int)
    }
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Net.run_server listener (serve t env upstream) ~on_error:(function
      | Eio.Io _ -> ()
      | exn -> Exn.reraise exn "manual relay"));
  t
;;

let start ~sw ~env ~listen_path ~upstream =
  let listener =
    Eio.Net.listen ~sw ~backlog:16 (Eio.Stdenv.net env) (`Unix listen_path)
  in
  serve_listener ~sw ~env ~upstream:(`Unix upstream) listener
;;

let start_tcp ~sw ~env ~upstream_port =
  let listener =
    Eio.Net.listen
      ~sw
      ~backlog:16
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | `Unix _ -> failwith "TCP relay listener returned a Unix address"
  in
  let upstream = `Tcp (Eio.Net.Ipaddr.V4.loopback, upstream_port) in
  serve_listener ~sw ~env ~upstream listener, port
;;
