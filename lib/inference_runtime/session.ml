open! Core

type t =
  { sw : Eio.Switch.t
  ; mutable closed : bool
  ; mutable release : (unit -> unit) list
  }

type error = Closed [@@deriving equal, sexp_of]

let switch t = t.sw
let is_closed t = t.closed

let on_release t release =
  if t.closed
  then Error Closed
  else (
    t.release <- release :: t.release;
    Ok ())
;;

let close t =
  if not t.closed
  then (
    t.closed <- true;
    let release = t.release in
    t.release <- [];
    let first = ref None in
    Eio.Cancel.protect (fun () ->
      List.iter release ~f:(fun f ->
        try f () with
        | exn ->
          if Option.is_none !first
          then first := Some (exn, Stdlib.Printexc.get_raw_backtrace ())));
    Option.iter !first ~f:(fun (exn, bt) -> Stdlib.Printexc.raise_with_backtrace exn bt))
;;

let create ~sw =
  let t = { sw; closed = false; release = [] } in
  Eio.Switch.on_release sw (fun () -> close t);
  t
;;
