open! Core

type registration =
  { identity : unit ref
  ; mutable active : bool
  ; close : unit -> unit
  }

type t =
  { sw : Eio.Switch.t
  ; mutable closed : bool
  ; identity : unit ref
  ; mutable release : registration list
  }

type error = Closed [@@deriving equal, sexp_of]

let switch t = t.sw
let is_closed t = t.closed

module Registration = struct
  type t = registration
  type error = Foreign_registration [@@deriving equal, sexp_of]
end

let register_release t close =
  if t.closed
  then Error Closed
  else (
    let registration = { identity = t.identity; active = true; close } in
    t.release <- registration :: t.release;
    Ok registration)
;;

let release_registration (t : t) (registration : Registration.t) =
  if not (phys_equal registration.identity t.identity)
  then Error Registration.Foreign_registration
  else if registration.active
  then
    if not (List.mem t.release registration ~equal:phys_equal)
    then Error Registration.Foreign_registration
    else (
      registration.active <- false;
      t.release
      <- List.filter t.release ~f:(fun other -> not (phys_equal registration other));
      Eio.Cancel.protect registration.close;
      Ok ())
  else Ok ()
;;

let on_release t close = Result.map (register_release t close) ~f:ignore

let close t =
  if not t.closed
  then (
    t.closed <- true;
    let release = t.release in
    t.release <- [];
    List.iter release ~f:(fun registration -> registration.active <- false);
    let first = ref None in
    Eio.Cancel.protect (fun () ->
      List.iter release ~f:(fun registration ->
        try registration.close () with
        | exn ->
          if Option.is_none !first
          then first := Some (exn, Stdlib.Printexc.get_raw_backtrace ())));
    Option.iter !first ~f:(fun (exn, bt) -> Stdlib.Printexc.raise_with_backtrace exn bt))
;;

let create ~sw =
  let t = { sw; closed = false; identity = ref (); release = [] } in
  Eio.Switch.on_release sw (fun () -> close t);
  t
;;
