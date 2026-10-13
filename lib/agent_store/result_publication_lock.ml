open! Core

type t = { mutex : Eio.Mutex.t }

type scope =
  { lock : t
  ; mutable live : bool
  }

let create () = { mutex = Eio.Mutex.create () }
let active : scope list Eio.Fiber.key = Eio.Fiber.create_key ()

let is_active t =
  Option.value (Eio.Fiber.get active) ~default:[]
  |> List.exists ~f:(fun scope -> scope.live && phys_equal scope.lock t)
;;

let with_binding t f =
  let stack = Option.value (Eio.Fiber.get active) ~default:[] in
  let scope = { lock = t; live = true } in
  Exn.protect
    ~f:(fun () -> Eio.Fiber.with_binding active (scope :: stack) f)
    ~finally:(fun () -> scope.live <- false)
;;

let with_owner t ~on_reentry f =
  if is_active t
  then on_reentry ()
  else (
    let outcome =
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        with_binding t (fun () ->
          try Ok (f ()) with
          | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())))
    in
    match outcome with
    | Ok value -> value
    | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace)
;;

let with_read t f =
  if is_active t
  then invalid_arg "recursive result publication read"
  else Eio.Mutex.use_ro t.mutex (fun () -> with_binding t f)
;;
