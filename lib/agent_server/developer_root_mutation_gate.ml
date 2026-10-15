open! Core

type t = { mutex : Eio.Mutex.t }

exception Recursive_mutation

let held : bool ref Eio.Fiber.key = Eio.Fiber.create_key ()
let create () = { mutex = Eio.Mutex.create () }

let with_owner t ~f =
  match Eio.Fiber.get held with
  | Some active when !active -> raise Recursive_mutation
  | None | Some _ ->
    Eio.Mutex.lock t.mutex;
    let active = ref true in
    Exn.protect
      ~f:(fun () -> Eio.Fiber.with_binding held active f)
      ~finally:(fun () ->
        Eio.Cancel.protect (fun () ->
          active := false;
          Eio.Mutex.unlock t.mutex))
;;
