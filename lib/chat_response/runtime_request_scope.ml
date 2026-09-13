open Core

type state =
  | Open of Moderation.Runtime_request.t list
  | Closed

type t = state Atomic.t

let key = Eio.Fiber.create_key ()
let capture () = Option.join (Eio.Fiber.get key)
let with_context context f = Eio.Fiber.with_binding key context f

let collect f =
  let context = Atomic.make (Open []) in
  Exn.protect
    ~finally:(fun () -> Atomic.set context Closed)
    ~f:(fun () ->
      with_context (Some context) (fun () ->
        let result = f () in
        match Atomic.exchange context Closed with
        | Open requests -> result, requests
        | Closed -> assert false))
;;

let emit requests =
  let rec append context =
    match Atomic.get context with
    | Closed -> Error "runtime request scope has ended"
    | Open previous as current ->
      let next = Open (Runtime_semantics.collapse (previous @ requests)) in
      (match Atomic.compare_and_set context current next with
       | true -> Ok ()
       | false -> append context)
  in
  match capture () with
  | None -> Error "runtime request scope is not installed"
  | Some context -> append context
;;
