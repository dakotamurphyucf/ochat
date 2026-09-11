open Core

type t =
  { session_id : string
  ; approval_store : Shell_access.Approval.store
  ; approval_provider : Approval_broker.provider
  ; check : unit -> (unit, string) result
  }

type binding =
  { services : unit -> (t, string) result
  ; active : bool Atomic.t
  }

let key = Eio.Fiber.create_key ()

let with_services services f =
  let binding = { services; active = Atomic.make true } in
  Exn.protect
    ~finally:(fun () -> Atomic.set binding.active false)
    ~f:(fun () -> Eio.Fiber.with_binding key (Some binding) f)
;;

let without_services f = Eio.Fiber.with_binding key None f

let current () =
  match Option.join (Eio.Fiber.get key) with
  | None -> Ok None
  | Some binding ->
    (match Atomic.get binding.active with
     | false -> Error "shell invocation services have expired"
     | true ->
       let open Result.Let_syntax in
       let%bind services = binding.services () in
       let check () =
         match Atomic.get binding.active with
         | false -> Error "shell invocation services have expired"
         | true -> services.check ()
       in
       let%map () = check () in
       Some { services with check })
;;
