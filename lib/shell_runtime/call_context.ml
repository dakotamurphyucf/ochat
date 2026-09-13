open Core

type t =
  { session_id : string
  ; approval_store : Shell_access.Approval.store
  ; approval_provider : Approval_broker.provider
  ; check : unit -> (unit, string) result
  }

type binding =
  { services : unit -> (t, string) result
  ; prepare_executor :
      Shell_access.Executor.config -> (Shell_access.Executor.config, string) result
  ; active : bool Atomic.t
  }

let key = Eio.Fiber.create_key ()

let with_services ?(prepare_executor = fun config -> Ok config) services f =
  let binding = { services; prepare_executor; active = Atomic.make true } in
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

let prepare_executor config =
  let open Result.Let_syntax in
  let%bind context = current () in
  match context, Option.join (Eio.Fiber.get key) with
  | None, _ -> Ok config
  | Some _, Some binding -> binding.prepare_executor config
  | Some _, None -> Error "shell executor adapter scope is unavailable"
;;
