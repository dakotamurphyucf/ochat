open Core

type t =
  { session_id : string
  ; approval_store : Shell_access.Approval.store
  ; approval_provider : Approval_broker.provider
  ; check : unit -> (unit, string) result
  }

type binding =
  { source : source
  ; prepare_executor :
      Shell_access.Executor.config -> (Shell_access.Executor.config, string) result
  ; active : bool Atomic.t
  }

and source =
  | Services of (unit -> (t, string) result)
  | Check_only of (unit -> (unit, string) result)

let key = Eio.Fiber.create_key ()

let with_services ?(prepare_executor = fun config -> Ok config) services f =
  let binding =
    { source = Services services; prepare_executor; active = Atomic.make true }
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set binding.active false)
    ~f:(fun () -> Eio.Fiber.with_binding key (Some binding) f)
;;

let with_check_only ?(prepare_executor = fun config -> Ok config) check f =
  let active = Atomic.make true in
  let check () =
    let open Result.Let_syntax in
    if not (Atomic.get active)
    then Error "shell invocation services have expired"
    else (
      let%bind () = check () in
      if Atomic.get active then Ok () else Error "shell invocation services have expired")
  in
  let prepare_executor config =
    let open Result.Let_syntax in
    let%bind config = prepare_executor config in
    let%map () = check () in
    Shell_access.Executor.with_execution_check config ~check
  in
  let binding = { source = Check_only check; prepare_executor; active } in
  Exn.protect
    ~finally:(fun () -> Atomic.set active false)
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
       (match binding.source with
        | Check_only check ->
          let%map () = check () in
          None
        | Services services ->
          let%bind services = services () in
          let check () =
            if not (Atomic.get binding.active)
            then Error "shell invocation services have expired"
            else (
              let%bind () = services.check () in
              if Atomic.get binding.active
              then Ok ()
              else Error "shell invocation services have expired")
          in
          let%map () = check () in
          Some { services with check }))
;;

let with_narrowed_check ~refresh f =
  let open Result.Let_syntax in
  let child_check () =
    let%bind check_now = refresh () in
    check_now ()
  in
  match Option.join (Eio.Fiber.get key) with
  | None ->
    let%map () = child_check () in
    with_check_only child_check f
  | Some parent ->
    let parent_live () =
      if Atomic.get parent.active
      then Ok ()
      else Error "shell parent invocation services have expired"
    in
    (match parent.source with
     | Check_only parent_check ->
       let check () =
         let%bind () = parent_live () in
         let%bind () = parent_check () in
         let%bind child_check_now = refresh () in
         let%bind () = parent_live () in
         let%bind () = parent_check () in
         let%bind () = child_check_now () in
         parent_live ()
       in
       let%map () = check () in
       with_check_only ~prepare_executor:parent.prepare_executor check f
     | Services parent_services ->
       let checked_services () =
         let%bind () = parent_live () in
         let%bind initial_services = parent_services () in
         let%bind () = initial_services.check () in
         let%bind child_check_now = refresh () in
         let%bind () = parent_live () in
         let%bind final_services = parent_services () in
         let%bind () = final_services.check () in
         let%bind () = child_check_now () in
         let%map () = parent_live () in
         final_services
       in
       let check () = Result.map (checked_services ()) ~f:ignore in
       let services () =
         let%map parent_services = checked_services () in
         { parent_services with check }
       in
       let prepare_executor config =
         let%bind () = parent_live () in
         parent.prepare_executor config
       in
       let%map () = check () in
       with_services ~prepare_executor services f)
;;

let prepare_executor config =
  let open Result.Let_syntax in
  let%bind context = current () in
  match context, Option.join (Eio.Fiber.get key) with
  | None, None -> Ok config
  | None, Some binding | Some _, Some binding -> binding.prepare_executor config
  | Some _, None -> Error "shell executor adapter scope is unavailable"
;;

let prepare_executor_for_call config =
  let open Result.Let_syntax in
  let%bind context = current () in
  let config =
    match context with
    | None -> config
    | Some context ->
      Shell_access.Executor.with_execution_scope
        config
        ~session_id:context.session_id
        ~approval_store:context.approval_store
        ~check:context.check
  in
  prepare_executor config
;;
