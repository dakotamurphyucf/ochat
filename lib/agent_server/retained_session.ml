open! Core
module P = Agent_protocol
module S = Agent_store.Session_store
module State = Agent_session.Session_state
module Registry = Session_registry

type t =
  { store : S.t
  ; registry : Registry.t
  ; read_owned : S.Handle.t -> (State.t, P.Error.t) Result.t
  }

let create ~store ~registry ~read_owned = { store; registry; read_owned }
let stored result = Result.map_error result ~f:Agent_store.Store_error.to_protocol_error
let fail code message = Error (P.Error.create code ~message ~retryable:false ())

let available t handle =
  if not (S.owns_handle t.store handle)
  then fail Persistence_error "retained Handle belongs to another Store"
  else
    S.Handle.metadata_checked handle
    |> Result.map_error ~f:(fun error ->
      P.Error.create
        Persistence_error
        ~message:(Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
        ~retryable:true
        ())
    |> Result.map ~f:(fun (_ : S.Metadata.t) -> ())
;;

let observe t ~session_id ~handle ~authorize ~f read =
  let open Result.Let_syntax in
  let%bind () = available t handle in
  let%bind state = read () in
  let summary = State.summary state in
  let%bind () =
    if
      P.Id.Session.equal summary.id session_id
      && P.Id.Session.equal (S.Handle.session_id handle) session_id
    then Ok ()
    else fail Persistence_error "retained reader returned another session"
  in
  let%bind () = available t handle in
  let%bind () = authorize summary in
  let%bind result = f handle state in
  let%bind () = available t handle in
  if Registry.is_closing t.registry
  then fail Server_shutting_down "retained host is closing"
  else Ok result
;;

let with_state t ~session_id ~authorize ~f =
  let open Result.Let_syntax in
  Registry.with_lifecycle t.registry session_id (fun reservation ->
    match Registry.Lifecycle_reservation.target reservation with
    | Absent -> fail Session_not_found "session is absent"
    | Loaded entry ->
      let%bind handle =
        Result.of_option
          entry.Registry.store_handle
          ~error:
            (P.Error.create
               Invalid_state
               ~message:"session has no durable Handle"
               ~retryable:false
               ())
      in
      let%bind () = available t handle in
      let%bind () = authorize (S.Handle.metadata handle).session in
      observe t ~session_id ~handle ~authorize ~f (fun () ->
        Agent_session.Session_actor.state entry.actor)
    | Indexed indexed ->
      let%bind () = authorize indexed.Agent_store.Session_index.Entry.session in
      Eio.Switch.run (fun sw ->
        let%bind handle =
          S.open_session
            t.store
            ~sw
            ~actor_lock_nonce:(P.Id.Transaction.create () |> P.Id.Transaction.to_string)
            session_id
          |> stored
        in
        Registry.with_recovery_handle t.registry ~store:t.store handle (fun () ->
          observe t ~session_id ~handle ~authorize ~f (fun () -> t.read_owned handle))))
;;
