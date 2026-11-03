open! Core
module P = Agent_protocol
module S = Agent_store.Session_store
module R = Agent_store.Session_archive_record

type t =
  { store : S.t
  ; handle : S.Handle.t
  ; observation : S.Lifecycle.Observation.t
  ; delta : Session_delta.t option
  }

let delta t = t.delta

let check t =
  S.check_lifecycle_observation t.store t.handle t.observation
  |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
;;

let prepare (state : Session_state.t) ~store ~handle ~observation =
  let open Result.Let_syntax in
  let t = { store; handle; observation; delta = None } in
  let%bind () = check t in
  let%bind () =
    if P.Id.Session.equal state.identity.session_id (S.Handle.session_id handle)
    then Ok ()
    else Error (P.Error.invalid_request "recurrence recovery belongs to another session")
  in
  let lifecycle = S.Lifecycle.Observation.value observation in
  let%map retired =
    match state.recurrence_catalog with
    | None -> Ok None
    | Some catalog ->
      Recurrence_catalog.retire_stale_births
        catalog
        ~current_revision:(R.revision lifecycle)
        ~archived:
          (match R.status lifecycle with
           | Active -> false
           | Archived | Removed -> true)
  in
  { t with
    delta =
      Option.map retired ~f:(fun catalog ->
        Session_delta.Recurrence_catalog_changed catalog)
  }
;;
