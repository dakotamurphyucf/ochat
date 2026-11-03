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

let reconcile_lifecycle_receipts observation ~store ~handle ~complete =
  let open Result.Let_syntax in
  let protocol result =
    Result.map_error result ~f:Agent_store.Store_error.to_protocol_error
  in
  let%bind () = S.check_lifecycle_observation store handle observation |> protocol in
  let previous = S.Lifecycle.Observation.value observation in
  let%bind () =
    List.fold_result (R.receipts previous) ~init:() ~f:(fun () receipt ->
      if receipt.R.Receipt.completion_acknowledged
      then Ok ()
      else
        S.complete_lifecycle_outcome
          store
          handle
          ~key:receipt.key
          ~request_digest:receipt.request_digest
          ~complete:(complete ~key:receipt.key ~request_digest:receipt.request_digest)
        |> protocol)
  in
  let%bind current = S.read_lifecycle store handle |> protocol in
  let value = S.Lifecycle.Observation.value current in
  if
    P.Id.Session.equal (R.session_id previous) (R.session_id value)
    && R.Revision.equal (R.revision previous) (R.revision value)
    && R.Status.equal (R.status previous) (R.status value)
    && R.Admission.equal (R.admission previous) (R.admission value)
  then Ok current
  else
    Error
      (P.Error.create
         Conflict
         ~message:"session lifecycle changed during receipt recovery"
         ~retryable:false
         ())
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
