open! Core
module P = Agent_protocol

type t =
  { delta : Session_delta.t option
  ; current : Recurrence_controller.observation
  }

let prepare (state : Session_state.t) ~controller =
  let open Result.Let_syntax in
  let%bind current = Recurrence_controller.observe controller state in
  let%map retired =
    match state.recurrence_catalog with
    | None -> Ok None
    | Some catalog ->
      Recurrence_catalog.retire_stale_births
        catalog
        ~current_revision:(Recurrence_controller.revision current)
        ~archived:(Recurrence_controller.archived current)
  in
  { delta =
      Option.map retired ~f:(fun catalog ->
        Session_delta.Recurrence_catalog_changed catalog)
  ; current
  }
;;

let check_current t =
  if Recurrence_controller.is_current t.current
  then Ok ()
  else
    Error
      (P.Error.create
         Conflict
         ~message:"recurrence reconciliation lifecycle changed"
         ~retryable:false
         ())
;;
