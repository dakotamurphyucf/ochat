open! Core
module P = Agent_protocol

let installation = Option.map ~f:Run_state.installation

let prepare_change (state : Session_state.t) ~delta =
  let rec selected (generation, index, catalog) = function
    | Session_delta.Batch changes ->
      List.fold changes ~init:(generation, index, catalog) ~f:selected
    | Created candidate ->
      candidate.identity.generation, candidate.run_state, candidate.recurrence_catalog
    | Reset_generation generation -> generation, index, catalog
    | Run_state_changed index -> generation, Some index, catalog
    | Recurrence_catalog_changed catalog -> generation, index, Some catalog
    | _ -> generation, index, catalog
  in
  let generation, index, catalog =
    selected (state.identity.generation, state.run_state, state.recurrence_catalog) delta
  in
  if
    generation = state.identity.generation
    && Option.equal
         Run_source_installation.equal
         (installation index)
         (installation state.run_state)
  then Ok delta
  else
    let open Result.Let_syntax in
    let%bind held =
      match catalog with
      | None -> Ok None
      | Some catalog -> Recurrence_catalog.hold_for_source catalog
    in
    match held with
    | None -> Ok delta
    | Some catalog ->
      (match Session_replacement_delta.classify delta with
       | Some replacement ->
         let candidate = Session_replacement_delta.state replacement in
         Ok
           (Session_replacement_delta.with_state
              replacement
              { candidate with recurrence_catalog = Some catalog })
       | None -> Ok (Session_delta.Batch [ delta; Recurrence_catalog_changed catalog ]))
;;

let installed (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind observer = Moderator_checkpoint.observer state.moderator in
  match state.run_state, observer with
  | Some index, Some observer ->
    let installed = Run_state.installation index in
    (match installed.source with
     | Some current when P.Invocation.equal_observer current observer ->
       Run_source_installation.captured installed ~generation:state.identity.generation
     | Some _ | None ->
       Error
         (P.Error.create
            Conflict
            ~message:"recurrence source installation is unavailable"
            ~retryable:false
            ()))
  | None, _ | Some _, None ->
    Error
      (P.Error.create
         Conflict
         ~message:"recurrence source installation is unavailable"
         ~retryable:false
         ())
;;
