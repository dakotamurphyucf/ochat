open! Core
module P = Agent_protocol

let capture = Moderator_source_installation.capture
let installation = Option.map ~f:Run_state.installation

let prepare_change (state : Session_state.t) ~delta =
  let rec selected (generation, index, catalog) = function
    | Session_delta.Batch changes ->
      List.fold changes ~init:(generation, index, catalog) ~f:selected
    | Created candidate ->
      candidate.identity.generation, candidate.run_state, candidate.goal_catalog
    | Reset_generation generation -> generation, index, catalog
    | Run_state_changed index -> generation, Some index, catalog
    | Goal_catalog_changed catalog -> generation, index, Some catalog
    | _ -> generation, index, catalog
  in
  let generation, index, catalog =
    selected (state.identity.generation, state.run_state, state.goal_catalog) delta
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
      | Some catalog ->
        Goal_run_resolution.plan_catalog catalog ~runs:index ~hold_for_review:true
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
              { candidate with goal_catalog = Some catalog })
       | None -> Ok (Session_delta.Batch [ delta; Goal_catalog_changed catalog ]))
;;

let recovery_delta (state : Session_state.t) ~now =
  let open Result.Let_syntax in
  let%bind run =
    match state.run_state with
    | None -> Ok None
    | Some index ->
      if
        List.for_all (Run_state.runs index) ~f:(fun run ->
          match run.P.Run.lifecycle with
          | Terminal _ -> true
          | Admitted | Active | Waiting _ -> false)
      then Ok None
      else if Int64.equal state.counters.revision Int64.max_value
      then
        Error
          (P.Error.create
             Invalid_state
             ~message:"session revision exhausted during run recovery"
             ~retryable:false
             ())
      else
        Result.map
          (Run_retirement.recover
             index
             ~session_revision:(Int64.succ state.counters.revision)
             ~now)
          ~f:Option.some
  in
  let%bind goal =
    match state.goal_catalog with
    | None -> Ok None
    | Some catalog ->
      Goal_run_resolution.plan_catalog
        catalog
        ~runs:(Option.first_some run state.run_state)
        ~hold_for_review:true
  in
  let changes =
    Option.to_list (Option.map run ~f:(fun run -> Session_delta.Run_state_changed run))
    @ Option.to_list
        (Option.map goal ~f:(fun goal -> Session_delta.Goal_catalog_changed goal))
  in
  match changes with
  | [] -> Ok None
  | changes -> Ok (Some (Session_delta.Batch changes))
;;
