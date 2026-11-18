open! Core

let prepare (state : Session_state.t) ~delta =
  let open Result.Let_syntax in
  let%bind candidate = Session_delta.apply state delta in
  match candidate.goal_catalog with
  | None -> Ok delta
  | Some catalog ->
    let%bind changed =
      Goal_run_resolution.plan_catalog
        catalog
        ~runs:candidate.run_state
        ~hold_for_review:false
    in
    (match changed with
     | None -> Ok delta
     | Some catalog ->
       (match Session_replacement_delta.classify delta with
        | Some replacement ->
          Ok
            (Session_replacement_delta.with_state
               replacement
               { candidate with goal_catalog = Some catalog })
        | None -> Ok (Session_delta.Batch [ delta; Goal_catalog_changed catalog ])))
;;
