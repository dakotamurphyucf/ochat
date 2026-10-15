open! Core
module O = Inference.Observation
module Terminal = Inference.Event.Terminal

type disposition =
  | Changed of O.Attempt_record.t
  | Unchanged

type error =
  | Conflict
  | Limit_exceeded

let ( let* ) result f = Result.bind result ~f

let record prior ~state ~observations =
  O.Attempt_record.create
    ~scope:(O.Attempt_record.scope prior)
    ~accounting_id:(O.Attempt_record.accounting_id prior)
    ~configuration:(O.Attempt_record.configuration prior)
    ~state
    ~observations
    ~omitted_diagnostics:(O.Attempt_record.omitted_diagnostics prior)
    ~limits:O.Admission.attempt
  |> Result.map_error ~f:(function
    | O.Error.Retention_limit | Json (Document_schema.Error.Limit_exceeded _) ->
      Limit_exceeded
    | Json _
    | Invalid_field _
    | Conflicting_revision
    | Conflicting_scope
    | Conflicting_kind -> Conflict)
;;

let replace_state prior state =
  let* next = record prior ~state ~observations:(O.Attempt_record.observations prior) in
  Ok (Changed next)
;;

let started prior =
  match O.Attempt_record.state prior with
  | Prepared -> replace_state prior Running
  | Running -> Ok Unchanged
  | Terminal _ | Interrupted _ -> Error Conflict
;;

let observed prior incoming =
  let state = O.Attempt_record.state prior in
  match state with
  | Prepared -> Error Conflict
  | Running | Terminal _ | Interrupted _ ->
    let* latest =
      O.Latest.create ~max_observations:64 ~max_retained_bytes:(64 * 1024)
      |> Result.map_error ~f:(fun _ -> Limit_exceeded)
    in
    let* latest =
      List.fold_result
        (O.Attempt_record.observations prior)
        ~init:latest
        ~f:(fun current observation ->
          O.Latest.observe current observation
          |> Result.map ~f:fst
          |> Result.map_error ~f:(fun _ -> Conflict))
    in
    let* latest, disposition =
      O.Latest.observe latest incoming
      |> Result.map_error ~f:(function
        | Retention_limit | Json (Document_schema.Error.Limit_exceeded _) ->
          Limit_exceeded
        | Json _
        | Invalid_field _
        | Conflicting_revision
        | Conflicting_scope
        | Conflicting_kind -> Conflict)
    in
    (match disposition with
     | Added | Replaced ->
       let* next = record prior ~state ~observations:(O.Latest.observations latest) in
       Ok (Changed next)
     | Duplicate | Stale -> Ok Unchanged)
;;

let finished prior terminal =
  match O.Attempt_record.state prior with
  | Prepared -> Error Conflict
  | Running -> replace_state prior (Terminal terminal)
  | Terminal existing ->
    if Terminal.equal existing terminal then Ok Unchanged else Error Conflict
  | Interrupted _ -> Error Conflict
;;

let interrupted prior ~reason ~delivery =
  match O.Attempt_record.state prior with
  | Prepared | Running -> replace_state prior (Interrupted { reason; delivery })
  | Terminal _ -> Error Conflict
  | Interrupted existing ->
    if
      O.Attempt_record.equal_interruption existing.reason reason
      && Terminal.equal_delivery existing.delivery delivery
    then Ok Unchanged
    else Error Conflict
;;
