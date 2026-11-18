open! Core
module P = Agent_protocol
module Q = P.Recurrence_query
module Execution = Recurrence_execution
module Attempt = Recurrence_execution_attempt
module Origin = Recurrence_run_origin
module Reference = Recurrence_run_reference

type t =
  { session : P.Session_ref.t
  ; session_revision : int64
  ; run : Q.Run_observation.t option
  }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let terminal = function
  | P.Run.Terminal.Completed _ -> Q.Outcome.Completed
  | Failed code -> Failed code
  | Cancelled -> Cancelled
  | Limited -> Limited
  | Interrupted -> Interrupted
;;

let outcome = function
  | Reference.Outcome.Completed -> Q.Outcome.Completed
  | Failed code -> Failed code
  | Cancelled -> Cancelled
  | Limited -> Limited
  | Interrupted -> Interrupted
;;

let lifecycle = function
  | P.Run.Lifecycle.Admitted -> Q.Run_observation.Admitted
  | Active -> Active
  | Waiting _ -> Waiting
  | Terminal value -> Terminal (terminal value)
;;

let project_visible (state : Session_state.t) ~intent ~attempt ~can_view =
  let open Result.Let_syntax in
  let request = Execution.Attempt.request attempt in
  let session = Recurrence_execution_intent.target intent in
  let%bind () = Attempt.validate request ~intent in
  let%bind () =
    if P.Id.Session.equal (P.Session_ref.session_id session) state.identity.session_id
    then Ok ()
    else conflict "recurrence target read returned another session"
  in
  let view run = { session; session_revision = state.counters.revision; run } in
  if not (can_view ~run_owner:(Attempt.principal_id request))
  then Ok (view None)
  else (
    let index = Option.value state.run_state ~default:Run_state.empty in
    let%bind evidence = Recurrence_run_evidence.find index ~intent ~attempt:request in
    match evidence with
    | None -> Ok (view None)
    | Some { origin; run; admission; terminal } ->
      let%bind () =
        match Execution.Attempt.origin attempt with
        | None -> Ok ()
        | Some retained ->
          let%bind () = Origin.validate retained ~intent ~attempt:request in
          Origin.validate_run retained ~run ~admission
      in
      let retained = Execution.Attempt.terminal attempt in
      let%map () =
        match retained, terminal with
        | None, _ -> Ok ()
        | Some retained, Some actual ->
          Reference.validate retained ~origin ~run ~receipt:(Reference.receipt actual)
        | Some _, None ->
          conflict "controller terminal recurrence evidence lacks target terminal"
      in
      view
        (Some
           { Q.Run_observation.id = run.id
           ; revision = run.revision
           ; status = lifecycle run.lifecycle
           ; retained_outcome =
               Option.map retained ~f:(fun value -> outcome (Reference.outcome value))
           }))
;;

let project state ~intent ~attempt =
  project_visible state ~intent ~attempt ~can_view:(fun ~run_owner:_ -> true)
;;

let project_public state ~intent ~attempt ~can_view =
  Result.map
    (project_visible state ~intent ~attempt ~can_view)
    ~f:(fun { session; session_revision; run } ->
      Q.Target.Visible { session; session_revision; run })
;;

let tagged name = `Object [ "kind", `String name ]

let outcome_json = function
  | Q.Outcome.Completed -> tagged "completed"
  | Failed code ->
    `Object
      [ "kind", `String "failed"
      ; ( "code"
        , Option.value_map code ~default:`Null ~f:(fun code ->
            `String (P.Error.code_to_string code)) )
      ]
  | Cancelled -> tagged "cancelled"
  | Limited -> tagged "limited"
  | Interrupted -> tagged "interrupted"
;;

let lifecycle_json = function
  | Q.Run_observation.Admitted -> tagged "admitted"
  | Active -> tagged "active"
  | Waiting -> tagged "waiting"
  | Terminal outcome -> outcome_json outcome
;;

let decimal n = `String (Int64.to_string n)

let to_json t =
  let common =
    [ "visibility", `String "visible"
    ; "session", P.Session_ref.to_json t.session
    ; "session_revision", decimal t.session_revision
    ]
  in
  match t.run with
  | None -> `Object (common @ [ "detail", `String "not_retained_or_not_recorded" ])
  | Some run ->
    `Object
      (common
       @ [ "detail", `String "retained"
         ; "run_id", P.Id.Run.to_json run.id
         ; "run_revision", decimal run.revision
         ; "observed_run", lifecycle_json run.status
         ; ( "retained_outcome"
           , Option.value_map run.retained_outcome ~default:`Null ~f:outcome_json )
         ])
;;
