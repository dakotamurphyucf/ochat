open! Core
module P = Agent_protocol
module Entry = Recurrence_entry
module Frontier = Recurrence_frontier
module Execution = Recurrence_execution

module Basis = struct
  type t =
    { session : P.Session_ref.t
    ; generation : int
    ; state_revision : int64
    ; catalog_revision : int64
    ; lifecycle_revision : P.Session_lifecycle.Revision.t
    ; automatic : bool
    ; source : P.Run_source.t option
    }
  [@@deriving equal]
end

module Definition_view = struct
  type t =
    { metadata : Recurrence_view.t
    ; schedule : Recurrence_definition.Schedule.t
    }
end

module Status = struct
  type t =
    { view : Definition_view.t
    ; frontier : Frontier.t
    ; planning : Recurrence_planning_status.t
    }
end

module History = struct
  type t =
    { view : Definition_view.t
    ; frontier : Frontier.t
    }
end

type request =
  | List
  | Get of P.Id.Recurrence.t
  | Status of P.Id.Recurrence.t
  | History of P.Id.Recurrence.t

type payload =
  | Listed of
      { basis : Basis.t
      ; entries : Definition_view.t list
      }
  | Found of
      { basis : Basis.t
      ; entry : Definition_view.t
      }
  | Current_status of
      { basis : Basis.t
      ; status : Status.t
      }
  | Retained_history of
      { basis : Basis.t
      ; history : History.t
      }

type t =
  { payload : payload
  ; current : Recurrence_controller.observation
  }

let check_current result =
  if Recurrence_controller.is_current result.current
  then Ok ()
  else
    Error
      (P.Error.create
         Conflict
         ~message:"recurrence read observation changed"
         ~retryable:false
         ())
;;

let basis result =
  match result.payload with
  | Listed { basis; _ }
  | Found { basis; _ }
  | Current_status { basis; _ }
  | Retained_history { basis; _ } -> basis
;;

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let read (state : Session_state.t) ~controller ~actor ~authorize ~planning ~request =
  let open Result.Let_syntax in
  let%bind () = Recurrence_command_admission.check_read actor ~authorize state in
  let%bind current = Recurrence_controller.observe controller state in
  let catalog = Option.value state.recurrence_catalog ~default:Recurrence_catalog.empty in
  let%bind source =
    let%bind observer = Moderator_checkpoint.observer state.moderator in
    match observer with
    | None -> Ok None
    | Some _ ->
      Result.map (Goal_catalog_source.capture state) ~f:(fun (_, source) -> Some source)
  in
  let basis =
    Basis.
      { session = Recurrence_controller.session controller
      ; generation = state.identity.generation
      ; state_revision = state.counters.revision
      ; catalog_revision = Recurrence_catalog.revision catalog
      ; lifecycle_revision = Recurrence_controller.revision current
      ; automatic = Recurrence_controller.automatic current
      ; source
      }
  in
  let view entry =
    let%map effective_control =
      Entry.effective_control
        entry
        ~current_revision:basis.lifecycle_revision
        ~archived:(Recurrence_controller.archived current)
    in
    Definition_view.
      { metadata = Recurrence_view.of_entry entry ~effective_control
      ; schedule = Recurrence_definition.schedule (Entry.current entry)
      }
  in
  let selected id =
    Result.of_option
      (Recurrence_catalog.find catalog id)
      ~error:(P.Error.invalid_request "recurrence identity is not retained")
  in
  let%bind result =
    match request with
    | List ->
      let%map entries =
        List.map (Recurrence_catalog.entries catalog) ~f:view |> Result.all
      in
      Listed { basis; entries }
    | Get id ->
      let%bind entry = selected id in
      let%map entry = view entry in
      Found { basis; entry }
    | Status id ->
      let%bind entry = selected id in
      let%bind view = view entry in
      let%map frontier = Entry.frontier entry in
      Current_status { basis; status = Status.{ view; frontier; planning } }
    | History id ->
      let%bind entry = selected id in
      let%bind view = view entry in
      let%map frontier = Entry.frontier entry in
      Retained_history { basis; history = History.{ view; frontier } }
  in
  let%bind () = Recurrence_command_admission.check_read actor ~authorize state in
  if Recurrence_controller.is_current current
  then Ok { payload = result; current }
  else conflict "recurrence read lifecycle observation changed"
;;

let decimal n = `String (Int64.to_string n)
let optional f = Option.value_map ~default:`Null ~f
let timestamp time = P.Timestamp.to_json (P.Timestamp.of_time_ns time)
let code code = `String (P.Error.code_to_string code)

let control_json = function
  | Entry.Enabled -> `Object [ "kind", `String "enabled" ]
  | Paused reason ->
    `Object
      [ "kind", `String "paused"
      ; ( "reason"
        , `String
            (match reason with
             | Operator_pause -> "operator_pause"
             | Controller_stop -> "controller_stop"
             | Source_review -> "source_review"
             | Recovery_review -> "recovery_review") )
      ]
  | Retired reason ->
    `Object
      [ "kind", `String "retired"
      ; ( "reason"
        , `String
            (match reason with
             | Operator_retire -> "operator_retire"
             | Lifecycle_changed -> "lifecycle_changed") )
      ]
;;

let view_json (definition : Definition_view.t) =
  let view = definition.metadata in
  `Object
    [ "id", P.Id.Recurrence.to_json view.id
    ; "controller", P.Session_ref.to_json view.controller
    ; "definition_revision", decimal view.definition_revision
    ; "control_revision", decimal view.control_revision
    ; "control", control_json view.control
    ; "source", P.Run_source.to_json view.source
    ; ( "birth_lifecycle_revision"
      , decimal (P.Session_lifecycle.Revision.to_int64 view.birth_lifecycle_revision) )
    ; "creator", P.Id.Principal.to_json view.creator
    ; "created_at", P.Timestamp.to_json view.created_at
    ; "schedule", Recurrence_definition.Schedule.to_json definition.schedule
    ; ( "target_kind"
      , `String
          (match view.target with
           | Existing_session -> "existing_session"
           | Fresh_child -> "fresh_child") )
    ; "policy", Recurrence_policy.to_json view.policy
    ]
;;

let basis_json (basis : Basis.t) =
  `Object
    [ "session", P.Session_ref.to_json basis.session
    ; "generation", `Number (Int.to_string basis.generation)
    ; "state_revision", decimal basis.state_revision
    ; "catalog_revision", decimal basis.catalog_revision
    ; ( "lifecycle_revision"
      , decimal (P.Session_lifecycle.Revision.to_int64 basis.lifecycle_revision) )
    ; ("automatic", if basis.automatic then `True else `False)
    ; "source", optional P.Run_source.to_json basis.source
    ]
;;

let totals_json frontier =
  let summary = Frontier.summary frontier in
  `Object
    [ "consumed", decimal summary.consumed
    ; "omitted", decimal summary.omitted
    ; "withdrawn", decimal summary.withdrawn
    ; "settled", decimal summary.settled
    ]
;;

let nominal_json nominal =
  `Object
    [ ( "occurrence"
      , Recurrence_identity.Occurrence.to_json (Recurrence_nominal.key nominal) )
    ; "at", timestamp (Recurrence_nominal.at nominal)
    ]
;;

let planning_json = function
  | Recurrence_planning_status.Available -> `Object [ "kind", `String "available" ]
  | Unavailable reason ->
    `Object
      [ "kind", `String "unavailable"
      ; ( "reason"
        , `String
            (match reason with
             | Not_configured -> "not_configured"
             | Not_granted -> "not_granted"
             | Expired -> "expired"
             | Closing -> "closing"
             | Lifecycle_not_automatic -> "lifecycle_not_automatic"
             | Controller_not_running -> "controller_not_running"
             | Source_unavailable -> "source_unavailable") )
      ]
;;

let issue_json = function
  | Frontier.Issue.Zone_unavailable -> `String "zone_unavailable"
  | Coverage_unavailable -> `String "coverage_unavailable"
  | Timing_exhausted -> `String "timing_exhausted"
  | Capacity_saturated -> `String "capacity_saturated"
  | Initial_delay_missed -> `String "initial_delay_missed"
  | Delay_nominal_missed -> `String "delay_nominal_missed"
  | Execution_interrupted -> `String "execution_interrupted"
  | Delay_successor_exhausted -> `String "delay_successor_exhausted"
;;

let reason_json = function
  | Frontier.Reason.Expired -> `String "expired"
  | Policy_skip -> `String "policy_skip"
  | Superseded -> `String "superseded"
  | Pending_capacity -> `String "pending_capacity"
  | Control_withdrawal -> `String "control_withdrawal"
;;

let protected_json frontier =
  List.map (Frontier.protected_children frontier) ~f:(fun reservation ->
    let occurrence = Recurrence_child_reservation.occurrence reservation in
    let linked =
      List.exists (Frontier.child_associations frontier) ~f:(fun association ->
        Recurrence_identity.Occurrence.equal
          occurrence
          (Recurrence_child_association.occurrence association))
    in
    `Object
      [ "nominal", nominal_json (Recurrence_child_reservation.nominal reservation)
      ; "evidence", `String "retained_controller_creation_history"
      ; ("link_recorded", if linked then `True else `False)
      ; ( "execution_recorded"
        , if Option.is_some (Frontier.find_execution frontier occurrence)
          then `True
          else `False )
      ])
;;

let render result ~read_target =
  let open Result.Let_syntax in
  (* One target snapshot per occurrence, shared by that occurrence's attempts.
     Drop it before reading the next occurrence: a history request must not
     retain hundreds of complete target session documents in a local cache. *)
  let attempt_json intent ~target_state attempt =
    let request = Execution.Attempt.request attempt in
    let%map target =
      match target_state with
      | Ok state ->
        Recurrence_target_view.project state ~intent ~attempt
        |> Result.map ~f:Recurrence_target_view.to_json
      | Error (error : P.Error.t) ->
        if P.Error.equal_code error.code Permission_denied
        then Ok (`Object [ "visibility", `String "denied" ])
        else if P.Error.equal_code error.code Session_not_found
        then
          Ok
            (`Object
                [ "visibility", `String "unavailable"; "reason", `String "not_found" ])
        else Error error
    in
    `Object
      [ "number", `Number (Int.to_string (Recurrence_execution_attempt.number request))
      ; "rejection", optional code (Execution.Attempt.rejection attempt)
      ; "uncertainty", optional code (Execution.Attempt.uncertainty attempt)
      ; ( "retry_resolution_recorded"
        , if Option.is_some (Execution.Attempt.retry_resolution attempt)
          then `True
          else `False )
      ; ( "admission_recorded"
        , if Option.is_some (Execution.Attempt.origin attempt) then `True else `False )
      ; ( "terminal_recorded"
        , if Option.is_some (Execution.Attempt.terminal attempt) then `True else `False )
      ; "target", target
      ]
  in
  let execution_json execution ~all_attempts =
    let intent = Execution.intent execution in
    let attempts =
      if all_attempts
      then Execution.attempts execution
      else [ Execution.current_attempt execution ]
    in
    let target_state = read_target (Recurrence_execution_intent.target intent) in
    let%map attempts =
      List.map attempts ~f:(attempt_json intent ~target_state) |> Result.all
    in
    `Object
      [ "nominal", nominal_json (Recurrence_execution_intent.nominal intent)
      ; "evidence", `String "retained_controller_history"
      ; "attempts", `Array attempts
      ]
  in
  let%map payload =
    match result.payload with
    | Listed { entries; _ } ->
      Ok (`Object [ "entries", `Array (List.map entries ~f:view_json) ])
    | Found { entry; _ } -> Ok (`Object [ "entry", view_json entry ])
    | Current_status { status; _ } ->
      let frontier = status.frontier in
      let executions = Frontier.executions frontier in
      let pending = List.filter executions ~f:Execution.is_pending in
      let latest =
        List.max_elt executions ~compare:(fun left right ->
          Time_ns.compare
            (Recurrence_nominal.at
               (Recurrence_execution_intent.nominal (Execution.intent left)))
            (Recurrence_nominal.at
               (Recurrence_execution_intent.nominal (Execution.intent right))))
      in
      let selected =
        match latest with
        | None -> pending
        | Some latest ->
          if List.exists pending ~f:(Execution.equal latest)
          then pending
          else pending @ [ latest ]
      in
      let%map executions =
        List.map selected ~f:(execution_json ~all_attempts:false) |> Result.all
      in
      `Object
        [ "entry", view_json status.view
        ; "planning", planning_json status.planning
        ; "planning_revision", decimal (Frontier.revision frontier)
        ; "pending_count", `Number (Int.to_string (Frontier.pending_count frontier))
        ; "unresolved_count", `Number (Int.to_string (Frontier.unresolved_count frontier))
        ; "issue", optional issue_json (Frontier.issue frontier)
        ; "totals", totals_json frontier
        ; "ready", `Array (List.map (Frontier.ready frontier) ~f:nominal_json)
        ; "lookahead", optional nominal_json (Frontier.lookahead frontier)
        ; ( "alarm_due_at"
          , optional
              (fun alarm -> timestamp (Recurrence_alarm.due_at alarm))
              (Frontier.alarm frontier) )
        ; "protected_creations", `Array (protected_json frontier)
        ; "occurrences", `Array executions
        ]
    | Retained_history { history; _ } ->
      let%map executions =
        List.map
          (Frontier.executions history.frontier)
          ~f:(execution_json ~all_attempts:true)
        |> Result.all
      in
      let omitted =
        List.map (Frontier.history history.frontier) ~f:(fun detail ->
          `Object
            [ ( "definition"
              , Recurrence_identity.Definition.to_json detail.Frontier.History.definition
              )
            ; "first_slot", decimal detail.first_slot
            ; "last_slot", decimal detail.last_slot
            ; ( "kind"
              , `String
                  (match detail.kind with
                   | Skipped -> "skipped"
                   | Withdrawn -> "withdrawn") )
            ; "reason", reason_json detail.reason
            ])
      in
      `Object
        [ "entry", view_json history.view
        ; "evidence", `String "retained_controller_history"
        ; "totals", totals_json history.frontier
        ; "retained_ranges", `Array omitted
        ; "protected_creations", `Array (protected_json history.frontier)
        ; "occurrences", `Array executions
        ]
  in
  `Object
    [ "version", `Number "1"; "basis", basis_json (basis result); "result", payload ]
;;
