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

type snapshot =
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
  { snapshot : snapshot
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

let snapshot result = result.snapshot

let snapshot_basis = function
  | Listed { basis; _ }
  | Found { basis; _ }
  | Current_status { basis; _ }
  | Retained_history { basis; _ } -> basis
;;

let basis result = snapshot_basis result.snapshot

let planning = function
  | Current_status { status; _ } -> Some status.planning
  | Listed _ | Found _ | Retained_history _ -> None
;;

let same_basis left right =
  Basis.equal (snapshot_basis left) (snapshot_basis right)
  && Option.equal Recurrence_planning_status.equal (planning left) (planning right)
;;

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let project
      (state : Session_state.t)
      ~session
      ~lifecycle_revision
      ~automatic
      ~archived
      ~planning
      ~request
  =
  let open Result.Let_syntax in
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
      { session
      ; generation = state.identity.generation
      ; state_revision = state.counters.revision
      ; catalog_revision = Recurrence_catalog.revision catalog
      ; lifecycle_revision
      ; automatic
      ; source
      }
  in
  let view entry =
    let%map effective_control =
      Entry.effective_control entry ~current_revision:basis.lifecycle_revision ~archived
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
;;

let read state ~controller ~actor ~authorize ~planning ~request =
  let open Result.Let_syntax in
  let%bind () = Recurrence_command_admission.check_read actor ~authorize state in
  let%bind current = Recurrence_controller.observe controller state in
  let%bind snapshot =
    project
      state
      ~session:(Recurrence_controller.session controller)
      ~lifecycle_revision:(Recurrence_controller.revision current)
      ~automatic:(Recurrence_controller.automatic current)
      ~archived:(Recurrence_controller.archived current)
      ~planning
      ~request
  in
  let%bind () = Recurrence_command_admission.check_read actor ~authorize state in
  if Recurrence_controller.is_current current
  then Ok { snapshot; current }
  else conflict "recurrence read lifecycle observation changed"
;;

let read_indexed state ~store ~handle ~indexed ~actor ~authorize ~request =
  let module S = Agent_store.Session_store in
  let open Result.Let_syntax in
  let%bind () = Recurrence_command_admission.check_read actor ~authorize state in
  let summary = Session_state.summary state in
  let%bind () =
    if
      P.Id.Session.equal summary.id (S.Handle.session_id handle)
      && Jsonaf.exactly_equal
           (P.Session.to_json summary)
           (P.Session.to_json indexed.Agent_store.Session_index.Entry.session)
    then Ok ()
    else conflict "recurrence indexed state differs from its canonical projection"
  in
  let%bind current =
    S.current_lifecycle store handle ~current_entry:indexed
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  let entry = S.Lifecycle.Current.entry current in
  let%bind () =
    if Agent_store.Session_index.Entry.equal indexed entry
    then Ok ()
    else conflict "recurrence indexed lifecycle binding changed"
  in
  let automatic =
    match entry.admission with
    | Automatic -> not entry.archived
    | Explicit_resume_required -> false
  in
  let%bind snapshot =
    project
      state
      ~session:
        (P.Session_ref.create ~server_id:(S.server_id store) ~session_id:summary.id)
      ~lifecycle_revision:entry.lifecycle_revision
      ~automatic
      ~archived:entry.archived
      ~planning:(Unavailable Controller_not_loaded)
      ~request
  in
  let%bind () = Recurrence_command_admission.check_read actor ~authorize state in
  if S.Lifecycle.Current.is_current current handle
  then Ok snapshot
  else conflict "recurrence indexed lifecycle observation changed"
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
             | Recovery_review -> "recovery_review"
             | Target_invalidated _ -> "target_review") )
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
             | Source_unavailable -> "source_unavailable"
             | Controller_not_loaded -> "controller_not_loaded") )
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

let protected_creations frontier ~f =
  List.map (Frontier.protected_children frontier) ~f:(fun reservation ->
    let occurrence = Recurrence_child_reservation.occurrence reservation in
    let linked =
      List.exists (Frontier.child_associations frontier) ~f:(fun association ->
        Recurrence_identity.Occurrence.equal
          occurrence
          (Recurrence_child_association.occurrence association))
    in
    let execution_recorded =
      Option.is_some (Frontier.find_execution frontier occurrence)
    in
    f
      (Recurrence_child_reservation.nominal reservation)
      ~link_recorded:linked
      ~execution_recorded)
;;

let protected_json frontier =
  protected_creations frontier ~f:(fun nominal ~link_recorded ~execution_recorded ->
    `Object
      [ "nominal", nominal_json nominal
      ; "evidence", `String "retained_controller_creation_history"
      ; ("link_recorded", if link_recorded then `True else `False)
      ; ("execution_recorded", if execution_recorded then `True else `False)
      ])
;;

(* One target snapshot per occurrence, shared by that occurrence's attempts.
   Drop it before reading the next occurrence. *)
let map_attempts execution ~read_target ~all_attempts ~f =
  let open Result.Let_syntax in
  let intent = Execution.intent execution in
  let attempts =
    if all_attempts
    then Execution.attempts execution
    else [ Execution.current_attempt execution ]
  in
  let target_state = read_target (Recurrence_execution_intent.target intent) in
  let%map attempts = List.map attempts ~f:(f intent ~target_state) |> Result.all in
  Recurrence_execution_intent.nominal intent, attempts
;;

let status_executions frontier =
  let executions = Frontier.executions frontier in
  let pending = List.filter executions ~f:Execution.is_pending in
  let at execution =
    Recurrence_nominal.at
      (Recurrence_execution_intent.nominal (Execution.intent execution))
  in
  match
    List.max_elt executions ~compare:(fun left right ->
      Time_ns.compare (at left) (at right))
  with
  | None -> pending
  | Some latest ->
    if List.exists pending ~f:(Execution.equal latest)
    then pending
    else pending @ [ latest ]
;;

let render result ~read_target =
  let open Result.Let_syntax in
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
    let%map nominal, attempts =
      map_attempts execution ~read_target ~all_attempts ~f:attempt_json
    in
    `Object
      [ "nominal", nominal_json nominal
      ; "evidence", `String "retained_controller_history"
      ; "attempts", `Array attempts
      ]
  in
  let%map payload =
    match result.snapshot with
    | Listed { entries; _ } ->
      Ok (`Object [ "entries", `Array (List.map entries ~f:view_json) ])
    | Found { entry; _ } -> Ok (`Object [ "entry", view_json entry ])
    | Current_status { status; _ } ->
      let frontier = status.frontier in
      let%map executions =
        List.map (status_executions frontier) ~f:(execution_json ~all_attempts:false)
        |> Result.all
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

module Public = struct
  module Q = P.Recurrence_query

  let definition (view : Definition_view.t) : Q.Definition.t =
    let metadata = view.metadata in
    let control : Q.Definition.control =
      match metadata.control with
      | Enabled -> Enabled
      | Paused reason ->
        Paused
          (match reason with
           | Operator_pause -> Operator_pause
           | Controller_stop -> Controller_stop
           | Source_review -> Source_review
           | Recovery_review -> Recovery_review
           | Target_invalidated _ -> Target_review)
      | Retired reason ->
        Retired
          (match reason with
           | Operator_retire -> Operator_retire
           | Lifecycle_changed -> Lifecycle_changed)
    in
    let target_kind : Q.Definition.target_kind =
      match metadata.target with
      | Existing_session -> Existing_session
      | Fresh_child -> Fresh_child
    in
    { id = metadata.id
    ; controller = metadata.controller
    ; definition_revision = metadata.definition_revision
    ; control_revision = metadata.control_revision
    ; control
    ; source = metadata.source
    ; birth_lifecycle_revision = metadata.birth_lifecycle_revision
    ; creator = metadata.creator
    ; created_at = metadata.created_at
    ; schedule = view.schedule
    ; target_kind
    ; policy = metadata.policy
    ; actions = []
    }
  ;;

  let basis basis ~availability : Q.Basis.t =
    { session = basis.Basis.session
    ; generation = basis.generation
    ; state_revision = basis.state_revision
    ; catalog_revision = basis.catalog_revision
    ; lifecycle_revision = basis.lifecycle_revision
    ; automatic = basis.automatic
    ; source = basis.source
    ; availability
    }
  ;;

  let planning ~selected : Recurrence_planning_status.t -> Q.Planning.t = function
    | Available -> if selected then Available else Unavailable Controller_not_selected
    | Unavailable reason ->
      Unavailable
        (match reason with
         | Not_configured -> Not_configured
         | Not_granted -> Not_granted
         | Expired -> Expired
         | Closing -> Closing
         | Lifecycle_not_automatic -> Lifecycle_not_automatic
         | Controller_not_running -> Controller_not_running
         | Source_unavailable -> Source_unavailable
         | Controller_not_loaded -> Controller_not_loaded)
  ;;

  let totals frontier : Q.Totals.t =
    let summary = Frontier.summary frontier in
    { consumed = summary.consumed
    ; omitted = summary.omitted
    ; withdrawn = summary.withdrawn
    ; settled = summary.settled
    }
  ;;

  let definition_id key : Q.Definition_id.t =
    let module Id = Recurrence_identity.Definition in
    { controller = Id.controller key
    ; recurrence_id = Id.recurrence_id key
    ; revision = Id.revision key
    }
  ;;

  let timestamp time = P.Timestamp.of_time_ns time

  let nominal value : Q.Nominal.t =
    let module Id = Recurrence_identity.Occurrence in
    let key = Recurrence_nominal.key value in
    { occurrence = { definition = definition_id (Id.definition key); slot = Id.slot key }
    ; at = timestamp (Recurrence_nominal.at value)
    }
  ;;

  let issue : Frontier.Issue.t -> Q.Status.issue = function
    | Zone_unavailable -> Zone_unavailable
    | Coverage_unavailable -> Coverage_unavailable
    | Timing_exhausted -> Timing_exhausted
    | Capacity_saturated -> Capacity_saturated
    | Initial_delay_missed -> Initial_delay_missed
    | Delay_nominal_missed -> Delay_nominal_missed
    | Execution_interrupted -> Execution_interrupted
    | Delay_successor_exhausted -> Delay_successor_exhausted
  ;;

  let range (detail : Frontier.History.t) : Q.History.range =
    { definition = definition_id detail.definition
    ; first_slot = detail.first_slot
    ; last_slot = detail.last_slot
    ; kind =
        (match detail.kind with
         | Skipped -> Skipped
         | Withdrawn -> Withdrawn)
    ; reason =
        (match detail.reason with
         | Expired -> Expired
         | Policy_skip -> Policy_skip
         | Superseded -> Superseded
         | Pending_capacity -> Pending_capacity
         | Control_withdrawal -> Control_withdrawal)
    }
  ;;

  let protected frontier =
    protected_creations frontier ~f:(fun value ~link_recorded ~execution_recorded ->
      { Q.Protected_creation.nominal = nominal value; link_recorded; execution_recorded })
  ;;

  let attempt ~can_view_run intent ~target_state attempt =
    let open Result.Let_syntax in
    let%map target =
      match target_state with
      | Ok state ->
        Recurrence_target_view.project_public
          state
          ~intent
          ~attempt
          ~can_view:can_view_run
      | Error (failure : P.Error.t) ->
        (match failure.code with
         | Permission_denied -> Ok Q.Target.Denied
         | Session_not_found -> Ok Q.Target.Unavailable
         | _ -> Error failure)
    in
    { Q.Attempt.number =
        Recurrence_execution_attempt.number (Execution.Attempt.request attempt)
    ; rejection = Execution.Attempt.rejection attempt
    ; uncertainty = Execution.Attempt.uncertainty attempt
    ; retry_resolution_recorded =
        Option.is_some (Execution.Attempt.retry_resolution attempt)
    ; admission_recorded = Option.is_some (Execution.Attempt.origin attempt)
    ; terminal_recorded = Option.is_some (Execution.Attempt.terminal attempt)
    ; target
    }
  ;;

  let occurrence execution ~read_target ~can_view_run ~all_attempts =
    let open Result.Let_syntax in
    let%map value, attempts =
      map_attempts execution ~read_target ~all_attempts ~f:(attempt ~can_view_run)
    in
    { Q.Occurrence.nominal = nominal value; attempts }
  ;;

  let render snapshot ~availability ~selected ~read_target ~can_view_run =
    let open Result.Let_syntax in
    let basis = basis (snapshot_basis snapshot) ~availability in
    let available value = Q.Result.Available { basis; can_create = false; value } in
    let occurrence = occurrence ~read_target ~can_view_run in
    match snapshot with
    | Listed { entries; _ } ->
      Ok (Q.Result.List (available (List.map entries ~f:definition)))
    | Found { entry; _ } -> Ok (Q.Result.Get (available (definition entry)))
    | Current_status { status; _ } ->
      let frontier = status.frontier in
      let%map occurrences =
        List.map (status_executions frontier) ~f:(occurrence ~all_attempts:false)
        |> Result.all
      in
      let view : Q.Status.t =
        { entry = definition status.view
        ; planning = planning ~selected status.planning
        ; planning_revision = Frontier.revision frontier
        ; pending_count = Frontier.pending_count frontier
        ; unresolved_count = Frontier.unresolved_count frontier
        ; issue = Option.map (Frontier.issue frontier) ~f:issue
        ; totals = totals frontier
        ; ready = List.map (Frontier.ready frontier) ~f:nominal
        ; lookahead = Option.map (Frontier.lookahead frontier) ~f:nominal
        ; alarm_due_at =
            Option.map (Frontier.alarm frontier) ~f:(fun alarm ->
              timestamp (Recurrence_alarm.due_at alarm))
        ; protected_creations = protected frontier
        ; occurrences
        }
      in
      Q.Result.Status (available view)
    | Retained_history { history; _ } ->
      let frontier = history.frontier in
      let%map occurrences =
        List.map (Frontier.executions frontier) ~f:(occurrence ~all_attempts:true)
        |> Result.all
      in
      let view : Q.History.t =
        { entry = definition history.view
        ; totals = totals frontier
        ; retained_ranges = List.map (Frontier.history frontier) ~f:range
        ; protected_creations = protected frontier
        ; occurrences
        }
      in
      Q.Result.History (available view)
  ;;
end

let render_public = Public.render
