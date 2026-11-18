open! Core
module J = Json_codec
module W = Recurrence_wire

module Availability = struct
  type t =
    | Active
    | Stopped
    | Unavailable

  let to_json = function
    | Active -> `String "active"
    | Stopped -> `String "stopped"
    | Unavailable -> `String "unavailable"
  ;;

  let of_json json =
    J.enum
      ~name:"recurrence controller availability"
      [ "active", Active; "stopped", Stopped; "unavailable", Unavailable ]
      json
  ;;
end

module Basis = struct
  type t =
    { session : Session_ref.t
    ; generation : int
    ; state_revision : int64
    ; catalog_revision : int64
    ; lifecycle_revision : Session_lifecycle.Revision.t
    ; automatic : bool
    ; source : Run_source.t option
    ; availability : Availability.t
    }

  let to_json t =
    `Object
      [ "session", Session_ref.to_json t.session
      ; "generation", W.integer_json t.generation
      ; "state_revision", W.counter_json t.state_revision
      ; "catalog_revision", W.counter_json t.catalog_revision
      ; "lifecycle_revision", W.lifecycle_json t.lifecycle_revision
      ; "automatic", W.bool_json t.automatic
      ; "source", W.option_json Run_source.to_json t.source
      ; "availability", Availability.to_json t.availability
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      W.fields
        json
        [ "session"
        ; "generation"
        ; "state_revision"
        ; "catalog_revision"
        ; "lifecycle_revision"
        ; "automatic"
        ; "source"
        ; "availability"
        ]
    in
    let%bind session = J.required_as fields "session" W.session in
    let%bind generation =
      J.required_as fields "generation" (J.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%bind state_revision = J.required_as fields "state_revision" W.counter in
    let%bind catalog_revision = J.required_as fields "catalog_revision" W.counter in
    let%bind lifecycle_revision = J.required_as fields "lifecycle_revision" W.lifecycle in
    let%bind automatic = J.required_as fields "automatic" J.bool in
    let%bind source = J.required_as fields "source" (W.nullable W.source) in
    let%bind availability = J.required_as fields "availability" Availability.of_json in
    let t =
      { session
      ; generation
      ; state_revision
      ; catalog_revision
      ; lifecycle_revision
      ; automatic
      ; source
      ; availability
      }
    in
    if
      Option.exists t.source ~f:(fun source ->
        not (Int.equal source.generation t.generation))
    then W.invalid "recurrence basis and source generations differ"
    else Ok t
  ;;
end

module Definition = struct
  type pause_reason =
    | Operator_pause
    | Controller_stop
    | Source_review
    | Recovery_review
    | Target_review

  let to_json_pause_reason = function
    | Operator_pause -> `String "operator_pause"
    | Controller_stop -> `String "controller_stop"
    | Source_review -> `String "source_review"
    | Recovery_review -> `String "recovery_review"
    | Target_review -> `String "target_review"
  ;;

  let of_json_pause_reason json =
    J.enum
      ~name:"recurrence pause reason"
      [ "operator_pause", Operator_pause
      ; "controller_stop", Controller_stop
      ; "source_review", Source_review
      ; "recovery_review", Recovery_review
      ; "target_review", Target_review
      ]
      json
  ;;

  type retire_reason =
    | Operator_retire
    | Lifecycle_changed

  let to_json_retire_reason = function
    | Operator_retire -> `String "operator_retire"
    | Lifecycle_changed -> `String "lifecycle_changed"
  ;;

  let of_json_retire_reason json =
    J.enum
      ~name:"recurrence retire reason"
      [ "operator_retire", Operator_retire; "lifecycle_changed", Lifecycle_changed ]
      json
  ;;

  type target_kind =
    | Existing_session
    | Fresh_child

  let to_json_target_kind = function
    | Existing_session -> `String "existing_session"
    | Fresh_child -> `String "fresh_child"
  ;;

  let of_json_target_kind json =
    J.enum
      ~name:"recurrence target kind"
      [ "existing_session", Existing_session; "fresh_child", Fresh_child ]
      json
  ;;

  type control =
    | Enabled
    | Paused of pause_reason
    | Retired of retire_reason

  let control_to_json = function
    | Enabled -> W.tagged "enabled" []
    | Paused reason -> W.tagged "paused" [ "reason", to_json_pause_reason reason ]
    | Retired reason -> W.tagged "retired" [ "reason", to_json_retire_reason reason ]
  ;;

  let control_of_json json =
    let open Result.Let_syntax in
    let%bind kind, fields = W.tag json in
    match kind with
    | "enabled" ->
      let%map () = Extension_codec.closed fields [ "kind" ] in
      Enabled
    | "paused" ->
      let%bind () = Extension_codec.closed fields [ "kind"; "reason" ] in
      let%map reason = J.required_as fields "reason" of_json_pause_reason in
      Paused reason
    | "retired" ->
      let%bind () = Extension_codec.closed fields [ "kind"; "reason" ] in
      let%map reason = J.required_as fields "reason" of_json_retire_reason in
      Retired reason
    | _ -> W.invalid "unsupported recurrence control state"
  ;;

  type t =
    { id : Id.Recurrence.t
    ; controller : Session_ref.t
    ; definition_revision : int64
    ; control_revision : int64
    ; control : control
    ; source : Run_source.t
    ; birth_lifecycle_revision : Session_lifecycle.Revision.t
    ; creator : Id.Principal.t
    ; created_at : Timestamp.t
    ; schedule : Recurrence_schedule.t
    ; target_kind : target_kind
    ; policy : Recurrence_policy.t
    ; actions : Recurrence_control.Verb.t list
    }

  let to_json t =
    `Object
      [ "id", Id.Recurrence.to_json t.id
      ; "controller", Session_ref.to_json t.controller
      ; "definition_revision", W.counter_json t.definition_revision
      ; "control_revision", W.counter_json t.control_revision
      ; "control", control_to_json t.control
      ; "source", Run_source.to_json t.source
      ; "birth_lifecycle_revision", W.lifecycle_json t.birth_lifecycle_revision
      ; "creator", Id.Principal.to_json t.creator
      ; "created_at", Timestamp.to_json t.created_at
      ; "schedule", Recurrence_schedule.to_json t.schedule
      ; "target_kind", to_json_target_kind t.target_kind
      ; "policy", Recurrence_policy.to_json t.policy
      ; "actions", W.array Recurrence_control.Verb.to_json t.actions
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      W.fields
        json
        [ "id"
        ; "controller"
        ; "definition_revision"
        ; "control_revision"
        ; "control"
        ; "source"
        ; "birth_lifecycle_revision"
        ; "creator"
        ; "created_at"
        ; "schedule"
        ; "target_kind"
        ; "policy"
        ; "actions"
        ]
    in
    let%bind id = J.required_as fields "id" Id.Recurrence.of_json in
    let%bind controller = J.required_as fields "controller" W.session in
    let%bind definition_revision = J.required_as fields "definition_revision" W.counter in
    let%bind control_revision = J.required_as fields "control_revision" W.counter in
    let%bind control = J.required_as fields "control" control_of_json in
    let%bind source = J.required_as fields "source" W.source in
    let%bind birth_lifecycle_revision =
      J.required_as fields "birth_lifecycle_revision" W.lifecycle
    in
    let%bind creator = J.required_as fields "creator" Id.Principal.of_json in
    let%bind created_at = J.required_as fields "created_at" Timestamp.of_json in
    let%bind schedule =
      J.required_as fields "schedule" Recurrence_schedule.of_public_json
    in
    let%bind target_kind = J.required_as fields "target_kind" of_json_target_kind in
    let%bind policy = J.required_as fields "policy" Recurrence_policy.of_public_json in
    let%bind actions =
      J.required_as fields "actions" (W.list 4 Recurrence_control.Verb.of_json)
    in
    let t =
      { id
      ; controller
      ; definition_revision
      ; control_revision
      ; control
      ; source
      ; birth_lifecycle_revision
      ; creator
      ; created_at
      ; schedule
      ; target_kind
      ; policy
      ; actions
      }
    in
    let module Verb = Recurrence_control.Verb in
    let applies : Verb.t -> bool = function
      | Create -> false
      | Edit | Retire ->
        (match control with
         | Enabled | Paused _ -> true
         | Retired _ -> false)
      | Enable ->
        (match control with
         | Paused (Operator_pause | Controller_stop | Source_review | Recovery_review) ->
           true
         | Enabled | Paused Target_review | Retired _ -> false)
      | Pause ->
        (match control with
         | Enabled -> true
         | Paused _ | Retired _ -> false)
    in
    if Int64.(definition_revision > control_revision)
    then W.invalid "recurrence definition revision exceeds its control revision"
    else if
      (not (List.for_all t.actions ~f:applies))
      || Option.is_some (List.find_a_dup t.actions ~compare:Verb.compare)
    then W.invalid "invalid recurrence action advice"
    else Ok t
  ;;
end
