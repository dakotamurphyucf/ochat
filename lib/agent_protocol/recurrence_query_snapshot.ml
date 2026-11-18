open! Core
module J = Json_codec
module W = Recurrence_wire
module Definition = Recurrence_query_definition.Definition
module Evidence = Recurrence_query_evidence
module Definition_id = Evidence.Definition_id
module Nominal = Evidence.Nominal
module Occurrence = Evidence.Occurrence
module Protected_creation = Evidence.Protected_creation

module Planning = struct
  type reason =
    | Not_configured
    | Not_granted
    | Expired
    | Closing
    | Lifecycle_not_automatic
    | Controller_not_running
    | Source_unavailable
    | Controller_not_loaded
    | Controller_not_selected

  let to_json_reason = function
    | Not_configured -> `String "not_configured"
    | Not_granted -> `String "not_granted"
    | Expired -> `String "expired"
    | Closing -> `String "closing"
    | Lifecycle_not_automatic -> `String "lifecycle_not_automatic"
    | Controller_not_running -> `String "controller_not_running"
    | Source_unavailable -> `String "source_unavailable"
    | Controller_not_loaded -> `String "controller_not_loaded"
    | Controller_not_selected -> `String "controller_not_selected"
  ;;

  let of_json_reason json =
    J.enum
      ~name:"recurrence planning reason"
      [ "not_configured", Not_configured
      ; "not_granted", Not_granted
      ; "expired", Expired
      ; "closing", Closing
      ; "lifecycle_not_automatic", Lifecycle_not_automatic
      ; "controller_not_running", Controller_not_running
      ; "source_unavailable", Source_unavailable
      ; "controller_not_loaded", Controller_not_loaded
      ; "controller_not_selected", Controller_not_selected
      ]
      json
  ;;

  type t =
    | Available
    | Unavailable of reason

  let to_json = function
    | Available -> W.tagged "available" []
    | Unavailable reason -> W.tagged "unavailable" [ "reason", to_json_reason reason ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind kind, fields = W.tag json in
    match kind with
    | "available" ->
      let%map () = Extension_codec.closed fields [ "kind" ] in
      Available
    | "unavailable" ->
      let%bind () = Extension_codec.closed fields [ "kind"; "reason" ] in
      let%map reason = J.required_as fields "reason" of_json_reason in
      Unavailable reason
    | _ -> W.invalid "unsupported recurrence planning state"
  ;;
end

module Totals = struct
  type t =
    { consumed : int64
    ; omitted : int64
    ; withdrawn : int64
    ; settled : int64
    }

  let to_json t =
    `Object
      [ "consumed", W.counter_json t.consumed
      ; "omitted", W.counter_json t.omitted
      ; "withdrawn", W.counter_json t.withdrawn
      ; "settled", W.counter_json t.settled
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "consumed"; "omitted"; "withdrawn"; "settled" ] in
    let%bind consumed = J.required_as fields "consumed" W.counter in
    let%bind omitted = J.required_as fields "omitted" W.counter in
    let%bind withdrawn = J.required_as fields "withdrawn" W.counter in
    let%bind settled = J.required_as fields "settled" W.counter in
    if
      Int64.(
        omitted <= consumed
        && withdrawn <= consumed - omitted
        && settled <= consumed - omitted - withdrawn)
    then Ok { consumed; omitted; withdrawn; settled }
    else W.invalid "recurrence lifetime partitions exceed consumed occurrences"
  ;;
end

module Status = struct
  type issue =
    | Zone_unavailable
    | Coverage_unavailable
    | Timing_exhausted
    | Capacity_saturated
    | Initial_delay_missed
    | Delay_nominal_missed
    | Execution_interrupted
    | Delay_successor_exhausted

  let to_json_issue = function
    | Zone_unavailable -> `String "zone_unavailable"
    | Coverage_unavailable -> `String "coverage_unavailable"
    | Timing_exhausted -> `String "timing_exhausted"
    | Capacity_saturated -> `String "capacity_saturated"
    | Initial_delay_missed -> `String "initial_delay_missed"
    | Delay_nominal_missed -> `String "delay_nominal_missed"
    | Execution_interrupted -> `String "execution_interrupted"
    | Delay_successor_exhausted -> `String "delay_successor_exhausted"
  ;;

  let of_json_issue json =
    J.enum
      ~name:"recurrence status issue"
      [ "zone_unavailable", Zone_unavailable
      ; "coverage_unavailable", Coverage_unavailable
      ; "timing_exhausted", Timing_exhausted
      ; "capacity_saturated", Capacity_saturated
      ; "initial_delay_missed", Initial_delay_missed
      ; "delay_nominal_missed", Delay_nominal_missed
      ; "execution_interrupted", Execution_interrupted
      ; "delay_successor_exhausted", Delay_successor_exhausted
      ]
      json
  ;;

  type t =
    { entry : Definition.t
    ; planning : Planning.t
    ; planning_revision : int64
    ; pending_count : int
    ; unresolved_count : int
    ; issue : issue option
    ; totals : Totals.t
    ; ready : Nominal.t list
    ; lookahead : Nominal.t option
    ; alarm_due_at : Timestamp.t option
    ; protected_creations : Protected_creation.t list
    ; occurrences : Occurrence.t list
    }

  let to_json t =
    `Object
      [ "entry", Definition.to_json t.entry
      ; "planning", Planning.to_json t.planning
      ; "planning_revision", W.counter_json t.planning_revision
      ; "pending_count", W.integer_json t.pending_count
      ; "unresolved_count", W.integer_json t.unresolved_count
      ; "issue", W.option_json to_json_issue t.issue
      ; "totals", Totals.to_json t.totals
      ; "ready", W.array Nominal.to_json t.ready
      ; "lookahead", W.option_json Nominal.to_json t.lookahead
      ; "alarm_due_at", W.option_json Timestamp.to_json t.alarm_due_at
      ; "protected_creations", W.array Protected_creation.to_json t.protected_creations
      ; "occurrences", W.array Occurrence.to_json t.occurrences
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      W.fields
        json
        [ "entry"
        ; "planning"
        ; "planning_revision"
        ; "pending_count"
        ; "unresolved_count"
        ; "issue"
        ; "totals"
        ; "ready"
        ; "lookahead"
        ; "alarm_due_at"
        ; "protected_creations"
        ; "occurrences"
        ]
    in
    let%bind entry = J.required_as fields "entry" Definition.of_json in
    let%bind planning = J.required_as fields "planning" Planning.of_json in
    let%bind planning_revision = J.required_as fields "planning_revision" W.counter in
    let%bind pending_count =
      J.required_as fields "pending_count" (J.bounded_int ~min:0 ~max:256)
    in
    let%bind unresolved_count =
      J.required_as fields "unresolved_count" (J.bounded_int ~min:0 ~max:256)
    in
    let%bind issue = J.required_as fields "issue" (W.nullable of_json_issue) in
    let%bind totals = J.required_as fields "totals" Totals.of_json in
    let%bind ready = J.required_as fields "ready" (W.list 32 Nominal.of_json) in
    let%bind lookahead = J.required_as fields "lookahead" (W.nullable Nominal.of_json) in
    let%bind alarm_due_at =
      J.required_as fields "alarm_due_at" (W.nullable Timestamp.of_json)
    in
    let%bind protected_creations =
      J.required_as fields "protected_creations" (W.list 256 Protected_creation.of_json)
    in
    let%map occurrences =
      J.required_as fields "occurrences" (W.list 257 Occurrence.of_json)
    in
    { entry
    ; planning
    ; planning_revision
    ; pending_count
    ; unresolved_count
    ; issue
    ; totals
    ; ready
    ; lookahead
    ; alarm_due_at
    ; protected_creations
    ; occurrences
    }
  ;;
end

module History = struct
  type kind =
    | Skipped
    | Withdrawn

  let to_json_kind = function
    | Skipped -> `String "skipped"
    | Withdrawn -> `String "withdrawn"
  ;;

  let of_json_kind json =
    J.enum
      ~name:"recurrence range kind"
      [ "skipped", Skipped; "withdrawn", Withdrawn ]
      json
  ;;

  type reason =
    | Expired
    | Policy_skip
    | Superseded
    | Pending_capacity
    | Control_withdrawal

  let to_json_reason = function
    | Expired -> `String "expired"
    | Policy_skip -> `String "policy_skip"
    | Superseded -> `String "superseded"
    | Pending_capacity -> `String "pending_capacity"
    | Control_withdrawal -> `String "control_withdrawal"
  ;;

  let of_json_reason json =
    J.enum
      ~name:"recurrence range reason"
      [ "expired", Expired
      ; "policy_skip", Policy_skip
      ; "superseded", Superseded
      ; "pending_capacity", Pending_capacity
      ; "control_withdrawal", Control_withdrawal
      ]
      json
  ;;

  type range =
    { definition : Definition_id.t
    ; first_slot : int64
    ; last_slot : int64
    ; kind : kind
    ; reason : reason
    }

  let to_json_range t =
    `Object
      [ "definition", Definition_id.to_json t.definition
      ; "first_slot", W.counter_json t.first_slot
      ; "last_slot", W.counter_json t.last_slot
      ; "kind", to_json_kind t.kind
      ; "reason", to_json_reason t.reason
      ]
  ;;

  let of_json_range json =
    let open Result.Let_syntax in
    let%bind fields =
      W.fields json [ "definition"; "first_slot"; "last_slot"; "kind"; "reason" ]
    in
    let%bind definition = J.required_as fields "definition" Definition_id.of_json in
    let%bind first_slot = J.required_as fields "first_slot" W.counter in
    let%bind last_slot = J.required_as fields "last_slot" W.counter in
    let%bind kind = J.required_as fields "kind" of_json_kind in
    let%bind reason = J.required_as fields "reason" of_json_reason in
    let t = { definition; first_slot; last_slot; kind; reason } in
    if Int64.(t.first_slot > t.last_slot)
    then W.invalid "recurrence history slots are reversed"
    else Ok t
  ;;

  type t =
    { entry : Definition.t
    ; totals : Totals.t
    ; retained_ranges : range list
    ; protected_creations : Protected_creation.t list
    ; occurrences : Occurrence.t list
    }

  let to_json t =
    `Object
      [ "entry", Definition.to_json t.entry
      ; "totals", Totals.to_json t.totals
      ; "retained_ranges", W.array to_json_range t.retained_ranges
      ; "protected_creations", W.array Protected_creation.to_json t.protected_creations
      ; "occurrences", W.array Occurrence.to_json t.occurrences
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      W.fields
        json
        [ "entry"; "totals"; "retained_ranges"; "protected_creations"; "occurrences" ]
    in
    let%bind entry = J.required_as fields "entry" Definition.of_json in
    let%bind totals = J.required_as fields "totals" Totals.of_json in
    let%bind retained_ranges =
      J.required_as fields "retained_ranges" (W.list 256 of_json_range)
    in
    let%bind protected_creations =
      J.required_as fields "protected_creations" (W.list 256 Protected_creation.of_json)
    in
    let%map occurrences =
      J.required_as fields "occurrences" (W.list 512 Occurrence.of_json)
    in
    { entry; totals; retained_ranges; protected_creations; occurrences }
  ;;
end
