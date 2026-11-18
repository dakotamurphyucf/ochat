(* Public Goal observations exclude command receipts and execution authority. *)
open! Core
module J = Json_codec
module W = Goal_wire

module Availability = struct
  type t =
    | Active
    | Stopped
    | Unavailable
  [@@deriving equal, sexp_of]

  let values = [ "active", Active; "stopped", Stopped; "unavailable", Unavailable ]

  let to_json = function
    | Active -> `String "active"
    | Stopped -> `String "stopped"
    | Unavailable -> `String "unavailable"
  ;;

  let of_json = J.enum ~name:"Goal session availability" values
end

module Run_observation = struct
  type status =
    | Admitted
    | Active
    | Waiting
    | Retained
    | Terminal
  [@@deriving equal, sexp_of]

  type t =
    | No_episode
    | Unavailable
    | Observed of
        { run_id : Id.Run.t
        ; revision : int64
        ; status : status
        }
  [@@deriving sexp_of]

  let status_to_json = function
    | Admitted -> `String "admitted"
    | Active -> `String "active"
    | Waiting -> `String "waiting"
    | Retained -> `String "retained"
    | Terminal -> `String "terminal"
  ;;

  let status_of_json =
    J.enum
      ~name:"Goal episode Run status"
      [ "admitted", Admitted
      ; "active", Active
      ; "waiting", Waiting
      ; "retained", Retained
      ; "terminal", Terminal
      ]
  ;;

  let to_json = function
    | No_episode -> W.tagged "no_episode" []
    | Unavailable -> W.tagged "unavailable" []
    | Observed { run_id; revision; status } ->
      W.tagged
        "observed"
        [ "run_id", Id.Run.to_json run_id
        ; "revision", W.counter_json revision
        ; "status", status_to_json status
        ]
  ;;

  let of_json json =
    let open Core.Result.Let_syntax in
    let%bind kind, fields = W.tag json in
    match kind with
    | "no_episode" ->
      let%map () = Extension_codec.closed fields [ "kind" ] in
      No_episode
    | "unavailable" ->
      let%map () = Extension_codec.closed fields [ "kind" ] in
      Unavailable
    | "observed" ->
      let%bind () =
        Extension_codec.closed fields [ "kind"; "run_id"; "revision"; "status" ]
      in
      let%bind run_id = J.required_as fields "run_id" Id.Run.of_json in
      let%bind revision = J.required_as fields "revision" W.counter in
      let%map status = J.required_as fields "status" status_of_json in
      Observed { run_id; revision; status }
    | _ -> W.invalid "unknown Goal Run observation"
  ;;
end

module Action = struct
  type t =
    | Revise_objective
    | Record_progress
    | Change_budget
    | Adopt_source
    | Hold
    | Abandon
    | Start
    | Resume
  [@@deriving compare, equal, sexp_of]

  let name = function
    | Revise_objective -> "revise_objective"
    | Record_progress -> "record_progress"
    | Change_budget -> "change_budget"
    | Adopt_source -> "adopt_source"
    | Hold -> "hold"
    | Abandon -> "abandon"
    | Start -> "start"
    | Resume -> "resume"
  ;;

  let values =
    [ Revise_objective
    ; Record_progress
    ; Change_budget
    ; Adopt_source
    ; Hold
    ; Abandon
    ; Start
    ; Resume
    ]
  ;;

  let to_json value = `String (name value)

  let of_json =
    J.enum ~name:"Goal action" (List.map values ~f:(fun action -> name action, action))
  ;;
end

module Goal_summary = struct
  type state =
    | Open of Goal.State.admission
    | Abandoned of Goal.Reason.t
    | Achieved

  type t =
    { id : Id.Goal.t
    ; session : Session_ref.t
    ; revision : int64
    ; source : Run_source.t
    ; objective : Goal.Objective.t
    ; progress : Goal.Progress.t
    ; budget : Goal.Budget.t
    ; state : state
    ; achievement_pending : bool
    }

  let of_goal goal =
    let state =
      match Goal.state goal with
      | Open admission -> Open admission
      | Closed (Abandoned reason) -> Abandoned reason
      | Closed (Achieved _) -> Achieved
    in
    { id = Goal.id goal
    ; session = Goal.session goal
    ; revision = Goal.revision goal
    ; source = Goal.source goal
    ; objective = Goal.objective goal
    ; progress = Goal.progress goal
    ; budget = Goal.budget goal
    ; state
    ; achievement_pending =
        Option.is_some (Goal.achievement_request goal)
        && Option.is_none (Goal.request_settlement goal)
    }
  ;;

  let state_to_json = function
    | Open Enabled -> W.tagged "enabled" []
    | Open (Held reason) -> W.tagged "held" [ "reason", Goal.Hold.to_json reason ]
    | Abandoned reason -> W.tagged "abandoned" [ "reason", Goal.Reason.to_json reason ]
    | Achieved -> W.tagged "achieved" []
  ;;

  let state_of_json json =
    let open Core.Result.Let_syntax in
    let%bind kind, fields = W.tag json in
    match kind with
    | "enabled" ->
      let%map () = Extension_codec.closed fields [ "kind" ] in
      Open Enabled
    | "held" ->
      let%bind () = Extension_codec.closed fields [ "kind"; "reason" ] in
      let%map reason = J.required_as fields "reason" W.hold in
      Open (Held reason)
    | "abandoned" ->
      let%bind () = Extension_codec.closed fields [ "kind"; "reason" ] in
      let%map reason = J.required_as fields "reason" Goal.Reason.of_json in
      Abandoned reason
    | "achieved" ->
      let%map () = Extension_codec.closed fields [ "kind" ] in
      Achieved
    | _ -> W.invalid "unknown Goal state"
  ;;

  let to_json t =
    `Object
      [ "id", Id.Goal.to_json t.id
      ; "revision", W.counter_json t.revision
      ; "source", Run_source.to_json t.source
      ; "objective", Goal.Objective.to_json t.objective
      ; "progress", Goal.Progress.to_json t.progress
      ; "budget", Goal.Budget.to_json t.budget
      ; "state", state_to_json t.state
      ; "achievement_pending", W.bool_json t.achievement_pending
      ]
  ;;

  let of_json ~session json =
    let open Core.Result.Let_syntax in
    let%bind fields =
      W.fields
        json
        [ "id"
        ; "revision"
        ; "source"
        ; "objective"
        ; "progress"
        ; "budget"
        ; "state"
        ; "achievement_pending"
        ]
    in
    let%bind id = J.required_as fields "id" Id.Goal.of_json in
    let%bind revision = J.required_as fields "revision" W.counter in
    let%bind source = J.required_as fields "source" W.source in
    let%bind objective = J.required_as fields "objective" Goal.Objective.of_json in
    let%bind progress = J.required_as fields "progress" Goal.Progress.of_json in
    let%bind budget = J.required_as fields "budget" W.budget in
    let%bind state = J.required_as fields "state" state_of_json in
    let%bind achievement_pending = J.required_as fields "achievement_pending" J.bool in
    match state, achievement_pending with
    | Achieved, true -> W.invalid "achieved Goal cannot have a pending achievement"
    | _ ->
      Ok
        { id
        ; session
        ; revision
        ; source
        ; objective
        ; progress
        ; budget
        ; state
        ; achievement_pending
        }
  ;;
end

module Execution_blocker = struct
  type t =
    | Stopped
    | Source_unavailable
    | Source_changed
    | Accounting_unavailable
    | Usage_uncertain
    | Policy_hold
    | Active_run
    | No_current_run
    | Unavailable
  [@@deriving compare, sexp_of]

  let name = function
    | Stopped -> "stopped"
    | Source_unavailable -> "source_unavailable"
    | Source_changed -> "source_changed"
    | Accounting_unavailable -> "accounting_unavailable"
    | Usage_uncertain -> "usage_uncertain"
    | Policy_hold -> "policy_hold"
    | Active_run -> "active_run"
    | No_current_run -> "no_current_run"
    | Unavailable -> "unavailable"
  ;;

  let values =
    [ Stopped
    ; Source_unavailable
    ; Source_changed
    ; Accounting_unavailable
    ; Usage_uncertain
    ; Policy_hold
    ; Active_run
    ; No_current_run
    ; Unavailable
    ]
  ;;

  let to_json value = `String (name value)

  let of_json =
    J.enum
      ~name:"Goal execution blocker"
      (List.map values ~f:(fun blocker -> name blocker, blocker))
  ;;
end
