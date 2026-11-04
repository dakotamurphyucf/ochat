open! Core
module P = Agent_protocol
module G = P.Goal
module X = Persistence_codec

module Mutation = struct
  type t =
    | Create of
        { id : P.Id.Goal.t
        ; objective : G.Objective.t
        ; budget : G.Budget.t
        }
    | Revise_objective of P.Id.Goal.t * G.Objective.t
    | Record_progress of P.Id.Goal.t * G.Progress.t
    | Hold of P.Id.Goal.t * G.Hold.t
    | Change_budget of P.Id.Goal.t * G.Budget.t
    | Adopt_source of P.Id.Goal.t
    | Abandon of P.Id.Goal.t * G.Reason.t
  [@@deriving sexp_of]

  let goal_id = function
    | Create { id; _ } -> id
    | Revise_objective (id, _)
    | Record_progress (id, _)
    | Hold (id, _)
    | Change_budget (id, _)
    | Adopt_source id
    | Abandon (id, _) -> id
  ;;

  let needs_source = function
    | Hold _ | Abandon _ -> false
    | Create _ | Revise_objective _ | Record_progress _ | Change_budget _ | Adopt_source _
      -> true
  ;;

  let hold_json = function
    | G.Hold.Operator_pause -> `Object [ "kind", `String "operator_pause" ]
    | Deadline_reached -> `Object [ "kind", `String "deadline_reached" ]
    | Source_review -> `Object [ "kind", `String "source_review" ]
    | Needs_input reason ->
      `Object [ "kind", `String "needs_input"; "reason", G.Reason.to_json reason ]
    | Budget_gate dimension ->
      let dimension =
        match dimension with
        | G.Budget.Dimension.Admitted_turns -> "admitted_turns"
        | Admitted_attempts -> "admitted_attempts"
        | Observed_tokens -> "observed_tokens"
      in
      `Object [ "kind", `String "budget_gate"; "dimension", `String dimension ]
    | Usage_uncertain component ->
      let component =
        match component with
        | Inference.Observation.Usage.Component.Input -> "input"
        | Output -> "output"
        | Reported_total -> "reported_total"
        | Cached_input -> "cached_input"
        | Cache_write_input -> "cache_write_input"
        | Reasoning_output -> "reasoning_output"
      in
      `Object [ "kind", `String "usage_uncertain"; "component", `String component ]
  ;;

  let to_json mutation =
    let kind, extra =
      match mutation with
      | Create { objective; budget; _ } ->
        ( "create"
        , [ "objective", `String (G.Objective.to_string objective)
          ; "budget", G.Budget.to_json budget
          ] )
      | Revise_objective (_, objective) ->
        "objective", [ "objective", `String (G.Objective.to_string objective) ]
      | Record_progress (_, progress) ->
        "progress", [ "progress", `String (G.Progress.to_string progress) ]
      | Hold (_, reason) -> "hold", [ "reason", hold_json reason ]
      | Change_budget (_, budget) -> "budget", [ "budget", G.Budget.to_json budget ]
      | Adopt_source _ -> "adopt_source", []
      | Abandon (_, reason) ->
        "abandon", [ "reason", `String (G.Reason.to_string reason) ]
    in
    `Object
      ([ "kind", `String kind; "goal_id", P.Id.Goal.to_json (goal_id mutation) ] @ extra)
  ;;
end

type t =
  { session_id : P.Id.Session.t
  ; generation : int
  ; source : P.Run_source.t option
  ; expected_catalog_revision : int64
  ; expected_goal_revision : int64 option
  ; key : P.Idempotency_key.t
  ; mutation : Mutation.t
  }

let create
      ~session_id
      ~generation
      ~source
      ~expected_catalog_revision
      ~expected_goal_revision
      ~key
      ~mutation
  =
  let open Result.Let_syntax in
  let%bind () =
    match source with
    | None -> Ok ()
    | Some source -> P.Run_source.validate source
  in
  let%bind _ = P.Id.Session.of_json (P.Id.Session.to_json session_id) in
  let%bind _ = P.Id.Goal.of_json (P.Id.Goal.to_json (Mutation.goal_id mutation)) in
  let%bind _ = P.Idempotency_key.of_json (P.Idempotency_key.to_json key) in
  let valid_goal_revision =
    match mutation, expected_goal_revision with
    | Mutation.Create _, None -> true
    | Create _, Some _ -> false
    | ( ( Revise_objective _
        | Record_progress _
        | Hold _
        | Change_budget _
        | Adopt_source _
        | Abandon _ )
      , Some revision ) -> Int64.(revision >= 0L)
    | ( ( Revise_objective _
        | Record_progress _
        | Hold _
        | Change_budget _
        | Adopt_source _
        | Abandon _ )
      , None ) -> false
  in
  if
    generation < 0
    || Int64.(expected_catalog_revision < 0L)
    || (not valid_goal_revision)
    || Bool.(Mutation.needs_source mutation <> Option.is_some source)
    || Option.exists source ~f:(fun source ->
      source.P.Run_source.generation <> generation)
  then Error (P.Error.invalid_request "invalid goal command fences")
  else
    Ok
      { session_id
      ; generation
      ; source
      ; expected_catalog_revision
      ; expected_goal_revision
      ; key
      ; mutation
      }
;;

let digest t =
  let json =
    `Object
      [ "kind", `String "goal_command_v1"
      ; "session_id", P.Id.Session.to_json t.session_id
      ; "generation", X.host_counter_to_json t.generation
      ; "source", X.option_json P.Run_source.to_json t.source
      ; "expected_catalog_revision", X.int64_json t.expected_catalog_revision
      ; "expected_goal_revision", X.option_json X.int64_json t.expected_goal_revision
      ; "key", P.Idempotency_key.to_json t.key
      ; "mutation", Mutation.to_json t.mutation
      ]
  in
  let open Result.Let_syntax in
  let%map encoded = P.Json_codec.canonical_string json in
  Digestif.SHA256.(digest_string encoded |> to_hex)
;;
