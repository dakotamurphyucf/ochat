open Core
module R = Chat_response.Runtime_semantics
module Policy = Chat_response.Automatic_turn_policy

type t =
  { policy : R.policy
  ; followup_turns : int
  ; started_ms : int64 list
  }
[@@deriving equal, sexp]

let create policy = { policy; followup_turns = 0; started_ms = [] }

let with_pauses t conditions =
  let pause_conditions =
    [ R.Pause_followup_turns; Pause_internal_event_drains ]
    |> List.filter ~f:(fun value ->
      List.mem conditions value ~equal:R.equal_pause_condition)
  in
  { t with policy = { t.policy with budget = { t.policy.budget with pause_conditions } } }
;;

let validate t =
  let budget = t.policy.budget in
  let valid_rate =
    match budget.turn_rate_limit with
    | None -> List.is_empty t.started_ms
    | Some { max_turns; window_ms } ->
      max_turns >= 0 && window_ms > 0 && List.length t.started_ms <= max_turns
  in
  match
    t.followup_turns >= 0
    && budget.max_followup_turns >= 0
    && budget.max_self_triggered_turns >= 0
    && budget.max_internal_event_drain >= 0
    && valid_rate
  with
  | true -> Ok ()
  | false ->
    Error (Agent_protocol.Error.invalid_request "invalid automatic-turn budget state")
;;

let milliseconds timestamp =
  Agent_protocol.Timestamp.to_time_ns timestamp
  |> Time_ns.to_int63_ns_since_epoch
  |> Int63.to_int64
  |> fun value -> Int64.(value / 1_000_000L)
;;

let decide t ~now =
  Policy.decide
    ~policy:t.policy
    ~followup_turns_started_since_user_submit:t.followup_turns
    ~started_followup_turn_timestamps_ms:t.started_ms
    ~now_ms:(milliseconds now)
;;

let note_operation t (operation : Agent_protocol.Operation.t) =
  match operation.kind with
  | Compaction | Turn (Administrative | Recovery_retry) -> t
  | Turn User_submit -> { t with followup_turns = 0 }
  | Turn (Moderator_request | Idle_followup) ->
    let now_ms = milliseconds operation.started_at in
    let started_ms =
      match t.policy.budget.turn_rate_limit with
      | None -> []
      | Some { window_ms; max_turns } ->
        let cutoff = Policy.cutoff_ms ~now_ms ~window_ms in
        now_ms :: List.filter t.started_ms ~f:(fun started -> Int64.(started >= cutoff))
        |> Fn.flip List.take (Int.max 0 max_turns)
    in
    { t with
      followup_turns =
        (if t.followup_turns = Int.max_value then Int.max_value else t.followup_turns + 1)
    ; started_ms
    }
;;

module X = Persistence_codec
module J = Agent_protocol.Json_codec

let rate_to_jsonaf (t : R.turn_rate_limit) =
  `Object
    [ "max_turns", X.integer_json t.max_turns; "window_ms", X.integer_json t.window_ms ]
;;

let rate_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind max_turns = X.required fields "max_turns" X.integer in
  let%bind window_ms = X.required fields "window_ms" X.integer in
  let t : R.turn_rate_limit = { max_turns; window_ms } in
  Ok t
;;

let rate_shape =
  X.shape_exn
    [ "max_turns", Document_schema.Shape.value; "window_ms", Document_schema.Shape.value ]
;;

let pause_to_jsonaf = function
  | R.Pause_followup_turns -> `String "followup_turns"
  | Pause_internal_event_drains -> `String "internal_event_drains"
;;

let pause_of_jsonaf =
  J.enum
    ~name:"pause condition"
    [ "followup_turns", R.Pause_followup_turns
    ; "internal_event_drains", R.Pause_internal_event_drains
    ]
;;

let budget_to_jsonaf (t : R.budget_policy) =
  `Object
    [ "max_self_triggered_turns", X.integer_json t.max_self_triggered_turns
    ; "max_followup_turns", X.integer_json t.max_followup_turns
    ; "max_internal_event_drain", X.integer_json t.max_internal_event_drain
    ; "turn_rate_limit", (X.option_json rate_to_jsonaf) t.turn_rate_limit
    ; "pause_conditions", (X.list_json pause_to_jsonaf) t.pause_conditions
    ]
;;

let budget_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind max_self_triggered_turns =
    X.required fields "max_self_triggered_turns" X.integer
  in
  let%bind max_followup_turns = X.required fields "max_followup_turns" X.integer in
  let%bind max_internal_event_drain =
    X.required fields "max_internal_event_drain" X.integer
  in
  let%bind turn_rate_limit =
    X.required fields "turn_rate_limit" (X.nullable rate_of_jsonaf)
  in
  let%bind pause_conditions =
    X.required fields "pause_conditions" (X.list pause_of_jsonaf)
  in
  let t : R.budget_policy =
    { max_self_triggered_turns
    ; max_followup_turns
    ; max_internal_event_drain
    ; turn_rate_limit
    ; pause_conditions
    }
  in
  Ok t
;;

let budget_shape =
  X.shape_exn
    [ "max_self_triggered_turns", Document_schema.Shape.value
    ; "max_followup_turns", Document_schema.Shape.value
    ; "max_internal_event_drain", Document_schema.Shape.value
    ; "turn_rate_limit", X.nullable_shape rate_shape
    ; "pause_conditions", X.array_shape_exn Document_schema.Shape.value
    ]
;;

let policy_to_jsonaf (t : R.policy) =
  `Object
    [ "honor_request_turn", X.bool_json t.honor_request_turn
    ; "honor_request_compaction", X.bool_json t.honor_request_compaction
    ; "budget", budget_to_jsonaf t.budget
    ]
;;

let policy_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind honor_request_turn = X.required fields "honor_request_turn" J.bool in
  let%bind honor_request_compaction =
    X.required fields "honor_request_compaction" J.bool
  in
  let%bind budget = X.required fields "budget" budget_of_jsonaf in
  let t : R.policy = { honor_request_turn; honor_request_compaction; budget } in
  Ok t
;;

let policy_shape =
  X.shape_exn
    [ "honor_request_turn", Document_schema.Shape.value
    ; "honor_request_compaction", Document_schema.Shape.value
    ; "budget", budget_shape
    ]
;;

let storage_to_jsonaf (t : t) =
  `Object
    [ "policy", policy_to_jsonaf t.policy
    ; "followup_turns", X.integer_json t.followup_turns
    ; "started_ms", (X.list_json X.int64_json) t.started_ms
    ]
;;

let storage_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind policy = X.required fields "policy" policy_of_jsonaf in
  let%bind followup_turns = X.required fields "followup_turns" X.integer in
  let%bind started_ms = X.required fields "started_ms" (X.list X.int64) in
  let t : t = { policy; followup_turns; started_ms } in
  let%map () = validate t in
  t
;;

let storage_shape =
  X.shape_exn
    [ "policy", policy_shape
    ; "followup_turns", Document_schema.Shape.value
    ; "started_ms", X.array_shape_exn Document_schema.Shape.value
    ]
;;

let to_jsonaf = storage_to_jsonaf
let of_jsonaf = storage_of_jsonaf
let shape = storage_shape
