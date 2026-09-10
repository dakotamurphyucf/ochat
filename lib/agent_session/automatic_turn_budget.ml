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
