open Core

type decision =
  | Allow_automatic_turn
  | Suppress_automatic_turn of
      { notice_key : string
      ; notice_text : string
      }

let has_pause_condition (policy : Runtime_semantics.policy) condition =
  List.mem
    policy.budget.pause_conditions
    condition
    ~equal:Runtime_semantics.equal_pause_condition
;;

let cutoff_ms ~now_ms ~window_ms =
  let window = Int64.of_int (Int.max 0 window_ms) in
  match Int64.(now_ms < min_value + window) with
  | true -> Int64.min_value
  | false -> Int64.(now_ms - window)
;;

let decide
      ~(policy : Runtime_semantics.policy)
      ~followup_turns_started_since_user_submit
      ~started_followup_turn_timestamps_ms
      ~now_ms
  =
  match has_pause_condition policy Pause_followup_turns with
  | true ->
    Suppress_automatic_turn
      { notice_key = "budget:pause-followup-turns"
      ; notice_text = "Automatic follow-up turns are paused by budget policy."
      }
  | false ->
    let limited =
      match policy.budget.turn_rate_limit with
      | None -> false
      | Some { max_turns; window_ms } ->
        let cutoff = cutoff_ms ~now_ms ~window_ms in
        List.count started_followup_turn_timestamps_ms ~f:(fun started ->
          Int64.(started >= cutoff))
        >= max_turns
    in
    (match limited with
     | true ->
       Suppress_automatic_turn
         { notice_key = "budget:turn-rate-limit"
         ; notice_text =
             "Automatic follow-up turn suppressed by the follow-up rate limit."
         }
     | false
       when followup_turns_started_since_user_submit >= policy.budget.max_followup_turns
       ->
       Suppress_automatic_turn
         { notice_key = "budget:max-followup-turns"
         ; notice_text =
             "Automatic follow-up turn suppressed after reaching the follow-up limit."
         }
     | false -> Allow_automatic_turn)
;;
