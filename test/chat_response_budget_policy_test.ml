open! Core

module App_runtime = Chat_tui.App_runtime
module Runtime_semantics = Chat_response.Runtime_semantics

let string_of_automatic_turn_decision = function
  | App_runtime.Allow_automatic_turn -> "allow"
  | App_runtime.Suppress_automatic_turn { notice_key; notice_text } ->
    Printf.sprintf "suppress:%s:%s" notice_key notice_text
;;

let%expect_test "default policy exposes the Phase 2 budget defaults" =
  let budget = Runtime_semantics.default_policy.budget in
  print_s
    [%sexp
      ( [ "max_self_triggered_turns", Int.to_string budget.max_self_triggered_turns
        ; "max_followup_turns", Int.to_string budget.max_followup_turns
        ; "max_internal_event_drain", Int.to_string budget.max_internal_event_drain
        ; ( "turn_rate_limit"
          , (match budget.turn_rate_limit with
             | None -> "none"
             | Some rate_limit ->
               Printf.sprintf "%d/%d" rate_limit.max_turns rate_limit.window_ms) )
        ]
        : (string * string) list )];
  [%expect
    {|
    ((max_self_triggered_turns 10) (max_followup_turns 1)
     (max_internal_event_drain 100) (turn_rate_limit none))
    |}];
  print_s
    [%sexp
      (List.map budget.pause_conditions ~f:(function
         | Runtime_semantics.Pause_followup_turns -> "pause_followup_turns"
         | Runtime_semantics.Pause_internal_event_drains ->
           "pause_internal_event_drains")
       : string list)];
  [%expect {| () |}]
;;

let%expect_test "automatic turn precedence prefers pause over rate and count" =
  let policy =
    { Runtime_semantics.default_policy with
      budget =
        { Runtime_semantics.default_budget_policy with
          max_followup_turns = 0
        ; turn_rate_limit = Some { max_turns = 0; window_ms = 60_000 }
        ; pause_conditions = [ Runtime_semantics.Pause_followup_turns ]
        }
    }
  in
  let decision =
    App_runtime.decide_automatic_turn
      ~policy
      ~followup_turns_started_since_user_submit:10
      ~started_followup_turn_timestamps_ms:[ 1; 2; 3 ]
      ~now_ms:3
      ~reason:App_runtime.Idle_followup
  in
  print_endline (string_of_automatic_turn_decision decision);
  [%expect
    {|
    suppress:budget:pause-followup-turns:Automatic follow-up turns are paused by budget policy.
    |}]
;;

let%expect_test "automatic turn precedence prefers rate limit over count limit" =
  let policy =
    { Runtime_semantics.default_policy with
      budget =
        { Runtime_semantics.default_budget_policy with
          max_followup_turns = 1
        ; turn_rate_limit = Some { max_turns = 1; window_ms = 60_000 }
        }
    }
  in
  let decision =
    App_runtime.decide_automatic_turn
      ~policy
      ~followup_turns_started_since_user_submit:1
      ~started_followup_turn_timestamps_ms:[ 100 ]
      ~now_ms:100
      ~reason:App_runtime.Moderator_request
  in
  print_endline (string_of_automatic_turn_decision decision);
  [%expect
    {|
    suppress:budget:turn-rate-limit:Automatic follow-up turn suppressed by the follow-up rate limit.
    |}]
;;

let%expect_test "user submit bypasses automatic follow-up suppressors" =
  let policy =
    { Runtime_semantics.default_policy with
      budget =
        { Runtime_semantics.default_budget_policy with
          max_followup_turns = 0
        ; turn_rate_limit = Some { max_turns = 0; window_ms = 60_000 }
        ; pause_conditions = [ Runtime_semantics.Pause_followup_turns ]
        }
    }
  in
  let decision =
    App_runtime.decide_automatic_turn
      ~policy
      ~followup_turns_started_since_user_submit:100
      ~started_followup_turn_timestamps_ms:[ 10; 20; 30 ]
      ~now_ms:30
      ~reason:App_runtime.User_submit
  in
  print_endline (string_of_automatic_turn_decision decision);
  [%expect {| allow |}]
;;

let%expect_test "pause-internal-event-drains is checked independently" =
  let policy =
    { Runtime_semantics.default_policy with
      budget =
        { Runtime_semantics.default_budget_policy with
          pause_conditions = [ Runtime_semantics.Pause_internal_event_drains ]
        }
    }
  in
  print_s
    [%sexp
      (App_runtime.should_pause_internal_event_drains ~policy : bool)];
  [%expect {| true |}]
;;
