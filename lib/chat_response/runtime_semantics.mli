open! Core

(** Turn-driver request collapsing and continuation helpers.

    Phase 2 keeps {!policy} as the owning OCaml home for the bounded-turn
    budget contract documented in [docs-src/chatml-budget-policy.md]. *)

type request = Moderation.Runtime_request.t

type turn_rate_limit =
  { max_turns : int
  ; window_ms : int
  }

type pause_condition =
  | Pause_followup_turns
  | Pause_internal_event_drains

type budget_policy =
  { max_self_triggered_turns : int
  ; max_followup_turns : int
  ; max_internal_event_drain : int
  ; turn_rate_limit : turn_rate_limit option
  ; pause_conditions : pause_condition list
  }

(** Host/runtime continuation policy.

    Today this record controls request-turn and compaction handling. Phase 2
    extends the same type with the budget-policy surface documented in
    [docs-src/chatml-budget-policy.md]. *)
type policy =
  { honor_request_turn : bool
  ; honor_request_compaction : bool
  ; budget : budget_policy
  }

type continue_decision =
  [ `Stop
  | `Continue
  ]

type decision =
  { continue : continue_decision
  ; end_session_reason : string option
  ; compaction_requested : bool
  ; forward : request list
  }

(** Default continuation policy.

    The Phase 2 budget defaults that remain attached to this policy are
    specified in [docs-src/chatml-budget-policy.md]. *)
val default_budget_policy : budget_policy
val default_policy : policy
val collapse : request list -> request list
val should_end_session : request list -> string option
val request_turn : request list -> bool
val request_compaction : request list -> bool

val next_self_triggered_turn_budget
  :  policy:policy
  -> request_turn_budget:int
  -> (int, string) result

(** Collapse surfaced runtime requests into one turn-driver decision.

    The Phase 2 budget-policy contract describes how later bounded-turn fields
    on {!policy} participate in this decision boundary:
    [docs-src/chatml-budget-policy.md]. *)
val decide_after_turn_end
  :  policy:policy
  -> tool_followup:bool
  -> request list
  -> decision
