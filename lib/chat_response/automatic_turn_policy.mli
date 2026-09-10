open Core

(** Shared host-started follow-up policy. In-loop model continuations retain their
    separate consecutive self-trigger budget. User turns bypass this decision. *)
type decision =
  | Allow_automatic_turn
  | Suppress_automatic_turn of
      { notice_key : string
      ; notice_text : string
      }

val has_pause_condition
  :  Runtime_semantics.policy
  -> Runtime_semantics.pause_condition
  -> bool

(** Pause takes precedence over rate, then count. The rate window includes its
    lower endpoint and future timestamps conservatively survive a clock rollback.
    Subtraction saturates at the int64 boundary instead of wrapping. *)
val decide
  :  policy:Runtime_semantics.policy
  -> followup_turns_started_since_user_submit:int
  -> started_followup_turn_timestamps_ms:int64 list
  -> now_ms:int64
  -> decision

val cutoff_ms : now_ms:int64 -> window_ms:int -> int64
