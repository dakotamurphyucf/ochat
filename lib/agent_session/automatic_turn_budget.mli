open Core

(** Durable accounting for qualified host-started turns. Runtime unload/rebuild
    preserves this value; only a newly admitted user turn resets the count.
    The rate history is independent of the user-turn count. *)
type t = private
  { policy : Chat_response.Runtime_semantics.policy
  ; followup_turns : int
  ; started_ms : int64 list
  }
[@@deriving equal, sexp]

val create : Chat_response.Runtime_semantics.policy -> t
val validate : t -> (unit, Agent_protocol.Error.t) result

(** Record only a newly admitted turn. State updates and compactions do not count.
    The caller must distinguish a new operation from updates to the existing one.
    Recovered state retains the recorded count, not a replay of external work. *)
val note_operation : t -> Agent_protocol.Operation.t -> t

val decide
  :  t
  -> now:Agent_protocol.Timestamp.t
  -> Chat_response.Automatic_turn_policy.decision
