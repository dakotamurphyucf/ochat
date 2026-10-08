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

(** Change only host pause flags, retaining admission counts, rate history and
    immutable ceilings. Repeated flags normalize to one stable representation. *)
val with_pauses : t -> Chat_response.Runtime_semantics.pause_condition list -> t

val validate : t -> (unit, Agent_protocol.Error.t) result

(** Record only a newly admitted turn. State updates and compactions do not count.
    The caller must distinguish a new operation from updates to the existing one.
    Recovered state retains the recorded count, not a replay of external work. *)
val note_operation : t -> Agent_protocol.Operation.t -> t

val decide
  :  t
  -> now:Agent_protocol.Timestamp.t
  -> Chat_response.Automatic_turn_policy.decision

(** Complete, validated named-field storage representation. Public receipt
    projections remain separate. *)
val to_jsonaf : t -> Jsonaf.t

val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
val policy_to_jsonaf : Chat_response.Runtime_semantics.policy -> Jsonaf.t

val policy_of_jsonaf
  :  Jsonaf.t
  -> (Chat_response.Runtime_semantics.policy, Agent_protocol.Error.t) result

val policy_shape : Document_schema.Shape.t
val pause_to_jsonaf : Chat_response.Runtime_semantics.pause_condition -> Jsonaf.t

val pause_of_jsonaf
  :  Jsonaf.t
  -> (Chat_response.Runtime_semantics.pause_condition, Agent_protocol.Error.t) result
