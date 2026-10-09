(** Pure ordinary deletion: removes the target and its actual tool occurrence
    partner, preserving every other canonical entry and all pending input. *)
type t

val prepare
  :  Session_state.t
  -> history_id:Agent_protocol.History.Id.t
  -> (t, Agent_protocol.Error.t) result

val canonical_history : t -> Agent_protocol.History.entry list
val retired_ids : t -> Agent_protocol.History.Id.t list
val initial_prompt_entry_count : t -> int

(** Rechecks the complete admitted state, including pending inputs and allocator. *)
val validate_basis : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
