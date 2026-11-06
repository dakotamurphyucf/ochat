(** Bounded authorized metadata projection, not execution permission. No provider
    rows, raw session document, private custody or command receipt history. *)
type t

(** Uses canonical Goal codecs and resolves each retained account against the
    actual ledger. Missing historical accounting stays absent, never zero usage.
    A source fence is DATA and may be absent before actual installation. *)
val capture
  :  Session_state.t
  -> session:Agent_protocol.Session_ref.t
  -> goal_id:Agent_protocol.Id.Goal.t option
  -> (t, Agent_protocol.Error.t) result

val to_json : t -> (Jsonaf.t, Agent_protocol.Error.t) result
