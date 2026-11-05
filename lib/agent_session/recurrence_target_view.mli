(** Compact read projection, not an opaque Actor observation or proof issuer.
    Caller must supply a state read through actual current target visibility. *)
type t

val project
  :  Session_state.t
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution.Attempt.t
  -> (t, Agent_protocol.Error.t) result

val to_json : t -> Jsonaf.t
