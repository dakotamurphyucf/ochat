(** Compact read projection, not an opaque Actor observation or proof issuer.
    Caller must supply a state read through actual current target visibility. *)
type t

val project
  :  Session_state.t
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution.Attempt.t
  -> (t, Agent_protocol.Error.t) result

val to_json : t -> Jsonaf.t

(** Same exact Run evidence as [project], with independent Run disclosure.
    A visible session may omit its Run. [can_view] is synchronous. *)
val project_public
  :  Session_state.t
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution.Attempt.t
  -> can_view:(run_owner:Agent_protocol.Id.Principal.t -> bool)
  -> (Agent_protocol.Recurrence_query.Target.t, Agent_protocol.Error.t) result
