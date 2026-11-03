(** Pure association planning for the actual controller admission owner. An
    intent/result grants no authority. The controller must authenticate its real
    Enabled-goal reservation and existing actor Run preparation, then persist
    both returned subcarriers with that exact new admission before execution.
    There is no public posthoc bind or optional-goal writer admission API. *)
type intent

val intent
  :  goal_id:Agent_protocol.Id.Goal.t
  -> expected_goal_revision:int64
  -> expected_catalog_revision:int64
  -> (intent, Agent_protocol.Error.t) result

val digest : intent -> string

type t

val prepare
  :  Session_state.t
  -> admission:Run_admission.t
  -> intent:intent
  -> (t, Agent_protocol.Error.t) result

val run_state : t -> Run_state.t
val catalog : t -> Goal_catalog.t
