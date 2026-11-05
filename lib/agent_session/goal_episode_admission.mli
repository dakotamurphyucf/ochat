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

(** Plan the exact original Goal/catalog CAS into an Enabled candidate and new
    held Workflow/episode/link atomically. No ordinary Run_admission is forged,
    original intent is not rebased, and existing accounting is never reset. *)
val prepare_held
  :  Session_state.t
  -> admission:Run_admission.Held_plan.t
  -> intent:intent
  -> (t, Agent_protocol.Error.t) result

val goal : t -> Agent_protocol.Goal.t
val link : t -> Run_goal_link.t
