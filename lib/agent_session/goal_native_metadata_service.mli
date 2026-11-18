type t

(** Actual Factory composition only. Closures resolve the SAME actor_ref and
    current profile. Service entry checks the lexical native admission; retained
    authorizers check the actual supplied transferred original, immutable
    principal/session/generation and current profile/state. Existing Actor
    stage/commit paths own callback, source and command-loan checks. The native
    token is not retained or extended. Start and Resume are staged under the
    same owner. The existing dispatcher executes accepted intents after release. *)
val create
  :  get:
       (admission:Goal_native_admission.t
        -> goal_id:Agent_protocol.Id.Goal.t option
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> create_goal:
       (admission:Goal_native_admission.t
        -> request:Agent_protocol.Goal_native_create_request.t
        -> (unit, Agent_protocol.Error.t) result)
  -> control:
       (admission:Goal_native_admission.t
        -> request:Agent_protocol.Goal_native_control_request.t
        -> (unit, Agent_protocol.Error.t) result)
  -> edit:
       (admission:Goal_native_admission.t
        -> request:Agent_protocol.Goal_edit.t
        -> (unit, Agent_protocol.Error.t) result)
  -> start:
       (admission:Goal_native_admission.t
        -> request:Agent_protocol.Goal_native_start_request.t
        -> (unit, Agent_protocol.Error.t) result)
  -> resume:
       (admission:Goal_native_admission.t
        -> request:Agent_protocol.Goal_resume_request.t
        -> (unit, Agent_protocol.Error.t) result)
  -> t

val get
  :  t
  -> admission:Goal_native_admission.t
  -> goal_id:Agent_protocol.Id.Goal.t option
  -> (Jsonaf.t, Agent_protocol.Error.t) result

val create_goal
  :  t
  -> admission:Goal_native_admission.t
  -> request:Agent_protocol.Goal_native_create_request.t
  -> (unit, Agent_protocol.Error.t) result

val control
  :  t
  -> admission:Goal_native_admission.t
  -> request:Agent_protocol.Goal_native_control_request.t
  -> (unit, Agent_protocol.Error.t) result

val edit
  :  t
  -> admission:Goal_native_admission.t
  -> request:Agent_protocol.Goal_edit.t
  -> (unit, Agent_protocol.Error.t) result

(** Stage a native initial episode under the exact selected Start admission.
    Local Complete is not callback ACK, Run admission or execution permission. *)
val start
  :  t
  -> admission:Goal_native_admission.t
  -> request:Agent_protocol.Goal_native_start_request.t
  -> (unit, Agent_protocol.Error.t) result

(** Stage continuation of an existing episode under the exact selected Resume
    admission. Local Complete describes staging until the owning callback ACK. *)
val resume
  :  t
  -> admission:Goal_native_admission.t
  -> request:Agent_protocol.Goal_resume_request.t
  -> (unit, Agent_protocol.Error.t) result
