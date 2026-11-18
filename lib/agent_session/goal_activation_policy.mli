(** Private pure association/policy facts. No Actor, epoch, callback, mutable
    custody or persistence owner is created here. *)
type t

val create
  :  goal:Agent_protocol.Goal.t
  -> link:Run_goal_link.t
  -> (t, Agent_protocol.Error.t) result

val goal_id : t -> Agent_protocol.Id.Goal.t

val check_association
  :  t
  -> state:Session_state.t
  -> run:Agent_protocol.Run.t
  -> admission:Agent_protocol.Run_receipt.t
  -> (unit, Agent_protocol.Error.t) result

(** Only a NEW episode's planning and final prewriter checks. No dimension
    counter ceiling or increment; the existing admission owner controls those. *)
val check_new_episode_eligibility
  :  t
  -> state:Session_state.t
  -> now:Agent_protocol.Timestamp.t
  -> (Goal_admission_policy.decision, Agent_protocol.Error.t) result

(** Ongoing Enabled/source/allocation-policy/account integrity only. Never
    reconsider token allowance or count ceilings of already-admitted work. *)
val check_policy : t -> state:Session_state.t -> (unit, Agent_protocol.Error.t) result

(** Same-allocation/account compatibility for an actual registered allocation.
    The Actor separately requires its retained physical startup cell and exact
    registration. Automatic admission holds do not cancel already-admitted work;
    explicit control changes monotonically retire that cell before saving. *)
val check_admitted_policy
  :  t
  -> state:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** Pure deadline comparison; callers supply a fresh sample at an effect
    timing boundary, before original and final callback-free basis checks. *)
val check_deadline
  :  t
  -> state:Session_state.t
  -> now:Agent_protocol.Timestamp.t
  -> (unit, Agent_protocol.Error.t) result
