(** Durable moderator subscription identity and terminal state. Workflow-specific
    state lives in the moderator. These functions do not execute timers or jobs. *)
type context =
  { id : Id.Subscription.t
  ; session_id : Id.Session.t
  ; generation : int
  ; invocation_id : Id.Invocation.t
  ; kind : string
  ; created_at : Timestamp.t
  ; deadline : Timestamp.t
  ; completion_schema : Jsonaf.t option
  ; wake : Completion.wake
  ; ingress_capability : Id.Capability.t option
  }
[@@deriving sexp]

type t = private
  { context : context
  ; epoch : int
  ; timer_id : Id.Schedule.t option
  ; job_id : Id.Job.t option
  ; result : Completion.t option
  ; completed_at : Timestamp.t option
  }
[@@deriving sexp]

val create : context -> (t, Error.t) result
val validate : t -> (unit, Error.t) result

val arm
  :  t
  -> expected_epoch:int
  -> timer_id:Id.Schedule.t option
  -> job_id:Id.Job.t option
  -> (t, Error.t) result

(** First terminal commit wins. A repeat returns the retained winner and [false].
    A stale epoch cannot complete a still-active subscription. Expiry cannot be
    recorded before the deadline. Actual work cancellation is a host action. *)
val finish
  :  t
  -> expected_epoch:int
  -> now:Timestamp.t
  -> Completion.t
  -> (t * bool, Error.t) result

val validate_transition : previous:t option -> t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
