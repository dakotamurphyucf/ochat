(** Private immutable native prefix. Capture is O(1) in inventory size; named
    queries inspect only the requested row and its parent. This is DATA, not an
    unchecked complete Context and never an execution capability. *)
type t = private
  { session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; current_source : Agent_protocol.Run_source.t option
  ; events : Agent_protocol.Moderator_execution.t list
  ; invocations : Agent_protocol.Invocation.t list
  ; jobs : Agent_protocol.Job.t list
  ; runs : Run_state.t option
  }

val create
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> current_source:Agent_protocol.Run_source.t option
  -> events:Agent_protocol.Moderator_execution.t list
  -> invocations:Agent_protocol.Invocation.t list
  -> jobs:Agent_protocol.Job.t list
  -> runs:Run_state.t option
  -> (t, Agent_protocol.Error.t) result

val of_context : Policy_cause_data.Context.t -> t

val check_context
  :  t
  -> Policy_cause_data.Context.t
  -> (unit, Agent_protocol.Error.t) result

val check_owner
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> (unit, Agent_protocol.Error.t) result

(** Named queries reject missing/duplicate rows, foreign sessions, future
    generations and invalid native values. Historical retained rows may be from
    an earlier generation; the Cause projector checks that exact generation and
    runtime Source additionally requires the current member/source. An absent
    carrier cannot turn a failed named query into None. *)
val event
  :  t
  -> string
  -> (Agent_protocol.Moderator_execution.t, Agent_protocol.Error.t) result

val invocation
  :  t
  -> string
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result

val job : t -> string -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

val member_of_job
  :  Agent_protocol.Job.t
  -> (Policy_cause_data.Member.t, Agent_protocol.Error.t) result

val job_member
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> (Policy_cause_data.Member.t, Agent_protocol.Error.t) result

val invocation_parent
  :  t
  -> Agent_protocol.Invocation.t
  -> (Policy_cause_data.Member.t option, Agent_protocol.Error.t) result

val job_parent
  :  t
  -> Agent_protocol.Job.t
  -> (Policy_cause_data.Member.t option, Agent_protocol.Error.t) result

module Queued_job : sig
  type t

  val id : t -> Agent_protocol.Id.Job.t
  val generation : t -> int
  val attempt : t -> int
  val source : t -> Agent_protocol.Run_source.t
  val origin : t -> Agent_protocol.Inference_origin.t option
  val actual_job : t -> Agent_protocol.Job.t option
  val retained_delivery : t -> Run_job_delivery.t option
end

val queued_job
  :  t
  -> Agent_protocol.Moderator_execution.t
  -> (Queued_job.t option, Agent_protocol.Error.t) result

module Event_parent : sig
  type t =
    | Root
    | Native of Policy_cause_data.Member.t
    | Retained_job of Queued_job.t
    | Unavailable
end

val event_ancestry
  :  t
  -> Agent_protocol.Moderator_execution.t
  -> (Event_parent.t, Agent_protocol.Error.t) result

val event_parent
  :  t
  -> Agent_protocol.Moderator_execution.t
  -> (Policy_cause_data.Member.t option, Agent_protocol.Error.t) result
