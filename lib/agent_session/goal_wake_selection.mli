(** Select Goal work from the Actor's current snapshot. The Actor owns the clocks,
    authority, any queue claim and all persisted effects. *)

module Request : sig
  type t =
    | Idle
    | Timer of Agent_protocol.Schedule.t
    | Job_delivery of Run_job_delivery.t
    | Job_completion of Agent_protocol.Job.t
end

module Selection : sig
  type t =
    | Ordinary
    | No_work
    | Suspended_goal of Goal_execution_selection.Suspension.t
    | Wake of Agent_protocol.Run_wake.t
end

type job_lookup =
  Agent_protocol.Id.Job.t -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

type schedule_lookup =
  Agent_protocol.Id.Schedule.t
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

val pending_run
  :  Session_state.t
  -> Agent_protocol.Run_wake.t
  -> (Agent_protocol.Run.t option, Agent_protocol.Error.t) result

val producer_ready
  :  Session_state.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> subscription_expired:
       (Agent_protocol.Subscription.t -> (bool, Agent_protocol.Error.t) result)
  -> find_job:job_lookup
  -> find_schedule:schedule_lookup
  -> Agent_protocol.Run.t
  -> Agent_protocol.Run_wake.t
  -> (bool, Agent_protocol.Error.t) result

val classify
  :  Session_state.t
  -> producer_ready:
       (Agent_protocol.Run.t
        -> Agent_protocol.Run_wake.t
        -> (bool, Agent_protocol.Error.t) result)
  -> Agent_protocol.Run_wake.t
  -> (Selection.t, Agent_protocol.Error.t) result

val validate_request
  :  Session_state.t
  -> find_job:job_lookup
  -> find_schedule:schedule_lookup
  -> Request.t
  -> (unit, Agent_protocol.Error.t) result

val select
  :  Session_state.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> subscription_expired:
       (Agent_protocol.Subscription.t -> (bool, Agent_protocol.Error.t) result)
  -> find_job:job_lookup
  -> find_schedule:schedule_lookup
  -> Request.t
  -> (Selection.t, Agent_protocol.Error.t) result
