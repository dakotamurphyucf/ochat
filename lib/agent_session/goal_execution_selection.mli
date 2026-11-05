(** Snapshot-only scheduling classification of an existing Run. This does not
    authorize execution, acquire a runtime, check a clock or charge a budget.
    Callers still validate the actual occurrence, original authority and source
    at the final admission boundary after any yielding operation. *)
module Suspension : sig
  type t = private
    { goal_id : Agent_protocol.Id.Goal.t
    ; hold : Agent_protocol.Goal.Hold.t
    }
end

type t =
  | Eligible
  | Suspended_goal of Suspension.t

(** A genuinely current Open Held Goal suspends selection without retiring its
    passive Wait. Stale Run/episode/source evidence and closed Goals reject;
    suspension must not be interpreted as loss of the Run's authority. A Run
    without a Goal association follows the existing ordinary execution path.
    [Eligible] is data, never a reusable execution permit. *)
val of_run : Session_state.t -> Agent_protocol.Run.t -> (t, Agent_protocol.Error.t) result
