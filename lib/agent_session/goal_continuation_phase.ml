open! Core

module Cause = struct
  type t =
    | Resume_command
    | Pending_wake
    | Claimed_job
end

module Use = struct
  type t =
    | Fresh_effect
    | Registered_allocation
end

type 'binding t =
  | Preparing
  | Source_bound of 'binding
  | Writer_pending of 'binding
  | Ready of 'binding * Goal_command_receipt.t
  | Execution_ready of 'binding
  | Waiting of Goal_command_receipt.t * Agent_protocol.Run_wake.t
  | Closed of Goal_command_receipt.t option

let receipt = function
  | Ready (_, receipt) | Waiting (receipt, _) -> Some receipt
  | Closed receipt -> receipt
  | Preparing | Source_bound _ | Writer_pending _ | Execution_ready _ -> None
;;

let close t = Closed (receipt t)

let acknowledge t ~binding ~receipt =
  match t with
  | Writer_pending current when phys_equal current binding -> Ready (binding, receipt)
  | Preparing
  | Source_bound _
  | Writer_pending _
  | Ready _
  | Execution_ready _
  | Waiting _
  | Closed _ -> Closed (Some receipt)
;;

let check_live
      t
      ~(cause : Cause.t)
      ~(use : Use.t)
      ~(goal_state : Agent_protocol.Goal.State.t)
  =
  let denied message =
    Error (Agent_protocol.Error.create Permission_denied ~message ~retryable:false ())
  in
  match cause, t, goal_state, use with
  | Resume_command, (Preparing | Source_bound _ | Writer_pending _), Open (Held _), _
  | Resume_command, Ready _, Open Enabled, _
  | Resume_command, Ready _, Open (Held _), Registered_allocation
  | Pending_wake, (Preparing | Source_bound _ | Execution_ready _), Open Enabled, _
  | Pending_wake, Execution_ready _, Open (Held _), Registered_allocation
  | ( Claimed_job
    , (Preparing | Source_bound _ | Execution_ready _)
    , Open (Enabled | Held _)
    , _ ) -> Ok ()
  | Resume_command, _, _, _ -> denied "Goal continuation is not live in its actual phase"
  | Pending_wake, _, _, _ -> denied "Goal wake construction is not live"
  | Claimed_job, _, _, _ -> denied "claimed Goal Job construction is not live"
;;
