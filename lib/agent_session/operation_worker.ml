open! Core

module Input = struct
  type t =
    { session_id : Agent_protocol.Id.Session.t
    ; session_generation : int
    ; operation : Agent_protocol.Operation.t
    ; history : History_entry.t list
    }
end

module Model_admission = struct
  type t =
    { check : managed:bool -> (unit, Agent_protocol.Error.t) result
    ; admit :
        managed:bool
        -> Inference_runtime.Prepared.t
        -> Inference_runtime.Attempt.t
        -> (unit -> unit, Agent_protocol.Error.t) result
    }
end

module Capabilities = struct
  type 'plan notification_consumer =
    'plan
    -> ( Chat_response.In_memory_stream.Safe_point_input.batch
         , Agent_protocol.Error.t )
         result

  type event_handler =
    executing:Agent_protocol.Moderator_execution.t
    -> policy_caller:Delegated_policy_caller.t option
    -> model_jobs:Model_job_admission.t
    -> allocate_inference:Inference_allocation.Port.t
    -> retirement_reason:string option
    -> event:Session.Snapshot.t
    -> execute:
         (invocation:Agent_protocol.Invocation.t
          -> (dispatched:Agent_protocol.Invocation.t
              -> capabilities:Invocation_capabilities.t
              -> (Agent_protocol.Invocation.outcome, Agent_protocol.Error.t) result)
          -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result)
    -> commit:
         (snapshot:Session.Moderator_state.Identity_snapshot.t
          -> requests:Agent_protocol.Invocation.follow_up
          -> (unit, Agent_protocol.Error.t) result)
    -> (unit, Agent_protocol.Error.t) result

  type t =
    { id_source : History_entry.Id_source.t
    ; callback_cleanup :
        unit -> (Callback_cleanup.Operation.t, Agent_protocol.Error.t) result
    ; allocate_inference : Inference_allocation.Port.t
    ; policy_caller : unit -> (Delegated_policy_caller.t, Agent_protocol.Error.t) result
    ; original_agent_actor :
        unit -> (Operator_authorization.t option, Agent_protocol.Error.t) result
    ; commit_entry : History_entry.t -> (unit, Agent_protocol.Error.t) result
    ; prepare_authoring_input :
        Chat_response.Authoring_materialization.t
        -> history:History_entry.t list
        -> effective:Chat_response.Moderation.Effective_entry.t list
        -> (History_entry.t list, Agent_protocol.Error.t) result
    ; commit_invocation_call :
        invocation:Agent_protocol.Invocation.t
        -> History_entry.t
        -> (unit, Agent_protocol.Error.t) result
    ; publish_invocation_output :
        invocation_id:Agent_protocol.Id.Invocation.t
        -> History_entry.t
        -> (unit, Agent_protocol.Error.t) result
    ; commit_moderator : Jsonaf.t option -> (unit, Agent_protocol.Error.t) result
    ; with_invocation :
        invocation:Agent_protocol.Invocation.t
        -> (dispatched:Agent_protocol.Invocation.t
            -> capabilities:Invocation_capabilities.t
            -> (Agent_protocol.Invocation.outcome, Agent_protocol.Error.t) result)
        -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result
    ; with_moderator_invocation :
        ?snapshot:
          (unit
           -> (Session.Moderator_state.Identity_snapshot.t, Agent_protocol.Error.t) result)
        -> invocation:Agent_protocol.Invocation.t
        -> (dispatched:Agent_protocol.Invocation.t
            -> capabilities:Invocation_capabilities.t
            -> commit:
                 (resolved:Agent_protocol.Invocation.t
                  -> snapshot:Session.Moderator_state.Identity_snapshot.t
                  -> (unit, Agent_protocol.Error.t) result)
            -> (unit, Agent_protocol.Error.t) result)
        -> (unit, Agent_protocol.Error.t) result
    ; with_moderator_observation :
        invocation_id:Agent_protocol.Id.Invocation.t
        -> (observing:Agent_protocol.Invocation.t
            -> capabilities:Invocation_capabilities.t
            -> commit:
                 (resolved:Agent_protocol.Invocation.t
                  -> snapshot:Session.Moderator_state.Identity_snapshot.t
                  -> (unit, Agent_protocol.Error.t) result)
            -> (unit, Agent_protocol.Error.t) result)
        -> (unit, Agent_protocol.Error.t) result
    ; with_next_moderator_observation :
        observer:Agent_protocol.Invocation.observer
        -> (observing:Agent_protocol.Invocation.t
            -> capabilities:Invocation_capabilities.t
            -> commit:
                 (resolved:Agent_protocol.Invocation.t
                  -> snapshot:Session.Moderator_state.Identity_snapshot.t
                  -> (unit, Agent_protocol.Error.t) result)
            -> (unit, Agent_protocol.Error.t) result)
        -> (bool, Agent_protocol.Error.t) result
    ; run_actions :
        Agent_protocol.Moderator_execution.t
        -> (Run_action_service.t option, Agent_protocol.Error.t) result
    ; with_moderator_event :
        snapshot:
          (unit
           -> (Session.Moderator_state.Identity_snapshot.t, Agent_protocol.Error.t) result)
        -> event:Chat_response.Moderation.Event.t
        -> event_handler
        -> (bool, Agent_protocol.Error.t) result
    ; with_queued_moderator_event :
        snapshot:
          (unit
           -> (Session.Moderator_state.Identity_snapshot.t, Agent_protocol.Error.t) result)
        -> event_handler
        -> (bool, Agent_protocol.Error.t) result
    ; manage_moderator_follow_up :
        observer:Agent_protocol.Invocation.observer
        -> (unit, Agent_protocol.Error.t) result
    ; admit_moderator_turn : unit -> (unit, Agent_protocol.Error.t) result
    ; admit_notification_turn : unit -> (unit, Agent_protocol.Error.t) result
    ; model_admission : Model_admission.t option
    ; root_context : Chat_response.Root_context.t option
    ; capacity_rollover : Chat_response.Capacity_rollover.t option
    ; select_rollover_followup :
        (Recovery_rollover_continuation.Hook.view
         -> Recovery_rollover_continuation.Hook.decision)
          option
        -> (unit, Agent_protocol.Error.t) result
    ; successor_inference : Chat_response.Root_context.Successor.t option
    ; successor_hint_references : Recovery_rollover_hint_ref.Witness.t option
    ; settle_tool_batch :
        (Chat_response.Settled_tool_batch.t
         -> (Chat_response.Settled_tool_batch.decision, Agent_protocol.Error.t) result)
          option
    ; consume_deferred : unit -> (History_entry.t list, Agent_protocol.Error.t) result
    ; consume_notifications : Notification_delivery.t notification_consumer
    ; consume_initial_notifications : Notification_delivery.idle notification_consumer
    ; request_permission :
        permission:Agent_protocol.Permission.t
        -> timeout_seconds:float option
        -> fallback:Agent_protocol.Permission.choice
        -> review_on_timeout:
             (unit
              -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
               option
        -> (Agent_protocol.Permission.resolution, Agent_protocol.Error.t) result
    ; request_review :
        permission:Agent_protocol.Permission.t
        -> review:
             (unit
              -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
        -> (Agent_protocol.Permission.resolution, Agent_protocol.Error.t) result
    ; responder_available : unit -> bool
    ; invocation_granted : tool_name:string -> identity_digest:string -> bool
    ; publish_live : Agent_protocol.Event.Recoverable.payload -> unit
    }
end

module Summary = struct
  type t =
    { final_history : History_entry.t list
    ; runtime_requests : Chat_response.Moderation.Runtime_request.t list
    ; moderator_snapshot : Jsonaf.t option
    }
end

type cancellation = { reason : string }

type outcome =
  | Completed of Summary.t
  | Cancelled of cancellation
  | Failed of Agent_protocol.Error.t

type run = sw:Eio.Switch.t -> input:Input.t -> Capabilities.t -> outcome

type t =
  { run : run
  ; capacity_aware : bool
  }

let create ~run = { run; capacity_aware = false }
let create_capacity_aware ~run = { run; capacity_aware = true }
let map_run t ~f = { t with run = f t.run }
let supports_capacity_rollover t = t.capacity_aware
let run t ~sw ~input capabilities = t.run ~sw ~input capabilities
