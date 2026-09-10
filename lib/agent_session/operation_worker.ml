open! Core

module Input = struct
  type t =
    { session_id : Agent_protocol.Id.Session.t
    ; session_generation : int
    ; operation : Agent_protocol.Operation.t
    ; history : History_entry.t list
    }
end

module Capabilities = struct
  type event_handler =
    executing:Agent_protocol.Moderator_execution.t
    -> retirement_reason:string option
    -> event:Session.Snapshot.t
    -> execute:
         (invocation:Agent_protocol.Invocation.t
          -> (dispatched:Agent_protocol.Invocation.t
              -> (Agent_protocol.Invocation.outcome, Agent_protocol.Error.t) result)
          -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result)
    -> commit:
         (snapshot:Session.Moderator_state.Identity_snapshot.t
          -> requests:Agent_protocol.Invocation.follow_up
          -> (unit, Agent_protocol.Error.t) result)
    -> (unit, Agent_protocol.Error.t) result

  type t =
    { id_source : History_entry.Id_source.t
    ; commit_entry : History_entry.t -> (unit, Agent_protocol.Error.t) result
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
            -> (Agent_protocol.Invocation.outcome, Agent_protocol.Error.t) result)
        -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result
    ; with_moderator_invocation :
        invocation:Agent_protocol.Invocation.t
        -> (dispatched:Agent_protocol.Invocation.t
            -> commit:
                 (resolved:Agent_protocol.Invocation.t
                  -> snapshot:Session.Moderator_state.Identity_snapshot.t
                  -> (unit, Agent_protocol.Error.t) result)
            -> (unit, Agent_protocol.Error.t) result)
        -> (unit, Agent_protocol.Error.t) result
    ; with_moderator_observation :
        invocation_id:Agent_protocol.Id.Invocation.t
        -> (observing:Agent_protocol.Invocation.t
            -> commit:
                 (resolved:Agent_protocol.Invocation.t
                  -> snapshot:Session.Moderator_state.Identity_snapshot.t
                  -> (unit, Agent_protocol.Error.t) result)
            -> (unit, Agent_protocol.Error.t) result)
        -> (unit, Agent_protocol.Error.t) result
    ; with_next_moderator_observation :
        observer:Agent_protocol.Invocation.observer
        -> (observing:Agent_protocol.Invocation.t
            -> commit:
                 (resolved:Agent_protocol.Invocation.t
                  -> snapshot:Session.Moderator_state.Identity_snapshot.t
                  -> (unit, Agent_protocol.Error.t) result)
            -> (unit, Agent_protocol.Error.t) result)
        -> (bool, Agent_protocol.Error.t) result
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
    ; consume_deferred : unit -> (History_entry.t list, Agent_protocol.Error.t) result
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
    ; publish_live :
        kind:Agent_protocol.Event.Recoverable.kind -> payload:Jsonaf.t -> unit
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

type t = { run : sw:Eio.Switch.t -> input:Input.t -> Capabilities.t -> outcome }

let create ~run = { run }
let run t ~sw ~input capabilities = t.run ~sw ~input capabilities
