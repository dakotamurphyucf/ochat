open! Core

(** Scoped Tool, Schedule and Model bridge for one actual legacy Manager callback.
    The host claim owns the Actor borrow through its exact Event acknowledgement.
    Constructing these service records does not grant effect authority. *)
module Services : sig
  type schedules =
    { after_ms : delay_ms:int -> payload:Jsonaf.t -> (string, string) result
    ; cancel : id:string -> (unit, string) result
    }

  type t =
    { jobs : Runtime_model_services.t
    ; schedules : schedules
    ; execute : Native_tool_invocation.executor
    ; executing : Agent_protocol.Moderator_execution.t
    ; check_current : unit -> (unit, Agent_protocol.Error.t) result
    ; is_halted : unit -> bool
    ; checkpoint : unit -> (Jsonaf.t option, Agent_protocol.Error.t) result
    }
end

(** The actual Manager issues [callback] under its execution gate. The host must
    claim that exact callback and supply services for its real constructor or
    installed owner, without an ambient fallback. [commit] saves the prospective
    full checkpoint, request intent and notifications in the SAME Event ACK.
    True means that checkpoint and cleanup completed; false means the physical
    Actor owner was unavailable. Saved checkpoint DATA grants no new authority. *)
type claim =
  manager:Chat_response.Moderator_manager.t
  -> callback:Chat_response.Moderator_manager.Callback.t
  -> (services:Services.t
      -> commit:
           (snapshot:Session.Moderator_state.Identity_snapshot.t
            -> requests:Agent_protocol.Invocation.follow_up
            -> notifications:string list
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (bool, Agent_protocol.Error.t) result

(** Actual committed outcome and exact acknowledged checkpoint DATA. Notifications
    and request intent are already durable; consumers must not publish or schedule
    them a second time. The returned outcome is preserved in full for inspection. *)
type t =
  { outcome : Chat_response.Moderation.Outcome.t
  ; checkpoint : Jsonaf.t option
  }

(** Execute one Boundary or exact Queue_head through the existing Manager
    transaction and supplied Actor bracket. Only an empty queue returns None;
    unavailable ownership rejects. Tool/Model/Schedule services are lexical and
    expire before return. Original exceptions and cancellation propagate; the
    host's existing failed/interrupted claim prevents replay of external effects.
    This neither drains a batch nor installs foreground/idle routing. *)
val run
  :  Chat_response.Moderator_manager.t
  -> claim:claim
  -> model_recipes:
       (Runtime_model_services.t
        -> Chat_response.Moderation.Capabilities.model_recipe String.Map.t)
  -> script_tools:Script_tool_calls.t
  -> selected:Chat_response.Tool_capability.t
  -> selection:Chat_response.Moderator_manager.Callback.Selection.t
  -> session_id:string
  -> history:History_entry.t list
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> now_ms:int
  -> (t option, Agent_protocol.Error.t) result
