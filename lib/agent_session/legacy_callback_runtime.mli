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
    Actual host claim/currentness/ACK errors retain their original typed cause
    across the Manager's script-facing string adapter. Script-only errors retain
    the existing Invalid_state mapping. This neither drains a batch nor installs
    foreground/idle routing. *)
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

(** Installed per-head host adapter. [run] is already bound to one actual
    foreground operation or to the idle claim; construction grants no authority.
    Every selected callback must acknowledge its own Event before returning. *)
module Installed : sig
  type callback =
    selection:Chat_response.Moderator_manager.Callback.Selection.t
    -> history:History_entry.t list
    -> available_tools:Openai.Responses.Request.Tool.t list
    -> now_ms:int
    -> (t option, Agent_protocol.Error.t) result

  type t

  val create : Chat_response.Moderator_manager.t -> run:callback -> t

  (** Consume only Queue_head, bounded to 0..256 callbacks, ending on empty queue,
      halt, End_session or failure. Each successful outcome's notifications were
      published by its own Event ACK and are removed from the returned DATA.
      Returned requests remain durable facts, never a second scheduling command.
      Failed/interrupted claims are rejected by the bound Actor claim. *)
  val drain
    :  t
    -> history:History_entry.t list
    -> available_tools:Openai.Responses.Request.Tool.t list
    -> now_ms:int
    -> max_events:int
    -> (Chat_response.Moderation.Outcome.t list, Agent_protocol.Error.t) result

  (** Use the ACTUAL foreground capabilities for follow-up ownership and the
      provider-before-model acknowledgement. Normalizes already-durable
      compaction/dependent-turn requests using the common foreground policy.
      Idle consumers cannot obtain this adapter from a bare operation ID. The
      returned callbacks raise [Chat_response.Ctx.Effect_rejected] with the exact
      host error instead of erasing it into their script-facing string result;
      the foreground worker maps that rejection to its factual failed outcome. *)
  val foreground_handlers
    :  t
    -> capabilities:Operation_worker.Capabilities.t
    -> ( Chat_response.In_memory_stream.moderator_event_handlers
         , Agent_protocol.Error.t )
         result
end
