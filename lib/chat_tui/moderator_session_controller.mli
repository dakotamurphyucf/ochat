open! Core

(** How request-turn runtime requests should be interpreted by the session
    controller. *)
type turn_request =
  | Ignore
  | Schedule of App_runtime.turn_start_reason

(** Structured session-controller actions derived from moderator runtime
    requests.

    Precedence rules:
    {ul
    {- [End_session] suppresses any scheduled turn.}
    {- Compaction requests remain visible even when [End_session] is present.}
    {- Pure runtime requests do not by themselves require a message refresh;
       callers can refresh separately when other moderator-visible state
       changes.}} *)
type t =
  { request_refresh : bool
  ; request_compact : bool
  ; request_turn : App_runtime.turn_start_reason option
  ; halt_reason : string option
  ; system_notices : string list
  ; internal_events_to_enqueue : App_events.internal_event list
  }

val of_outcomes
  :  policy:Chat_response.Runtime_semantics.policy
  -> turn_request:turn_request
  -> Chat_response.Moderation.Outcome.t list
  -> t

val of_runtime_requests
  :  policy:Chat_response.Runtime_semantics.policy
  -> turn_request:turn_request
  -> Chat_response.Moderation.Runtime_request.t list
  -> t

val of_runtime_request
  :  policy:Chat_response.Runtime_semantics.policy
  -> turn_request:turn_request
  -> Chat_response.Moderation.Runtime_request.t
  -> t

val drain_internal_events
  :  moderator:Chat_response.In_memory_stream.moderator
  -> now_ms:int
  -> history:Openai.Responses.Item.t list
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> turn_request:turn_request
  -> (t, string) result
