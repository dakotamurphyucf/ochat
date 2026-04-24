open! Core

(** Public turn-preparation and safe-point helpers for embedders.

    This module gives a stable public framing to the existing
    {!In_memory_stream} turn-boundary helpers. It prepares request inputs and
    interprets surfaced moderation outputs, but it does not own the host
    session-controller policy.

    For the canonical safe-point and effective-history semantics, see
    [docs-src/chatml-safe-point-and-effective-history.md].

    Approval suspension is intentionally distinct from {!Safe_point_input}.
    Deferred safe-point input feeds a future request boundary, while
    [Approval.ask_text] and [Approval.ask_choice] pause the current live
    script and resume it later through the moderator-facing UI request
    boundary. *)

module Moderation = Chatml_moderation
module Moderator = Chatml_moderator
module Res = Openai.Responses

module Safe_point_input : sig
  type t = In_memory_stream.Safe_point_input.t = { consume : unit -> string option }
end

type moderator =
  { manager : Moderator.t
  ; session_id : string
  ; session_meta : Jsonaf.t
  ; runtime_policy : Runtime_semantics.policy
  }

type pending_ui_request = Moderator.pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of { prompt : string; choices : string array }

type moderated_tool_call =
  { call_item : Res.Item.t
  ; kind : Tool_call.Kind.t
  ; name : string
  ; payload : string
  ; synthetic_result : Res.Tool_output.Output.t option
  ; runtime_requests : Moderation.Runtime_request.t list
  }

val pending_ui_request : moderator -> pending_ui_request option

val resume_ui_request
  :  moderator
  -> response:string
  -> (Moderation.Outcome.t list, string) result

(** [prepare_turn_inputs ?safe_point_input ?moderator ~available_tools ~now_ms
    ~history ()] applies the turn-start safe point before the next model call.

    This helper prepares effective request history and may append transient
    safe-point input, but it does not decide broader host scheduling policy. *)
val prepare_turn_inputs
  :  moderator:moderator option
  -> ?safe_point_input:Safe_point_input.t
  -> available_tools:Res.Request.Tool.t list
  -> now_ms:int
  -> history:Res.Item.t list
  -> unit
  -> (Res.Item.t list, string) result

(** [finish_turn ?moderator ~available_tools ~now_ms ~history] applies the
    end-of-turn safe point and returns surfaced runtime requests. *)
val finish_turn
  :  moderator:moderator option
  -> available_tools:Res.Request.Tool.t list
  -> now_ms:int
  -> history:Res.Item.t list
  -> (Moderation.Runtime_request.t list, string) result

(** [moderate_tool_call ...] applies pre-tool moderation and returns the
    effective tool invocation together with any surfaced runtime requests. *)
val moderate_tool_call
  :  moderator:moderator option
  -> available_tools:Res.Request.Tool.t list
  -> now_ms:int
  -> history:Res.Item.t list
  -> kind:Tool_call.Kind.t
  -> name:string
  -> payload:string
  -> call_id:string
  -> item_id:string option
  -> (moderated_tool_call, string) result

(** [handle_tool_result ...] applies the post-tool safe point for [item] and
    returns surfaced runtime requests. *)
val handle_tool_result
  :  moderator:moderator option
  -> available_tools:Res.Request.Tool.t list
  -> now_ms:int
  -> history:Res.Item.t list
  -> name:string
  -> kind:Tool_call.Kind.t
  -> item:Res.Item.t
  -> (Moderation.Runtime_request.t list, string) result
