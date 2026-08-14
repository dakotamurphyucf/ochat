open Core
open Eio.Std
module Res = Openai.Responses
module Res_stream = Res.Response_stream
module Res_item = Res.Item

type input_event = Notty.Unescape.event

type input_capability =
  | Disabled
  | Normal
  | Interaction of string
[@@deriving sexp_of, compare, equal]

type typeahead_done =
  { generation : int
  ; base_input : string
  ; base_cursor : int
  ; text : string
  }

type shell_grant_revoke_outcome =
  { grants : Session.Shell_state.Approval_grant.persisted list
  ; audit_sequence : int64 option
  ; error : string option
  }

type internal_event =
  [ `Resize
  | `Resize_settled of int
  | `Redraw
  | `Ui_frame_presented of int * input_capability
  | `Load_discovered_grammars of Highlight_grammar_discovery.Source.t list
  | `Startup_render_finished of int * Chat_startup_render.outcome
  | `Width_rendered of Chat_message_render_job.result
  | `Width_render_failed of Chat_message_render_job.t * exn
  | `Prepare_chat_destination of Controller_types.chat_destination
  | `Moderator_wakeup
  | `Moderator_input_response of string
  | `Shell_approval_changed
  | `Shell_management_refresh_requested of int
  | `Shell_management_loaded of
      int * string * (Shell_security_page_state.audit_page, string) result
  | `Shell_grant_revoke_requested of int * string
  | `Shell_grant_revoke_finished of int * string * shell_grant_revoke_outcome
  | `Moderator_startup_completed of
      (Chat_response.Moderation.Outcome.t list, string) result
  | `Moderator_overlay_changed of Chat_response.Moderation.Overlay_change.t
  | `Start_turn of App_runtime.turn_start_reason
  | `Streaming_started of int * Switch.t
  | `Stream of int * Res_stream.t
  | `Stream_batch of int * Res_stream.t list
  | `Sourced_stream of int * Chat_response.Sourced_response_event.t
  | `Sourced_stream_batch of int * Chat_response.Sourced_response_event.t list
  | `History_stream of int * Chat_response.History_stream_event.t
  | `History_stream_batch of int * Chat_response.History_stream_event.t list
  | `Tool_execution of int * Chat_response.Tool_execution_event.t
  | `Tool_output of int * History_entry.t
  | `Moderator_runtime_request of int * Chat_response.Moderation.Runtime_request.t
  | `Streaming_done of int * History_entry.t list
  | `Streaming_error of int * exn
  | `Typeahead_started of int * Switch.t
  | `Typeahead_done of int * typeahead_done
  | `Typeahead_error of int * exn
  | `Submit_requested of App_runtime.submit_request
  | `Compact_requested
  | `Compaction_started of int * Switch.t
  | `Compaction_done of int * History_entry.t list
  | `Compaction_error of int * exn
  ]
