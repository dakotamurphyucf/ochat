open Core

(** Event types used by {!Chat_tui.App}'s event loop.

    {!Chat_tui.App} uses two event queues:
    {ul
    {- [input_event] values coming from {!Notty_eio.Term.run}.}
    {- [internal_event] values produced by background fibres (streaming and
       compaction, type-ahead) and by the redraw scheduler.}}

    Streaming and compaction events are tagged with an operation id allocated
    by {!Chat_tui.App_runtime.alloc_op_id}.  The reducer discards events whose
    id does not match the currently active operation, which makes it safe for
    cancelled worker fibres to race with new work.

    Type-ahead uses {!Type_ahead_controller} events and checks attachment identity,
    context epoch, generation, draft and cursor at admission and completion. *)

(** Raw terminal events (keypresses, paste start/end, ...). *)
type input_event = Notty.Unescape.event

(** Input ownership advertised by a presented terminal frame. *)
type input_capability =
  | Disabled
  | Normal
  | Interaction of string
[@@deriving sexp_of, compare, equal]

type shell_grant_revoke_outcome =
  { grants : Session.Shell_state.Approval_grant.persisted list
  ; audit_sequence : int64 option
  ; error : string option
  }

(** Internal events emitted by helper fibres and schedulers.

    The reducer treats the following events as operation lifecycle messages
    scoped by the tagged id:
    {ul
    {- [`Streaming_*] – assistant streaming request lifecycle.}
    {- [`Compaction_*] – history compaction lifecycle.}
    {- [`Typeahead] – type-ahead completion lifecycle.}}
*)
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
  | `Streaming_started of int * Eio.Switch.t
  | `Stream of int * Openai.Responses.Response_stream.t
  | `Stream_batch of int * Openai.Responses.Response_stream.t list
  | `Sourced_stream of int * Chat_response.Sourced_response_event.t
  | `Sourced_stream_batch of int * Chat_response.Sourced_response_event.t list
  | `History_stream of int * Chat_response.History_stream_event.t
  | `History_stream_batch of int * Chat_response.History_stream_event.t list
  | `Tool_execution of int * Chat_response.Tool_execution_event.t
  | `Tool_output of int * History_entry.t
  | `Moderator_runtime_request of int * Chat_response.Moderation.Runtime_request.t
  | `Streaming_done of int * History_entry.t list
  | `Streaming_error of int * exn
  | `Typeahead of Type_ahead_controller.event
  | `Submit_requested of App_runtime.submit_request
  | `Compact_requested
  | `Compaction_started of int * Eio.Switch.t
  | `Compaction_done of int * History_entry.t list
  | `Compaction_error of int * exn
  ]
