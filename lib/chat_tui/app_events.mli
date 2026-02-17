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

    Type-ahead completion uses the same pattern: a background worker publishes
    [`Typeahead_started] with its switch, then eventually reports
    [`Typeahead_done] / [`Typeahead_error].  The reducer additionally validates
    a completion against a snapshot (generation, base input, base cursor) so a
    stale suggestion cannot be applied after further editing. *)

(** Raw terminal events (keypresses, paste start/end, ...). *)
type input_event = Notty.Unescape.event

(** Payload emitted when a type-ahead request completes successfully.

    The fields form a snapshot of the editor at the time the request was
    started:
    {ul
    {- [generation] is {!Chat_tui.Model.typeahead_generation} when the request was
       launched.}
    {- [base_input] is the full draft buffer.}
    {- [base_cursor] is the cursor position (byte offset) within [base_input].}
    {- [text] is the suggested suffix to insert at [base_cursor].}}

    The reducer compares these fields to the current model state and applies
    the completion only when it is still applicable. *)
type typeahead_done =
  { generation : int
  ; base_input : string
  ; base_cursor : int
  ; text : string
  }

(** Internal events emitted by helper fibres and schedulers.

    The reducer treats the following events as operation lifecycle messages
    scoped by the tagged id:
    {ul
    {- [`Streaming_*] – assistant streaming request lifecycle.}
    {- [`Compaction_*] – history compaction lifecycle.}
    {- [`Typeahead_*] – type-ahead completion lifecycle.}}
*)
type internal_event =
  [ `Resize
  | `Redraw
  | `Streaming_started of int * Eio.Switch.t
  | `Stream of int * Openai.Responses.Response_stream.t
  | `Stream_batch of int * Openai.Responses.Response_stream.t list
  | `Tool_output of int * Openai.Responses.Item.t
  | `Streaming_done of int * Openai.Responses.Item.t list
  | `Streaming_error of int * exn
  | `Typeahead_started of int * Eio.Switch.t
  | `Typeahead_done of int * typeahead_done
  | `Typeahead_error of int * exn
  | `Submit_requested of App_runtime.submit_request
  | `Compact_requested
  | `Compaction_started of int * Eio.Switch.t
  | `Compaction_done of int * Openai.Responses.Item.t list
  | `Compaction_error of int * exn
  ]
