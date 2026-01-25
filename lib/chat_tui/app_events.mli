(** Event types used by {!Chat_tui.App}'s event loop.

    {!Chat_tui.App} uses two event queues:
    {ul
    {- [input_event] values coming from {!Notty_eio.Term.run}.}
    {- [internal_event] values produced by background fibres (streaming and
       compaction) and by the redraw scheduler.}}

    Streaming and compaction events are tagged with an operation id allocated
    by {!Chat_tui.App_runtime.alloc_op_id}.  The reducer discards events whose
    id does not match the currently active operation, which makes it safe for
    cancelled worker fibres to race with new work. *)

(** Raw terminal events (keypresses, paste start/end, ...). *)
type input_event = Notty.Unescape.event

(** Internal events emitted by helper fibres and schedulers. *)
type internal_event =
  [ `Resize
  | `Redraw
  | `Streaming_started of int * Eio.Switch.t
  | `Stream of int * Openai.Responses.Response_stream.t
  | `Stream_batch of int * Openai.Responses.Response_stream.t list
  | `Tool_output of int * Openai.Responses.Item.t
  | `Streaming_done of int * Openai.Responses.Item.t list
  | `Streaming_error of int * exn
  | `Submit_requested of App_runtime.submit_request
  | `Compact_requested
  | `Compaction_started of int * Eio.Switch.t
  | `Compaction_done of int * Openai.Responses.Item.t list
  | `Compaction_error of int * exn
  ]
