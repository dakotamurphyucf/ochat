(** Apply streaming events to the UI model.

    The OpenAI Responses stream produces very fine-grained updates (token
    deltas, tool call argument fragments, tool outputs, ...).  The app reduces
    those events to {!Types.patch} lists via {!Chat_tui.Stream} and then applies
    the patches to {!Model.t}.

    This module centralises the "apply + request redraw" part so that the main
    reducer stays focused on control flow. *)

(** [apply_stream_event model throttler ev] applies a single streaming event.

    @param model UI model to mutate by applying patches derived from [ev].
    @param throttler Redraw throttle to notify after applying patches.
    @param ev Single OpenAI streaming event.

    Example:
    {[
      Chat_tui.App_stream_apply.apply_stream_event
        runtime throttler ~viewport_height ev
    ]} *)
val apply_stream_event
  :  App_runtime.t
  -> Redraw_throttle.t
  -> viewport_height:int
  -> Openai.Responses.Response_stream.t
  -> unit

(** [apply_stream_batch model throttler evs] applies a batch of streaming events.

    The helper coalesces adjacent {!Types.Append_text} patches targeting the
    same buffer to keep patch volume small without losing incremental updates.

    @param model UI model to mutate by applying patches derived from [evs].
    @param throttler Redraw throttle to notify after applying patches.
    @param evs Batch of OpenAI streaming events to apply.

    Example:
    {[
      Chat_tui.App_stream_apply.apply_stream_batch
        runtime throttler ~viewport_height evs
    ]} *)
val apply_stream_batch
  :  App_runtime.t
  -> Redraw_throttle.t
  -> viewport_height:int
  -> Openai.Responses.Response_stream.t list
  -> unit

val apply_sourced_stream_event
  :  App_runtime.t
  -> Redraw_throttle.t
  -> viewport_height:int
  -> Chat_response.Sourced_response_event.t
  -> unit

val apply_sourced_stream_batch
  :  App_runtime.t
  -> Redraw_throttle.t
  -> viewport_height:int
  -> Chat_response.Sourced_response_event.t list
  -> unit

(** [apply_history_stream_event runtime event] installs a completed root
    provider item under the driver-allocated ID in [event]. Sourced child
    events do not enter parent canonical history. *)
val apply_history_stream_event
  :  App_runtime.t
  -> Chat_response.History_stream_event.t
  -> unit

val apply_history_stream_batch
  :  App_runtime.t
  -> Chat_response.History_stream_event.t list
  -> unit

(** [apply_tool_output model throttler item] applies a tool output item and
    appends it to the history.

    @param model UI model to mutate by applying patches derived from [item] and
           appending [item] to the history.
    @param throttler Redraw throttle to notify after applying patches.
    @param item History item carrying tool output.

    Example:
    {[
      Chat_tui.App_stream_apply.apply_tool_output model throttler item
    ]} *)
val apply_tool_output : App_runtime.t -> Redraw_throttle.t -> History_entry.t -> unit

(** [replace_history runtime redraw_immediate items] replaces the model’s history
    and derived transcript messages. Compatible target-width preparation is
    reconciled by stable row identity and revision, then resumed; callers must
    cancel preparation first for an incompatible replacement.

    This is typically used when streaming finishes and the authoritative list
    of items (including tool outputs) is known.

    @param runtime UI runtime whose model should be updated using the shared
           moderated-or-canonical visible history projection.
    @param redraw_immediate Callback used to render immediately after replacing
           the history.
    @param items Full OpenAI item list that should become the new history.

    Example:
    {[
      Chat_tui.App_stream_apply.replace_history runtime redraw_immediate items
    ]} *)
val replace_history : App_runtime.t -> (unit -> unit) -> History_entry.t list -> unit
