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
      Chat_tui.App_stream_apply.apply_stream_event model throttler ev
    ]} *)
val apply_stream_event
  :  Model.t
  -> Redraw_throttle.t
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
      Chat_tui.App_stream_apply.apply_stream_batch model throttler evs
    ]} *)
val apply_stream_batch
  :  Model.t
  -> Redraw_throttle.t
  -> Openai.Responses.Response_stream.t list
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
val apply_tool_output : Model.t -> Redraw_throttle.t -> Openai.Responses.Item.t -> unit

(** [replace_history model redraw_immediate items] replaces the model’s history
    and derived transcript messages.

    This is typically used when streaming finishes and the authoritative list
    of items (including tool outputs) is known.

    @param model UI model to mutate by replacing its history and rebuilding
           derived fields such as [messages] and the tool-output index.
    @param redraw_immediate Callback used to render immediately after replacing
           the history.
    @param items Full OpenAI item list that should become the new history.

    Example:
    {[
      Chat_tui.App_stream_apply.replace_history model redraw_immediate items
    ]} *)
val replace_history : Model.t -> (unit -> unit) -> Openai.Responses.Item.t list -> unit
