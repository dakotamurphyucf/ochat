(** Main event loop for the terminal UI.

    {!Chat_tui.App_reducer.run} is the central "reducer" loop that consumes
    terminal input events and internal events (streaming, compaction, redraw),
    mutates the {!Model.t} and requests re-renders.

    The reducer enforces a simple concurrency policy:
    {ul
    {- At most one operation (streaming or compaction) is active at a time.}
    {- Additional submits/compactions are queued in FIFO order.}
    {- [Esc] (outside Insert mode, or with modifiers) cancels the active
       operation; when idle, it exits the UI.}}

    Separately, the reducer also manages a background type-ahead completion
    worker (see {!Chat_tui.Type_ahead_provider}).  Type-ahead work is tracked
    by {!Chat_tui.App_runtime.typeahead_op} and is {b independent} of
    streaming/compaction:
    {ul
    {- it can run while a stream is in flight;}
    {- it is debounced after input edits;}
    {- results are applied only when they still match the editor snapshot
       (generation, base input, base cursor).}}
*)

(** [run ...] runs the main event loop and blocks until the UI should stop.

    The reducer is single-threaded with respect to the UI model: it is the only
    place that should mutate [runtime.model].  Background fibres communicate by
    pushing {!App_events.internal_event} values into the internal event stream.

    All inputs are bundled in {!Chat_tui.App_reducer.Context.t}.

    @return [true] iff the user quit via [Esc] while idle.

    Example (as used by {!Chat_tui.App.run_chat}):
    {[
      let quit_via_esc = Chat_tui.App_reducer.run ctx in
      ignore quit_via_esc
    ]} *)
module Context : sig
  type t =
    { runtime : App_runtime.t
    ; shared : App_context.Resources.t
    ; submit : App_submit.Context.t
    ; compaction : App_compaction.Context.t
    ; cancelled : exn
    }
end

val run : Context.t -> bool
