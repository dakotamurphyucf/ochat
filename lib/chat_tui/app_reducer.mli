(** Main event loop for the terminal UI.

    {!Chat_tui.App_reducer.run} is the central "reducer" loop that consumes
    terminal input events and internal events (streaming, compaction, redraw),
    mutates the {!Model.t} and requests re-renders.

    The reducer enforces a simple concurrency policy:
    {ul
    {- At most one operation (streaming or compaction) is active at a time.}
    {- Additional submits/compactions are queued in FIFO order.}
    {- [Esc] cancels the active operation; when idle, it exits the UI.}}
*)

(** [run ...] runs the main event loop and blocks until the UI should stop.

    The reducer is single-threaded with respect to the UI model: it is the only
    place that should mutate [runtime.model].  Background fibres communicate by
    pushing {!App_events.internal_event} values into [internal_stream].

    @param env Provides stdin/stdout, filesystem access, and a clock.
    @param ui_sw UI switch used to fork background fibres (streaming, compaction,
           redraw throttle).
    @param cwd User working directory used for resolving relative paths in prompt
           parsing and tool execution.
    @param cache Shared chat-response cache used by raw-XML conversion.
    @param datadir Per-run/per-session directory for caches and tool outputs.
    @param session Active session (if any) used for persistence and compaction.
    @param term Active Notty terminal used by the controller and redraw.
    @param runtime Shared mutable runtime state holding [runtime.model].
    @param input_stream Terminal input events from {!Notty_eio.Term.run}.
    @param internal_stream Streaming/compaction lifecycle events and redraw
           requests.
    @param system_event Out-of-band notes to include in assistant context without
           rendering them as user-visible messages.
    @param throttler Coalesces frequent redraw requests into a target FPS.
    @param redraw_immediate Renders immediately (bypassing the throttle).
    @param redraw Renders the current model (typically invoked by the throttle).
    @param handle_submit Asynchronous streaming worker invoked on submit.
    @param parallel_tool_calls Controls whether tool calls may run concurrently
           during streaming.
    @param cancelled Exception used to cancel streaming (typically
           {!Chat_tui.App_streaming.Cancelled}).
    @return [true] iff the user quit via [Esc] while idle.

    Example (as used by {!Chat_tui.App.run_chat}):
    {[
      let quit_via_esc =
        Chat_tui.App_reducer.run
          ~env
          ~ui_sw
          ~cwd
          ~cache
          ~datadir
          ~session
          ~term
          ~runtime
          ~input_stream
          ~internal_stream
          ~system_event
          ~throttler
          ~redraw_immediate
          ~redraw
          ~handle_submit
          ~parallel_tool_calls:true
          ~cancelled:Chat_tui.App_streaming.Cancelled
          ()
      in
      ignore quit_via_esc
    ]} *)
val run
  :  env:Eio_unix.Stdenv.base
  -> ui_sw:Eio.Switch.t
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> cache:Chat_response.Cache.t
  -> datadir:Eio.Fs.dir_ty Eio.Path.t
  -> session:Session.t option
  -> term:Notty_eio.Term.t
  -> runtime:App_runtime.t
  -> input_stream:App_events.input_event Eio.Stream.t
  -> internal_stream:App_events.internal_event Eio.Stream.t
  -> system_event:string Eio.Stream.t
  -> throttler:Redraw_throttle.t
  -> redraw_immediate:(unit -> unit)
  -> redraw:(unit -> unit)
  -> handle_submit:App_submit.handle_submit
  -> parallel_tool_calls:bool
  -> cancelled:exn
  -> unit
  -> bool
