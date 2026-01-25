(** Local (synchronous) effects of a user submit, plus spawning the streaming worker.

    When the user hits enter, the UI applies immediate local updates
    (append the user message, clear the editor, show a "(thinking…)"
    placeholder, request a redraw) and then spawns the asynchronous OpenAI
    request.  This module owns those submit-specific steps.

    The helper is intentionally stateful: it mutates the supplied {!Model.t}
    and uses {!Chat_tui.App_runtime.t} to record that streaming is starting. *)

(** Captured editor state at the time of submission. *)
type request = App_runtime.submit_request

(** [capture_request ~model] snapshots the current editor buffer.

    @param model UI model supplying the current input buffer and draft mode.

    The returned value is used as an immutable submit payload so subsequent
    edits do not affect the request being processed.

    Example:
    {[
      let req = Chat_tui.App_submit.capture_request ~model in
      ignore (req : Chat_tui.App_submit.request)
    ]} *)
val capture_request : model:Model.t -> request

(** [clear_editor ~model] resets the draft buffer after a submit.

    This clears the input text, resets the cursor position to 0 and switches
    the draft mode back to {!Model.Plain}.

    @param model UI model to mutate in-place. *)
val clear_editor : model:Model.t -> unit

(** Signature of the streaming worker that will be spawned after local submit
    effects are applied.

    @param env Provides network and clock resources for the request.
    @param history OpenAI item history snapshot that seeds the request.
    @param internal_stream Receives streaming lifecycle and delta events.
    @param system_event Receives out-of-band notes that should enter the next
           request’s context but must not be rendered as user messages.
    @param datadir Directory used for response caches and tool artefacts.
    @param parallel_tool_calls Controls whether tool calls may run concurrently.
    @param history_compaction Forwards to the chat-response driver’s lightweight
           compaction pipeline.
    @param op_id Tags events so the reducer can ignore stale messages. *)
type handle_submit =
  env:Eio_unix.Stdenv.base
  -> history:Openai.Responses.Item.t list
  -> internal_stream:App_events.internal_event Eio.Stream.t
  -> system_event:string Eio.Stream.t
  -> datadir:Eio.Fs.dir_ty Eio.Path.t
  -> parallel_tool_calls:bool
  -> history_compaction:bool
  -> op_id:int
  -> unit

(** [start ... submit_request] applies local submit effects and then spawns the
    streaming worker fibre.

    The function:
    {ul
    {- moves the draft into the transcript (as plain text or raw XML); }
    {- clears the editor and scrolls to the bottom; }
    {- injects an assistant placeholder message; }
    {- marks the runtime as [Starting_streaming]; and }
    {- forks a fibre that runs [handle_submit] and reports results via
       [internal_stream].}}

    @param env Supplies filesystem and clock resources used during submission.
    @param ui_sw UI switch used to fork background fibres.
    @param cwd Directory used to resolve relative paths in raw-XML drafts.
    @param cache Shared chat-response cache used when converting raw-XML.
    @param datadir Per-run/per-session directory for cache and tool output.
    @param term Terminal used to compute viewport height for scrolling.
    @param runtime Updated to [Starting_streaming] and provides [runtime.model].
    @param internal_stream Receives streaming lifecycle events and redraw
           requests.
    @param system_event Queue used to enqueue notes when submitting during an
           active streaming operation.
    @param throttler Used to request a redraw after local effects are applied.
    @param handle_submit Asynchronous worker that performs the OpenAI request.
    @param parallel_tool_calls Forwarded to [handle_submit].
    @param submit_request Captured editor snapshot to submit.

    Example:
    {[
      let req = Chat_tui.App_submit.capture_request ~model in
      Chat_tui.App_submit.clear_editor ~model;
      Chat_tui.App_submit.start
        ~env
        ~ui_sw
        ~cwd
        ~cache
        ~datadir
        ~term
        ~runtime
        ~internal_stream
        ~system_event
        ~throttler
        ~handle_submit
        ~parallel_tool_calls:true
        req
    ]}
*)
val start
  :  env:Eio_unix.Stdenv.base
  -> ui_sw:Eio.Switch.t
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> cache:Chat_response.Cache.t
  -> datadir:Eio.Fs.dir_ty Eio.Path.t
  -> term:Notty_eio.Term.t
  -> runtime:App_runtime.t
  -> internal_stream:App_events.internal_event Eio.Stream.t
  -> system_event:string Eio.Stream.t
  -> throttler:Redraw_throttle.t
  -> handle_submit:handle_submit
  -> parallel_tool_calls:bool
  -> request
  -> unit
