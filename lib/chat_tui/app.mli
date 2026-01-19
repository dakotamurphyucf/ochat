(** Terminal chat application – event-loop, streaming, export, and persistence.

     {!Chat_tui.App} is the orchestration layer that powers the Ochat terminal
     UI.

     It wires together:
     {ul
     {- {!Chat_tui.Model} for mutable UI state}
     {- {!Chat_tui.Controller} for key handling}
     {- {!Chat_tui.Renderer} for full-screen rendering}
     {- {!Notty_eio.Term} for terminal IO}
     {- {!Chat_response.Driver} for OpenAI streaming and tool execution}
     {- {!Context_compaction.Compactor} for user-triggered history compaction}}

     Use {!run_chat} to boot the UI and block until the user quits.

     Most callers should treat everything other than {!run_chat} as
     test-support: these helpers are exposed to enable white-box unit and
     integration tests of the event-loop and streaming behaviour.

     @canonical Chat_tui.App *)

open! Core

(** Runtime artefacts derived from the static chat prompt. *)
type prompt_context =
  { cfg : Chat_response.Config.t (** Behavioural settings (temperature, …) *)
  ; tools : Openai.Responses.Request.Tool.t list
    (** Tools exposed to the assistant at runtime. *)
  ; tool_tbl : (string, string -> Openai.Responses.Tool_output.Output.t) Base.Hashtbl.t
    (** Mapping [tool_name -> implementation]. *)
  }

(** Persistence policy to use when the UI terminates.

    The value controls whether a {!Session.t} snapshot derived from the
    final {!Model.t} is written back to disk at the end of {!run_chat}.  The
    policy is ignored when no [session] was supplied. *)
type persist_mode =
  [ `Always
  | `Never
  | `Ask
  ]

(** [add_placeholder_thinking_message m] appends a transient
    "(thinking…)" assistant message to [m].  It is removed as soon as the
    first streaming token arrives. *)
val add_placeholder_thinking_message : Model.t -> unit

(** [add_placeholder_stream_error model msg] appends a transient error message
    to [model] so failures during streaming surface in the transcript instead
    of being silently logged. *)
val add_placeholder_stream_error : Model.t -> string -> unit

(** [add_placeholder_compact_message model] appends a transient "(compacting…)"
    assistant message to [model] while context compaction is running. *)
val add_placeholder_compact_message : Model.t -> unit

(** [persist_snapshot env session model] persists the in-memory [model] back
    into [session] and writes it to disk.

    The helper copies the canonical history, task list and key/value store
    from [model] into the supplied {!Session.t} and then delegates to
    {!Session_store.save}.  It is a no-op when [session] is [None]. *)
val persist_snapshot : Eio_unix.Stdenv.base -> Session.t option -> Model.t -> unit

(** Immediate (synchronous) UI updates that happen right after the user
    submits the draft but {b before} the OpenAI request is sent.

    The helper:
    {ul
    {- snapshots the current draft buffer, converts it into a user history item
       (plain text or raw-XML, depending on [Model.draft_mode]);}
    {- appends a renderable user message to {!Model.messages} and updates
       {!Model.history_items};}
    {- clears {!Model.input_line}, resets the caret and enables
       {!Model.auto_follow};}
    {- scrolls the viewport so the freshly submitted message is visible;}
    {- injects a transient "(thinking…)" assistant placeholder; and}
    {- pushes a [`Redraw] request on [ev_stream].}}

    Actual network IO and streaming are delegated to {!handle_submit}.

    This function performs no network IO.
*)
val apply_local_submit_effects
  :  dir:Eio.Fs.dir_ty Eio.Path.t
  -> env:Eio_unix.Stdenv.base
  -> cache:Chat_response.Cache.t
  -> model:Model.t
  -> ev_stream:
       ([> `Redraw
        | `Stream of Openai.Responses.Response_stream.t
        | `Stream_batch of Openai.Responses.Response_stream.t list
        | `Replace_history of Openai.Responses.Item.t list
        | `Function_output of Openai.Responses.Function_call_output.t
        | `Tool_output of Openai.Responses.Item.t
        ]
        as
        'ev)
         Eio.Stream.t
  -> term:Notty_eio.Term.t
  -> unit

(** Launch an {b asynchronous} OpenAI completion request and stream the
    results back into the UI.

    A fresh {!Eio.Switch.t} is created so the request can be cancelled
    independently via \[Esc\].  All network IO happens in spawned fibres –
    the call therefore returns {i immediately} and never blocks the event
    loop.

    {2 Batching strategy}

    Individual token events arrive at sub-millisecond latency which is far
    too fast for human eyes.  To avoid wasting CPU cycles (and battery!)
    contiguous [`Stream] events are coalesced into a single
    [`Stream_batch] if they arrive within a small time-window (12 ms by
    default).  The window can be tweaked—primarily for benchmarking—via the
    environment variable [$OCHAT_STREAM_BATCH_MS] (valid range 1–50 ms).

    Tool output items are *not* batched because they are rare and often
    trigger further side-effects.

    {2 Parameters}

    @param env  The current {!Eio.Stdenv.t} (typically [Eio_unix.Stdenv.base]).
    @param model Mutable state record that will be updated while the stream
           progresses.
    @param ev_stream  Event queue shared with the main UI loop.  Both
           [`Stream] and [`Stream_batch] events are emitted here, as well as
           [`Tool_output] values.
    @param system_event  Out-of-band messages (e.g. notes from the user)
           that should appear in the assistant’s context but must not be
           rendered in the viewport.
    @param prompt_ctx  Runtime artefacts (model temperature, tool
           declarations, …) derived from the static prompt.
    @param datadir  Directory used to store temporary artefacts such as the
           response cache and tool outputs.
    @param parallel_tool_calls  When [true] (default) tool invocations are
           evaluated concurrently; otherwise they run sequentially.
    @param history_compaction  Forwarded to
           {!Chat_response.Driver.run_completion_stream_in_memory_v1} to
           enable its lightweight history-compaction pipeline (collapsing
           redundant file-read entries).
 *)
val handle_submit
  :  env:Eio_unix.Stdenv.base
  -> model:Model.t
  -> ev_stream:
       ([> `Redraw
        | `Stream of Openai.Responses.Response_stream.t
        | `Stream_batch of Openai.Responses.Response_stream.t list
        | `Replace_history of Openai.Responses.Item.t list
        | `Function_output of Openai.Responses.Function_call_output.t
        | `Tool_output of Openai.Responses.Item.t
        ]
        as
        'ev)
         Eio.Stream.t
  -> system_event:string Eio.Stream.t
  -> prompt_ctx:prompt_context
  -> datadir:Eio.Fs.dir_ty Eio.Path.t
  -> parallel_tool_calls:bool
  -> history_compaction:bool
  -> unit

(** Boot the TUI and block until the user terminates the program.

    Calling [run_chat ~env ~prompt_file ()] is the primary way to start an
    interactive Ochat session from an executable.  The function initialises
    a full-screen {!Notty_eio.Term}, parses the ChatMarkdown prompt, builds
    an initial {!Model.t} and then runs the main event-loop until the user
    quits.

    On shutdown the helper can:
    {ul
    {- optionally export the full conversation as ChatMarkdown (either
       automatically or after a [y/N] prompt, depending on how the user
       exited the UI and the value of [?export_file]);}
    {- optionally persist the session snapshot according to
       [?persist_mode].}}

    Parameters:
    {ul
    {- [env] – The standard environment supplied by {!Eio_main.run}.}
    {- [prompt_file] – Path to the [*.chatmd*] prompt that seeds the
       conversation, declares tools and configures default model settings.}
    {- [session] – Optional persisted session that should be resumed.
       When present, its history, tasks and key/value store take precedence
       over the defaults from [prompt_file].}
    {- [export_file] – Optional override for the ChatMarkdown export
       path.  When omitted the prompt file path is reused.}
    {- [persist_mode] – Policy controlling whether the session snapshot
       is written back on exit.  Defaults to [`Ask].}
    {- [parallel_tool_calls] – Allow multiple tool calls to run in parallel
       (default: [true]).}}
 *)
val run_chat
  :  env:Eio_unix.Stdenv.base
  -> prompt_file:string
  -> ?session:Session.t
  -> ?export_file:string
  -> ?persist_mode:persist_mode
  -> ?parallel_tool_calls:bool
  -> unit
  -> unit
