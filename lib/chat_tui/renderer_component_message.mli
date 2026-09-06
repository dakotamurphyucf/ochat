(** [render_message ~width ~selected ~tool_output ~role ~text ~hi_engine] renders one
    transcript message to a Notty image.

    The returned image includes the standard message framing used by the chat
    page:

    {ul
    {- a blank spacer row;}
    {- a header row with an icon + capitalised role;}
    {- another blank row;}
    {- the message body (markdown-aware); and}
    {- a trailing gap row.}}

    Text is sanitised with {!Chat_tui.Util.sanitize} [~strip:false] so that
    Notty never sees control characters.

    Tool output special cases are enabled when [tool_output] is [Some _]. For
    example, [Apply_patch] output is split into a prose preamble and a patch
    section highlighted with the internal ["ochat-apply-patch"] grammar. A
    [Read_file { path }] output may be rendered as syntax-highlighted code when
    the file extension of [path] can be mapped via {!Chat_tui.Renderer.lang_of_path}.

    @param width Target width in terminal cells.
    @param selected Whether to render with selection highlighting (reverse video).
    @param tool_output Optional classification metadata used for specialised tool
           rendering.
    @param role Message role string (e.g. ["assistant"], ["user"], ["tool_output"]).
           A role ending in [" Agent"] uses Tool styling while preserving the
           supplied label.
    @param text Raw message body text.
    @param hi_engine Shared TextMate highlight engine (see {!Renderer_highlight_engine.get}). *)
val render_message
  :  width:int
  -> selected:bool
  -> tool_output:Types.tool_output_kind option
  -> role:string
  -> text:string
  -> hi_engine:Highlight_tm_engine.t
  -> ?search_query:string option
  -> ?tool_call_outcome:Ochat_function.Trace.outcome
  -> unit
  -> Notty.I.t

(** [render_detached ~runtime job] renders one immutable message snapshot
    without reading mutable TUI page state. The result repeats every identity
    field required for stale-work rejection.

    The supplied runtime owns highlighting and optional code-image caching.
    This function performs no model, scroll-box, app-stream, redraw, or
    terminal operations. *)
val render_detached
  :  runtime:Chat_message_render_job.Runtime.t
  -> Chat_message_render_job.t
  -> Chat_message_render_job.result

(** [render_synchronously ~hi_engine job] adapts the existing shared
    single-domain code cache to {!render_detached}. *)
val render_synchronously
  :  hi_engine:Highlight_tm_engine.t
  -> Chat_message_render_job.t
  -> Chat_message_render_job.result

(** [install_highlights bindings] installs immutable worker-produced
    width-independent spans in the synchronous renderer cache. *)
val install_highlights : Chat_message_render_job.Highlight_cache.binding list -> unit

(** [install_prepared ... prepared] installs one immutable worker-produced
    semantic representation in the synchronous renderer cache. *)
val install_prepared
  :  row_id:Projected_message.Id.t
  -> row_revision:int
  -> role:string
  -> text:string
  -> tool_output:Types.tool_output_kind option
  -> Chat_message_render_job.Prepared_message.t
  -> unit

(** [apply_overlay ~selected ~search_query layout] paints selection and search
    attributes over an immutable message layout without changing its geometry. *)
val apply_overlay
  :  selected:bool
  -> search_query:string option
  -> Chat_message_render_job.Layout.t
  -> Notty.I.t

(** [render_header_line ~width ~selected ~role ~hi_engine] renders just the
    header row through the same late-overlay path as transcript rows.

    The chat page uses this to implement the one-row sticky header at the top
    of the history viewport.

    @param width Target width in terminal cells.
    @param selected Whether to render with selection highlighting (reverse video).
    @param role Message role string.
    @param hi_engine Shared TextMate highlight engine. *)
val render_header_line
  :  width:int
  -> selected:bool
  -> role:string
  -> hi_engine:Highlight_tm_engine.t
  -> ?search_query:string option
  -> unit
  -> Notty.I.t

(** [should_drop_markdown_delimiter ~scopes ~text] returns [true] when a
    TextMate-tokenised segment represents only a delimiter run for bold/italic
    or inline code and should therefore be hidden from the rendered output.

    This is exposed primarily for unit tests and debugging of markdown
    marker suppression. *)
val should_drop_markdown_delimiter : scopes:string list -> text:string -> bool

(** [suppress_markdown_delimiters spans] filters out delimiter segments from a
    list of scope-preserving spans.

    This is exposed primarily for unit tests and debugging of markdown marker
    suppression. *)
val suppress_markdown_delimiters
  :  Highlight_tm_engine.scoped_span list
  -> Highlight_tm_engine.scoped_span list

(** [clear_code_cache ()] invalidates width-bucketed code images and
    width-independent highlighted spans. Call this after extending the shared
    grammar registry at runtime. *)
val clear_code_cache : unit -> unit
