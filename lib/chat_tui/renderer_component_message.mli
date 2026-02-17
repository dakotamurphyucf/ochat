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
    @param text Raw message body text.
    @param hi_engine Shared TextMate highlight engine (see {!Renderer_highlight_engine.get}). *)
val render_message
  :  width:int
  -> selected:bool
  -> tool_output:Types.tool_output_kind option
  -> role:string
  -> text:string
  -> hi_engine:Highlight_tm_engine.t
  -> Notty.I.t

(** [render_header_line ~width ~selected ~role ~hi_engine] renders just the header
    row (icon + role label).

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
