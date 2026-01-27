(** Full-screen renderer for the terminal chat UI.

     [Chat_tui.Renderer] turns the current {!Chat_tui.Model.t} into a
     composite {!Notty.I.t} image plus a caret position for the input box.
     It acts as the "view" in the TUI architecture: it reads the model and
     terminal size and leaves input handling and state updates to
     {!Chat_tui.Controller} and {!Chat_tui.App}.

     {1 Pages and routing}

     {!render_full} is the single entry point. Internally, the renderer routes
     to a page-specific implementation via {!Chat_tui.Renderer_pages} based on
     {!Chat_tui.Model.active_page}.  Today only the chat page exists, but the
     structure is intended to accommodate future full-screen pages.

     {1 Chat page layout}

     The chat page screen is laid out top-to-bottom as:

     {ul
     {- a virtualised, scrollable history viewport backed by a
        {!Notty_scroll_box.t};}
     {- optionally, a one-row sticky header (when there is enough vertical
        space) that repeats the role header of the first fully visible
        message;}
     {- a one-line mode / status bar;}
     {- a framed, multi-line input box.}}

     {1 State and caching}

     Rendering is pure with respect to the outside world (no I/O), but the
     renderer does mutate a few cache fields inside [model] as part of its
     contract (stored under the active page's state in {!Chat_tui.Model.pages}):

     {ul
     {- per-message render cache and cached height / prefix-sum arrays used
        for scroll virtualisation;}
     {- the chat page scroll box (see {!Chat_tui.Model.scroll_box}),
        updated to honour {!Chat_tui.Model.auto_follow} and to clamp the
        scroll offset to the valid range for the current viewport height.}}

     {1 Text handling and highlighting}

     {ul
     {- Message bodies are sanitised with {!Chat_tui.Util.sanitize}
        [~strip:false] so that [Notty.I.string] never sees control
        characters; newlines are preserved.}
     {- Fenced code blocks delimited by three backticks or three tildes are
        detected via {!Chat_tui.Markdown_fences.split}. Non-HTML blocks are
        highlighted with {!Chat_tui.Highlight_tm_engine} configured with the
        shared registry from {!Chat_tui.Highlight_registry}.}
     {- Messages classified as tool output via
        {!Chat_tui.Model.tool_output_by_index} and
        {!Chat_tui.Types.tool_output_kind} may be rendered with specialised
        layouts. For example:
        {ul
        {- [Apply_patch] output splits into a status preamble and a patch
           section highlighted using the internal ["ochat-apply-patch"]
           grammar.}
        {- [Read_file { path }] output may infer a syntax-highlighting
           language via {!lang_of_path}. Markdown files are rendered using
           the normal Markdown pipeline (including fence splitting) so that
           fenced code blocks can be highlighted by their own info strings.}
        {- [Read_directory] output is tinted to distinguish it from prose.}}}
     {- Non-code paragraphs are highlighted as markdown when a grammar is
        available. When markdown highlighting falls back to plain text,
        a small ["**bold**"] / ["__bold__"] heuristic is used to preserve
        emphasis in common cases.}}

     Each non-empty message is preceded by a header row that shows an icon
     and the capitalised role label (for example ["assistant"], ["user"],
     ["tool"]). The message body does not include an inline ["role:"]
     prefix so that copying terminal selections yields clean snippets.

     {1 Cursor position}

     The cursor coordinates returned by {!render_full} are absolute screen
     coordinates suitable for {!Notty_unix.Term.cursor} and
     {!Notty_eio.Term.cursor}.

     {b Limitation:} the caret position is derived from byte offsets in the
     input buffer. With multi-byte UTF-8 and East-Asian wide glyphs, the
     cursor may not line up with the displayed text.
 *)
val render_full : size:int * int -> model:Model.t -> Notty.I.t * (int * int)
(** [render_full ~size ~model] builds the full screen image and the cursor
    position.

    @param size Terminal size [(width, height)] in cells.
    @param model Current UI state (messages, input buffer, selection, scroll
           box, modes, and renderer caches).

    The result is [(image, (cx, cy))] where [image] is the composite screen
    and [(cx, cy)] is the caret position inside the input box in absolute
    cell coordinates with [(0, 0)] at the top-left corner of the terminal.

    [render_full] updates renderer caches inside [model] (message image
    cache, cached heights/prefix sums, and the embedded scroll box) to make
    subsequent renders cheaper.

    Behaviour
    {ul
    {- the history viewport renders only those messages that can become
       visible in a [height]-row window and pads with transparent rows
       above and below so that its logical height matches the full
       transcript;}
    {- per-message render results are cached in [model] keyed by terminal
       width and message text; when the width changes, the cache and
       prefix-sum arrays are rebuilt;}
    {- when [Chat_tui.Model.auto_follow model] is [true], the history view
       scrolls to the bottom after updating its content image; otherwise the
       existing scroll offset stored in the model's {!Notty_scroll_box.t} is
       respected.}}

    Example – integrate into a Notty event loop:
    {[
      let render term model =
        let w, h = Notty_eio.Term.size term in
        let image, (cx, cy) = Chat_tui.Renderer.render_full ~size:(w, h) ~model in
        Notty_eio.Term.image term image;
        Notty_eio.Term.cursor term (Some (cx, cy))
      in
      ()
    ]} *)

(** [lang_of_path path] performs best-effort language inference for
    [read_file] tool outputs.

    The helper inspects the file extension of [path] and returns a
    TextMate-style language identifier when known.  For example:
    {ul
    {- [".ml"] and [".mli"] map to ["ocaml"];}
    {- [".md"] maps to ["markdown"];}
    {- [".json"] maps to ["json"];}
    {- [".sh"] maps to ["bash"].}}

    Paths without an extension and unrecognised extensions yield [None].
    The function is exposed primarily for unit tests and debugging of the
    renderer's tool-output handling. *)
val lang_of_path : string -> string option
