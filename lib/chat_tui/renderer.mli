(** Full-screen renderer for the terminal chat UI.

    Render the current {!Chat_tui.Model.t} into a composite {!Notty.I.t}
    image plus cursor position. This module acts as the "view" in a
    Model–View–Update style architecture; it reads the model and terminal
    size and leaves input handling and state updates to {!Chat_tui.Controller}
    and {!Chat_tui.App}.

    Layout
    {ul
    {- a virtualised, scrollable history viewport at the top backed by a
       {!Chat_tui.Notty_scroll_box.t}; when there is enough vertical space,
       a single sticky header row repeats the role label of the first fully
       visible message so that the speaker stays visible while scrolling;}
    {- a one-line mode / status bar in the middle;}
    {- a framed, multi-line input box at the bottom.}}

    Rendering is pure with respect to the outside world but does mutate a
    few cache fields inside [model] as part of its contract:

    {ul
    {- per-message render cache and height-prefix arrays used to avoid
       re-highlighting and re-wrapping unchanged messages;}
    {- the scroll box stored in [model], updated to honour
       {!Chat_tui.Model.auto_follow} and to clamp the scroll offset to the
       valid range for the current viewport height.}}

    Text handling and highlighting
    {ul
    {- message bodies are first sanitised with
       {!Chat_tui.Util.sanitize}[ ~strip:false] so that [Notty.I.string]
       never sees control characters (Notty rejects them); newlines are
       preserved;}
    {- fenced code blocks delimited by three backticks or three tildes are
       detected via {!Chat_tui.Markdown_fences.split}; non-HTML blocks are
       highlighted with {!Chat_tui.Highlight_tm_engine} configured with the
       {!Chat_tui.Highlight_theme.github_dark} palette and the shared
       registry from {!Chat_tui.Highlight_registry};}
    {- non-code paragraphs are highlighted as markdown when a grammar is
       available, otherwise rendered as plain text with a simple
       ["**bold**"]/["__bold__"] heuristic.}}

    Each message is preceded by a header line that centres the role label
    (e.g. ["assistant"], ["user"], ["tool"]) and tints it using a small
    colour palette; the body lines themselves do not carry an inline
    ["role:"] prefix so that copying code from the terminal yields clean
    snippets.

    Cursor position

    The cursor coordinates returned by {!render_full} are absolute screen
    coordinates suitable for {!Notty_unix.Term.cursor} and
    {!Notty_eio.Term.cursor}.
*)
val render_full : size:int * int -> model:Model.t -> Notty.I.t * (int * int)
(** [render_full ~size ~model] builds the full screen image and the cursor
    position.

    {ul
    {- [size] is [(width, height)] in terminal cells;}
    {- [model] is the current UI state (messages, input buffer, selection,
       scroll box, modes, etc.).}}

    The result is [(image, (cx, cy))] where [image] is the composite screen
    and [(cx, cy)] is the caret position inside the input box in absolute
    cell coordinates with [(0, 0)] at the top-left corner of the terminal.

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

(** Best-effort language inference used for [read_file] tool outputs.

    The helper inspects the file extension and returns a TextMate
    language identifier when known (for example ["ml"] → ["ocaml"],
    ["md"] → ["markdown"]).  Unrecognised or extension-less paths
    yield [None].  Exposed primarily for unit tests. *)
val lang_of_path : string -> string option

