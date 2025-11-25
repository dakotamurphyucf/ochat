(** Full-screen renderer for the terminal UI (history, status bar, input box).

    The module assembles a complete Notty image of the chat interface from an
    immutable snapshot of {!Chat_tui.Model.t}. The layout consists of:
    - a virtualised, scrollable history viewport at the top,
    - a one-line mode/status bar,
    - a multi-line input box with cursor and optional selection.

    Rendering is side-effect free with two exceptions that are part of the UI
    contract:
    - the model’s internal per-message render cache is maintained to avoid
      re-highlighting and re-wrapping unchanged messages, and
    - the scroll box inside the model is updated to implement auto-follow.

    Text handling and highlighting
    - Message text is sanitised via {!Chat_tui.Util.sanitize} to avoid control
      characters that {!Notty.I.string} rejects (see Notty docs on control
      characters). Newlines are preserved.
    - Fenced code blocks (three backticks or three tildes) are detected via
      {!Chat_tui.Markdown_fences.split} and rendered with
      {!Chat_tui.Highlight_tm_engine}, falling back to plain text when the
      language cannot be resolved.

    Cursor position
    - The returned cursor coordinates are absolute screen coordinates suitable
      for {!Notty_unix.Term.cursor} or {!Notty_eio.Term.cursor}.

    Performance characteristics
    - The renderer caches per-message images keyed by terminal width and
      message text. Heights are tracked and prefix-summed to render only the
      visible slice of the history.
*)
val render_full : size:int * int -> model:Model.t -> Notty.I.t * (int * int)
(** [render_full ~size ~model] builds the full screen image and the cursor
    position.

    - [size] is [(width, height)] in terminal cells.
    - [model] is the current UI state (messages, input line, selection,
      scroll box, mode, etc.).

    Returns [(image, (cx, cy))] where [image] is the composite screen and
    [(cx, cy)] is the caret position inside the input box in absolute cell
    coordinates, with [(0, 0)] at the top-left corner of the screen.

    Behaviour
    - When [Model.auto_follow model] is [true], the history view scrolls to
      the bottom automatically after updating the content image.
    - If the terminal width changed since the last call, cached per-message
      renders are invalidated to ensure correct wrapping.

    Example – integrate into a Notty event loop:
    {[
      let render term model =
        let (w, h) = Notty_eio.Term.size term in
        let img, (cx, cy) = Chat_tui.Renderer2.render_full ~size:(w, h) ~model in
        Notty_eio.Term.image term img;
        Notty_eio.Term.cursor term (cx, cy)
      in
      ()
    ]} *)
