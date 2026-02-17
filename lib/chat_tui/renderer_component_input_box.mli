(** Input box renderer.

    The returned cursor is relative to the top-left corner of the returned
    image and can be translated to absolute screen coordinates by the
    caller.

    The component renders either:

    {ul
    {- the multi-line insert buffer from {!Model.input_line} (Insert/Normal); or}
    {- the single-line ':' prompt buffer from {!Model.cmdline} (Cmdline).}}

    When a type-ahead completion exists and is relevant (see
    {!Model.typeahead_is_relevant}), the renderer additionally draws a "ghost"
    completion inline on the cursor line, starting at the cursor column. If the
    completion contains newlines, only the first line is rendered and the cursor
    line additionally shows a dim indicator of the form [“… (+N more lines)”].
    Ghost text is never part of the selection highlight and does not affect the
    returned cursor position.

    Completion text is sanitised with {!Chat_tui.Util.sanitize} [~strip:false]
    and rendered in line fragments so that no newline is ever passed to
    [Notty.I.string].

    Selection highlighting is applied only in Insert/Normal mode.  Cursor
    positioning is based on byte offsets and therefore may drift for multi-byte
    UTF-8 glyphs. *)

(** [render ~width ~model] renders the framed input box.

    @param width Width in terminal cells. Must be at least 2 so the border can
           be drawn.
    @param model Source of editor state (mode, input buffers, cursor/selection).

    Returns [(img, (cx, cy))] where [(cx, cy)] is the caret position relative to
    the returned image with [(0, 0)] in the top-left corner. In particular, the
    returned cursor already accounts for the border and prompt prefix. *)
val render : width:int -> model:Model.t -> Notty.I.t * (int * int)
