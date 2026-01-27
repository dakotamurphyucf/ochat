(** Input box renderer.

    The returned cursor is relative to the top-left corner of the returned
    image and can be translated to absolute screen coordinates by the
    caller.

    The component renders either:

    {ul
    {- the multi-line insert buffer from {!Model.input_line} (Insert/Normal); or}
    {- the single-line ':' prompt buffer from {!Model.cmdline} (Cmdline).}}

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
