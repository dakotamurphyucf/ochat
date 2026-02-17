(** One-line mode / status bar renderer.

    The status bar shows the current editor mode (Insert/Normal/Cmdline) and a
    draft-mode hint (Plain vs Raw XML) for the input buffer.

    When a type-ahead completion exists and is relevant (see
    {!Chat_tui.Model.typeahead_is_relevant}), the renderer appends a fixed hint
    string describing the type-ahead key bindings. The status bar is always
    snapped to the requested width so the presence/absence of hints does not
    change the overall layout.

    The returned image is exactly one row tall and is intended to sit between
    the history viewport and the input box. *)

(** [render ~width ~model] renders the status bar.

    @param width Target width in terminal cells. The returned image is padded to
           this width.
    @param model Source of mode and draft-mode flags.
*)
val render : width:int -> model:Model.t -> Notty.I.t
