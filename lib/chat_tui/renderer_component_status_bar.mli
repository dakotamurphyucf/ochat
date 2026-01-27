(** One-line mode / status bar renderer.

    The status bar shows the current editor mode (Insert/Normal/Cmdline) and a
    draft-mode hint (Plain vs Raw XML) for the input buffer.

    The returned image is exactly one row tall and is intended to sit between
    the history viewport and the input box. *)

(** [render ~width ~model] renders the status bar.

    @param width Target width in terminal cells. The returned image is padded to
           this width.
    @param model Source of mode and draft-mode flags.
*)
val render : width:int -> model:Model.t -> Notty.I.t
