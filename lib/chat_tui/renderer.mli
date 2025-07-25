(** Terminal renderer – converts immutable chat {!Model} data into Notty
    images.

    The renderer is **pure**: every function in this module returns a value
    that depends only on its arguments – there are no hidden side–effects.
    State that belongs to the UI (scroll offsets, colour scheme, cursor
    position …) lives in {!Model}; this module merely *reads* that state and
    produces {!module:Notty} images.

    Design decisions
    {ul
    {-  The colour palette is centralised here so that it is easy to tweak
        the visual style in one place.}
    {-  Word-wrapping is delegated to {!Util.wrap_line}.  This keeps
        rendering free from terminal I/O – the logic can be unit-tested
        without spawning an actual TTY.}}

    @since 0.1.0 *)

open Types

(** [attr_of_role role] returns the display attribute used to render chat
    messages coming from [role].  Unknown roles map to {!Notty.A.empty}. *)

val attr_of_role : role -> Notty.A.t

(** [message_to_image ~max_width ?selected (role, txt)] returns a boxed image
    of [(role, txt)].

    Parameters
    {ul
    {-  [max_width] – total width available in terminal cells.  The rendered
        text is word-wrapped so that the outer frame never exceeds this
        width.  Must be ≥ 1.}
    {-  [?selected] – highlights the message by drawing it in reverse video
        (defaults to [false]).}}

    Invariants
    {ul
    {-  Blank or whitespace-only [txt] yields {!Notty.I.empty}.}
    {-  The result never exceeds [max_width] columns.}}

    The first line is prefixed with "[role]: ", subsequent wrapped lines are
    indented so that text aligns. *)
val message_to_image : max_width:int -> ?selected:bool -> message -> Notty.I.t

(** [history_image ~width ~messages ~selected_idx] vertically concatenates
    [messages] into a scrollable history view.

    {ul
    {-  [width] – terminal width used for each individual message.}
    {-  [selected_idx] – 0-based index of the highlighted message
        (`None` = no highlight).}}

    Every message is rendered with {!message_to_image}; the function merely
    stacks the resulting images with {!Notty.I.vcat}. *)
val history_image
  :  width:int
  -> messages:message list
  -> selected_idx:int option
  -> Notty.I.t

(** [render_full ~size:(w, h) ~model] computes the complete TUI.

    Returns an image representing the whole screen (history viewport,
    status-bar, and multi-line input box) **plus** the zero-based cursor
    position to be provided to {!Notty_eio.Term.cursor}.

    Notes
    {ul
    {-  [size] – available terminal area in *cells* (columns × rows).}
    {-  The function is side-effect-free except that it may mutate the
        internal offset of the {!Notty_scroll_box.t} stored in [model] in
        order to honour the *auto-follow* flag.  This is considered part of
        the model’s mutable state.}}

    Performance: the function allocates a fresh image every call; use an
    external diffing strategy if you need to minimise redraws. *)
val render_full : size:int * int -> model:Model.t -> Notty.I.t * (int * int)
