(** Virtualised history viewport renderer.

    This module renders the scrollable transcript area of the chat page.  It is
    “virtualised” in the sense that it only renders the messages that can
    intersect the current [height]-row viewport, while still returning an image
    whose {e logical} height matches the full transcript.  The caller can then
    feed the result to {!Notty_scroll_box.set_content} and let
    {!Notty_scroll_box.render} crop the visible window.

    The implementation relies on (and mutates) renderer caches stored in
    {!Chat_tui.Model.Chat_page_state.t}:

    {ul
    {- a per-message image cache keyed by message index;}
    {- cached per-message heights and their prefix sums used to translate
       scroll offsets into visible indices.}}

    The module does {b not} modify the scroll offset itself; it reads the
    current scroll position from {!Chat_tui.Model.scroll_box}. *)

(** [render ~model ~width ~height ~messages ~selected_idx ~render_message] renders
    the transcript into a single image.

    @param model Mutable model holding caches and the current scroll position.
    @param width Target width in terminal cells. The returned image is
           [hsnap]-ed to this width.
    @param height Height of the scroll viewport in terminal cells.
    @param messages Transcript to render (top-to-bottom).
    @param selected_idx Zero-based index of the selected message (Normal mode),
           or [None] when nothing is selected.
    @param render_message Callback that renders one message. It is expected to
           produce an image sized for [width] (typically by [hsnap]-ing).

    The function updates the model's cached message images and height arrays.
    When [selected_idx] is set, the selected variant of the corresponding
    message is computed lazily and cached.

    The returned image includes transparent padding above and below the visible
    block so that its logical height matches the full transcript height. *)
val render
  :  model:Model.t
  -> width:int
  -> height:int
  -> messages:Types.message list
  -> selected_idx:int option
  -> render_message:(idx:int -> selected:bool -> Types.message -> Notty.I.t)
  -> Notty.I.t

(** [top_visible_index ~model ~scroll_height ~messages] returns the index of the
    first message whose {e header} is sufficiently below the top of the visible
    scroll window.

    The chat page uses this to implement a one-row “sticky” header: while the
    header of the first fully visible message is scrolled off-screen, the
    sticky header repeats it. When the real header is still visible in the top
    couple of rows, the function returns [None] to avoid duplicating the label.

    @param model Source of the current scroll offset (and cached heights).
    @param scroll_height Height of the scroll viewport, in terminal cells.
    @param messages Transcript to analyse.

    Returns [None] when [messages] is empty or when the cache arrays are
    missing/stale. *)
val top_visible_index
  :  model:Model.t
  -> scroll_height:int
  -> messages:Types.message list
  -> int option
