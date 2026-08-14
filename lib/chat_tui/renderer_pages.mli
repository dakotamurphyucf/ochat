(** Page router for the TUI renderer.

    Chat and Agent pages own independent scroll state. Chat renders the
    canonical transcript and editor; Agent renders transient active tool calls
    without mutating hidden Chat state. *)

(** [page_of_model model] returns the active page to render.

    This is a small helper that exists primarily to keep the dispatch logic in
    {!render} readable. *)
val page_of_model : Model.t -> Model.Page_id.t

(** [render ~size ~model] dispatches to the renderer for the active page.

    @param size Terminal size [(width, height)] in cells.
    @param model Current UI state and caches.

    Returns [(img, cursor)]. On Chat, [cursor] is the absolute input cursor.
    The non-editable Agent page returns [(0, 0)]. *)
val render : size:int * int -> model:Model.t -> Notty.I.t * (int * int)
