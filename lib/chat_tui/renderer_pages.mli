(** Page router for the TUI renderer.

    The renderer is structured as a small “page” framework so that future
    full-screen pages can reuse common components (input box, status bar,
    highlighting engine) while providing page-specific layouts.

    Currently only {!Chat_tui.Model.Page_id.Chat} exists. *)

(** [page_of_model model] returns the active page to render.

    This is a small helper that exists primarily to keep the dispatch logic in
    {!render} readable. *)
val page_of_model : Model.t -> Model.Page_id.t

(** [render ~size ~model] dispatches to the renderer for the active page.

    @param size Terminal size [(width, height)] in cells.
    @param model Current UI state and caches.

    Returns [(img, (cx, cy))] where [(cx, cy)] is the absolute cursor position
    for the input box. *)
val render : size:int * int -> model:Model.t -> Notty.I.t * (int * int)
