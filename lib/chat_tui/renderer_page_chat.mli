(** Chat page renderer.

    This page is the primary full-screen view: a scrollable transcript, an
    optional sticky header, a status bar, and the input box.  The implementation
    composes the smaller renderer components:

    {ul
    {- {!Renderer_component_history} for the scrollable transcript;}
    {- {!Renderer_component_message} for message framing and highlighting;}
    {- {!Renderer_component_status_bar} for mode hints;}
    {- {!Renderer_component_input_box} for the prompt box.}} *)

(** [render ~size ~model] renders the chat page.

    @param size Terminal size [(width, height)] in cells.
    @param model Current UI state. The renderer updates caches in [model] (see
           {!Chat_tui.Renderer}).

    Type-ahead preview: when Insert mode is active and
    {!Chat_tui.Model.typeahead_preview_open} is true, the renderer overlays a
    preview popup inside the transcript region (using Notty's overlay operator)
    so the input box geometry and scroll-box state do not change.

    Returns [(img, (cx, cy))] where [(cx, cy)] is the absolute cursor position
    for the input box. *)
val render : size:int * int -> model:Model.t -> Notty.I.t * (int * int)
