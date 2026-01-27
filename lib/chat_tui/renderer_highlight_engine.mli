(** Shared TextMate highlight engine for the renderer.

    Rendering is single-threaded; the engine is cached so that future
    pages/components do not accidentally instantiate multiple highlight
    engines per render.

    The returned engine is configured with:

    {ul
    {- {!Chat_tui.Highlight_theme.github_dark} as the colour theme;}
    {- a shared registry from {!Chat_tui.Highlight_registry.get} that includes
       bundled grammars (e.g. markdown, JSON, and the internal
       ["ochat-apply-patch"] grammar).}}

    Callers should treat the engine as an immutable, shared resource and must
    not mutate its registry or theme.
*)
val get : unit -> Highlight_tm_engine.t
(** [get ()] returns the global highlight engine instance used by the renderer.

    The value is memoised; repeated calls return the same engine. *)
