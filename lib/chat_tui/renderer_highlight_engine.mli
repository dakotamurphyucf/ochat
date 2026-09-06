(** Shared TextMate highlight engine for the renderer.

    Rendering is single-threaded; the engine is cached for the current shared
    registry generation and replaced after grammar discovery publishes a new
    registry.

    The returned engine is configured with:

    {ul
    {- {!Chat_tui.Highlight_theme.github_dark} as the colour theme;}
    {- a shared registry from {!Chat_tui.Highlight_registry.get} that includes
       bundled grammars (e.g. markdown, JSON, and the internal
       ["ochat-apply-patch"] grammar).}}

    Callers should treat each returned engine as immutable and must not mutate
    its registry or theme.
*)
val get : unit -> Highlight_tm_engine.t
(** [get ()] returns the global highlight engine instance used by the renderer.

    The value is memoised until the shared registry generation changes. *)
