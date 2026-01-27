# `Renderer_highlight_engine` — shared TextMate highlight engine

Highlighting in the renderer is driven by `Chat_tui.Highlight_tm_engine`. To avoid
recreating and reloading TextMate resources on every frame, the chat renderer uses
a single shared engine instance.

## API

```ocaml
val get : unit -> Chat_tui.Highlight_tm_engine.t
```

`get ()` returns the memoised engine configured with:

- the `github_dark` theme (`Chat_tui.Highlight_theme.github_dark`), and
- a shared registry (`Chat_tui.Highlight_registry.get`).

This is safe because rendering is single-threaded in the current TUI.

