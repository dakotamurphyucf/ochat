# `Renderer_page_chat` — the main chat page renderer

`Chat_tui.Renderer_page_chat` composes the current chat page:

1. history viewport (`Renderer_component_history` + `Notty_scroll_box`),
2. optional sticky header,
3. status bar, and
4. input box.

It also ensures that model caches are reset when the terminal width changes.

## API

```ocaml
val render
  :  size:int * int
  -> model:Chat_tui.Model.t
  -> Notty.I.t * (int * int)
```

Returns `(img, (cx, cy))` where `(cx, cy)` is the absolute cursor position for
the input box.

