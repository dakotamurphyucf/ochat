# `Renderer_component_input_box` — framed input prompt (and ':' prompt)

`Chat_tui.Renderer_component_input_box` renders the bottom prompt area of the UI:

- In Insert/Normal mode, it renders the multi-line `Model.input_line` buffer.
- In Cmdline mode, it renders the `Model.cmdline` buffer prefixed by `:`.

It also returns a cursor position relative to the returned image.

## API

```ocaml
val render
  :  width:int
  -> model:Chat_tui.Model.t
  -> Notty.I.t * (int * int)
```

Parameters:

- `width`: width in terminal cells (must be at least 2 to draw borders).
- `model`: provides the active buffer and cursor/selection state.

Returns `(img, (cx, cy))`, where `(cx, cy)` is the caret position **relative**
to `(img)` with `(0,0)` at the image’s top-left corner.

## Selection and cursor notes

- Selection highlighting is applied only in Insert/Normal mode (Cmdline ignores it).
- Cursor positioning is based on **byte offsets** (`Model.cursor_pos` or
  `Model.cmdline_cursor`), which can drift for multi-byte UTF-8 and wide glyphs.

