# `Renderer_component_input_box` — framed input prompt (and ':' prompt)

`Chat_tui.Renderer_component_input_box` renders the bottom prompt area of the UI:

- In Insert/Normal mode, it renders the multi-line `Model.input_line` buffer.
- In Cmdline mode, it renders the `Model.cmdline` buffer prefixed by `:`.

It also returns a cursor position relative to the returned image.

## Type-ahead “ghost” completion

When a type-ahead completion exists and is relevant (see
`Model.typeahead_is_relevant`), the input box renderer draws a dim inline
“ghost” suffix:

- it is drawn at the cursor column on the cursor row only
- it does **not** affect the returned cursor position
- it is not part of selection highlighting
- multi-line completions do not change layout:
  - only the first line is rendered inline
  - remaining lines are represented as an indicator: `… (+N more lines)`

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

