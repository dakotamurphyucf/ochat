# Renderer — turning chat history into pixels

`chat_tui/renderer` is a **pure rendering layer** for the terminal user
interface.  It takes immutable snapshots of the application
[`Model`](model.doc.md) and converts them into [`Notty`](https://github.com/pqwy/notty)
images.  All concerns related to **layout, word-wrapping, colours and cursor
placement** live here; higher-level modules decide *what* to render, the
renderer decides *how*.

---

## High-level architecture

```
┌───────────┐       ┌────────────────┐        ┌─────────────┐
│ Controller│ ----▶ │   Model (t)    │ ----▶  │  Renderer   │
└───────────┘       └────────────────┘        └─────────────┘
                                    (pure functions ⟶ Notty.I.t)
```

The renderer never mutates `Model.t` (with the single, explicit exception of
adjusting the scroll-box offset when *auto-follow* is active).  For test
pyramids this means you can instantiate a dummy model and snapshot the
resulting `Notty.I.t` to ensure the visual output stays stable.

## Colour palette

| Role          | Attribute                                   |
|-------------- |---------------------------------------------|
| `assistant`   | `fg lightcyan`                              |
| `user`        | `fg yellow`                                 |
| `developer`   | `fg red`                                    |
| `tool`        | `fg lightmagenta`                           |
| `fork`        | `fg lightyellow`                            |
| `reasoning`   | `fg lightblue`                              |
| `system`      | `fg lightblack` (dim)                       |
| `tool_output` | `fg lightgreen`                             |
| `error`       | `fg red ++ st reverse` (red on black)       |

Values are created with `Notty.A.(...)` and centralised in
[`attr_of_role`](#attr_of_role).

---

## API

### `attr_of_role : role -> Notty.A.t`

Maps a chat role string ("assistant", "user", …) to a Notty attribute.  Unknown
roles fall back to `Notty.A.empty` so callers do not need special handling.

```ocaml
let cyan = Renderer.attr_of_role "assistant"    (* fg lightcyan *)
let none = Renderer.attr_of_role "unknown-role"  (* A.empty      *)
```

### `message_to_image : ~max_width:int -> ?selected:bool -> message -> Notty.I.t`

Creates a framed, word-wrapped image for a single message.

```ocaml
open Notty

let img =
  Renderer.message_to_image
    ~max_width:80
    ~selected:true
    ("assistant", "Hello, world! This text will be wrapped automatically.")

Notty_unix.output_image img; (* see examples/notty *)
```

Behaviour:

- Empty or whitespace-only text returns `I.empty` so that pipelines can filter
  out noise with `I.is_empty`.
- `selected:true` flips the colours via `Notty.A.st reverse`.

### `history_image : width:int -> messages:message list -> selected_idx:int option -> Notty.I.t`

High-level helper that pipes each element of `messages` through
`message_to_image` and stacks the results with `I.vcat`.  Useful for
presenting the entire chat log inside a `Notty_scroll_box`.

```ocaml
let history = Renderer.history_image ~width:80 ~messages ~selected_idx:None in
Notty_unix.output_image history
```

### `render_full : size:int * int -> model:Model.t -> Notty.I.t * (int * int)`

The main entry point.  Builds the complete screen (history viewport, mode
status bar and multi-line input box) and returns:

1. the `Notty.I.t` to blit, and
2. the 2-tuple cursor position `(x, y)` in *absolute* screen coordinates.

Example integration with `Notty_eio`:

```ocaml
let rec ui_loop term model =
  let (w, h) = Notty_eio.Term.size term in
  let img, cursor = Renderer.render_full ~size:(w, h) ~model in
  Notty_eio.Term.image term img;
  Notty_eio.Term.cursor term cursor;
  (* wait for events, mutate model, recurse … *)
```

---

## Implementation notes

* **Word-wrapping** – `Util.wrap_line` wraps by *byte* count.  Double-width
  glyphs therefore occupy a single counted unit; this can lead to lines that
  visually exceed `max_width` if the input contains a lot of CJK text.
* **Performance** – the renderer allocates fresh `I.t` values on each call.
  In practice this is not a bottleneck because Notty images are cheap, but a
  diffing strategy could reduce redraw bandwidth further.
* **Scroll-box side-effects** – to implement *auto-follow* the renderer calls
  `Notty_scroll_box.scroll_to_bottom`, which mutates the scroll-box inside the
  model.  This is the single intentional deviation from pure functional
  style.

---

## Known limitations

1. **CJK width** – as mentioned above, line width is measured in bytes, not
   display cells.  Full-width characters may spill over the right border.
2. **Emoji support** – Notty does not currently support colour emoji.  They
   are rendered as text fallback or black-and-white glyphs depending on the
   terminal.
3. **Horizontal resize artefacts** – when the terminal is shrunk quickly the
   scroll-box may momentarily display horizontal artefacts because Notty
   redraws happen asynchronously.  They disappear on the next full render.

---

## Unit testing

Because every function is deterministic you can write approval tests that
serialise `Notty.I.t` to ASCII art and compare the output against a reference
file.  See `tests/renderer_expect.ml` for an example (not part of the public
library).

