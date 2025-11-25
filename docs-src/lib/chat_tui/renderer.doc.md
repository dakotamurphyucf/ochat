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

The renderer keeps a small per-message render cache inside the `Model.t` and
may adjust the scroll-box offset when *auto-follow* is active.  For test
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

### `is_toollike : role -> bool`

Returns `true` for roles that should be rendered as tool output. At the
moment this includes `"tool"` and `"tool_output"`. Tool-like roles render
their body as a single code block and try to infer a language (`bash`,
`diff`, `json`) when no explicit fenced language is present.

### `label_of_role : role -> string`

Returns the textual label used as the message prefix on the first line
(`role ^ ": "`). Currently this is the role unchanged.

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

* Empty or whitespace-only text returns `I.empty` so that pipelines can filter
  out noise with `I.is_empty`.
* `selected:true` flips the colours via `Notty.A.st reverse` while preserving
  the message's base hue.  This keeps the highlight consistent across roles.
* Soft-wrapping respects the `"role: "` prefix.  Hard newlines (`\n`) split
  the text into paragraphs that are wrapped independently.

### `history_image : model:Model.t -> width:int -> height:int -> messages:message list -> selected_idx:int option -> Notty.I.t`

Creates the *virtualised* chat history image that is fed into the internal
`Notty_scroll_box`.

Parameters

* `~model` – the mutable [`Model.t`](model.doc.md) that holds the scroll box
  and the per-message render cache.
* `~width` – current terminal width in cells.  Serves as the cache key.
* `~height` – height of the *viewport*.  Only messages that can become visible
  in the rectangle of `width × height` cells are rendered on each call.
* `~messages` – full chat transcript in chronological order.
* `~selected_idx` – index of the highlighted message, if any.

The function

1. updates/increments prefix sums of message heights in `model`,
2. figures out which slice of `messages` intersects with the viewport, and
3. concatenates only those images with `I.vcat`, padding with transparent
   space above and below so the total height stays intact.

```ocaml
let history =
  Renderer.history_image
    ~model ~width:80 ~height:25 ~messages ~selected_idx:None
in
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

* **Word-wrapping** – wrapping is performed by measuring the display-cell
  width of candidate rows via `Notty.I.string`/`Notty.I.width` and cutting
  before the next character would exceed the available columns. Hard newlines
  split the input into paragraphs that wrap independently. For background
  on width calculation and its limitations, see Notty’s documentation on
  Unicode geometry (Uucp.Break.tty_width_hint).
* **Input sanitisation** – invalid UTF-8 sequences are stripped **once, at
  render time**, instead of for every streaming delta. This avoids repeated
  sanitisation and re-wrapping of the same text while keeping on-screen
  formatting unchanged and reducing word-wrap work per frame.
* **Performance** – the renderer caches per-message `Notty.I.t` images and
  their heights in the model, keyed by terminal width and message text.  The
  selected variant is populated on demand; caches are invalidated on width
  changes or message updates.
* **Input selection** – starting with version 0.3 users can mark a range in
  the multi-line input buffer.  The renderer draws the highlighted span in
  reverse video (same foreground/background hue) while keeping the rest of
  the line untouched.  Because the highlight is recomputed every frame no
  additional cache entries are created.
* **Mutable state in the model** – to implement *auto-follow* the renderer
  calls `Notty_scroll_box.scroll_to_bottom`, and it maintains the render cache
  described above.  The functional API seen by callers remains unchanged.

---

## Known limitations

1. **Display width heuristics** – Notty approximates display width using
   Uucp.Break.tty_width_hint. Some terminals may render East-Asian wide
   glyphs or emoji differently, so wrapping can occasionally be off by a
   cell.
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

