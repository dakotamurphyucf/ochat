# `Renderer_component_history` — virtualised transcript viewport

`Chat_tui.Renderer_component_history` renders the *scrollable transcript area* of the
chat page. It is designed to work with `Notty_scroll_box`:

1. It builds a **logically tall** image representing the whole transcript.
2. The caller installs it via `Notty_scroll_box.set_content`.
3. The caller asks `Notty_scroll_box.render` for the visible `width × height`
   window.

To make this efficient, the component caches per-message renders and their heights
inside `Model.t` (under the chat page state).

## API

### `render`

```ocaml
val render
  :  model:Chat_tui.Model.t
  -> width:int
  -> height:int
  -> messages:Chat_tui.Types.message list
  -> selected_idx:int option
  -> render_message:
       (idx:int -> selected:bool -> Chat_tui.Types.message -> Notty.I.t)
  -> Notty.I.t
```

Renders the transcript as a single image that:

- is `width` cells wide,
- has logical height equal to the sum of cached per-message heights, and
- includes transparent padding above and below the visible range so that
  scrolling works correctly.

Parameters:

- `model`: provides the per-message image cache and height arrays; also provides
  the current scroll offset via `Model.scroll_box`.
- `width`: target width for the returned image (messages should be `hsnap`’d).
- `height`: viewport height (used to determine which messages intersect).
- `messages`: the transcript (top-to-bottom).
- `selected_idx`: which message index is selected in Normal mode (if any).
- `render_message`: callback for rendering a single message.

Notes:

- The component **does not change** the scroll offset; it only reads it and clamps
  it logically for range computations.
- Heights are (re)computed when caches are missing or when entries are marked dirty
  via `Model.invalidate_img_cache_index`.

### `top_visible_index`

```ocaml
val top_visible_index
  :  model:Chat_tui.Model.t
  -> scroll_height:int
  -> messages:Chat_tui.Types.message list
  -> int option
```

Returns the message index whose header should appear in the **sticky header**
row (or `None` if no sticky header should be shown).

The chat page uses this to avoid rendering a sticky header when the real message
header is still visible near the top of the scroll window.

## Example (chat page)

`Renderer_page_chat` wires the history component like this:

```ocaml
let history_img =
  Chat_tui.Renderer_component_history.render
    ~model
    ~width:w
    ~height:scroll_height
    ~messages
    ~selected_idx:(Chat_tui.Model.selected_msg model)
    ~render_message
in
Notty_scroll_box.set_content (Chat_tui.Model.scroll_box model) history_img;
```

