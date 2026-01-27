# Chat_tui.Renderer

Render the current `Chat_tui.Model.t` into colourful
[Notty](https://pqwy.github.io/notty/doc/notty/) images for display in a
terminal. The module implements the full-screen *view* of the terminal chat
UI: it reads the model and terminal size and returns a composite image plus
the cursor position.

State mutations (persistence, networking, input handling) live in other
modules such as `Chat_tui.Controller` and `Chat_tui.App`. The renderer is
pure with respect to the outside world, but it does maintain a few
**internal caches** stored on the model (see below).

---

## Pages and routing

`Chat_tui.Renderer.render_full` is the single public entry point. Internally
the renderer routes to a full-screen page based on `Model.active_page`.  At
the moment only the chat page exists, but the module structure is intended
to accommodate additional pages later.

## Layout

The chat page turns a `Model.t` into a three-part screen:

1. **History viewport (top)** — scrollable chat transcript, virtualised so
   that only potentially-visible messages are rendered. When there is enough
   vertical space, a *sticky header* row shows the role label ("Assistant",
   "User", …) for the first fully visible message and stays pinned while the
   viewport scrolls.
2. **Status bar (middle)** — one line summarising the editor mode and draft
   mode (e.g. `-- INSERT -- -- RAW --`).
3. **Input box (bottom)** — a framed, multi-line editor for the pending
   prompt or command line.

Roughly:

```text
┌──────────── history (scrollable, virtualised) ────────────┐
│ Assistant                                                 │  ← optional sticky header
│                                                           │
│  assistant: Hello, how can I help?                       │
│  user:      Could you explain…                           │
│  …                                                       │
├──────────────────── status bar ───────────────────────────┤
│ -- INSERT --                                              │
├──────────────────── input editor ─────────────────────────┤
│> The current multi-line prompt…                           │
└───────────────────────────────────────────────────────────┘
```

The history viewport is backed by `Notty_scroll_box.t` from this project; it
tracks a vertical scroll offset and renders a window of fixed height onto a
larger logical image.

---

## Text and code rendering

Message bodies are rendered in two stages:

1. **Block splitting** — `Chat_tui.Markdown_fences.split` partitions the
   message text into a sequence of blocks:
   - plain text paragraphs (`Text`), and
   - fenced code blocks (`Code { lang; code }`), delimited by three backticks
     or three tildes.
2. **Per-block rendering** — each block is turned into one or more Notty
   images, respecting the available width.

Key details:

- **Sanitisation** – before any rendering, the text is passed through
  `Chat_tui.Util.sanitize ~strip:false`. This guarantees that
  `Notty.I.string` never sees forbidden control characters while preserving
  newlines. (Notty rejects C0 controls in `I.string`.)

- **Headers and roles** – each message is preceded by a single *header line*
  showing an icon and the capitalised role label (e.g. `"Assistant"`,
  `"User"`, `"Tool"`). Colours are chosen by a small internal theme that
  maps role strings to `Notty.A.t` attributes. Message bodies themselves do
  **not** include an inline `"role: "` prefix, so copying code from the
  terminal yields clean snippets.

- **Developer messages** – for role `"developer"`, a leading
  `"developer:"` prefix inside the message body is stripped to avoid
  duplicated labels between the header and the text.

- **Paragraphs** – non-code blocks are treated as markdown when a TextMate
  grammar is available. The renderer uses `Chat_tui.Highlight_tm_engine` with
  the `Chat_tui.Highlight_theme.github_dark` palette and the shared registry
  from `Chat_tui.Highlight_registry` to obtain `(Notty.A.t * string)` spans
  per line. When highlighting falls back (no registry, unknown language, or
  tokenisation error), paragraphs are rendered as plain text with a small
  heuristic that recognises `"**bold**"` / `"__bold__"` runs and applies a
  bold attribute.

- **Code blocks** – fenced blocks (except `lang = "html"`, which is treated
  as plain text) are highlighted with `Highlight_tm_engine.highlight_text`
  using the block's language tag when available. Code is wrapped to the
  available width while preserving indentation and colouring.

- **Tool outputs** – messages classified as tool output via
  `Chat_tui.Types.tool_output_kind` receive specialised treatment:
  - `Apply_patch` responses are split into a status preamble and a fenced
    patch section highlighted using the internal `"ochat-apply-patch"`
    grammar.
  - `Read_file { path }` responses use `Chat_tui.Renderer.lang_of_path path`
    to infer a syntax-highlighting language when possible (for example,
    `.ml` and `.mli` map to `"ocaml"`, `.md` to `"markdown"`, `.json` to
    `"json"`, `.sh` to `"bash"`).
  - `Read_directory` responses are rendered as plain text but tinted with a
    directory-specific style to distinguish them from regular prose.

- **Wrapping** – both text and code are wrapped on *cell* boundaries using
  Notty's notion of width (`Notty.I.width (Notty.I.string attr s)`). This
  accounts for most combining characters and emoji; a few terminal/Unicode
  combinations may still disagree slightly.

- **Selection** – when a message is "selected" in the model
  (`Model.selected_msg`), its header and body are redrawn in reverse video.
  This is implemented by composing attributes with `Notty.A.st reverse`.

---

## Scrolling, caching, and virtualisation

Rendering the entire transcript on each frame would be too slow once a
session grows. `Chat_tui.Renderer` therefore uses a combination of
virtualisation and caches stored on the model:

- **Per-message image cache** – the chat page state stores a cache mapping
  message indices to `Model.msg_img_cache` entries (see
  `Model.find_img_cache` / `Model.set_img_cache`). Each entry stores the
  unselected and selected images for a single message, together with the
  width they were rendered at and their heights.

- **Height prefix sums** – the chat page state also stores cached heights
  and prefix sums (accessible via `Model.msg_heights` / `Model.height_prefix`).
  Given a scroll offset and viewport height, the renderer performs two
  binary searches over the prefix array to determine the index range of
  messages that can be visible.

- **Incremental maintenance** – when the underlying text of a message may
  have changed, other parts of the UI mark its index dirty via
  `Model.invalidate_img_cache_index`. The renderer consumes and clears the
  resulting dirty list via `Model.take_and_clear_dirty_height_indices`,
  recomputes heights for dirty entries, and updates the prefix array
  in-place.

- **Scroll box integration** – the chat page's history viewport is backed by
  a `Notty_scroll_box.t` stored in the chat page state (accessible via
  `Model.scroll_box`). Each call to `render_full` updates the scroll box
  content image to the most recent history view. If `Model.auto_follow model`
  is `true`, the scroll offset is snapped to the bottom; otherwise the
  existing offset (possibly adjusted by user input via
  `Notty_scroll_box.scroll_by` and friends) is respected and clamped to the
  valid range for the current viewport height.

- **Sticky headers** – when there is at least one free row above the scroll
  viewport, the renderer duplicates the header line of the first fully
  visible message and draws it just above the viewport. If that header would
  overlap the natural header position (very small viewports), the sticky
  header is suppressed.

These caches are internal to the view layer; user code should treat them as
an implementation detail and use helpers from `Chat_tui.Model` to invalidate
them when mutating messages outside the normal update path.

---

## Public API

The module exposes two entry points:

### `render_full : size:int * int -> model:Chat_tui.Model.t -> Notty.I.t * (int * int)`

`render_full ~size ~model` builds the full-screen image and returns the
cursor position.

- `size` — `(width, height)` of the terminal in character cells
- `model` — current UI state (`messages`, `input_line`, editor mode, draft
  mode, selection, chat page `scroll_box`, and internal render caches)

The result is `(image, (cx, cy))` where:

- `image : Notty.I.t` is the composite screen (history, status bar, input),
  sized exactly to `size`.
- `(cx, cy)` are the absolute screen coordinates of the caret inside the
  input box, suitable for `Notty_eio.Term.cursor` or
  `Notty_unix.Term.cursor`. The origin `(0, 0)` is the top-left corner of the
  terminal.

  The cursor is derived from byte offsets in the active input buffer:
  `Model.cursor_pos` in Insert/Normal mode and `Model.cmdline_cursor` in
  Cmdline mode.

Behavioural notes:

- The history viewport only renders messages that can be visible for the
  current scroll position, plus transparent padding above and below so that
  its logical height matches the full transcript.
- Per-message render results are cached in the model, keyed by terminal
  width and message text. When the width changes (e.g. on terminal resize),
  caches and prefix arrays are rebuilt or incrementally adjusted.
- When `Model.auto_follow model` is `true`, the view automatically scrolls to
  the bottom after updating its content image. Otherwise the existing scroll
  offset in the model's `Notty_scroll_box.t` is preserved.

### `lang_of_path : string -> string option`

`lang_of_path path` performs best-effort language inference for `read_file`
tool outputs.

It inspects the file extension of `path` and returns a TextMate-style
language identifier when known. In particular:

- `.ml` and `.mli` map to `"ocaml"`
- `.md` maps to `"markdown"`
- `.json` maps to `"json"`
- `.sh` maps to `"bash"`

Paths without an extension, or with unrecognised extensions, yield `None`.
The function is exposed primarily for unit tests and to keep the
renderer-specific heuristic out of the higher-level model and controller
modules.

---

## Example: draw once with Notty_eio

The following example initialises a minimal model with a single assistant
message and draws it once using `Notty_eio`. Error handling and key events
are omitted for brevity.

```ocaml
open Core

let empty_model () : Chat_tui.Model.t =
  let msg_buffers : (string, Chat_tui.Types.msg_buffer) Base.Hashtbl.t =
    Base.Hashtbl.create (module String)
  in
  let fn_by_id : (string, string) Base.Hashtbl.t =
    Base.Hashtbl.create (module String)
  in
  let reasoning_by_id : (string, int ref) Base.Hashtbl.t =
    Base.Hashtbl.create (module String)
  in
  let tool_output_by_index
    : (int, Chat_tui.Types.tool_output_kind) Base.Hashtbl.t
    = Base.Hashtbl.create (module Int)
  in
  let kv_store : (string, string) Base.Hashtbl.t =
    Base.Hashtbl.create (module String)
  in
  let scroll_box = Notty_scroll_box.create Notty.I.empty in
  Chat_tui.Model.create
    ~history_items:[]
    ~messages:[ "assistant", "Welcome to ochat!" ]
    ~input_line:""
    ~auto_follow:true
    ~msg_buffers
    ~function_name_by_id:fn_by_id
    ~reasoning_idx_by_id:reasoning_by_id
    ~tool_output_by_index
    ~tasks:[]
    ~kv_store
    ~fetch_sw:None
    ~scroll_box
    ~cursor_pos:0
    ~selection_anchor:None
    ~mode:Chat_tui.Model.Insert
    ~draft_mode:Chat_tui.Model.Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0

let () =
  Eio_main.run @@ fun env ->
  let input = Eio.Stdenv.stdin env in
  let output = Eio.Stdenv.stdout env in
  let term = Notty_eio.Term.create ~input ~output () in
  let model = empty_model () in
  let size = Notty_eio.Term.size term in
  let image, (cx, cy) = Chat_tui.Renderer.render_full ~size ~model in
  Notty_eio.Term.image term image;
  Notty_eio.Term.cursor term (Some (cx, cy));
  (* Keep the frame on screen until the process is cancelled. *)
  Eio.Fiber.await_cancel ()
```

In a real application you would call `render_full` from inside a loop that
reacts to key and resize events, updates the model, and re-renders as
necessary.

---

## Known issues and limitations

- **Control characters** – Notty rejects C0 control characters and newlines in
  `I.string`. The renderer sanitises text once per cached render via
  `Util.sanitize ~strip:false`, but callers should still avoid embedding raw
  control characters in messages.

- **Unicode width** – wrapping relies on Notty's notion of cell width. Most
  modern terminals follow the same rules, but some wide or combining
  characters may still cause minor alignment differences.

- **Highlighting fallbacks** – when no TextMate grammar is available or
  tokenisation fails, both markdown paragraphs and code blocks fall back to
  uncoloured rendering (apart from the simple bold heuristic for markdown).

- **Caching semantics** – the renderer assumes that message text changes are
  accompanied by appropriate cache invalidation via helpers such as
  `Model.invalidate_img_cache_index` or `Model.clear_all_img_caches`. If you
  mutate `Model.messages` directly without doing so, the history view may
  temporarily show stale renders.

---

## Related modules

- `Chat_tui.Model` — definition of `Model.t` and helpers for manipulating the
  UI state and render caches.
- `Chat_tui.Types` — core chat types (`role`, `message`, `msg_buffer`,
  high-level `patch` commands).
- `Chat_tui.Highlight_tm_engine` / `Chat_tui.Highlight_theme` — TextMate-based
  syntax highlighting used for markdown and code.
- `Notty_scroll_box` — scrolling helper that backs the history
  viewport.
- `Notty`, `Notty_eio` — terminal drawing and IO used by the renderer.

