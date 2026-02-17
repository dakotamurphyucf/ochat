 # Renderer2 — full-screen TUI compositor (virtualised, cached)

 `chat_tui/renderer2` renders the entire terminal UI into a single
 [`Notty`](https://github.com/pqwy/notty) image. It virtualises the history,
 uses syntax highlighting for fenced code blocks, maintains a per-message
 render cache, and computes the absolute cursor position for the input box.

 It is the drop-in successor of [`renderer`](renderer.doc.md), sharing the same
 high-level responsibilities while improving performance and structure.

 ---

 ## Overview

 ```
 ┌───────────┐       ┌────────────────┐        ┌───────────────────┐
 │ Controller│ ----▶ │   Model (t)    │ ----▶  │  Renderer2 (Notty)│
 └───────────┘       └────────────────┘        └───────────────────┘
                                     (pure functions ⟶ Notty.I.t)
 ```

 Renderer2 assembles three regions into the final image:

 1. History viewport (virtualised and scrollable via `Notty_scroll_box`)
 2. Status bar (mode indicator and draft mode)
 3. Multi-line input box (borders, selection, and cursor)

 The history uses a prefix-sum of message heights and binary search to render
 only the visible slice. Messages are split into paragraphs and fenced code
 blocks (` ``` ` or ` ~~~ `) using `Chat_tui.Markdown_fences`, then highlighted
 with `Chat_tui.Highlight_tm_engine` if a language can be resolved.

 Text is sanitised with `Chat_tui.Util.sanitize` to remove control characters
 that `Notty.I.string` rejects (Notty disallows C0/C1 controls and newlines in
 text segments).

 ---

 ## API

 ### `render_full : size:int * int -> model:Model.t -> Notty.I.t * (int * int)`

 Builds the full-screen image and returns the absolute cursor position.

 Parameters
 - `size` — `(width, height)` in terminal cells.
 - `model` — current UI state with message list, input buffer, selection,
   scroll box, and caches.

 Returns `(image, (cx, cy))` where:
 - `image` is the composite screen image (history, status bar, input box), and
 - `(cx, cy)` is the caret location inside the input box in screen coordinates
   suitable for `Notty_unix.Term.cursor` or `Notty_eio.Term.cursor`.

 Behaviour
 - If `Model.auto_follow model` is true the history view scrolls to the bottom
   whenever new content is rendered.
 - On width changes the renderer clears cached per-message images to ensure
   correct wrapping; otherwise it reuses cached Notty images and heights.

 Example – integration with Notty_eio:
 {[
   let render term model =
     let (w, h) = Notty_eio.Term.size term in
     let img, (cx, cy) = Chat_tui.Renderer2.render_full ~size:(w, h) ~model in
     Notty_eio.Term.image term img;
     Notty_eio.Term.cursor term (cx, cy)
   in
   ()
 ]}

 ---

 ## Rendering details

 - Roles and colours — The first line of each message is prefixed with
   `role ^ ": "` and coloured according to the role. Selection uses
   `Notty.A.st reverse` while preserving the role colour.
 - Paragraphs — A message is split on hard newlines into paragraphs. Each
   paragraph is wrapped to the available width (after the prefix) using
   display-cell width as measured by Notty.
 - Inline markdown delimiters — When rendering a markdown paragraph, the
   renderer suppresses the delimiter marker characters for:
   - bold (`**...**` and `__...__`)
   - italics (`*...*` and `_..._`)
   - inline code (backticks, including multi-backtick spans)

   The enclosed text remains styled (bold/italic/inline-code) and other
   punctuation remains visible (for example list bullets are unaffected).
 - Fenced code blocks — Detected by `Chat_tui.Markdown_fences.split` and
   rendered with `Chat_tui.Highlight_tm_engine`. A simple width-bucket cache
   keyed by (`role-class`, `lang`, `md5(lang^NUL^code)`, `width-bucket`) avoids
   re-highlighting on every frame. Selected code blocks are re-rendered without
   caching to apply reverse video.
 - Viewport virtualisation — The renderer stores message heights and their
   prefix sums in the model. Given the current scroll offset and viewport
   height it uses binary search to select the intersecting range of messages
   and renders only those plus transparent padding above/below so the total
   virtual height remains stable.
 - Status bar — Shows `-- INSERT --`, `-- NORMAL --` or `-- CMD --`. When the
   draft buffer is in raw-XML mode an additional `-- RAW --` marker is shown.
 - Input box — Drawn with a single-cell border. The content is the current
   prompt (or the `:` command-line when in `Cmdline` mode). Selections are
   highlighted with reverse video and can span multiple lines.

 ---

 ## Known issues and limitations

 - Notty control characters — `Notty.I.string` rejects control characters.
   Renderer2 sanitises message text with `Chat_tui.Util.sanitize`, but callers
   should avoid passing untrusted control characters to other Notty APIs.
 - Geometry heuristics — Notty’s display width is heuristic (see its docs on
   Unicode geometry). East-Asian wide glyphs and some emoji may cause off-by-1
   wrapping on certain terminals.
 - Grammar loading diagnostics — If a bundled TextMate grammar fails to load,
   the registry initialisation may print a diagnostic to stdout. This is most
   visible for optional grammars; markdown highlighting continues to work as
   long as the markdown grammar loads successfully.

 ---

 ## See also

 - [`Chat_tui.Model`](model.doc.md) — state and render caches used by the renderer
 - [`Chat_tui.Markdown_fences`](markdown_fences.doc.md) — fenced block detection
 - [`Chat_tui.Highlight_theme`](highlight_theme.doc.md) — mapping scopes to attributes
 - [`Chat_tui.Highlight_tm_loader`](highlight_tm_loader.doc.md) — loading TextMate grammars
 - [`ochat.notty_scroll_box`](../notty_scroll_box.doc.md) — local scrollable viewport
