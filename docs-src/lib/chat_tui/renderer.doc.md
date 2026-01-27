# Renderer — rendering the Chat TUI to a Notty image

`Chat_tui.Renderer` is the terminal UI “view” layer. It takes the current
mutable [`Model.t`](model.doc.md) plus the terminal size and produces:

- a composite [`Notty.I.t`](https://github.com/pqwy/notty) image for the whole screen, and
- an absolute cursor position for the input box.

The renderer is pure with respect to the outside world (no I/O), but it
*intentionally mutates* a few cache fields inside `Model.t` (per-message image
caches, cached message heights/prefix sums, and the embedded
`Notty_scroll_box.t`) to make subsequent frames cheaper.

Internally, `Chat_tui.Renderer` is a thin façade over a small page/component
framework:

- `Renderer_pages` dispatches based on `Model.active_page`.
- `Renderer_page_chat` implements the current chat page.
- `Renderer_component_*` modules provide reusable parts (history viewport,
  message rendering, status bar, input box).

## Screen layout

Top-to-bottom, the renderer produces:

1. A scrollable, virtualised history viewport (rendered through
   `Notty_scroll_box`).
2. Optionally, a one-row sticky header showing the role of the first fully
   visible message.
3. A one-line status bar (`-- INSERT --`, `-- NORMAL --`, `-- CMD --` plus
   draft-mode hints).
4. A framed, multi-line input box (with selection highlighting when active).

## Message rendering rules

Within the history viewport:

- Each non-empty message is rendered as:
  - a blank line,
  - a header row containing an icon plus the capitalised role label, then
  - another blank line,
  - the body (markdown-aware), and
  - a trailing blank spacer line.
- Message bodies are sanitised with `Chat_tui.Util.sanitize ~strip:false` so
  that `Notty.I.string` does not see control characters.
- Fenced code blocks (``` or ~~~) are detected via
  `Chat_tui.Markdown_fences.split` and highlighted via
  `Chat_tui.Highlight_tm_engine` with the shared registry from
  `Chat_tui.Highlight_registry`.
- Tool output can be rendered specially when the message index is present in
  `Model.tool_output_by_index` (see `Chat_tui.Types.tool_output_kind`).

### Tool output special cases

The renderer currently has dedicated layouts for some built-in tools:

- `Apply_patch`: splits the output into a “status preamble” and a patch
  section, and highlights the patch using the internal `ochat-apply-patch`
  grammar.
- `Read_file { path }`: may infer a language from `path`. For Markdown files,
  the renderer uses the standard Markdown pipeline (including fenced-block
  splitting) so embedded code blocks can be highlighted by their info strings.
- `Read_directory`: applies a different tint to help distinguish directory
  listings from regular prose.

## Public API

Only two identifiers are exported from the library interface.

### `render_full`

```ocaml
val render_full
  :  size:int * int
  -> model:Chat_tui.Model.t
  -> Notty.I.t * (int * int)
```

`render_full ~size ~model` builds the full screen image and returns
`(image, (cx, cy))`, where `(cx, cy)` is the caret position inside the input
box in absolute screen coordinates.

The renderer updates caches inside `model` as part of this call.

Example integration with `Notty_eio`:

```ocaml
let render term model =
  let w, h = Notty_eio.Term.size term in
  let image, (cx, cy) = Chat_tui.Renderer.render_full ~size:(w, h) ~model in
  Notty_eio.Term.image term image;
  Notty_eio.Term.cursor term (Some (cx, cy))
```

### `lang_of_path`

```ocaml
val lang_of_path : string -> string option
```

`lang_of_path path` performs best-effort language inference for `read_file`
tool output. It inspects the last file extension (as defined by
`Core.Filename.split_extension`) and returns a TextMate-style language tag.

Example:

```ocaml
Chat_tui.Renderer.lang_of_path "foo.ml" = Some "ocaml";
Chat_tui.Renderer.lang_of_path "README.md" = Some "markdown";
Chat_tui.Renderer.lang_of_path "data.json" = Some "json";
Chat_tui.Renderer.lang_of_path "script.sh" = Some "bash";
Chat_tui.Renderer.lang_of_path "no_extension" = None
```

## Known limitations

1. Cursor positioning in the input box is derived from byte offsets in
   `Model.cursor_pos` (or `Model.cmdline_cursor` in command-line mode). Multi-byte
   UTF-8 and East-Asian wide glyphs can cause the visual caret to drift.
2. Notty’s geometry is based on `Uucp.Break.tty_width_hint` and can be wrong
   for some Unicode sequences (see Notty’s documentation on Unicode vs. text
   geometry).
3. If the input box grows taller than the available terminal height, the
   composed image can exceed the requested `size`. Backends typically crop the
   output, but the cursor may end up off-screen.

