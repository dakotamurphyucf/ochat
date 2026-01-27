# `Renderer_component_message` — rendering and highlighting a single message

`Chat_tui.Renderer_component_message` is responsible for turning a single
`(role, text)` message into a Notty image. It applies:

- sanitisation (`Chat_tui.Util.sanitize ~strip:false`),
- message framing (blank line, header, blank line, body, gap),
- markdown/code-fence splitting (`Chat_tui.Markdown_fences.split`),
- TextMate-based highlighting (`Chat_tui.Highlight_tm_engine`),
- selection styling (reverse video), and
- special layouts for tool output when `tool_output` metadata is available.

## API

### `render_message`

```ocaml
val render_message
  :  width:int
  -> selected:bool
  -> tool_output:Chat_tui.Types.tool_output_kind option
  -> role:string
  -> text:string
  -> hi_engine:Chat_tui.Highlight_tm_engine.t
  -> Notty.I.t
```

Parameters:

- `width`: target width of the returned image.
- `selected`: whether to render with selection highlighting.
- `tool_output`: classification metadata (from `Model.tool_output_by_index`)
  used to enable special tool-output layouts (e.g. `Apply_patch`, `Read_file`).
- `role`: role label (e.g. `"assistant"`, `"user"`, `"tool_output"`).
- `text`: message body.
- `hi_engine`: shared highlight engine (see `Renderer_highlight_engine.get`).

### `render_header_line`

```ocaml
val render_header_line
  :  width:int
  -> selected:bool
  -> role:string
  -> hi_engine:Chat_tui.Highlight_tm_engine.t
  -> Notty.I.t
```

Renders just the header row (icon + capitalised role). This is used by the chat
page to draw the sticky header.

## Tool-output special cases

The `tool_output` flag changes how the body is rendered:

- `Apply_patch`: splits at the first blank line; prose above, then a patch block
  highlighted with the internal `ochat-apply-patch` grammar.
- `Read_file { path }`: if `Renderer_lang.lang_of_path path` returns a language
  (except markdown), the entire tool output is rendered as a highlighted code block.
- `Read_directory`: applies a distinct tint so directory listings stand out.

