# `Chat_tui.Stream` — translate OpenAI stream events into UI patches

`Chat_tui.Stream` is the protocol “adapter” between the OpenAI Responses
stream (`Openai.Responses.Response_stream.t`) and the TUI’s internal update
language (`Types.patch`).

The module inspects incremental streaming events (text deltas, tool call
arguments, reasoning summaries, …) and returns declarative patch lists that
are later applied to `Model.t` by the app event loop.

## Table of contents

1. [High-level workflow](#workflow)
2. [Public API](#api)
3. [Pipeline notes (batching, coalescing, sanitisation)](#pipeline)
4. [Tool metadata tracking](#tool-metadata)
5. [Examples](#examples)

## High-level workflow <a id="workflow"></a>

```text
OpenAI HTTP stream
  └─► Response_stream.t events
        └─► Chat_tui.Stream.handle_event(s)
              └─► Types.patch list
                    └─► Model.apply_patches
```

In the TUI, these functions are typically called from
[`Chat_tui.App_stream_apply`](app_stream_apply.doc.md), not from user code.

## Public API <a id="api"></a>

### `handle_event`

`handle_event ~model ev` converts a single `Response_stream.t` event into a
list of patches.

The function covers the event variants needed by the TUI, including:

- `Output_text_delta` (assistant text)
- `Output_item_added` (new output message, reasoning block, tool call)
- `Reasoning_summary_text_delta`
- tool-call argument deltas and “done” markers

Unknown event variants are ignored and yield `[]`.

### `handle_events`

`handle_events ~model evs` is a convenience wrapper:

```ocaml
let patches = Chat_tui.Stream.handle_events ~model evs in
ignore (Model.apply_patches model patches)
```

### Tool output helpers

The OpenAI Responses API can represent tool output either as a dedicated
`Function_call_output.t` record, or as a history item (`Item.Function_call_output`
or `Item.Custom_tool_call_output`).

`Chat_tui.Stream` supports both:

- `handle_fn_out ~model out` for `Function_call_output.t`
- `handle_tool_out ~model item` for output items

## Pipeline notes <a id="pipeline"></a>

The stream handling pipeline is split across modules:

- `Chat_tui.Stream` produces patches, but does not apply them.
- `Chat_tui.App_stream_apply` applies patches and requests redraws.
- `Chat_tui.App_streaming` batches very frequent stream events into
  `Stream_batch` for efficiency.
- `Chat_tui.Renderer` performs sanitisation and wrapping during rendering.

This separation keeps the streaming-to-patch mapping concentrated in one
place while allowing the app to tune performance (batching/coalescing)
independently.

## Tool metadata tracking <a id="tool-metadata"></a>

For some tools, the TUI benefits from knowing *what* the tool was doing:

- `read_file` / `read_directory` — record the referenced path
- `apply_patch` — record that the output corresponds to a patch application

Because OpenAI can interleave tool call arguments and tool outputs (especially
when tool calls run in parallel), `Chat_tui.Stream` updates tool metadata both:

- when the tool call is announced (`Output_item_added`), and
- when the final arguments are received (“arguments_done” / “input_done”)

If a tool output arrived *before* metadata was available, the module updates
the already-rendered metadata and invalidates the relevant render cache entry
so highlighting can be applied promptly.

## Examples <a id="examples"></a>

Processing a single stream event:

```ocaml
let patches = Chat_tui.Stream.handle_event ~model ev in
ignore (Model.apply_patches model patches)
```

Processing a batch:

```ocaml
let patches = Chat_tui.Stream.handle_events ~model evs in
ignore (Model.apply_patches model patches)
```

Handling a tool output item:

```ocaml
let patches = Chat_tui.Stream.handle_tool_out ~model item in
ignore (Model.apply_patches model patches)
```

