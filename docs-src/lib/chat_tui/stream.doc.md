# `Chat_tui.Stream`

Translate raw OpenAI *stream* events into declarative TUI updates.

`Chat_tui.Stream` is the glue layer between the low-level
`ochat.Openai` bindings and the rest of the terminal user interface.  The
OpenAI ChatCompletions HTTP endpoint can be used in *streaming* mode where
every partial token, reasoning summary or tool invocation is sent as a
small JSON blob.  Handling those blobs directly throughout the code base
would clutter otherwise pure modules with protocol details — therefore the
responsibility is centralised here.

The module analyses every incoming
[`Openai.Responses.Response_stream.t`](https://bridge-between-odoc-and-web/)
value and emits one or more [`Types.patch`](../types.doc.md) commands.  A
patch is a tiny, *side-effect free* instruction that describes how the
model — and consequently the UI — needs to change.  The patches are later
applied by `Model.apply_patches`.  A small amount of book-keeping (the
`active_fork` flag and its associated index) is still performed directly on
the [`Model.t`](./model.doc.md) value, but the message history itself is
modified exclusively through patches.

---

## Table of contents

1. [High-level workflow](#high-level-workflow)
2. [Public API](#public-api)
   * [`handle_fn_out`](#val-handle_fn_out)
   * [`handle_event`](#val-handle_event)
   * [`handle_events`](#val-handle_events)
3. [Implementation / pipeline notes](#implementation--pipeline-notes)
4. [Examples](#examples)
5. [Current limitations](#current-limitations)

---

## High-level workflow

```text
                       ┌─ HTTP stream ─────────────────┐
                       ▼                               │
         OpenAI → Response_stream.t → Chat_tui.Stream → Types.patch list
                                                ▲      │
                                                │      │
                                                └───────┘
```

1. The **controller** starts a streaming ChatCompletions request.
2. Each JSON blob is decoded into a value of
   `Openai.Responses.Response_stream.t`.
3. The value is fed into `Chat_tui.Stream.handle_event` (or
   `handle_events`).
4. The resulting list of patches is fed into `Model.apply_patches` which
   mutates the *single* state record understood by the renderer.

---

## Public API

### `val handle_fn_out`

```ocaml
handle_fn_out
  ~model         (* current UI model – may be updated in fork book-keeping *)
  (out : Openai.Responses.Function_call_output.t)
  : Types.patch list
```

Converts the final result of a *function/tool call* into one
`Types.Set_function_output` patch.

Additional behaviour:

* If the function call belonged to the assistant’s **fork** mechanism the
  helper clears `Model.active_fork`/`Model.fork_start_index` so that
  subsequent deltas revert to the standard styling.

### `val handle_event`

```ocaml
handle_event ~model (ev : Openai.Responses.Response_stream.t)
```

Inspects a **single** streaming event and returns zero or more patches.  The
function currently knows about the following event classes:

| Event variant | Emitted patches |
| ------------- | -------------- |
| `Output_text_delta` | `Ensure_buffer` · `Append_text` |
| `Output_item_added` | `Ensure_buffer` · `Set_function_name` · *(optional)* book-keeping |
| `Output_message` | `Ensure_buffer` · `Append_text` |
| `Reasoning_summary_text_delta` | `Ensure_buffer` · `Update_reasoning_idx` · `Append_text` |
| `Function_call_arguments_delta` | `Ensure_buffer` · `Append_text` |
| `Function_call_arguments_done`  | `Ensure_buffer` · `Append_text` *(closing paren)* |

All other variants are ignored for now and therefore yield an empty list.

### `val handle_events`

```ocaml
handle_events ~model evs =
  List.concat_map evs ~f:(handle_event ~model)
```

Pure convenience wrapper when a caller already buffered multiple events.  It
does not introduce additional side-effects beyond those already performed
inside `handle_event`.

---

## Implementation / pipeline notes

```text
┌────────────────────────────────┐    Raw UTF-8
│ OpenAI HTTP stream             │───► text deltas
└────────────────────────────────┘
            │                     (no sanitisation)
            ▼
   Chat_tui.Stream (this module) ──► `Append_text` patches (one per delta)
            │
            │      coalesce + batch
            ▼
   Chat_tui.App.Stream_batch ──────► consolidated patch list
            │
            ▼     sanitize + wrap (once)
   Chat_tui.Renderer ──────────────► cached Notty images
```

1. **Raw text deltas** — `Stream` forwards the *verbatim* text received
   from OpenAI.  Previously every delta passed through a UTF-8 validator and
   word-wrapper which wasted CPU and repeatedly invalidated the per-message
   render cache.

2. **Batching & coalescing** — the `Stream_batch` helper inside
   `Chat_tui.App` merges **adjacent** `Append_text` patches that target the
   same message buffer.  This shrinks the patch list while retaining fine
   grained updates for the live log view.

3. **Renderer-side sanitisation** — invalid byte sequences are stripped and
   lines are wrapped **once per cached render** in `Chat_tui.Renderer`.
   Because the cache key contains both *terminal width* and *message text*
   the sanitisation happens at most once for every unique `(w, text)`
   combination.  In practice this reduces the number of cache invalidations
   and word-wrap operations per frame by an order of magnitude without
   altering the final on-screen layout.

The end-user should not notice any visual difference — only lower CPU usage
and smoother scrolling when large streams are active.

---

## Examples

> The snippets below assume that `model` is a valid `Model.t` value and
> that `ev` / `evs` are decoded streaming events.

### Processing a single delta

```ocaml
let patches = Chat_tui.Stream.handle_event ~model ev in
ignore (Model.apply_patches model patches);
```

### Processing an entire batch

```ocaml
let patches = Chat_tui.Stream.handle_events ~model evs in
ignore (Model.apply_patches model patches);
```

### Handling the output of a tool call

```ocaml
let patches =
  Chat_tui.Stream.handle_fn_out ~model function_call_output in
Model.apply_patches model patches |> ignore;
```

---

## Current limitations

* **Mutable look-ups** – the implementation still checks and manipulates
  some mutable sub-fields inside the model to decide which patch to emit.
  This will disappear once the planned *pure model* refactor lands.

* **Partial event coverage** – only the event variants needed by the TUI are
  handled today.  Unknown variants are silently ignored.  Extending the
  mapping is straightforward: add a new pattern-match case in
  `stream.ml` and emit the required patches.

