# `Chat_tui.Model` – central application state

`Chat_tui.Model` bundles every piece of data that the Ochat TUI needs in
order to:

1. render the conversation buffer,
2. keep track of user input and selections, and
3. coordinate background fibres such as streaming API calls.

At the time of writing the record is still **mutable**.  Each field is a
direct reference that is modified in place by the controller or the
renderer.  The long-term goal is to migrate towards an _Elm-ish_ architecture
where a pure `{model → patch → model}` function rebuilds an **immutable**
value, but the refactor happens incrementally so existing code keeps
working.  Consequently most helpers in this module are thin wrappers around
simple mutations that will later be replaced by pure transformations.

---

## Table of contents

1. [Creating a model – `create`](#create)
2. [Accessors](#accessors)
3. [Command-mode helpers](#command-mode-helpers)
4. [Command-line helpers](#command-line-helpers)
5. [Undo / Redo](#undo--redo)
6. [Type-ahead completion](#type-ahead-completion)
7. [Fork helpers](#fork-helpers)
8. [Rendering cache helpers](#rendering-cache-helpers)
9. [Tool-output metadata](#tool-output-metadata)
10. [Applying patches – `apply_patch`](#apply_patch)
11. [Known limitations](#known-limitations)

---

### Creating a model – `create` <a id="create"></a>

```ocaml
val create :
  history_items:Openai.Responses.Item.t list ->
  messages:Types.message list ->
  input_line:string ->
  auto_follow:bool ->
  msg_buffers:(string, Types.msg_buffer) Base.Hashtbl.t ->
  function_name_by_id:(string, string) Base.Hashtbl.t ->
  reasoning_idx_by_id:(string, int ref) Base.Hashtbl.t ->
  tool_output_by_index:(int, Types.tool_output_kind) Base.Hashtbl.t ->
  tasks:Session.Task.t list ->
  kv_store:(string, string) Base.Hashtbl.t ->
  fetch_sw:Eio.Switch.t option ->
  scroll_box:Notty_scroll_box.t ->
  cursor_pos:int ->
  selection_anchor:int option ->
  mode:editor_mode ->
  draft_mode:draft_mode ->
  selected_msg:int option ->
  undo_stack:(string * int) list ->
  redo_stack:(string * int) list ->
  cmdline:string ->
  cmdline_cursor:int ->
  t
```

`create` merely *packs* the many independent references into a single record
so they can be passed around conveniently.  The function is **shallow**: the
arguments are stored as-is – no copying, validation or initialisation takes
place.  Mutating one of the original refs later on therefore affects the
model instance as well.

Example – initialise a fresh, empty model:

```ocaml
open Core

let empty () =
  Chat_tui.Model.create
    ~history_items:[]
    ~messages:[]
    ~input_line:""
    ~auto_follow:true
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box:(Notty_scroll_box.create Notty.I.empty)
    ~cursor_pos:0
    ~selection_anchor:None
    ~mode:Insert
    ~draft_mode:Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
```

---

### Accessors <a id="accessors"></a>

| Function | Purpose |
|----------|---------|
| `input_line` | Current text in the prompt. |
| `cursor_pos` | Byte-offset of the caret inside `input_line`. |
| `selection_anchor` | Start of a selection or `None`. |
| `selection_active` | `true` whenever a selection is active. |
| `messages` | Renderable `(role, text)` tuples. |
| `auto_follow` | `true` → scroll follows new messages automatically. |
| `tool_output_by_index` | Per-message classification metadata for tool-output messages (see [Tool-output metadata](#tool-output-metadata)). |
| `tasks` | Background jobs associated with the current session. |
| `kv_store` | Arbitrary key–value store used by plugins and tools. |
| `cmdline` / `cmdline_cursor` | Current ':' command buffer and its caret position. |

Additional mutators exist that operate on these fields (`clear_selection`,
`set_selection_anchor`, `set_cmdline`, `set_cmdline_cursor` …) and do exactly
what their names suggest.

---

### Command-mode helpers <a id="command-mode-helpers"></a>

* `toggle_mode` switches between the Vim-flavoured *Insert* and *Normal*
  states.  Invoking the function while the command line is active returns to
  *Insert* as well.
* `set_draft_mode` chooses whether the prompt contains plain user text or a
  raw XML tool invocation.
* `select_message` focuses a message in *Normal* mode so it can be yanked or
  deleted.

---

### Command-line helpers <a id="command-line-helpers"></a>

When the user presses `:` the UI enters *command line* mode.  The buffer and
cursor position live in the `cmdline` / `cmdline_cursor` fields.

* **Reading:** use `cmdline` and `cmdline_cursor` to inspect the current
  contents and caret position.
* **Writing:** `set_cmdline` and `set_cmdline_cursor` mutate the buffer and
  the cursor, respectively.  Both functions operate on {e byte} indices – the
  caller is responsible for maintaining valid UTF-8 boundaries.

---

### Undo / Redo <a id="undo--redo"></a>

`push_undo` takes a snapshot of the prompt before a mutation happens and
puts it on top of the undo ring.  `undo` and `redo` move back and forth in
that ring.  Both return `true` when a change was applied.

---

### Type-ahead completion <a id="type-ahead-completion"></a>

Type-ahead completion is a single-candidate suffix suggestion for the current
draft buffer. The completion lives in:

- `typeahead_completion : typeahead_completion option`
- `typeahead_preview_open : bool`
- `typeahead_preview_scroll : int`
- `typeahead_generation : int`

High-level semantics:

- A completion is considered *relevant* only when its snapshot
  (`base_input`, `base_cursor`) still matches the current editor state (see
  `typeahead_is_relevant`).
- Accepting a completion (`accept_typeahead_all` / `accept_typeahead_line`)
  calls `push_undo` exactly once so the acceptance can be undone (typically via
  Normal mode `u`).
- Any “mode-like” transitions (leaving Insert mode, clearing the editor on
  submit) should clear the type-ahead state and bump `typeahead_generation` so
  stale asynchronous results are ignored.

This module intentionally does not perform network I/O. Fetching completions is
done elsewhere (see `Chat_tui.Type_ahead_provider` and the reducer wiring in
`Chat_tui.App_reducer`).

---

### Fork helpers <a id="fork-helpers"></a>

Long-running [fork tool](../functions.doc.md#fork) calls (e.g. the
conversation re-writer or the summariser) stream their output into the UI.
`active_fork` stores the _call-id_ of the process so the renderer can
highlight incoming deltas.  `fork_start_index` remembers where the forked
output began inside the message list.

---

### Rendering cache helpers <a id="rendering-cache-helpers"></a>

Rendering a markdown-heavy chat history is surprisingly expensive because
every mouse movement or keystroke may invalidate word-wrapping and ANSI
colour escapes.  To keep interactive latencies low the renderer therefore
caches the **Notty** image for each message in a small hash-table that
lives inside the model:

*Key* → message index `int` (0-based)  
*Value* → [`msg_img_cache`](#type-msg_img_cache) record containing the
original text, pre-rendered image(s) and their heights.

The helpers below expose a minimal API that allows the renderer to flush or
update the cache only when required.

| Function | Behaviour |
|----------|-----------|
| `last_history_width` | Width (cells) the cached images are valid for. |
| `set_last_history_width` | Updates the width tracking field. **Does not** flush the cache. |
| `clear_all_img_caches` | Drops every entry – used after a hard resize. |
| `invalidate_img_cache_index` | Removes the entry for a single message. |
| `find_img_cache` | Reads a cache entry, returns `None` when missing. |
| `set_img_cache` | Inserts / overwrites a cached render. |
| `take_and_clear_dirty_height_indices` | Returns the list of message indices whose cached height might be stale and clears the internal tracking list. |

Algorithm sketch used by the renderer:

1. Check the history pane width.  
   If the value differs from `last_history_width`:
   * flush the entire cache via `clear_all_img_caches`,
   * call `set_last_history_width` with the new width, then
   * restart the render pass.
2. For each message consult `find_img_cache`.  
   * *Hit* → reuse the cached image.  
   * *Miss* → render, then store via `set_img_cache`.
3. When streaming deltas modify an existing message the controller calls
   `invalidate_img_cache_index` for the affected index only.

These helpers are **private to the TUI layer**.  External modules must not
reach into or rely on the cache.

---
### Tool-output metadata <a id="tool-output-metadata"></a>

The OpenAI streaming API exposes a rich history of response items
(`Openai.Responses.Item.t`), only some of which become visible chat
messages. To let the renderer treat **tool outputs** specially without
hard-coding tool names, the model tracks a small side map:

```ocaml
tool_output_by_index : (int, Types.tool_output_kind) Hashtbl.t
```

* Keys are zero-based indices into `messages` (the renderable transcript).
* Values are `Types.tool_output_kind` tags such as `Apply_patch`,
  `Read_file { path = … }` or `Other { name = Some "diff" }`.

Entries exist **only** for messages that represent the output of a tool
call and for which the TUI could successfully link the output back to its
corresponding `Function_call` item. Regular assistant text leaves no
entry.

During streaming, the `Set_function_output` patch is responsible for
populating or updating this map. When the entire history is replaced at
once (for example after context compaction or a `Replace_history` event)
you must call `rebuild_tool_output_index` so that `tool_output_by_index`
stays in sync with both `history_items` and `messages`:

```ocaml
Model.set_history_items model new_items;
Model.set_messages model (Chat_tui.Conversation.of_history new_items);
Model.rebuild_tool_output_index model;
```

Downstream consumers – primarily the renderer – can then decide how to
display a message based on its classification. For example, a
`Read_file { path = Some p }` output can be rendered using a path-aware
syntax highlighter.

---

### Applying patches – `apply_patch` <a id="apply_patch"></a>

The controller does **not** mutate the model directly but instead emits
small, declarative {!Chat_tui.Types.patch} values such as

```ocaml
Append_text { id; role = "assistant"; text = "\nnew delta…" }
```

`apply_patch` interprets those commands and updates the model in place.
`apply_patches` folds over a list of commands.

`add_history_item` is a low-level helper that appends a raw
`Openai.Responses.Item.t` to the canonical history _without_ touching the
visible message list.  The function is useful when streaming responses
populate the history retro-actively (i.e. after all deltas have already
been rendered).

---

### Known limitations <a id="known-limitations"></a>

1. **Mutable interior** – All fields are still mutable which makes reasoning
   about state a bit harder and prevents time-travel debugging.  This is an
   implementation detail and will change once refactoring step 6 is
   complete.
2. **Implicit invariants** – A handful of fields (e.g. the
   `msg_buffers` ↔ `messages` relationship) must stay in sync but are not
   enforced at the type level yet.  Care must be taken when introducing new
   patch constructors that manipulate the message list.


