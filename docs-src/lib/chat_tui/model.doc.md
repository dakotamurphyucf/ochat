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
  the cursor, respectively. Cursor offsets are bytes; the cursor setter clamps
  them to an extended-grapheme boundary. Callers must supply valid UTF-8 text.

---

### Undo / Redo <a id="undo--redo"></a>

`with_edit_checkpoint` wraps shared controller dispatch. When an action changes
draft text without already modifying the undo stack, it saves the previous
text/cursor and clears redo. Typeahead acceptance and explicit Normal edits
keep their existing checkpoints without duplication. Undo/redo restores text
and cursor and clears the selection anchor; it never reverses transcript
mutations or external tool effects.

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

Long-running [fork tool](../chat_response/fork.doc.md) calls (e.g. the
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
*Value* → [`msg_img_cache`](#message-image-cache) record containing the
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

## Shell page and modal state

`pages.shell_security` is isolated from Chat history/layout caches. It owns its
snapshot, tab, scroll box, stable grant/audit selections, management load
generation, shell approval modal, grant revocation modal, and moderator modal.
Opening or editing an overlay therefore does not invalidate message rendering.

## Additional implementation notes

The following retained notes describe the original local/controller implementation.
For native/daemon ownership and projections use [the current integration guide](../../agent-server/embedding.md).
Shared rendering APIs remain useful; legacy persistence/controller assumptions
are not daemon session ownership. Consult the current `.mli` for exact signatures.

### Chat_tui.Model

Mutable snapshot of the terminal chat UI state.

`Chat_tui.Model` concentrates every piece of information that the Ochat
terminal UI needs to render the current session and to react to user input.
It is intentionally "fat": instead of threading many independent references
through every function, callers pass around a single `Model.t` value.

The record is still **mutable** because the refactor towards a pure
Elm-style architecture (immutable model + explicit patches) is being rolled
out incrementally. A future change is expected to turn `t` into an immutable
value rebuilt by `apply_patch` instead of modified in place.

---

## Overview

From a high level, `Model.t` groups several concerns:

- **Canonical history** – `history_items : Openai.Responses.Item.t list`
  contains the full OpenAI chat history used for streaming and persistence.

- **Renderable transcript** – `messages : Chat_tui.Types.message list`
  holds the subset of history that is visible in the UI: role/content pairs
  plus transient placeholders.

- **Draft prompt and modes** – `input_line`, `cursor_pos`,
  `selection_anchor`, `editor_mode`, and `draft_mode` track the state of the
  bottom-of-screen editor.

- **Command line** – `cmdline` and `cmdline_cursor` back the `:` command
  line used in Normal and Cmdline modes.

- **Streaming buffers and tools** – `msg_buffers`, `function_name_by_id`,
  `reasoning_idx_by_id`, and `tool_output_by_index` maintain state for
  in-flight assistant replies and tool calls.

- **Tasks and key–value store** – `tasks : Session.Task.t list` models
  background work, while `kv_store` is a small mutable map for per-session
  metadata (used by tools and integrations).

- **Scrolling and layout** – `scroll_box : Notty_scroll_box.t`,
  `auto_follow`, `selected_msg`, and the message-image caches drive the
  history viewport and selected-message highlighting.

- **Render caches** – `msg_img_cache`, `msg_heights`, `height_prefix`,
  `last_history_width`, and `dirty_height_indices` cache expensive
  rendering work for the history pane. They are maintained by
  `Chat_tui.Renderer` via the helpers in this module.

- **Streaming control** – legacy operation switches belong to `App_runtime.op`,
  not the compatibility `Model.fetch_sw` field. Native/daemon cancellation uses
  the session client. Active tool-call presentation is separate from operation
  ownership; see [App](app.doc.md).

Most code outside `Chat_tui` should treat `Model.t` as an opaque container
and use the exported helpers rather than poking fields directly. This keeps
state mutations and invariants local to one module.

---

## Types

### `type t`

```ocaml
type t = {
  mutable history_items      : Openai.Responses.Item.t list;
  mutable messages           : Chat_tui.Types.message list;
  mutable input_line         : string;
  mutable auto_follow        : bool;
  msg_buffers                : (string, Chat_tui.Types.msg_buffer) Base.Hashtbl.t;
  function_name_by_id        : (string, string) Base.Hashtbl.t;
  reasoning_idx_by_id        : (string, int ref) Base.Hashtbl.t;
  tool_output_by_index       : (int, Chat_tui.Types.tool_output_kind) Base.Hashtbl.t;
  mutable tasks              : Session.Task.t list;
  kv_store                   : (string, string) Base.Hashtbl.t;
  mutable fetch_sw           : Eio.Switch.t option;
  scroll_box                 : Notty_scroll_box.t;
  mutable cursor_pos         : int;
  mutable selection_anchor   : int option;
  mutable mode               : editor_mode;
  mutable draft_mode         : draft_mode;
  mutable selected_msg       : int option;
  mutable undo_stack         : (string * int) list;
  mutable redo_stack         : (string * int) list;
  mutable cmdline            : string;
  mutable cmdline_cursor     : int;
  mutable active_fork        : string option;
  mutable fork_start_index   : int option;
  mutable msg_img_cache      : (int, msg_img_cache) Base.Hashtbl.t;
  mutable last_history_width : int option;
  mutable msg_heights        : int array;
  mutable height_prefix      : int array;
  mutable dirty_height_indices : int list;
}
```

Key invariants:

- `cursor_pos` and `selection_anchor` are **byte indices** into
  `input_line`, not character counts.
- `msg_heights` and `height_prefix` are kept consistent with `messages` and
  `last_history_width` by the renderer. External code should not mutate
  them directly.
- `tool_output_by_index` is keyed by the index into `messages`, not into
  `history_items`.

The type derives `Fields` via `[@@deriving fields ~getters ~setters]`, so
functions such as `history_items`, `set_history_items`, `mode` and
`set_mode` are also available for callers that want a more generic
field-based style.

### Editor and draft modes

```ocaml
type editor_mode =
  | Insert
  | Normal
  | Cmdline

type draft_mode =
  | Plain
  | Raw_xml
```

- `Insert` – default mode. Printable keys edit `input_line` and move the
  caret. Cursor positions and selections are byte indices.
- `Normal` – Vim-inspired command mode. Keys largely operate on messages and
  selections instead of directly editing the draft.
- `Cmdline` – a `:` command line is active. Its contents live in
  `cmdline` / `cmdline_cursor`. Leaving this mode usually returns to
  `Insert`.

Draft mode chooses how `input_line` is interpreted on submission:

- `Plain` – treat the buffer as ordinary markdown that goes directly to the
  OpenAI API.
- `Raw_xml` – treat the buffer as low-level XML describing tool invocations
  (used by the command palette and some advanced workflows).

### Message-image cache

```ocaml
type msg_img_cache = {
  width            : int;
  text             : string;
  img_unselected   : Notty.I.t;
  height_unselected: int;
  img_selected     : Notty.I.t option;
  height_selected  : int option;
}
```

Per-message render cache maintained by `Chat_tui.Renderer`:

- `width` – history-pane width (in terminal cells) at which the images were
  rendered.
- `text` – original message text. Used to cheaply detect when the cache is
  stale.
- `img_unselected` / `height_unselected` – pre-rendered Notty image for the
  message in its normal state and its height.
- `img_selected` / `height_selected` – lazily created variant used when the
  message is selected.

---

## Core helpers

### Construction

```ocaml
val create :
  history_items:Openai.Responses.Item.t list ->
  messages:Chat_tui.Types.message list ->
  input_line:string ->
  auto_follow:bool ->
  msg_buffers:(string, Chat_tui.Types.msg_buffer) Base.Hashtbl.t ->
  function_name_by_id:(string, string) Base.Hashtbl.t ->
  reasoning_idx_by_id:(string, int ref) Base.Hashtbl.t ->
  tool_output_by_index:(int, Chat_tui.Types.tool_output_kind) Base.Hashtbl.t ->
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

`create` is a **shallow** constructor: it stores the arguments directly in
the record without copying or validation. Mutating a hashtable passed to
`create` later also changes the model.

This function mainly exists to bundle many pre-existing references into a
single value when bootstrapping `Chat_tui.App`. Callers are expected to
construct maps and other mutable structures themselves and pass them in.

### Prompt and selection

- `input_line : t -> string` – current contents of the multi-line draft
  prompt at the bottom of the screen.
- `cursor_pos : t -> int` – caret position in **bytes** within
  `input_line`. Always between `0` and `String.length input_line`.
- `selection_anchor : t -> int option` – starting byte offset of the active
  selection, if any.
- `clear_selection : t -> unit` – drop the active selection.
- `set_selection_anchor : t -> int -> unit` – mark a position as the start
  of a selection.
- `selection_active : t -> bool` – whether a selection anchor is present.

Cursor and selection logic is implemented by `Chat_tui.Controller`. The
model helpers are small building blocks used from that controller.

### Messages, tasks, and metadata

- `messages : t -> Chat_tui.Types.message list` – current list of
  renderable messages, including transient placeholders.
- `tasks : t -> Session.Task.t list` – tasks associated with the current
  session.
- `kv_store : t -> (string, string) Base.Hashtbl.t` – mutable key–value
  store used by tools and integrations to stash small bits of state.
- `tool_output_by_index : t -> (int, Chat_tui.Types.tool_output_kind) Base.Hashtbl.t`
  – mapping from message index (in `messages`) to a coarse classification of
  tool outputs (e.g. `Apply_patch`, `Read_file { path }`). This powers
  specialised rendering in `Chat_tui.Renderer`.
- `auto_follow : t -> bool` – whether the history viewport should
  automatically follow new messages.

`history_items` (accessible via the generated `history_items` accessor) is
the canonical OpenAI transcript; `messages` is derived from it plus
placeholders.

### Modes and command line

- `toggle_mode : t -> unit` – toggle between `Insert` and `Normal` editor
  modes; if in `Cmdline`, return to `Insert`.
- `set_draft_mode : t -> draft_mode -> unit` – change how the draft buffer
  will be interpreted on submission.
- `select_message : t -> int option -> unit` – set or clear the currently
  selected message index (used by Normal mode and the renderer).

Command-line helpers:

- `cmdline : t -> string` – current `:` command-line contents (without the
  leading `:` character).
- `cmdline_cursor : t -> int` – caret position within `cmdline` (bytes).
- `set_cmdline : t -> string -> unit` – overwrite the command-line buffer.
- `set_cmdline_cursor : t -> int -> unit` – move the cursor inside the
  command line.

### Fork helpers

- `active_fork : t -> string option` – identifier of the currently running
  fork-style tool call, if any.
- `set_active_fork : t -> string option -> unit` – update `active_fork`.
- `fork_start_index : t -> int option` – index into `messages` that marked
  the start of the current fork.
- `set_fork_start_index : t -> int option -> unit` – update
  `fork_start_index`.

These are used by `Chat_tui.Stream` and the renderer to visually group
forked tool output.

### Undo / redo

- `push_undo : t -> unit` – push the current `(input_line, cursor_pos)`
  pair onto the undo stack and clear the redo stack.
- `undo : t -> bool` – pop a previous state from the undo stack, push the
  current one onto the redo stack, and restore the popped state. Returns
  `false` when there was nothing to undo.
- `redo : t -> bool` – inverse of `undo`; returns `false` when there was
  nothing to redo.

Undo/redo only affects the **draft prompt**, not history or command-line
state.

### Patches and history

- `apply_patch : t -> Chat_tui.Types.patch -> t` – execute a single
  high-level patch (see `Chat_tui.Types.patch`) by mutating the model in
  place. Returns the same model value for ergonomic piping.
- `apply_patches : t -> Chat_tui.Types.patch list -> t` – fold
  `apply_patch` over a list of patches.
- `add_history_item : t -> Openai.Responses.Item.t -> t` – append a raw
  OpenAI history item to `history_items` without touching `messages`.
- `rebuild_tool_output_index : t -> unit` – rebuild
  `tool_output_by_index` from the current `history_items`, pairing
  `Function_call_output` entries with their corresponding visible messages.

The **streaming path** uses `apply_patches` to evolve the model in response
to events produced by `Chat_tui.Stream`. Operations that replace the entire
history at once (e.g. context compaction or session load) are expected to
call `rebuild_tool_output_index` afterwards.

### Rendering caches

These helpers are used almost exclusively by `Chat_tui.Renderer`:

- `last_history_width : t -> int option` – history-pane width (cells) for
  which height caches are valid.
- `set_last_history_width : t -> int option -> unit` – update that width.
- `clear_all_img_caches : t -> unit` – flush `msg_img_cache` and all height
  caches. Used on major layout changes.
- `invalidate_img_cache_index : t -> idx:int -> unit` – remove the cached
  images for a single message and record that its height may have changed.
- `find_img_cache : t -> idx:int -> msg_img_cache option` – look up cached
  render data for a single message.
- `set_img_cache : t -> idx:int -> msg_img_cache -> unit` – store cached
  render data for a message index.
- `take_and_clear_dirty_height_indices : t -> int list` – return and clear
  the list of message indices whose heights must be recomputed.

External callers normally do not need to interact with these functions
directly; they are part of the view-layer implementation detail.

---

## Examples

### Building a minimal model

The snippet below constructs a minimal empty model that can be rendered with
`Chat_tui.Renderer`:

```ocaml
open Core

let empty_model () : Chat_tui.Model.t =
  let msg_buffers = Base.Hashtbl.create (module String) in
  let function_name_by_id = Base.Hashtbl.create (module String) in
  let reasoning_idx_by_id = Base.Hashtbl.create (module String) in
  let tool_output_by_index = Base.Hashtbl.create (module Int) in
  let kv_store = Base.Hashtbl.create (module String) in
  let scroll_box = Notty_scroll_box.create Notty.I.empty in
  Chat_tui.Model.create
    ~history_items:[]
    ~messages:[]
    ~input_line:""
    ~auto_follow:true
    ~msg_buffers
    ~function_name_by_id
    ~reasoning_idx_by_id
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
```

You can then render this model once using the renderer:

```ocaml
Eio_main.run @@ fun env ->
  let term =
    Notty_eio.Term.create
      ~input:(Eio.Stdenv.stdin env)
      ~output:(Eio.Stdenv.stdout env)
      ()
  in
  let model = empty_model () in
  let size = Notty_eio.Term.size term in
  let image, (cx, cy) = Chat_tui.Renderer.render_full ~size ~model in
  Notty_eio.Term.image term image;
  Notty_eio.Term.cursor term (Some (cx, cy));
  Eio.Fiber.await_cancel ()
```

### Using undo / redo on the draft prompt

`push_undo`, `undo`, and `redo` operate only on `input_line` and
`cursor_pos`:

```ocaml
open Core

let demo_undo () =
  let model = empty_model () in
  (* Start with some text. *)
  Chat_tui.Model.set_input_line model "hello";
  Chat_tui.Model.set_cursor_pos model 5;

  (* Take a snapshot, then modify the buffer. *)
  Chat_tui.Model.push_undo model;
  Chat_tui.Model.set_input_line model "hello, world";
  Chat_tui.Model.set_cursor_pos model 12;

  assert (Chat_tui.Model.input_line model = "hello, world");

  (* Undo restores the previous contents and cursor. *)
  assert (Chat_tui.Model.undo model);
  assert (Chat_tui.Model.input_line model = "hello");
  assert (Chat_tui.Model.cursor_pos model = 5);

  (* Redo moves forward again. *)
  assert (Chat_tui.Model.redo model);
  assert (Chat_tui.Model.input_line model = "hello, world")
```

The setters `set_input_line` and `set_cursor_pos` are generated by
`[@@deriving fields ~getters ~setters]`.

### Applying streaming patches

When the OpenAI client delivers streaming deltas, higher layers convert them
to `Chat_tui.Types.patch` values and feed them through `apply_patches`:

```ocaml
let apply_stream_delta (model : Chat_tui.Model.t) ~(id : string) ~(role : string)
    ~(delta : string) : unit =
  let open Chat_tui.Types in
  let patches = [
    Ensure_buffer { id; role };
    Append_text { id; role; text = delta };
  ] in
  ignore (Chat_tui.Model.apply_patches model patches)
```

`Ensure_buffer` creates a streaming buffer and a placeholder visible message
on first use; `Append_text` appends to the buffer and updates the message
text while keeping render caches in sync.

---

## Known issues and limitations

- **Mutable design** – `Model.t` is currently mutable and shared between
  controllers, renderer, and app. Callers should avoid accessing it from
  multiple domains or Eio fibers without external synchronisation.

- **Unicode representation** – cursor positions and selections are stored as
  byte indices aligned by `Utf8_edit` to extended grapheme boundaries.
  Arbitrary direct buffer writes must still contain valid UTF-8. Word motions
  use whitespace rather than language-specific segmentation.

- **History/message alignment is not enforced** – the relationship between
  `history_items`, `messages`, and `tool_output_by_index` is maintained by
  higher layers. Mutating these fields out-of-band without rebuilding
  indices (e.g. via `rebuild_tool_output_index`) can produce inconsistent
  UI state.

- **Render cache invariants** – `msg_img_cache`, `msg_heights`, and
  `height_prefix` are implementation details of the renderer. Directly
  mutating them without using the dedicated helpers can lead to stale or
  corrupted output.

---

## Related modules

- `Chat_tui.Types` – core chat and patch types (`role`, `message`,
  `msg_buffer`, `patch`, `tool_output_kind`).
- `Chat_tui.Controller` – key handling and high-level reactions that mutate
  the model.
- `Chat_tui.Renderer` – pure rendering of `Model.t` into Notty images and
  cursor positions.
- `Chat_tui.App` – orchestration layer that ties the model, controller, and
  renderer into a running TUI.
- `ochat.Notty_scroll_box` – scroll-box abstraction backing the history
  viewport.
