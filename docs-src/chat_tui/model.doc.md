# Chat_tui.Model

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

- **Forks and streaming control** – `active_fork`, `fork_start_index`, and
  `fetch_sw : Eio.Switch.t option` track the currently active fork-style
  tool call and the switch used to cancel or await the streaming request.

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

- **Byte-based editing** – cursor positions and selections are stored as
  byte indices, not grapheme clusters. Some Unicode characters may require
  multiple bytes and can therefore be split by movement and deletion
  operations.

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
