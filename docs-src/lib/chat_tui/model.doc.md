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
6. [Fork helpers](#fork-helpers)
7. [Applying patches – `apply_patch`](#apply_patch)
8. [Known limitations](#known-limitations)

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

Additional mutators exist that operate on these fields (`clear_selection`,
`set_selection_anchor` …) and do exactly what their names suggest.

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
cursor position live in the `cmdline` / `cmdline_cursor` fields.  The helper
functions mirror those in the accessors section and allow the controller to
edit the line in place.

---

### Undo / Redo <a id="undo--redo"></a>

`push_undo` takes a snapshot of the prompt before a mutation happens and
puts it on top of the undo ring.  `undo` and `redo` move back and forth in
that ring.  Both return `true` when a change was applied.

---

### Fork helpers <a id="fork-helpers"></a>

Long-running [fork tool](../functions.doc.md#fork) calls (e.g. the
conversation re-writer or the summariser) stream their output into the UI.
`active_fork` stores the _call-id_ of the process so the renderer can
highlight incoming deltas.  `fork_start_index` remembers where the forked
output began inside the message list.

---

### Applying patches – `apply_patch` <a id="apply_patch"></a>

The controller does **not** mutate the model directly but instead emits
small, declarative {!Chat_tui.Types.patch} values such as

```ocaml
Append_text { id; role = "assistant"; text = "\nnew delta…" }
```

`apply_patch` interprets those commands and updates the model in place.
`apply_patches` is a convenience helper that folds a list of commands.

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


