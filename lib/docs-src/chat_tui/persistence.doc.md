# `Chat_tui.Persistence` – Saving ChatMarkdown transcripts and tool output

`Chat_tui.Persistence` is the *single* I/O façade used by the TUI to turn the
in-memory conversation state into a durable ChatMarkdown transcript on disk.
It has only two public entry-points:

* [`write_user_message`](#write_user_message) – update the trailing `<user>`
  block while the user is still typing.
* [`persist_session`](#persist_session) – append every message generated since
  the last refresh and, optionally, store large tool payloads as separate JSON
  files.

Both helpers embrace the capability style promoted by
[Eio](https://github.com/ocaml-multicore/eio): instead of passing raw strings
the caller must supply a directory capability, ensuring that the persistence
layer cannot break out of its sandbox.

---

## Table of contents

1. [Why is this a separate module?](#why-module)
2. [`write_user_message`](#write_user_message)
3. [`persist_session`](#persist_session)
4. [Known limitations](#limitations)

---

### 1 · Why is this a separate module? <a id="why-module"></a>

File-system concerns should not pollute the
[model–view–update](https://guide.elm-lang.org/architecture/) flow that drives
the TUI.  Centralising all persistence logic behind the small `Persistence`
API offers a couple of advantages:

* **Isolation** – The controller and renderer deal exclusively with *pure*
  OCaml data.  No accidental `Eio` calls leak into the UI layer.
* **Consistency** – A single module implements one canonical mapping from the
  OpenAI AST to ChatMarkdown, guaranteeing that manual editing and automatic
  saving always agree.
* **Extensibility** – Adding support for future OpenAI item variants only
  requires touching `Persistence`.

---

### 2 · `write_user_message` <a id="write_user_message"></a>

```ocaml
val write_user_message :
  dir:Eio.Fs.dir_ty Eio.Path.t ->
  file:string ->
  string ->
  unit
```

Updates the *last* `<user>` block in the ChatMarkdown document.  If the file
already ends with an empty stub the function replaces it in place; otherwise a
fresh block is appended.

Example – keep the prompt on disk while the user is typing:

```ocaml
(* inside the controller *)
Persistence.write_user_message
  ~dir:(Eio.Stdenv.cwd env)
  ~file:"prompt.chatmd"
  current_input
```

---

### 3 · `persist_session` <a id="persist_session"></a>

```ocaml
val persist_session :
  dir:Eio.Fs.dir_ty Eio.Path.t ->
  prompt_file:string ->
  datadir:Eio.Fs.dir_ty Eio.Path.t ->
  cfg:Chat_response.Config.t ->
  initial_msg_count:int ->
  history_items:Openai.Responses.Item.t list ->
  unit
```

Serialises every item whose index is **≥ `initial_msg_count`** into
`prompt_file`.  The function is *append-only* – existing content is never
rewritten.

Behavior summary:

| Item variant                              | Resulting ChatMarkdown block                                                             |
|-------------------------------------------|-------------------------------------------------------------------------------------------|
| `Input_message` (role = `user`)           | `<user>` text `</user>`                                                                  |
| `Input_message` (role = `assistant`)      | `<assistant>` text `</assistant>`                                                        |
| `Input_message` (role = `tool`)           | `<tool_response>` text `</tool_response>`                                                |
| `Output_message`                          | `<assistant id="…"> RAW| text |RAW </assistant>`                                        |
| `Function_call` / `Function_call_output`  | Inline `RAW| … |RAW` or external `<doc …>` depending on `cfg.show_tool_call`             |
| `Reasoning`                               | `<reasoning>` with nested `<summary>` children                                           |

External files use the naming scheme `N.{tool_call_id}.json` where *N* is a
monotonically increasing counter per session.  They live underneath
`datadir/.chatmd` and are referenced through relative `<doc>` links so moving
the whole folder preserves consistency.

---

### 4 · Known limitations <a id="limitations"></a>

1. **No truncation support** – Very large assistant messages are written in
   full which might slow down editors when the prompt grows beyond a few
   megabytes.
2. **Single-writer assumption** – Concurrent calls from multiple fibres would
   interleave blocks.  A higher-level lock should guarantee that only one
   persistence operation runs at a time.
3. **Not transactional** – Crashes in the middle of a write might leave the
   transcript in an invalid state.  Practical experience shows this to be rare
   enough not to warrant a full journalling system.


