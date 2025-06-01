# Chat-TUI – Command-Mode (Vim-style) Roadmap

> **Purpose**  Introduce a *Normal/Command* mode similar to Vim while keeping the
> current Insert/Readline behaviour intact. This document tracks design and
> implementation tasks for the feature.

## 1  Goals

* Dual-mode TUI:
  * **Insert** – existing behaviour (draft editing, Meta+Enter submits).
  * **Normal** – single-key navigation/editing, `:` command-line, search.
* Clear mode indicator in status bar.
* Zero disruption to existing users (starts in *Insert* mode).

## 2  Architecture changes

| Area | Change |
|------|--------|
| `Model.t` | Add `mode : mode` with `type mode = Insert | Normal`.<br/>Helpers: `mode`, `set_mode`, `toggle_mode`. |
| `Controller` | Split `handle_key` into `handle_key_insert` & `handle_key_normal`; dispatch by `Model.mode`. |
| `Renderer` | Extend status-line to show `-- INSERT -- / -- NORMAL --`. |

## 3  Normal-mode Keymap (MVP)

```
Movement       h j k l   ← ↓ ↑ →
               w / b     next / prev word
               0 / $     line start / end
               gg / G    top / bottom of conversation

Editing        i         → Insert
               a         append → Insert
               o / O     open new line below / above
               x         delete char
               dd        delete line to kill-buffer

History        u / Ctrl-r   undo / redo draft
               Ctrl-n/p     previous / next drafts

Submit         Meta-Enter or :w   send draft
Quit           q or :q           quit
Search (MVP-2) /pattern  n / N   incremental search
```

## 4  `:` Command-line (phase 3)

* Re-use bottom input bar when first key is `:` in Normal mode.
* Parse simple commands:
  * `:w`   send draft
  * `:q`   quit
  * `:wq`  send & quit
  * `:set wrap 80` → call existing `/wrap` helper
  * `:open FILE`   switch prompt file
  * `:saveas FILE` save **current conversation** (including new messages) to
    `FILE` and continue chatting in that file (alias `:fork`).  Implementation:
    reuse existing `Persistence.persist_session` with supplied filename, then
    reload prompt.
  * `:fork FILE`  – synonym for `:saveas` but **does not** switch; starts a new
    Chat-TUI instance in background for that file (future).

* Draft-insertion helpers (Normal mode shortcuts *and* `:` commands):
  * `Ctrl-d` / `:insert doc PATH`    → insert `<doc src="PATH" local/>` at cursor
  * `Ctrl-i` / `:insert img PATH`    → insert `<img src="PATH" local/>`
  * `Ctrl-a` / `:insert agent FILE`  → insert `<agent src="FILE" local></agent>`

Implementation notes
--------------------
1. These helpers **switch the draft to a *raw* XML block** so that the
   embedded helper tags are preserved exactly:

   ```xml
   <user>
   RAW|
   Here is a picture:
   <img src="./diagram.png" local/>
   |RAW
   </user>
   ```

2. Chat-TUI currently treats user drafts as plain text. **Introduce an explicit
   “Raw-XML Edit Mode”** instead of heuristics: 

   * Toggle with `Ctrl-r` in Insert mode or `r` in Normal mode.
   * Status bar shows `-- RAW --`.
   * While active, the draft buffer is wrapped into the `<user> … RAW| … |RAW
     </user>` scaffold automatically on submission.

   Implementation work:
   * Add `draft_mode : [ Plain | Raw_xml ]` to `Model`.
   * Submission path checks the mode; if `Raw_xml`, it creates the history
     item verbatim (no escaping) and then resets to `Plain`.

3. When the draft is submitted the converter pipeline
   (`Chat_response.Converter`) already resolves `<doc/>`, `<img/>`, `<agent/>`
   elements, so no additional change is needed on the backend—only the
   front-end needs to preserve the raw message.


## 4.1  Prompt-structure editing page

Separate *Prompt Edit* buffer (opened via `:prompt` or `F2`):

| Command | Action |
|---------|--------|
| `:prompt config`         | insert or edit top-level `<config …/>` element |
| `:prompt dev`            | add `<msg role="developer">…` |
| `:prompt tool NAME CMD`  | add/edit `<tool …>` declaration |
| `:prompt delete N`       | physically delete message *N* from buffer |

Navigation in prompt-edit page remains Vim-style; on `:w` the XML fragment is
saved back and the main chat buffer is refreshed.

## 4.2  Message-level selection & editing

Requirement: users can pick an existing conversation message (via keyboard
scroll or mouse click) and then delete or edit it before exporting.

Design
------
1. **Model additions**
   * `selected_msg : int option` — index into `Model.messages`.

2. **Controller**
   * Normal-mode keys:
     * `[` / `]`      – move selection up / down one message.
     * `g``g` / `G`   – top / bottom (re-use motion keys when *not* in draft).
     * `v`            – toggle visual selection (future multi-select).
     * `d`            – delete selected message (`:prompt delete <idx>`).
   * Mouse:
     * `Notty` reports `Unescape.Mouse` events – translate a left-click inside
       the conversation pane into the nearest message index and set
       `selected_msg`.

3. **Renderer**
   * When `selected_msg = Some i` draw that message with a distinct
     background (e.g. reverse video) so the user can see the selection.

4. **Deletion / Editing**
   * `:delete` (alias `d` in Normal mode) removes the selected message from
     `Model.history_items` *and* `Model.messages`.
   * `:edit` opens the selected message body into the draft buffer in *raw*
     mode for inline editing; on `:w` it overwrites the original message.

Edge-cases: tool-call items, reasoning blocks → treat them as atomic messages
for now; advanced UX could group them.

## 4.3  File-path *Intellisense* inside `src="…"`

When editing in **Raw-XML mode** and the cursor is inside a quoted `src="…"` of
`<doc>`, `<img>` or `<agent>` we want tab-completion (and live suggestions) for
relative paths.

Behaviour
---------
* `Tab` cycles through matches; `Shift-Tab` cycles backwards.
* `Ctrl-Space` shows an overlay list (Notty popup) of up to *10* matches.
* Matches come from a recursive search **rooted at the project folder** with
  `rg --files` (already vendored via our `Rg` helper) filtered by the current
  prefix.
* If the prefix starts with `./` we resolve relative to the folder that
  contains the *current* prompt file; otherwise relative to repo root.

Implementation sketch
---------------------
1. **Parser** – during draft editing keep track of the cursor position; a small
   regex like `\bsrc="([^"]*)"` determines whether we are inside a `src`.
2. **Completion engine**
   * Use the existing `functions.rg` tool (`rg -g "*<prefix>*" --files`) to
     gather candidates lazily when the prefix changes.
   * Maintain an LRU cache so repeated presses of `Tab` are instant.
3. **Controller hooks**
   * In Raw mode intercept `Tab` / `Shift-Tab` / `Ctrl-Space` and ask the
     completion engine for `next / prev / list`.
   * Update the draft text by replacing the `src` substring with the chosen
     completion and adjusting `cursor_pos`.
4. **Renderer overlay**
   * Draw completion list at bottom of screen or floating tooltip near cursor
     (simpler: bottom).
   * Highlight current selection.

Fallback: if `rg` returns >10 results, show first 10 plus “(+ N more …)” line.

## 5  Implementation Phases

| Phase | Deliverable | Est. |
|-------|-------------|------|
| 0 | Scaffolding – mode field, `Esc` / `i`, status bar | 0.5 d |
| 1 | Core motions (h j k l, word/line jumps) | 1 d |
| 2 | Edit ops (+ undo ring) | 1.5 d |
| 3 | `:` command-line | 1 d |
| 4 | Search & polish | 1 d |
| Docs | Update README & keymap sheet | 0.5 d |
| Tests | Unit & e2e tests | 1 d |
| **Total** | **≈ 6 days** |

## 6  Hardening ideas (post-MVP)

* Visual mode + OSC52 clipboard yanking.
* `.chat-tui.rc` for custom key mappings.
* Macro recording (`q` regs).
* Split panes (conversation | draft | command log).
* Lua scripting hook (NeoVim style).

## 8  Additional Enhancements (backlog)

Below items are **not** required for the first MVP but are planned for the
post-merge cycle as they significantly improve robustness and UX.

### 8.1  Background upload watchdog *(was 1.4)*

*After each `Persistence.persist_session` run*

1. Immediately reload the written prompt file and attempt to parse it via
   `Prompt_template.Chat_markdown.parse_chat_inputs`.
2. If parsing fails, keep a backup of the original file (`.bak`) and show a
   blocking error dialog offering to restore or discard the changes.

Implementation: wrap `persist_session` in a helper `Safe_persist.save` with the
above checks; integrate with autosave later.

### 8.2  Split-pane layout *(was 2.1)*

* Togglable with `:vsplit` / `:hsplit` / `F3`.
* Left pane → conversation (scroll-only); right pane → draft + command line.
* Resize with `Ctrl-W` `>` / `<` like Vim.

Renderer change: compute two sub-images and `Notty.I.hcat` / `vcat` them.

### 8.3  Multi-conversation switcher *(was 2.2)*

* `:buffers` lists currently opened prompt files (`*` marks active).
* `:bn` / `:bp` cycle, `:b NUMBER` jumps.
* Internally keep a `(filename * model)` table and re-render on switch.

### 8.4  Snippet tab-stop expansion *(was 2.3)*

* Extend snippet syntax: `$1`, `${2:default}` placeholders.
* After insertion, cursor jumps sequentially through `$N` spots on `Tab`.

Controller holds `snippet_state : int option` that tracks current tab-stop.

### 8.5  Live token counter *(was 2.4)*

* Status bar segment shows: `U:123  A~456  Buf:789` where
  *U* = tokens in last user draft, *A* = estimated assistant reply tokens
  (`Openai.Request.max_tokens` – so we know budget), *Buf* = total tokens in
  conversation window.
* Compute via `Tikitoken.count` in a low-priority fibre every 500 ms.

### 8.6  Theming *(was 2.5)*

* `:set theme=dark|light|solarized` (persists to `~/.config/chat-tui/config`).
* Renderer picks colours from `Theme.current : Theme.t`.

### 8.7  AI-powered formatter *(was 2.6)*

* `/format js` (or `:format js`) – if no local formatter, send draft to
  OpenAI with system prompt “format the following JavaScript code”.  Replace
  draft with formatted result.
* Extensible map `language -> (local_fmt | ai_fmt)`.

### 8.8  Config wizard *(was 4.3)*

* `:wizard` opens a step-by-step guide that asks for model, temperature,
  reasoning effort, etc., then inserts/updates the `<config …/>` element in
  the prompt file.
* Implemented as an internal *agent* prompt so we can iterate quickly.

## 7  Testing Strategy

* Unit tests for Controller: feed synthetic `Notty.Unescape.event`s, assert
  `Model` mutations.
* Quickcheck random Insert/Normal edit sequences round-trip draft.
* E2E expect-test: spawn Chat-TUI in pseudo-tty, replay key-scripts.

---


## 9  Technical integration blueprint (in-depth code review)

> The following section maps every feature in this roadmap to *concrete* code
> changes inside `lib/chat_tui/`.  Line-numbers refer to the revision that was
> current when this document was generated.

### 9.1  Model extensions (`model.ml`)

```ocaml
type editor_mode = Insert | Normal
type draft_mode  = Plain | Raw_xml

type t = {
  …;
  mutable mode         : editor_mode;   (* new *)
  mutable draft_mode   : draft_mode;    (* new *)
  mutable selected_msg : int option;    (* for message selection *)
  mutable undo_stack   : string list;   (* for `u` / Ctrl-r *)
  mutable redo_stack   : string list;
}

val toggle_mode    : t -> unit
val set_draft_mode : t -> draft_mode -> unit
val select_message : t -> int option -> unit
```

`Model.create` in **app.ml** needs two extra arguments and defaults to
`Insert` / `Plain`.

### 9.2  Patch additions (`types.ml`)

```ocaml
| Add_user_message_raw of { xml : string }
```

`apply_patch` must insert the raw XML unchanged into
`history_items`/`messages`.

### 9.3  Controller refactor (`controller.ml` → sub-modules)

* Split giant `handle_key` into:
  * `insert.ml   → handle_key_insert`
  * `normal.ml   → handle_key_normal`
  * `cmdline.ml  → handle_key_cmdline` (command-line state)
  * `prompt_edit.ml` (keys when prompt-edit buffer active)

* Dispatch in old `handle_key`:

```ocaml
match Model.mode model with
| Insert  -> Insert.handle_key …
| Normal  -> Normal.handle_key …
| Cmdline -> Cmdline.handle_key …
```

* Mouse-selection: capture `
  ` `Mouse (`Press (`Left,_,_), col, row)`  → translate via scroll-box to
  message index and set `selected_msg`.

* Raw-XML toggle: `Ctrl-r` in Insert, `r` in Normal → `Model.set_draft_mode`.

* File-path completion hooks only active when `draft_mode = Raw_xml` **and**
  regex `\bsrc="([^"]*)"` matches at cursor.

### 9.4  Path completion engine (`path_completion.ml`)

```ocaml
module Path_completion : sig
  type t
  val create       : unit -> t
  val suggestions  : t -> cwd:string -> prefix:string -> string list
  val next         : t -> dir:[ `Fwd | `Back ] -> string option
end
```

Uses `functions.rg` to populate cache (≤10 entries) and an internal LRU for
speed.

### 9.5  Renderer updates (`renderer/*.ml`)

* Status bar now shows: mode (`INSERT|NORMAL|RAW`), token counter, split
  indicator.
* Highlight `selected_msg` with `A.(st reverse)`.
* Provide `Renderer.overlay` for completion list + error dialogs.
* Add `theme.ml`; pull colours from `Theme.current` (dark/light/solarized).
* Split-pane support: new helper `layout ?split …` that `I.hcat`s or `I.vcat`s
  history + draft images.

### 9.6  Buffer abstraction in `app.ml`

```ocaml
type buffer =
  | Chat        of Model.t
  | Prompt_edit of Prompt_edit.t

mutable buffers : (string, buffer) Hashtbl.t
mutable current_buffer : string
```

`Renderer.render_full` becomes `Renderer.render_buffer` (delegates to
`Renderer.view.ml`).

Command-mode ex commands that operate on buffers:

```text
:buffers   – list, highlight current (* marker)
:bn / :bp  – next / previous buffer (wrap)
:b N       – jump to buffer index
:prompt    – open prompt-edit buffer for current file
```

Parsing lives in `cmdline.ml`; execution handled in `app.ml` by mutating the
`buffers` table and posting a `Redraw` event.

### 9.7  Prompt-edit module (`prompt_edit.ml`)

* Holds `lines : string list` and high-level helpers to insert/modify
  `<config>`, `<tool>`, etc.
* Serialises back to XML fragment on `:w`.

### 9.8  Persistence watchdog

In `app.ml`:

```ocaml
let safe_persist ~file model =
  Persistence.persist_session …;
  try CM.parse_chat_inputs … |> ignore with ex -> restore_backup_and_alert …
```


### 9.9  Autosave & token counter fibres

* Autosave every 30 s to `file~autosave`.
* Token counter: `Tikitoken.count` on draft + last assistant output → stored in
  model; renderer reads it.

### 9.10  Theming & config

* `Theme.current` mutable; `:set theme=` updates + redraw.
* Persist in `~/.config/chat-tui/config`.

### 9.11  Testing additions

* `controller_test.ml` – property-based checks for Insert/Normal mode.
* `path_completion_test.ml` – ensure suggestions stable.
* Expect script in CI running pseudo-TTY conversation.

### 9.12  File / module map

```
chat_tui/
  model.ml
  controller/
    insert.ml normal.ml cmdline.ml prompt_edit.ml
  renderer/
    view.ml overlay.ml theme.ml
  path_completion.ml
  prompt_edit.ml
  app.ml
```

Implementing these scaffolds first will make all roadmap features slot in
cleanly without monolithic diffs later.


