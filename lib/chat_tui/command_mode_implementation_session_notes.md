# Command-Mode – Implementation Log

This markdown document is our **scratch-pad** for the incremental command-mode
merge.  It gets updated after every programming session so that future phases
can pick up the exact context without re-reading the whole repository.

## Session 1 — Phase 0 (Scaffolding)
<SESSION>
Deliverables from *command_mode_implementation_steps.md* completed:

1. **Model extensions**
   * Added `editor_mode` (`Insert | Normal`) and `draft_mode`
     (`Plain | Raw_xml`) variants.
   * Extended `Model.t` with fields:
     `mode`, `draft_mode`, `selected_msg`, `undo_stack`, `redo_stack`.
   * Provided helpers `toggle_mode`, `set_draft_mode`, `select_message`.
   * `Model.create` signature updated (new mandatory arguments).
   * Updated the only call-site in `app.ml` to use the new parameters (defaults
     `Insert`, `Plain`, `None`, `[]`, `[]`).

2. **Status-bar indicator** (renderer):
   * Added one-line status bar between history viewport and input editor.
   * Shows `-- INSERT --` or `-- NORMAL --` depending on `Model.mode`.
   * Minor layout changes – history viewport height reduced by one row; cursor
     Y-coordinate adjusted accordingly.

3. **Controller – basic mode toggle**
   * Refactored existing monolithic `handle_key`:
       * Extracted original Insert-mode logic into
         `handle_key_insert` (no functional changes).
       * Introduced lightweight wrapper `handle_key` which dispatches on
         `Model.mode` and implements the Phase 0 key-map:
           * `Esc` (while in **Insert**) → switch to *Normal* mode.
           * `i`   (while in **Normal**) → return to *Insert* mode.
           * `Esc` in **Normal** keeps previous behaviour (*cancel / quit*).

4. **Build & tests**
   * `dune build` and `dune runtest --diff-command=diff -u` both complete
     successfully (duplicate OpenSSL linker flags are unrelated).
   * Added expect-test `test/chat_tui_command_mode_test.ml` which asserts that
     successive `Model.toggle_mode` calls cycle *Insert → Normal → Insert*.

### Short notes / gotchas

* `Insert` / `Normal` constructors live at top-level (not inside `Model`), so
  they can be referenced without module prefix.
* Deriving `fields` automatically created `set_mode`, `mode`, etc.; helpers
  merely wrap common operations.
* Renderer now reserves a line for the status bar – **always** rendered; we
  may revisit to hide it when conversation height is too small (< 3 rows).
* `controller.ml` was quite large; the quick wrapper approach avoids a full
  sub-module split (scheduled for Phase 1).
* Future phases must remember to adapt scrolling logic if additional fixed
  rows (command line) are added.

---

Next step → **Phase 1**: split controller, implement movement keys (h/j/k/l,
word motions, etc.).
</SESSION>

## Session 2 — Phase 1 (Normal-mode motions)
<SESSION>
Deliverables from *command_mode_implementation_steps.md* completed:

1. **Controller split into sub-modules**
   * Added `controller_types.ml` with the central `reaction` variant so that
     sub-modules can share it without circular dependencies.
   * Implemented `controller_normal.ml` containing the new Normal-mode
     key-handler.
   * `controller.ml` now re-exports the constructors via a type-alias and
     delegates Normal-mode events to the new module.

2. **Cursor motions implemented**
   Normal-mode supports the following keys now:
     • `h` / `l` – move left / right.
     • `j` / `k` – move cursor vertically within the multi-line draft.
     • `w` / `b` – next / previous word.
     • `0` / `$` – start / end of line.
     • `gg`        – scroll viewport to top.
     • `G`         – scroll to bottom.

   A small internal flag `pending_g` recognises the `gg` chord.

3. **Dune plumbing**
   Library stanza updated to include the two new modules so that they become
   part of `chat_tui`.

4. **Tests**
   Added expect-test `chat_tui_normal_mode_cursor_test.ml` which asserts that
   `w` and `b` move the draft cursor from 0 → 6 and back in the string
   "hello world".

5. **Build status**
   `dune build` and full test-suite (`dune runtest --diff-command=diff -u`) are
   green.  Remaining linker warnings about duplicate `-lcrypto` / `-lssl` are
   unrelated.

### Notes / gotchas for next phases

* Variant re-export (`type reaction = Controller_types.reaction = …`) keeps
  `Controller.Redraw` usable everywhere – no downstream churn.
* Motion logic is still byte-based; UTF-8 grapheme correctness postponed.
* Helpers like `line_bounds` are duplicated between insert & normal modules –
  plan a small utility sub-module or move them to `Model` after Phase 2.
* The scroll-box height calculation replicates the Insert-mode implementation;
  revisit once the command-line row lands in Phase 3 so bottom scrolling stays
  aligned.

Next step → **Phase 2**: Normal-mode edit operations (i/a/o/O, x, dd, undo/redo).
</SESSION>

## Session 3 — Phase 2 (Edit operations & Undo/Redo)
<SESSION>
Phase-2 milestones from *command_mode_implementation_steps.md* are now in place.

1. **Model – full undo/redo ring**
   • Converted `undo_stack` / `redo_stack` to store **both** draft text and cursor
     position: `(string * int) list`.
   • Added helpers `push_undo`, `undo`, `redo` in `Model`.
   • Utility: `push_undo` auto-clears `redo_stack` on every new edit so the ring
     behaves like Vim’s single-branch history.

2. **Normal-mode editing commands** (`controller_normal.ml`)

   Key-bindings implemented exactly as specced:

   • `a`  – move cursor **one character right** (if possible) and enter *Insert*.
   • `o`  – open **new empty line below**, place cursor at BOL, switch to Insert.
   • `O`  – open **new empty line above**, ditto.
   • `x`  – delete character under cursor (no change when at EOL / EOF).
   • `dd` – delete current line (including trailing newline if present).  A new
     `pending_dd` flag mirrors the existing `pending_g` logic so we detect the
     `dd` chord.
   • `u`  – undo one step; `Ctrl-r` – redo.  Both return *Unhandled* when ring
     is empty so upper layers can ignore.

   All edit commands call `Model.push_undo` **before** mutating the draft to
   ensure the previous state is recoverable.

3. **Controller dispatcher**
   • Existing `controller.ml` already handled `i` for *Insert*.
   • No changes required elsewhere; edit operations live entirely inside the
     Normal-mode sub-module.

4. **Tests**
   • Added expect-test `chat_tui_undo_test.ml` covering a simple
     *push → edit → undo → redo* round-trip.
   • Expanded `test/dune` with a new library stanza.
   • Full `dune runtest --diff-command=diff -u` passes.

5. **Misc tweaks / gotchas**
   • `controller_normal.ml` resets `pending_dd` when any other key is pressed
     (pattern match directly after the similar `pending_g` reset).
   • Implementation uses byte offsets like the rest of the code; future UTF-8
     work still pending.
   • Newline handling for `o` / `O` tries hard to keep the buffer clean when
     editing the last line without a terminating `\n`.

6. **Open tasks pushed to Phase 3+**
   • Insert-mode edits currently *don’t* push to the undo ring; once Normal-mode
     stabilises we’ll wire `controller_insert.ml` so every mutation records an
     undo snapshot.
   • Consider maximum ring size (e.g. 1000 snapshots) to bound memory.

Build status: `dune build` and test-suite green (only the usual duplicate
OpenSSL linker warnings).

Next step → **Phase 3**: `:` command-line, Cmdline mode, and ex-style
commands.
</SESSION>

## Session 4 — Phase 3 (`:` Command-line)
<SESSION>
Deliverables from *command_mode_implementation_steps.md* — **Phase 3** — have been implemented.

1. **New editor mode `Cmdline`**
   * Added `Cmdline` constructor to `Model.editor_mode`.
   * `Model.t` gained fields `cmdline` and `cmdline_cursor`.
   * Helper accessors (`cmdline`, `cmdline_cursor`, setters) exposed via the interface.
   * `Model.create` signature extended; all call-sites (app + tests) updated.

2. **Controller – command-line handler**
   * New module `controller_cmdline.ml` implements a minimal ex-style line editor.
   * Supported keys: ASCII insertion, Backspace, ←/→, `Esc` to abort, `Enter` to execute.
   * Parsed commands:
       – `:q` / `:quit` → `Quit`
       – `:w`           → `Submit_input`
       – `:wq`          → `Quit` (simplified)
       – unknown / unimplemented (`:open`, `:saveas …`) → `Redraw`.
   * Dispatcher in `controller.ml` updated; Normal-mode now detects `:` to enter Cmdline mode.

3. **Renderer updates**
   * Status bar tag shows `-- CMD --`.
   * Input box is reused as command bar with `:` prefix; cursor maths adjusted.

4. **Tests**
   * Added `chat_tui_cmdline_mode_test.ml` to verify the `:q<Enter>` sequence emits `Quit`.
   * All existing tests updated for new `Model.create` parameters.

5. **Build status**
   * `dune build` and full test-suite are green (duplicate `-lcrypto/-lssl` warnings unchanged).

### Notes / Gotchas for future phases

* `:open` / `:saveas` only parsed – actual execution deferred to Phase 5 when multi-buffer support lands.
* `Model.toggle_mode` now maps `Cmdline → Insert` so tests using it remain valid.
* Undo-ring still ignores command-line edits (matches Vim behaviour).
* Renderer now forks on `Model.mode` in three spots – remember to update when additional modes (Prompt-edit, Raw-XML) are added.

</SESSION>

## Session 5 — Phase 4 (Raw-XML mode & File-path completion)
<SESSION>
Deliverables from *command_mode_implementation_steps.md* — **Phase 4** — are now in place.

1. **Raw-XML draft mode**
   • Implemented key-bindings to toggle the new *Raw* draft sub-mode:
     – `Ctrl-r` in **Insert** mode.
     – `r`       in **Normal** mode (distinct from `Ctrl-r` redo).
   • `Model.set_draft_mode` is used by both handlers; dispatcher logic lives
     in `controller.ml` (Insert) and `controller_normal.ml` (Normal).

2. **Status-bar indicator**
   • `renderer.ml` now appends `-- RAW --` to the existing mode tag whenever
     `Model.draft_mode = Raw_xml` so the user can immediately see the toggle.

3. **Raw submission path**
   • Added new `Types.patch` constructor `Add_user_message_raw  of { xml }`.
   • `Model.apply_patch` handles it by inserting the verbatim XML fragment
     into the canonical history list while still displaying a *sanitised*
     version in the visible message list.
   • `app.ml / apply_local_submit_effects` now chooses between
     `Add_user_message` and `Add_user_message_raw` depending on the current
     draft mode; after a raw submission the mode automatically resets to
     `Plain`.

4. **Path completion engine (stub)**
   • New module `path_completion.{ml,mli}` offers a **blocking**, directory
     cache-backed implementation of the API sketched in the design doc.
     *Note*: this is a **temporary** synchronous version that uses
     `Stdlib.Sys.readdir`; it will be replaced by an asynchronous Eio
     version in a later phase.
   • Module added to the dune library stanza so other components can start
     using it.  No UI integration yet – that will happen together with the
     overlay renderer in Phase 5.

5. **Key-map adjustments**
   • `Ctrl-r`/`r` toggles take precedence over existing bindings so we placed
     the pattern-matches **before** the more generic cases in both
     controllers.

6. **Renderer changes**
   • Status bar string concatenates mode tag and RAW tag to keep layout
     stable; everything is rendered with the same `status_attr` colours.

7. **Build & tests**
   • `dune build` and `dune runtest --diff-command=diff -u` pass.  Existing
     test-suite required no changes because raw-mode features are additive.
   • Added linker stub warning fix in `path_completion.ml` by switching to
     `Stdlib.Sys.readdir`.

### Notes / gotchas for next phases

* `path_completion` is deliberately simplistic; it blocks the main fibre on
  large directories.  The asynchronous cache described in *path_completion.md*
  should replace it before we hook Tab-completion into the insert handler.
* We serialise raw XML messages using a custom `_type = "raw_xml"` field in
  the `Openai.Responses.Input_message.Text` record.  Down-stream components
  ignore unknown types so this is safe, but we need to update the JSON
  schema once tool support lands.
* After submission we forcibly reset `draft_mode` to `Plain`.  This mimics
  Vim’s `:set paste` UX and avoids confusion for users that forget to toggle
  back.
* There is an overlap between `Ctrl-r` (redo) and `Ctrl-r` (toggle) in
  Normal mode – we kept redo (`Ctrl-r`) and chose **bare** `r` for the toggle
  instead.
* Overlay popup with live suggestions is postponed to Phase 5 so as to keep
  this merge small and reviewable.

Next step → **Phase 5**: Prompt-edit buffer & message-level selection, plus
overlay integration for path completion.
</SESSION>

## Session 6 — Phase 5 (Message Selection & Cmdline Delete/Edit)
<SESSION>
Deliverables from *command_mode_implementation_steps.md* **Phase 5** tackled.  Prompt-edit buffer support will follow in Phase 6 once a proper multi-buffer architecture is in place, but the most visible part (message-level selection + `:delete`/`:edit`) is now functional.

1. **Normal-mode message selection**
   • Added `[ ]` motions in `controller_normal.ml`; they move the selection one message up/down, starting at top (`]`) or bottom (`[`) when none was active.  
   • `gg` now selects the first message, `G` the last, in addition to the scroll behaviour from earlier phases.

2. **Renderer highlight**
   • `renderer.ml` extends `message_to_image` with optional `~selected` flag that applies `A.(st reverse)` to all colours when active.  
   • `history_image` receives `~selected_idx` and forwards the flag for each element.  
   • Public signatures updated in `renderer.mli`.

3. **Cmd-line commands**
   • `controller_cmdline.ml` understands `:delete` (`:d`) and `:edit` (`:e`).  
     – Delete removes the selected message from `Model.messages` and adjusts/clears the pointer.  
     – Edit copies the selected text into the draft buffer, switches to *Insert* + *Raw* mode so the user can revise it.

4. **Integer comparators vs `open String`**
   • Opening `String` in `execute_command` shadowed polymorphic compare operators and broke int comparisons.  Fixed with explicit `Int.(…)` wrappers – keep this pitfall in mind for future cmd-line parsing work.

5. **Prompt-edit buffer scaffolding**
   • Decided to postpone the actual `buffer` table until the prompt-edit page is implemented; no code emitted yet so we avoid dead-code warnings.  The design from §9.6 remains unchanged.

6. **Build & smoke test**
   • `dune build` succeeds (only duplicate `-lcrypto/-lssl` linker warnings).  
   • Existing test-suite green; manual TUI check confirms highlight and delete/edit commands work.

### Notes / Todo

* Deleting still doesn’t remove items from `history_items`; we’ll add a helper in Phase 6 when prompt-edit buffer can affect both lists coherently.
* Highlight uses reverse-video; when theming lands we should expose a dedicated `Theme.highlight` colour.
* Path-completion overlay still missing – blocked by async rewrite of `path_completion`.
* Prompt-edit buffer will need its own renderer + insert/normal key-maps; revisit controller splits to avoid bloat.

</SESSION>

