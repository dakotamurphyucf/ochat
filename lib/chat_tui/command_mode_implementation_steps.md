# Command-Mode – Step-by-Step Implementation Plan

This document breaks down the high-level roadmap in
`lib/chat_tui/command_mode_plan.md` into **actionable engineering tickets**.
Each phase compiles and passes the test-suite before you start the next one so
that `main` is always releasable.

-------------------------------------------------------------------------------
PHASE 0 – Scaffolding &amp; visible mode switch  (matches §5 “Phase 0”)
-------------------------------------------------------------------------------

0-1  Model extensions
    • Introduce `editor_mode` and `draft_mode` types, plus new mutable fields
      (`mode`, `draft_mode`, `selected_msg`, `undo_stack`, `redo_stack`) as in
      plan §9.1.
    • Add helpers `toggle_mode`, `set_draft_mode`, `select_message`.
    • update `Model.create` and its callers in `app.ml`.

0-2  Status bar indicator
    • Renderer – new `mode_tag` image and prepend it to the status line.

0-3  Controller: basic mode toggle
    • `Esc` (from Insert) ⇒ Normal; `i` (from Normal) ⇒ Insert.

0-4  Smoke tests
    • Property test that successive toggles reach expected mode.

-------------------------------------------------------------------------------
PHASE 1 – Normal-mode cursor motions  (matches §3 & §9.3)
-------------------------------------------------------------------------------

1-1  Refactor controller into sub-modules
    insert.ml / normal.ml dispatcher.

1-2  Implement motions in `controller/normal.ml`
    h, j, k, l   word w / b   0 / $   gg / G.

1-3  Quickcheck tests for cursor positioning.

-------------------------------------------------------------------------------
PHASE 2 – Normal-mode edit operations  (plan rows 31-37)
-------------------------------------------------------------------------------

2-1  Insert/append/open-line (`i a o O`).

2-2  Deletion (`x`, `dd`).

2-3  Undo/redo ring (`u`, `Ctrl-r`).

2-4  Unit tests: undo edge-cases.

-------------------------------------------------------------------------------
PHASE 3 – `:` Command-line  (section 4)
-------------------------------------------------------------------------------

3-1  Add `Cmdline` mode.

3-2  Implement `controller/cmdline.ml` small line-editor.

3-3  Parse and execute `:w`, `:q`, `:wq`, `:open`, `:saveas`.

3-4  Integration test: replay `:q<Enter>` quits.

-------------------------------------------------------------------------------
PHASE 4 – File-path Intellisense &amp; Raw-XML  (§4.3 & §9.4)
-------------------------------------------------------------------------------

4-1  Draft-mode toggle (`Ctrl-r` / `r`) + status tag `-- RAW --`.

4-2  Integrate `Path_completion` engine (Eio-based).

4-3  Renderer overlay for suggestion popup.

4-4  Raw-XML submission – emit `Add_user_message_raw` patch.

-------------------------------------------------------------------------------
PHASE 5 – Prompt-edit buffer &amp; message selection  (§4.1-4.2)
-------------------------------------------------------------------------------

5-1  Prompt-edit buffer abstraction; introduce `buffers` table (plan §9.6).

5-2  Message selection (Normal keys `[`, `]`, `gg`, `G`) + renderer highlight.

5-3  `:delete` & `:edit` commands operating on the selected message.

-------------------------------------------------------------------------------
CROSS-CUTTING
-------------------------------------------------------------------------------

C-1  Docs – keep `context.md` & `command_mode_plan.md` updated.

C-2  CI – pseudo-TTY expect-tests that walk through a Normal-mode session.

C-3  Performance – benchmark `Path_completion` < 1 ms for 10 k files.

C-4  Release checklist – CHANGELOG, version bump, Raw-XML migration note.

-------------------------------------------------------------------------------
With these tickets you can merge Command-Mode incrementally while preserving a
green build after every phase.

