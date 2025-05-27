# Chat-TUI – Terminal ChatGPT Client

A fast, keyboard-driven terminal interface for conversing with OpenAI’s ChatGPT models.  Chat-TUI is built on top of the Notty text UI toolkit and Eio’s lightweight concurrency, giving you a fully featured chat experience that runs everywhere your terminal does.

---

## 1  Overview

* **Single-binary app** – ships as the `chat-tui` executable.
* **Streaming replies** – tokens appear as soon as the OpenAI API streams them.
* **Multi-line editor** – draft messages in a mini text-editor supporting familiar Emacs/Readline keys.
* **Slash commands** – utilities such as `/wrap`, `/count`, `/format`, `/expand`.
* **Session persistence** – conversation is written back to the original Markdown file when you quit, drafts are autosaved on every keystroke.
* **Extensible** – the UI logic lives in `lib/chat_tui/`, making it easy to embed Chat-TUI inside tests or other front-ends.


---

## 2  Usage

### 2.1  Installing & Running

```bash
# from the repo root
$ opam install .   # pulls in dependencies (Notty, Eio …) and pins chat-tui

# run directly with dune (handy while hacking)
$ dune exec chat-tui

# or, if installed system-wide
$ chat-tui
```

### 2.2  CLI flags

| Flag | Default | Meaning |
|------|---------|---------|
| `-file FILE` | `./prompts/interactive.md` | Path to the Markdown conversation buffer.  The file is created if it doesn’t exist. |

Example:

```bash
$ chat-tui -file ~/notes/ai/brainstorm.md
```

---

## 3  Features

* **Rich history view** – scroll back through the entire conversation while streaming continues.
* **Auto-follow** – viewport keeps the newest assistant tokens in view; disabled automatically when you scroll away.
* **Inline formatting helpers**
  * `/wrap 80` – hard-wrap the current draft to 80 columns.
  * `/count` – show character/line statistics.
  * `/format ocaml` – run `ocamlformat` on the draft.
  * `/expand NAME` – insert a predefined snippet (see `Snippet.available ()`).
* **Kill-ring** – `Ctrl-K`, `Ctrl-U`, `Ctrl-W`, `Ctrl-Y` just like Emacs.
* **Selection & clipboard** – toggle selection with `Alt-v`, copy with `Ctrl-C`, cut with `Ctrl-X`.
* **Code generation tools** – Chat-TUI exposes `tool` declarations from the prompt file, enabling function calling (see `chat_response` library).

---

## 4  TUI Commands & Key Bindings

### 4.1  Submitting & Cancelling

| Keys | Action |
|------|--------|
| `Meta+Enter` | Send the current draft to ChatGPT |
| `Esc` | Cancel the running request, or quit if idle |
| `Ctrl-C` / `q` | Quit immediately |

### 4.2  Editing the Draft

Common Emacs/Readline shortcuts – all work inside the multi-line editor:

| Keys | Description |
|------|-------------|
| **Navigation** |
| `← / →` | Move by character |
| `Ctrl / Alt + ← / →` | Move by word |
| `Ctrl-A / Ctrl-E` | Start / end of line |
| `Ctrl+Home / Ctrl+End` | Start / end of message |
| `Ctrl+↑ / Ctrl+↓` | Move cursor up / down a line |
| **Editing** |
| `Backspace` | Delete char before cursor |
| `Ctrl-W` / `Alt+Backspace` | Kill previous word |
| `Ctrl-K` | Kill to end-of-line |
| `Ctrl-U` | Kill to beginning-of-line |
| `Ctrl-Y` | Yank (paste) last kill |
| **Selection** |
| `Alt-v` / `Alt-s` | Toggle selection anchor |
| `Ctrl-C` | Copy selection |
| `Ctrl-X` | Cut selection |
| **Indentation & Duplicates** |
| `Alt+Shift+→ / ←` | Indent / unindent current line |
| `Alt+Shift+↑ / ↓` | Duplicate line above / below |
| **Literal newlines** |
| `Enter` | Insert newline inside draft |

### 4.3  Conversation Pane

| Keys | Action |
|------|--------|
| `↑ / ↓` | Scroll by one line (auto-follow off) |
| `Page Up / Page Down` | Scroll by one page |
| `Home / End` | Top / bottom of history |
| `Ctrl-L` | Force redraw (handy after terminal corruption) |

### 4.4  Slash (`/…`) Commands

Type these inside your draft (usually on the last line) and hit `Meta+Enter`:

| Command | Purpose |
|---------|---------|
| `/wrap N` | Re-flow draft so every paragraph is wrapped at **N** columns |
| `/count` | Print draft statistics (chars & lines) |
| `/format [lang]` | Format code – currently only `ocaml` is recognised |
| `/expand NAME` | Insert named snippet; try `/expand signature` |

---

## 5  Developer Guide

### 5.1  Project Layout

```
bin/
  chat_tui.ml          – tiny entry-point, CLI flag parsing
lib/chat_tui/
  app.ml               – main UI loop (Notty + Eio)
  model.ml             – mutable state container
  renderer.ml          – builds Notty images from Model
  controller.ml        – keyboard handling layer
  stream.ml            – translates OpenAI streaming events → patches
  ... (supporting modules)
```

### 5.2  Building

```bash
$ opam switch create . 5.1.1  # or your preferred OCaml version
$ opam install --deps-only .
$ dune build @all              # compile everything
```

### 5.3  Extending Slash Commands

1. Open `lib/chat_tui/app.ml` and locate `handle_submit ()`.
2. Add a new `else if String.is_prefix user_msg ~prefix:"/yourcmd"` branch. (should be and will be a `match` statement)
3. Use `Model.apply_patch` to show feedback, or schedule side-effects via `Cmd.run`.

### 5.4  Adding Key Bindings

`lib/chat_tui/controller.ml` is the single place that translates `Notty.Unescape.event` → `reaction`:

```ocaml
| `Key (`ASCII 'n', [ `Ctrl ]) ->
    custom_action model;
    Redraw
```

Return one of:
* `Redraw` – model mutated → UI must refresh.
* `Submit_input`, `Cancel_or_quit`, `Quit` – high-level intents handled by `app.ml`.

### 5.5  Rendering

The **Renderer** converts the model into a Notty image.  It’s a pure module so you can unit-test layout decisions without starting a terminal:

```ocaml
let img, _cursor = Renderer.render_full ~size:(80, 24) ~model in
Notty_unix.output_image img;
```

### 5.6  Troubleshooting

* **Keys not recognised?**  Run `dune exec key-dump` to inspect what your terminal sends.
* **Weird redraws after resize?**  Hit `Ctrl-L` to force a full re-render.
* **Streaming hangs?**  Press `Esc` to cancel the current OpenAI request.

---

*Happy chatting!* 🎉

