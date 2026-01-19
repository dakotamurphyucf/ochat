# chat_tui – interactive terminal client

```console
$ chat-tui -file prompts/interactive.md
```

Think of **chat_tui** as the *interactive face* of your prompt-as-code
workflow: each `.chatmd` file becomes a **self-contained agent** once you
declare a handful of tools.  Need a refactoring bot?  Draft
`prompts/refactor.chatmd`, mount `apply_patch`, `odoc_search` and a custom
`shell_check` wrapper, then open the file in the TUI:

```console
$ chat-tui -file prompts/refactor.chatmd
```

No servers to deploy, no runtime config – the static files plus your shell or
OCaml tool implementations *are* the application.  The same technique scales
from a quick one-off helper up to a fleet of purpose-built agents (release
manager, design-doc auditor, knowledge-base explainer) – each living in its
own `.chatmd` and selectable via `chat-tui`.

Below is a one-page *muscle-memory* cheat-sheet distilled from the daily
usage of the maintainers.  Print it, tape it to the wall, thank us later.

| Mode | Keys (subset) | Action |
|------|---------------|--------|
| **Insert** | *free typing* | edit the draft prompt |
|            | `Meta+Enter` | submit the draft |
|            | `Esc` | switch to **Normal** mode |
|            | `Ctrl-k / Ctrl-u / Ctrl-w` | delete to *EOL* / *BOL* / previous word |
|            | `↑ / ↓ / PageUp / PageDown` | scroll history (disables auto-follow) |
|            | `Meta-v` / `Meta-s` | toggle selection anchor in the draft; combine with `Ctrl-C` / `Ctrl-X` / `Ctrl-Y` |
|            | `Ctrl-R` | toggle between plain markdown and **Raw XML** draft mode |
| **Normal** | `j / k`, `gg / G` | navigate history; jump to top / bottom |
|            | `dd`, `u`, `Ctrl-R` | delete line / undo / redo |
|            | `o / O` | insert line below / above and jump into Insert |
|            | `[` / `]` | move the **selected message** up / down |
|            | `:` | open command-line mode |
| **Cmd (:)** | `:w` | submit the draft (same as `Meta+Enter`) |
|             | `:q`, `:wq` | quit the TUI (export and snapshots are handled on exit) |
|             | `:c`, `:cmp`, `:compact` | compact context (see below) |
|             | `:d`, `:delete` | delete selected message |
|             | `:e`, `:edit` | copy selected message into the editor in Raw XML mode |

Pro tip — use `[` / `]` in Normal mode to select the message you care
about, then `:e` to yank its text into the draft, wrap it in e.g.
`!apply_patch`, and submit with `Meta+Enter`.

For the full keymap (including word motions and selection variants), see
`docs-src/lib/chat_tui/controller.doc.md` and
`docs-src/lib/chat_tui/controller_normal.doc.md`.

Features

* Live streaming of tool output, reasoning deltas & assistant text
* Auto-follow & scroll-history with Notty
* Manual **context compaction** via `:compact` (`:c`, `:cmp`) – summarises older messages when the history grows too large and replaces it with a concise summary, saving tokens and latency.
* Persists conversation under `.chatmd/` so you can resume later

---

## Context compaction (`:compact`)

When a session grows beyond a comfortable token budget you can shrink it
on demand:

```text
:compact          # alias :c or :cmp
```

Compaction can only run when no response is currently streaming; if you
try while the model is still thinking you'll get a small inline error
message and nothing else will change.

Ochat will:

1. Take a *snapshot* of the current history.
2. Score messages based on relevance to the current context via the relevance judge.
3. Pass the most-relevant messages to the summariser which produces a
   compacted version of the conversation. 
4. Replace the original messages *in-place* and update the viewport.
5. Archive the original history in a `<session>/archive` folder


> Tip	When compaction runs under a session, the pre-compaction snapshot
> is archived automatically and the compacted history will be saved when
> you exit `chat-tui`, subject to `--auto-persist` / `--no-persist`.
> `:w` still submits the current draft; it does not control snapshot
> saving.

### Under the hood – how the compaction pipeline works

Calling `:compact` triggers a **four-stage pipeline** implemented in
`lib/context_compaction/`:

| Stage | Module | What happens |
|-------|--------|--------------|
| ① Load config | `Context_compaction.Config` | Reads `~/.config/ochat/context_compaction.json` (or XDG-override).  Missing file → hard-coded defaults `{ context_limit = 20_000 ; relevance_threshold = 0.5 }`. |
| ② Score relevance | `Context_compaction.Relevance_judge` | For **every** message the *Importance judge* asks a small reward-model to rate how indispensable the line is on a scale **0–1**.  No `OPENAI_API_KEY` or network?  It returns the deterministic stub `0.5`, keeping semantics reproducible in CI. |
| ③ Summarise keepers | `Context_compaction.Summarizer` | The messages whose score is ≥ `relevance_threshold` are passed to GPT-4.1 (or an offline stub) together with a purpose-built system prompt.  The model then writes a rich summary |
| ④ Rewrite history | `Context_compaction.Compactor` | The function returns a **new transcript** that contains the original *first* item (usually the `<system>` prompt) **plus** *at most one* extra `<system-reminder>` message that embeds the summary.  If anything blows up along the way the original history is returned verbatim – the feature can never brick the session. |

Configuration snippet

```jsonc
// ~/.config/ochat/context_compaction.json
{
  "context_limit": 10000,          // tighten character budget
  "relevance_threshold": 0.7       // be more aggressive when pruning
}
```

Programmatic use

```ocaml
let compacted =
  Context_compaction.Compactor.compact_history
    ~env:(Some stdenv)   (* pass Eio capabilities when network access is OK *)
    ~history:full_history
in
send_to_llm (compacted @ new_user_turn)
```


**Self-serve checklist – 10 seconds to first answer**

1. `chat-tui -file prompts/blank.chatmd` – starts in *Insert* mode with an empty history.
2. Type *“2+2?”*, hit **⌥ ↵** (Meta+Enter) → an O-series model replies *“4”*.
3. Type `:` then `q` and press **Enter** – this quits the TUI; on exit `chat-tui` exports the conversation to `prompts/blank.chatmd` (unless you decline the export prompt).

### Power-user workflow – *code-edit-test* in one window

That’s it – *no* OpenAI dashboard visit, *no* shell scripts.  Everything, including model name and temperature, is stored in the document you can now commit to Git.


Programmatic embedding:

```ocaml
Io.run_main @@ fun env ->
  Chat_tui.App.run_chat ~env ~prompt_file:"prompts/interactive.md" ()
```

## Advanced behaviours

### Out-of-band notes while streaming

If a response is currently streaming and you submit the draft again
(either with `Meta+Enter` or via `:w` in command mode), `chat-tui` does
not queue a new visible user turn. Instead it injects the text as a
*Note From the User* into the in-flight request so the model can take it
into account mid-stream.

### Draft modes and Raw XML

`Ctrl-R` toggles the draft between plain markdown and **Raw XML**. In Raw
XML mode the editor contents are interpreted as low-level ChatMarkdown
XML. The `:e` / `:edit` command uses this to copy the selected message
into the editor and switch to Raw XML so you can tweak tool calls or tags
before resubmitting.

### Tuning redraw and streaming

Two environment variables let you tune responsiveness:

- `OCHAT_TUI_FPS` – target frames-per-second for redraw throttling
  (default 30; values ≤ 0 are clamped to 1).
- `OCHAT_STREAM_BATCH_MS` – batch window in milliseconds for streaming
  events (1–50, default around 12 ms). Smaller values make the UI more
  responsive at the cost of more redraws.

---

## 📑 Persistent sessions – pause, resume & branch your chats

The `chat-tui` executable (installed by `opam install ochat`; run as
`dune exec chat_tui --` from the repo) now **persists the full conversation state automatically** under
`$HOME/.ochat/sessions/<id>` so you can close the terminal, pull the latest
commit and pick up the thread days later – tool cache and all.

Key facts at a glance:

* A *session* captures:  
  • the prompt that seeded the run (a copy is stored as `prompt.chatmd`)  
  • the complete message history (assistant, tool calls, reasoning deltas…)  
  • the per-session tool cache (`.chatmd/cache.bin`)  
  • misc metadata (task list, virtual-FS root, user-defined key/value pairs)

* Snapshots live in a single binary file `snapshot.bin` alongside the prompt
  copy – easy to back-up, copy or sync.

* When you open a prompt without explicit flags `chat-tui` hashes the prompt
  path and resumes the matching snapshot if present – **zero-config resume**.

CLI flags (all mutually-exclusive where it makes sense):

| Flag | Action |
|------|--------|
| `--list-sessions` | enumerate `(id\t<prompt_file>)` of every stored snapshot |
| `--session <ID>` | resume the given session (fails if it doesn’t exist) |
| `--new-session`  | ignore any existing snapshot for that prompt and start fresh |
| `--session-info <ID>` | print metadata (history length, timestamps, prompt path) |
| `--reset-session <ID>` | archive the current snapshot (timestamped) and restart; combine with `--keep-history` or change `--prompt-file` |
| `--rebuild-from-prompt <ID>` | delete history & cache, rebuild snapshot from the stored prompt copy – perfect after editing `prompt.chatmd` manually |
| `--export-session <ID> --out FILE` | convert a snapshot plus attachments to a standalone `.chatmd` document |
| `--parallel-tool-calls` / `--no-parallel-tool-calls` | enable or disable concurrent execution of tool calls during a run (default: parallel tool calls are enabled) |
| `--auto-persist` / `--no-persist` | control whether session snapshots are saved on exit (`--auto-persist` = always save without prompting; `--no-persist` = never save) |

Interactive workflow examples:

```console
# 1️⃣  Enumeration
$ chat-tui --list-sessions
6f9ab3d5  prompts/interactive.md
a821c9f0  prompts/refactor.chatmd

# 2️⃣  Resume last week’s debugging chat
$ chat-tui --session 6f9ab3d5

# 3️⃣  Branch off a clean slate (keeps the old snapshot untouched)
$ chat-tui -file prompts/interactive.md --new-session

# 4️⃣  Export a finished session to share with teammates
$ chat-tui --export-session a821c9f0 --out docs/refactor_walkthrough.chatmd

# 5️⃣  Reset but keep the conversation history and switch prompt
$ chat-tui --reset-session a821c9f0 --keep-history --prompt-file prompts/new_spec.md
```

`--auto-persist` saves on exit without confirmation; `--no-persist` drops
changes – useful in CI or when you want a quick throw-away run.

Under the hood **Session_store** migrates old snapshots transparently,
maintains advisory locks to prevent concurrent writes, and provides helpers
surfaced by the flags above.  Snapshot writes happen inside an Eio fiber so
the UI never blocks.

➡️  See `lib/session_store.mli` for the authoritative API contract.

---

## 🔁 Recursive meta-prompting – automate **prompt refinement**

Ochat now ships a **first-class prompt-improvement loop** powered by
_recursive meta-prompting_ and exposed via the `mp_refine_run` helper.
This is a separate CLI (not currently wired into `chat-tui`); run it as
`mp_refine_run` (or `dune exec mp_refine_run --` from the repo).
Give it

1. a **task** (what the prompt should accomplish) and
2. an optional **draft prompt**,

and it will iterate:

• generate *k* candidate prompts with an **O-series model** (e.g., `o3`),  
• score them via an OpenAI reward-model,  
• select the best using a Thompson bandit, and  
• stop when the score plateaus or the iteration budget is exhausted.

The refined prompt is printed to *stdout* or appended to a file – perfect for
CI pipelines where prompts live under version control.

CLI flags at a glance:

| Flag | Purpose |
|------|---------|
| `-task-file FILE` *(required)* | Markdown file describing the task |
| `-input-file FILE` | Existing prompt to refine (omit to start from scratch) |
| `-output-file FILE` | Append the result instead of printing to *stdout* |
| `-action generate\|update` | Create a new prompt or mutate an existing one |
| `-prompt-type general\|tool` | Assistant prompt vs tool description |
| `-meta-factory BOOL` | Use the pure, offline Prompt Factory to create/iterate a prompt pack (non-destructive). Default: `false`. |
| `-meta-factory-online BOOL` | Enable the online Prompt Factory strategy inside the refinement loop and for greenfield prompts. Default: `true`. |
| `-classic-rmp BOOL` | Force the classic recursive meta-prompting strategy (disables `-meta-factory-online`). Default: `false`. |

Quick examples:

```console
# 1️⃣  Draft a brand-new assistant prompt
$ mp_refine_run -task-file tasks/summarise.md

# 2️⃣  Improve an existing tool schema and persist the update
$ mp_refine_run \
    -task-file tasks/translate_task.md \
    -input-file  prompts/translate_draft.md \
    -output-file prompts/translate_refined.md \
    -action      update \
    -prompt-type tool

# 3️⃣  Generate using the offline Prompt Factory (pure, no network)
$ mp_refine_run -task-file tasks/summarise.md -meta-factory true

# 4️⃣  Disable online factory and run the classic loop only
$ mp_refine_run -task-file tasks/summarise.md -classic-rmp true
```

All heavy-lifting lives under `lib/meta_prompting` – functors, evaluators,
bandit logic and convergence checks.  The CLI is a thin wrapper around
`Mp_flow.first_flow`/`Mp_flow.tool_flow`; have a look at
`bin/mp_refine_run.ml` or the annotated API docs in
`lib/meta_prompting/mp_flow.mli` for the full story.

### New: Prompt Factory – offline and online

The meta‑prompting subsystem now includes a Prompt Factory that can either:

* run **offline** (pure, deterministic) via `Meta_prompting.Prompt_factory` to create or iterate self‑contained prompt packs, or
* run **online** (LLM‑backed) via `Meta_prompting.Prompt_factory_online` to propose revised prompts and full packs using OpenAI’s Responses API.

How it ties into the CLI

* `-meta-factory` switches the run to the offline factory (no network) and returns a prompt pack or a minimal update pack.
* With `-meta-factory` off, `-meta-factory-online` (default: true) augments the refinement loop with online proposals and is also used for greenfield generation when no `-input-file` is supplied.
* `-classic-rmp` disables the online factory entirely and runs the original recursive loop.

Programmatic note

`Mp_flow.first_flow` and `Mp_flow.tool_flow` accept `?use_meta_factory_online:bool` to enable or disable the online strategy from code (default: enabled).

Customization

When using the online factory you can override the default templates by placing files under `meta-prompt/templates/` and guard‑rails under `meta-prompt/integration/`. The `meta-prompt/` folder is git‑ignored by default.

