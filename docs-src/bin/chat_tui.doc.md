# chat_tui – Terminal Ochat client

`chat_tui` is a convenience wrapper around the high-level
`Chat_tui.App` module.  It turns the library into an end-user binary
that you can launch from a shell.

---

## 1 Synopsis

```console
$ chat-tui [-file FILE]
           [--list-sessions]
           [--session NAME | --new-session]
           [--session-info NAME]
           [--reset-session NAME [--keep-history] [--prompt-file FILE]]
           [--rebuild-from-prompt NAME]
           [--export-session NAME --out FILE]
           [--export-file FILE]
           [--help-short]
           [--format FORMAT | --json]
           [--dry-run]
           [-prompt-preview-max N]
           [--parallel-tool-calls | --no-parallel-tool-calls]
           [--auto-persist | --no-persist]
```

### Subcommands (new, optional)

In addition to the flag-based interface above (kept for backwards
compatibility), `chat-tui` also offers a subcommand-oriented interface:

```console
$ chat-tui sessions -help
$ chat-tui sessions list [--format tsv|json] [--json]
$ chat-tui sessions info NAME [--format human|tsv|json] [--json]
$ chat-tui sessions export NAME --out FILE
$ chat-tui sessions reset NAME [--keep-history] [--prompt-file FILE] [--dry-run]
$ chat-tui sessions rebuild-from-prompt NAME [--dry-run] [-prompt-preview-max N]
```

### Ask AI subcommand (ask AI questions about using chat-tui):

```console
$ chat-tui ask-ai -query QUERY
```

### Config file (optional)

You can store default flags in a config file and have them applied to every
invocation of `chat-tui` (the command-line still wins over config defaults).

Default path:

* `$XDG_CONFIG_HOME/ochat/chat-tui.args` (when `XDG_CONFIG_HOME` is set)
* `~/.config/ochat/chat-tui.args` (fallback)

Format:

* parsed as whitespace-separated arguments (one or more per line)
* blank lines and lines starting with `#` are ignored

Control:

* disable config: `--no-config`
* override path: `--config FILE`
* env override path: `OCHAT_CHAT_TUI_CONFIG=FILE`
* debug merged defaults: `--print-effective-args`

## 2 Command-line flags

| Flag | Default | Description |
|------|---------|-------------|
| `-file FILE` | `./prompts/interactive.md` | ChatMarkdown / Markdown document that seeds the conversation buffer, declares function-callable tools and stores default settings. |
| `--list-sessions` | *(n/a)* | Enumerate all existing session identifiers along with their prompt file and exit. Cannot be combined with any other session-related flag. Default output is TSV (`id<TAB>prompt_file`); use `--format json` / `--json` for JSON. |
| `--session NAME` | *(derived from `-file`)* | Resume the existing session identified by `NAME`. Errors out if the snapshot is missing. |
| `--new-session` | *(false)* | Force creation of a **fresh** session even if a deterministic one already exists for the chosen prompt file. Mutually exclusive with `--session`. |
| `--session-info NAME` | *(n/a)* | Print metadata (prompt path, last modified timestamp, history length, task count) for the given session and exit. Mutually exclusive with `--session` and `--new-session`. Use `--format json` / `--json` for JSON. |
| `--export-session NAME` | *(n/a)* | Convert the specified session snapshot to a standalone ChatMarkdown file. Requires `--out` and is incompatible with other session flags. |
| `--out FILE` | *(required with `--export-session`)* | Destination file for `--export-session`. Directories are created automatically; existing files trigger an overwrite confirmation prompt. |
| `--export-file FILE` | *(n/a)* | After the interactive session ends, append the full transcript to `FILE` in ChatMarkdown format. Cannot be combined with `--export-session`. |
| `--reset-session NAME` | *(n/a)* | Archive the current snapshot of `NAME` and start a brand new session. Incompatible with most other session flags (see CLI help). |
| `--keep-history` | *(false)* | Retain the existing conversation history when resetting a session (requires `--reset-session`). |
| `--prompt-file FILE` | *(n/a)* | Replace the prompt when resetting a session (requires `--reset-session`). |
| `--rebuild-from-prompt NAME` | *(n/a)* | Recreate the deterministic snapshot for `NAME` from its stored `prompt.chatmd` file and exit. |
| `--parallel-tool-calls` / `--no-parallel-tool-calls` | `--parallel-tool-calls` | Enable / disable concurrent execution of function-callable tools during the conversation. |
| `--auto-persist` / `--no-persist` | prompt | Force or suppress snapshot saving on exit instead of asking interactively. |
| `--help-short` | *(n/a)* | Print a short usage summary and exit (use `--help` for the full manual). |
| `--format FORMAT` | *(mode-dependent)* | Output format for `--list-sessions` / `--session-info`. Supported: `tsv`, `human`, `json` (not all formats apply to both modes). |
| `--json` | *(n/a)* | Alias for `--format json` (for `--list-sessions` / `--session-info`). |
| `--dry-run` | *(false)* | Print what would happen and exit (including a truncated prompt preview when available). If `--keep-history` is set for `--reset-session`, also prints a preview of the kept history items (as ChatMarkdown tags). Supported with `--reset-session` and `--rebuild-from-prompt` (and their `sessions` subcommands). |
| `-prompt-preview-max N` | `2000` | Max chars of prompt preview printed by `--dry-run`. Use `0` for unlimited. |

The following flags are handled before command-line parsing (and therefore may
not show up in `-help` output):

* `--config FILE`
* `--no-config`
* `--print-effective-args`

### Examples

List all sessions:

```console
$ chat-tui --list-sessions
42b1ac08  prompts/interactive.md
5f3c9cc4  prompts/project_x.chatmd
```

Inspect a single session:

```console
$ chat-tui --session-info 42b1ac08
Session: 42b1ac08
Prompt file: /home/alice/prompts/interactive.md
Last modified: 2025-07-25 18:39:02
History items: 57
Tasks: 3
```

Export a session to ChatMarkdown:

```console
$ chat-tui --export-session 42b1ac08 --out exports/interactive_export.chatmd
Session '42b1ac08' exported to exports/interactive_export.chatmd
```

If the file does not exist it is created on exit so you can resume the
session later.

## 3 Behaviour

1. `chat_tui` creates (if needed) the hidden directory `.chatmd` in the
   current working directory.  The directory holds cache files and
   transient artefacts produced by tools.
2. It then calls `Io.run_main` which in turn delegates to
   `Eio_main.run` to bootstrap an event loop.
3. Finally it invokes `Chat_tui.App.run_chat` with the chosen prompt
   file.  Control is handed over to the TUI engine; the wrapper will
   not return until the user quits (`/quit` or *Ctrl-c*).
4. When the **meta-refine** toggle is active (either via the `/meta_refine`
   command or the *Ctrl-r* shortcut) the draft message is first passed through
   [`Recursive_mp.refine`].  The TUI previews the resulting **diff** – additions
   in green, deletions in red – so that you can confirm or cancel before the
   message leaves your machine.

## 4 Programmatic embedding

Applications can reuse the TUI component directly instead of spawning
an external process:

```ocaml
let () =
  Io.run_main @@ fun env ->
  Chat_tui.App.run_chat ~env ~prompt_file:"./prompts/demo.chatmd" ()
```

This is exactly what the `chat_tui` binary does under the hood.

## 5 Exit codes

| Code | Meaning |
|------|---------|
| 0 | Normal termination (user quit). |
| ≠0 | Unhandled exception – inspect the console for the stack-trace. |

## 6 Limitations & notes

* **Single window** – each execution manages one conversation.  Run
  multiple instances for parallel chats.
* **Unix-only** – relies on `Eio_main` and `Notty`, therefore does not
  currently work in JavaScript or MirageOS environments.
* **No live reload** – editing `FILE` while the TUI is running has no
  effect; restart to load the changes.
