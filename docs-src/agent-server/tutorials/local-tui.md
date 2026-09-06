# Run the TUI without a daemon

Follow [build and provider setup](../quickstart.md) first. From the repository root:

```sh
dune exec bin/chat_tui.exe -- --no-config --local \
  -file docs-src/examples/agent-server/prompts/hello.chatmd
```

The prompt contains no initial user request, so merely opening it does not ask
the model a question. Enter a message in insert mode, then use the submission
binding in the [TUI guide](../../guide/chat_tui.md). On macOS the manual checks
used Option+Enter. Confirm that streaming ends and the busy status clears.
Exit insert mode with Esc and quit with `:q`; the shell should return normally.

To use a different project's cwd as `${workspace}`, launch the installed
`chat-tui` after changing into that directory and give an absolute prompt path:

```sh
chat-tui --no-config --local -file /absolute/path/to/agent.chatmd
```

The path above is a placeholder. There is no native local `--workspace` or
`--data-root` TUI option. `${tool_dir}` is this launch directory, while
`${prompt_dir}` is the prompt's directory. Imports have their own `${source_dir}`.

## Persistence and compatibility

Native local TUI has process-bound transient state. Quit ends the host; it does
not turn it into a background daemon. Use a detached daemon session when it must
outlive the terminal. Local stdio or the embedding API can use a persistent data
root without a daemon, but remain process-bound.

Older file-backed sessions remain available through compatibility flags, without
explicit `--local`. For example (replace the prompt path):

```sh
chat-tui --no-config -file /absolute/path/to/agent.chatmd --new-session
chat-tui --no-config --list-sessions
```

Use IDs from that legacy listing only with the corresponding legacy commands.
Do not confuse them with daemon session IDs. Legacy `--authorize-shell-manifest`
also selects that compatibility path. See [shell host integration](../../guide/chatmd-shell-host-integration.md).

## Config precedence

TUI configuration may come from `OCHAT_CHAT_TUI_CONFIG` or XDG config locations.
It contributes arguments before final mode normalization. `--no-config` makes
these tutorials reproducible. `--print-effective-args` helps diagnose unexpected
mode selection; review output for sensitive local paths before sharing it.
See the [CLI reference](../../bin/chat_tui.doc.md) for exact flags and conflicts.
