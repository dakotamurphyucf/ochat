# chat-tui command reference

The installed name is `chat-tui`; in a checkout use
`dune exec bin/chat_tui.exe -- ...`. See the [interactive guide](../guide/chat_tui.md)
for keys and views and [host concepts](../agent-server/concepts.md) for lifetimes.

## Synopsis

```text
chat-tui --no-config --local -file FILE
chat-tui --no-config --connect URI --new-daemon-session --prompt NAME --workspace NAME [--detached | --owner-bound]
chat-tui --no-config --connect URI --session ID [--read-only | --owner-bound]
chat-tui --no-config --connect URI --list-sessions
```

A Unix URI is `unix:///absolute/path.sock`; HTTP(S) requires the relevant
`--bearer-token-file FILE`. Connected creation accepts configured names;
raw protocol clients must discover catalog IDs.

## Modes and flag constraints

| Flags | Meaning / restrictions |
|---|---|
| `-file FILE` | Local prompt; default path is `./prompts/interactive.md`, which need not exist in a clean checkout. Supply a tracked/user-created file. |
| `--local` | Native transient process-bound host; workspace is launch cwd. No workspace/data-root override here. |
| `--connect URI` | Daemon mode; incompatible with local runtime/persistence flags. |
| `--new-daemon-session --prompt NAME --workspace NAME` | Create and attach; do not combine with existing `--session`. |
| `--detached` | Explicit creation liveness; also the connected creation default. |
| `--owner-bound` | Create/attach with exclusive renewable owner lease. Incompatible with detached/read-only. |
| `--disconnect-grace-ms MS` | Nonnegative owner grace; default 30000. |
| `--read-only` | Observe an existing session without write authority. Credential read scopes still apply. |
| `--bearer-token-file FILE` | Connected HTTP token loaded privately with Eio; rejected for Unix endpoints. |
| `--session ID` | Connected: daemon ID. Implicit local: legacy store ID and mode. |
| `--new-session` | Legacy local UUID session; not daemon creation. |
| `--export-file FILE` | Legacy interactive transcript export. |
| `--auto-persist`, `--no-persist` | Legacy interactive snapshot persistence controls. |
| `--parallel-tool-calls`, `--no-parallel-tool-calls` | Legacy interactive execution controls, not native/connected runtime overrides. |
| `--authorize-shell-manifest` | Legacy interactive exact-manifest authorization for this process. |
| `--textmate-grammar FILE` | Repeatable additional grammar files for interactive rendering. |

The legacy-only flags select the implicit legacy path if `--local` is absent.
Explicit `--local` rejects them. Do not silently combine mode examples.

## Administration

Use one selector per command. Without `--connect`, session listing/info/export/
reset/rebuild operate on the legacy store, not native transient sessions.

| Selector | Relevant flags and behavior |
|---|---|
| `--list-sessions` | `--format human|tsv|json` or `--json`; selected host's sessions. |
| `--session-info ID` | Inspect selected host; output format controls apply. |
| `--start-session ID` | Connected only; start stopped session. |
| `--stop-session ID` | Connected only; graceful by default, `--cancel` cancels active work. |
| `--delete-session ID` | Connected stopped session; removes by default. Use `--archive` for archive policy. Treat removal as destructive. |
| `--reset-session ID` | `--keep-history`; legacy can also replace `--prompt-file FILE`. Connected reset cannot replace prompt. |
| `--rebuild-from-prompt ID` | Rebuild selected session; not ordinary start. |
| `--export-session ID --out FILE` | Export selected session; remote downloads verify blob length/digest before installation. |
| `--dry-run` | Legacy reset/rebuild only; not daemon administration. |
| `-prompt-preview-max N` | Legacy dry-run preview length; 0 means unlimited. |

`--cancel` is valid only with stop; `--archive` only with delete. Connected
administration rejects interactive creation/attachment flags. Legacy subcommands
`sessions list/info/export/reset/rebuild-from-prompt` remain available; do not
assume prepending `--connect` makes those separate subcommand parsers remote.
Use the flag-based connected commands above.

## Configuration and help

Config file arguments come from `OCHAT_CHAT_TUI_CONFIG`, explicit `--config FILE`,
or the XDG `ochat/chat-tui.args` location (fallback `~/.config/ochat/chat-tui.args`).
`--no-config` disables them; `--print-effective-args` displays normalized inputs.
Keep sensitive paths private when sharing output.

`-help`/`--help`, `--help-short`, `-version` and `-build-info` expose discovery
information. Some long-help introductory session descriptions refer to the
legacy host; the mode normalizer and this host-qualified reference govern native
and connected behavior. `ask-ai -query QUERY` is an optional AI help workflow,
not an offline CLI-help replacement.

## Examples

- [Native local TUI](../agent-server/tutorials/local-tui.md)
- [Unix daemon and owner-bound variation](../agent-server/tutorials/unix-daemon.md)
- [HTTP and a transcript-only observer](../agent-server/tutorials/http-client.md)
- [Shell authorization by host](../guide/chatmd-shell-host-integration.md)

## Behaviour

Connected Unix/HTTP sessions restore stable history IDs, drafts and bounded
active-call summaries on reconnect. Completion/cancellation clears transient
activity; recovered snapshots are replacements, not appended duplicate rows.
The Agent page displays nested/fork progress separately from root history.
Committed overlays affect the effective view without overwriting canonical history.

## Programmatic embedding

Use the shared [agent APIs](../agent-server/embedding.md) for new integrations;
the TUI is a rendering client, not the daemon's session owner.

## Exit codes

Successful one-shot operations exit zero; invalid mode/arguments and runtime or
protocol failures exit nonzero with diagnostics. Quitting native local closes the
host. Quitting a connected client detaches and restores the terminal; detached
daemon sessions remain.

## Limitations & notes

Native local persistence and legacy flags are not interchangeable. Owner-bound
loss follows detection/lease/grace rather than immediate process equivalence.
Headless PTY validation does not cover every terminal emulator.

## Shell runtime authorization and management

Use [shell host integration](../guide/chatmd-shell-host-integration.md).
`ochat shell` legacy-store management is not daemon administration.

## Optional typeahead in every TUI mode

See [typeahead setup, defaults, keys and privacy](../guide/chat_tui.md#type-ahead-availability-and-privacy).
The five flags are `--typeahead off|manual|auto`, `--typeahead-model MODEL`,
`--typeahead-history-messages N` (0–3), `--typeahead-debounce-ms N` (100–5000),
and `--typeahead-max-output-tokens N` (1–512).
Defaults are off, gpt-5.6-luna, 0, 200 and 200 respectively.

Example argument-file entries:

```text
--typeahead manual
--typeahead-model gpt-5.6-luna
--typeahead-history-messages 0
```

These settings do not select legacy execution. Explicit CLI values override
the corresponding file values; `--no-config` bypasses the file. The
`--print-effective-args` output includes configured/explicit flags, never the
provider key. Enabling requires a local nonblank `OPENAI_API_KEY` and permits
sending unsent draft text through local `API_URL`, with additional charges.
Daemon credentials are not used for suggestions.
