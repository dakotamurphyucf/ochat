# Context_compaction.Config

Configure compaction for the process that owns the agent: the local TUI process
or the daemon, not a remote client's environment.

## Configuration

Create a private JSON file at one of these locations:

1. `$XDG_CONFIG_HOME/ochat/context_compaction.json`, or
   `$HOME/.config/ochat/context_compaction.json` when XDG_CONFIG_HOME is unset.
2. `$HOME/.ochat/context_compaction.json`.

The first valid, readable file wins. Reads use Eio capabilities. Missing,
unreadable, malformed, duplicate-key or invalid-value files are skipped; if none
is valid, defaults apply. Unknown keys are ignored. Cancellation propagates.

```json
{
  "context_limit": 20000,
  "relevance_filtering": false,
  "relevance_threshold": 0.5
}
```

- `context_limit`: positive integer; maximum **estimated resulting history
  tokens**. The estimate encodes each serialized OpenAI item with o200k_base
  and adds eight tokens per item. It is a local sizing rule, not a guarantee
  of the provider's full request token count, image cost, or model context limit.
- `relevance_filtering`: boolean, default false. Enabling it grades groups before
  summarization and can add paid provider requests. It is not needed for normal
  compaction.
- `relevance_threshold`: finite number from 0 to 1, default 0.5. Used only when
  relevance filtering is enabled. Groups scoring below it are omitted from
  the summarizer input, except policy-containing groups and the latest group.

Tool calls and their outputs are kept together for relevance selection.
Filtering and summarization are lossy. A budget failure returns an error without
installing replacement history; instructions are not silently truncated to fit.

## API

`Config.load ~env ()` reads the search paths above. `Config.load ()` returns
defaults without filesystem access. `load_paths ~env paths` provides explicit
paths for embedders/tests. `is_valid` checks a constructed configuration.
`Compact_config` retains the `default` and `load` compatibility aliases.

The [compactor](compactor.doc.md) uses this configuration automatically.
[Relevance_judge](relevance_judge.doc.md) is also available independently.

Sources: [interface](../../lib/context_compaction/config.mli),
[implementation](../../lib/context_compaction/config.ml).
