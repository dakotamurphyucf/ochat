# Commands and executables

Start with `chat-tui` for a local agent. Other commands provide file-backed
runs, optional hosting, retrieval, and development helpers. These are installed
executable names; from the checkout you can also use the corresponding Dune
targets described in each reference.

| Command | Purpose | Reference |
|---|---|---|
| `chat-tui` | Native local, daemon-connected, or legacy file-backed terminal UI | [CLI](chat_tui.doc.md), [controls](../guide/chat_tui.md) |
| `ochat` | File-backed completion, shell management, indexing, and other workflows | [Command overview](main.doc.md), [completion](../cli/chat-completion.md), [shell management](../cli/shell-runtime-management.md) |
| `ochat-agent-server` | Durable daemon over private Unix sockets and optional HTTP | [Server CLI](ochat_agent_server.doc.md) |
| `ochat-agent-stdio` | Local subprocess agent host or daemon gateway | [Stdio CLI](ochat_agent_stdio.doc.md) |
| `md-index` / `md-search` | Build and query Markdown documentation indexes | [Indexer](md_index.doc.md), [search](md_search.doc.md) |
| `odoc-index` / `odoc-search` | Build and query OCaml API documentation indexes | [Indexer](odoc_index.doc.md), [search](odoc_search.doc.md) |
| `mp-refine-run` | Generate, score, and refine prompts | [CLI](mp_refine_run.doc.md), [library guide](../lib/meta_prompting.doc.md) |
| `mcp_server` | Deprecated ChatMD prompt-serving compatibility | [Compatibility reference](mcp_server.doc.md) |
| `dsl_script` | Experimental embedded ChatML demonstration | [Demo reference](dsl_script.doc.md) |
| `key-dump` | Diagnose terminal key sequences | [Reference](key_dump.doc.md) |
| `highlight-debug` | Inspect TextMate token scopes | [Developer utilities](developer-utilities.md#highlight-debug) |
| `terminal_render` | Render a local bitmap using terminal color blocks | [Developer utilities](developer-utilities.md#terminal_render) |

The `ochat` utility group also includes `tokenize` and `html-to-markdown`;
see the [main command reference](main.doc.md). Markdown/odoc indexers are
separate executables, not `ochat md-index` or `ochat md-search` subcommands.
See [source-only demos](developer-utilities.md#source-only-and-historical-demos)
before trying older binary documentation.

Use the selected executable's help for current flags. Do not combine legacy
TUI session or manifest-authorization flags with explicit native `--local`;
do not use legacy store commands with daemon session IDs. The
[host-mode guide](../agent-server/concepts.md) explains those boundaries.

For task-oriented instructions, see [examples](../examples/README.md),
[search setup](../guide/search-and-indexing.md), or the
[documentation home](../README.md).
