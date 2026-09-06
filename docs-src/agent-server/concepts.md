# Concepts and execution modes

## Ownership

```text
TUI / custom client / stdio gateway
                │
        Unix socket or HTTP
                │
      protocol dispatcher + authorization
                │
        session actor (one writer)
            ┌───┴──────────────┐
      runtime/workers     durable store
     tools and ChatML    journals/snapshots/artifacts
```

Embedded hosts connect directly to the same core instead of opening a daemon
listener. The TUI renders a client projection; it does not own the daemon's turn
loop. Workers execute outside the serialized actor and return results to it.
Subscribers have bounded queues: a slow reader is disconnected rather than
blocking every other reader or the agent.

## Three independent lifetimes

- **Connection**: one transport/logical client channel. It can have multiple
  session attachments. HTTP uses a connection ID across requests.
- **Liveness**: detached agents outlive clients; owner-bound agents use renewable
  ownership and disconnect grace; process-bound agents live inside their host.
- **Persistence**: durable state can be loaded after restart; transient state is
  disposable. Neither persists an executing process or OCaml continuation.

| Host | Workspace | State and lifetime | Entry point |
|---|---|---|---|
| Native local TUI | Launch cwd | Transient, process-bound | `chat-tui --local -file FILE` |
| Local stdio | Cwd or `--workspace` | Process-bound; durable with `--data-root`, otherwise transient | `ochat-agent-stdio --local --prompt FILE` |
| Daemon-connected TUI | Configured catalog workspace | Durable; detached by default on creation, optional owner-bound | `chat-tui --connect URI ...` |
| Daemon stdio gateway | Selected through protocol | Daemon session lifetime; gateway EOF only drops its connection | `ochat-agent-stdio --connect URI` |
| HTTP/Unix client | Selected through protocol | Daemon session lifetime | Initialize, create/attach |
| OCaml embedding | Host-supplied | Host-selected supported options | `Agent_server.Embedded.start` / `Daemon.start` |
| Legacy local TUI | Launch cwd | Older file-backed session options | Implicit local mode with compatibility flags |

Native local TUI is also the default without mode-selecting compatibility flags.
For standalone local stdio, the current binary needs the
[documented private-data-root workaround](troubleshooting.md#local-stdio-rng-initialization)
for transient-root RNG startup.
`--session`, `--new-session`, `--export-file`, `--no-persist`, `--auto-persist`,
parallel-tool flags and `--authorize-shell-manifest` select the older implicit
local path. They cannot be combined with explicit `--local`. Daemon `--session`
instead selects a daemon session. See the [TUI CLI](../bin/chat_tui.doc.md).

## Prompts, workspaces, and authority

A prompt catalog entry names a ChatMD source and an allowed set of workspace
entries. A session pins a prompt revision and resolved workspace. Workspace
selection sets `${workspace}`; it does not grant filesystem or network access.
Tools, shell manifests, permission profiles, and host policy determine authority.
The operator must avoid unintended conflicts between allowed prompt/workspace pairs.

ChatMD is the authoring/interchange format. Daemon journals, snapshots, prompt
artifacts, blobs, security state and indexes are the authoritative persisted state.
Exporting ChatMD is not backing up that state. Recovery classifies uncertain
in-flight effects; it does not promise exactly-once execution of external tools.

## Protocol versus MCP

Ochat's agent protocol is not MCP. A generic MCP client cannot connect to an Ochat
stdio gateway just because both use JSON. MCP tools in ChatMD are maintained
outbound integrations. The old MCP server that publishes ChatMD prompts is a
separate deprecated compatibility host.
