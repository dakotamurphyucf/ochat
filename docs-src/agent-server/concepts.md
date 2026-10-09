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
| Native local TUI | Launch cwd, retained workspace on selection | Durable by default, process-bound; explicit transient | `chat-tui --local -file FILE` |
| Local stdio | Cwd or `--workspace` for creation | Durable by default, process-bound; explicit transient | `ochat-agent-stdio --local --prompt FILE` |
| Daemon-connected TUI | Configured catalog workspace | Durable; detached by default on creation, optional owner-bound | `chat-tui --connect URI ...` |
| Daemon stdio gateway | Selected through protocol | Daemon session lifetime; gateway EOF only drops its connection | `ochat-agent-stdio --connect URI` |
| HTTP/Unix client | Selected through protocol | Daemon session lifetime | Initialize, create/attach |
| OCaml embedding | Host-supplied | Host-selected supported options | `Agent_server.Embedded.start` / `Daemon.start` |
| Legacy local TUI | Launch cwd | Older file-backed session options | Implicit local mode with compatibility flags |

Native local TUI is also the default without mode-selecting compatibility flags.
Native local storage defaults to `$HOME/.ochat/agent-store`; `--data-root`
selects an explicit absolute root and `--transient` opts out of retention. Neither
starts a background daemon. Missing HOME has no cwd fallback. Legacy records
are not implicitly migrated. In explicit `--local` mode, `--session ID` selects
an existing native session; `--list-sessions`, `--session-info` and
`--export-session` read the retained native catalog without selection. Without
`--local`, compatibility flags such as `--new-session`, `--export-file`,
`--no-persist`, `--auto-persist` and parallel-tool flags keep the older path.
`--authorize-shell-manifest` supports native
local mode when combined with `--local`; without it, the flag retains legacy
compatibility behavior. Daemon `--session`
instead selects a daemon session. See the [TUI CLI](../bin/chat_tui.doc.md).

## Client handoff: admission and lifetime

Keep the durable session reference, executing host and client attachment separate.
An attachment grants the connection's current permitted access to a session;
it does not identify a new session or transfer the runtime host's credentials.

A successful `session.run.start` reply contains an admission receipt. It proves
that the host accepted that exact request, not that the run completed. A terminal
receipt or retained terminal outcome supplies completion evidence. Receipts do
not confer current execution authority. After a lost reply, reconcile the
original host, principal and idempotency key through `command.receipt`; an
unavailable outcome does not justify another execution with a fresh key. See
[explicit run admission](protocol.md#explicit-run-admission).

A daemon stdio gateway owns its connection, not the daemon host. Gateway EOF
closes that connection and detaches its attachments; it does not shut down the
daemon or finish unrelated sessions. Owner-bound sessions still follow their
configured disconnect grace. A local process-bound host has a separate owning
lifetime: ending that host cannot promise that its background workers continue.
Durable recovery preserves recorded state and classifies interrupted work; it
does not preserve a running process. Clients must distinguish detach, explicit
session controls and host shutdown in their own interface.

Host shutdown closes scheduler admissions and cancels and joins the initial-start
scheduler before retiring session actors. Cancellation preserves an unfinished
durable start intent rather than recording a permanent startup failure caused by
actor retirement. Protected startup and ownership cleanup must finish before the
join completes; this ordering does not impose a separate shutdown timeout on
those protected sections.

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
