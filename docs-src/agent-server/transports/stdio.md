# Stdio transport

## Local process-bound host

```sh
ochat-agent-stdio --local --prompt /absolute/path/agent.chatmd \
  --workspace /absolute/path/project --data-root /absolute/private/session-store
```

Paths are examples, not built-in locations. Workspace defaults to launch cwd.
Relative local paths resolve there; `${tool_dir}` remains launch cwd. `--data-root`
enables durable storage, but stdin EOF/process exit still ends this host. Without
it, the intended embedded path allocates transient private data, but the current
stock binary has an [RNG startup bug](../troubleshooting.md#local-stdio-rng-initialization)
on that path. Use the explicit private root shown above until it is fixed.
The host creates its
initial session; initialize and call `session.list`, then attach to the returned
session. It is not necessary to create a second session to begin using it.

## Daemon gateway

```sh
ochat-agent-stdio --connect unix:///absolute/private/agent.sock
ochat-agent-stdio --connect http://127.0.0.1:8787 --bearer-token-file /absolute/private/client.token
```

The gateway doesn't own the agent. Choose sessions with protocol methods; there
is no gateway `--session` flag. `--prompt`, `--workspace`, and `--data-root` are
local-only. Bearer-token-file is only accepted for an HTTP endpoint. Exactly one
of `--local` and `--connect` is required.

## Subprocess integration

1. Spawn with separate stdin/stdout/stderr pipes. Do not merge stderr into stdout.
2. Send and flush one complete NDJSON initialization envelope. Keep stdin open.
3. Correlate response IDs and verify negotiated version/features/limits.
4. Discover catalogs or the initial local session. Create/attach as appropriate.
5. Run independent bounded readers for notifications and diagnostic stderr while
   sending requests. One response is not necessarily the next output line.
6. Handle permission requests only through an authorized read/write attachment;
   an already-pending request without expiry can remain after an approver leaves.
   New generic `ask` invocations with no responder take the configured fallback.
7. On reconnect, use durable event replay or replace the projection from snapshot.
8. Close stdin deliberately when done. Local EOF stops the host; gateway EOF
   detaches. Owner-bound daemon liveness then follows lease/grace policy.

The stock binary limits input lines to 16 MiB, outgoing entries to 1024 and local
connection attachments to 64. Treat limits as rejection/backpressure boundaries,
not a promise to buffer arbitrary model output. Responses and notifications are
JSON-RPC-style envelopes; this is not MCP or LSP `Content-Length` framing.

See [the tutorial](../tutorials/stdio-client.md), [protocol](../protocol.md), and
[binary reference](../../bin/ochat_agent_stdio.doc.md).
