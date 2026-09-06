# Unix socket transport

The daemon always provides its configured Unix socket. Use an absolute URI such
as `unix:///private/ochat/agent.sock`; the common client can expand `~/` when a home
directory is supplied. Do not use a TCP port or MCP endpoint for this URI.

The socket parent must exist, be owned by the effective user, and not be writable
by others. Startup probes an existing socket before treating it as stale. Another
live owner or store lock is an error, not a reason to remove files manually.
Keep socket paths short enough for the platform's Unix-address limit.

Peer credentials authenticate same-user clients and produce a stable UID-derived
principal. The supported credential lookup is platform-specific; filesystem
permissions and peer validation both matter. A path string itself is not a token.

## Wire contract

One UTF-8 JSON-RPC-style envelope per newline, with a bounded maximum line length.
Initialize first using the [protocol handshake](../protocol.md). Requests have
IDs; responses and asynchronous notifications may interleave. Never match replies
by line position. A transport connection can attach to multiple sessions.

Use the stock stdio gateway to avoid writing a socket adapter:

```sh
ochat-agent-stdio --connect "unix://$OCHAT_DEMO/agent.sock"
```

Paste the first line of the [discovery fixture](../../examples/agent-server/clients/discover.ndjson),
wait for its response, then issue catalog/session requests. EOF closes this
connection, not a detached session. Output is NDJSON on stdout; diagnostics are
on stderr. The [Unix tutorial](../tutorials/unix-daemon.md) covers TUI attachment.

For OCaml clients use `Agent_transport_client.Endpoint.connect`, a scoped Eio
switch and `Agent_client.Connection`; see the [compiled example](../../examples/agent-server/clients/docs_example.ml).
Apply snapshot/replay results on reconnect rather than appending duplicate rows.
