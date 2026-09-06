# ochat-agent-stdio – local host and daemon gateway

`ochat-agent-stdio` exposes the Ochat agent protocol as newline-delimited JSON
on standard input and output. Diagnostics are written only to standard error.

## Local mode

Current checkout: supply a private `--data-root` for the working standalone
path. Omitting it hits the [RNG startup limitation](../agent-server/troubleshooting.md#local-stdio-rng-initialization).

```console
$ ochat-agent-stdio --local --prompt ./prompts/coding.chatmd \
    --workspace /work/project --data-root /work/agent-data
```

Local mode hosts the shared embedded session engine. The process owns the
session lifetime; input EOF closes the protocol connection and shuts down the
embedded host.

## Daemon gateway mode

```console
$ ochat-agent-stdio --connect unix:///run/user/1000/ochat.sock
$ ochat-agent-stdio --connect https://agents.example.test \
    --bearer-token-file ./agent.token
```

Gateway mode parses each input envelope, forwards typed requests through the
common daemon client, and writes responses plus asynchronous notifications
through one bounded stdout writer. It supports Unix sockets and HTTP POST/SSE.
Input EOF closes the client connection and detaches its attachments; detached
daemon sessions continue running.

All protocol methods are forwarded, including bounded `blob.read` requests.
This lets an stdio client download a server-owned export without receiving or
interpreting a server filesystem path. Clients must verify the advertised
length and SHA-256 before installing the result.

Bearer-token files are loaded through Eio. Tokens must be nonempty and contain
no whitespace or control characters. Token contents are not placed in endpoint
descriptions or errors, and bearer credentials are rejected for Unix sockets.

## Complete client integration

See [stdio framing and lifecycle](../agent-server/transports/stdio.md) and the
[runnable tutorial](../agent-server/tutorials/stdio-client.md). Initialize before
catalog/session requests, retain request IDs, and handle notifications while
reading responses. Local mode creates its initial session; list and attach to it.
There is no gateway `--session` flag: use protocol selection. Local-only options
cannot be combined with `--connect`; `--data-root` changes persistence, not EOF
liveness. Use `-help` for the complete accepted CLI. The stock input line limit
is 16 MiB and outgoing queue capacity is 1024.
