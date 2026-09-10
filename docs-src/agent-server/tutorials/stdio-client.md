# Integrate a stdio client

Initialize the protocol, discover and attach to a session, then distinguish acknowledgements from streamed completion.

## Prerequisites and command context

Complete [installation](../quickstart.md) and [private example setup](../../examples/agent-server/README.md). Commands run from the repository root with the active opam environment. Local mode owns a process-bound host with an explicit private durable data root; gateway mode requires a running daemon. Discovery is offline. Sending the example message requires provider credentials in the host environment and incurs charges.

Read the current [provider TLS and permission boundaries](../permissions-and-security.md)
before model work or deployment. [Build troubleshooting](../troubleshooting.md)
includes the Apple Silicon/OpenBLAS setup path.


## Local host

Build the binaries and prepare [the private example](../../examples/agent-server/README.md).
Run this interactively; leave stdin open:

```sh
dune exec bin/ochat_agent_stdio.exe -- --local --prompt "$OCHAT_DEMO/hello.chatmd" \
  --workspace "$OCHAT_DEMO/workspace" --data-root "$OCHAT_DEMO/stdio-state"
```

Paste the `protocol.initialize` line from
[discover.ndjson](../../examples/agent-server/clients/discover.ndjson). Once its
response arrives, paste the `session.list` line. The result includes the initial
local session. No model call is needed for these steps.

Send the following envelope after replacing `SESSION_ID` with its actual ID
(this is a template, not the literal request to send):

```json
{"jsonrpc":"2.0","id":"attach","method":"session.attach","params":{"session_id":"SESSION_ID","requested_mode":"read_write","subscribe":true,"idempotency_key":"tutorial-attach-1"}}
```

Retain the returned attachment ID and use it with that session:

```json
{"jsonrpc":"2.0","id":"message","method":"session.send_message","params":{"session_id":"SESSION_ID","attachment_id":"ATTACHMENT_ID","content":{"kind":"plain_text","text":"Say hello briefly.","attachments":[]},"idempotency_key":"tutorial-message-1"}}
```

This message is billable provider work. Its response acknowledges disposition;
completion arrives through events. Reuse a mutation key only to retry the same
payload, not for a new message. See [protocol synchronization](../protocol.md).

EOF ends this local host. The explicit `--data-root` preserves durable records;
it does not keep a process running after the client exits.

Omit `--data-root` to use a private transient root that is removed on EOF.
Embedded startup initializes the RNG before creating that root. The example above
uses a durable root so records remain available after exit; this does not keep
the host running. See [startup troubleshooting](../troubleshooting.md#local-stdio-rng-initialization)
if an older binary reports an uninitialized generator.

## Gateway to a detached daemon

Start [the Unix daemon](unix-daemon.md), then run:

```sh
dune exec bin/ochat_agent_stdio.exe -- --connect "unix://$OCHAT_DEMO/agent.sock"
```

Repeat initialize/list/attach using a daemon session ID. EOF closes the gateway;
list the session from another connection and confirm the detached agent remains.
You can instead supply a loopback HTTP URI and `--bearer-token-file`.

For noninteractive discovery, the complete tracked stream is executable:

```sh
dune exec bin/ochat_agent_stdio.exe -- --connect "unix://$OCHAT_DEMO/agent.sock" \
  < docs-src/examples/agent-server/clients/discover.ndjson
```

This intentionally ends at EOF after discovery; it is not a long-running agent
client. Applications must keep pipes open, flush writes, demultiplex response IDs,
and handle asynchronous notifications. The compiled
[OCaml client](../../examples/agent-server/clients/docs_example.ml) demonstrates
the shared Unix/HTTP connection lifecycle; the stock gateway owns the stdio loop.

## Checkpoint, troubleshooting, and next step

Initialization and session listing should return matching JSON-RPC response IDs; attaching supplies an attachment ID for mutations. A send-message acknowledgement is not a completed assistant response. For missing initialization, ensure stdin remains open, each envelope ends with a newline, and responses are read before dependent requests. EOF ends a local host or disconnects a gateway; detached daemon sessions remain. Stop all relevant processes before archiving/removing your recorded demo root, including stdio-state. Next, [connect over HTTP](http-client.md).
