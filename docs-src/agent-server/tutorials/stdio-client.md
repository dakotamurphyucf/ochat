# Integrate a stdio client

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

Current checkout limitation: standalone local stdio without `--data-root` can
fail before initialization with “The default generator is not yet initialized.”
Transient-root allocation requests a random ID before the binary initializes
the RNG. The command above avoids that path by supplying a private durable root;
it is deliberately not a transient-session example. See
[startup troubleshooting](../troubleshooting.md#local-stdio-rng-initialization).

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
