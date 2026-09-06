# HTTP client walkthrough

## Start an isolated authenticated listener

Prepare [the private examples](../../examples/agent-server/README.md). Stop any
Unix-only daemon using that root, then in Terminal A:

```sh
dune exec bin/ochat_agent_server.exe -- -config "$OCHAT_DEMO/http.sexp" -validate-only
dune exec bin/ochat_agent_server.exe -- -config "$OCHAT_DEMO/http.sexp"
```

This binds 127.0.0.1:8787 and uses generated hashed credentials. If the port is
occupied, choose another port in the private config and all client URIs; do not
terminate an unrelated process. Keep `admin.token` and `observer.token` private.

## Use the maintained client adapter

Terminal B can create a durable session directly through the TUI:

```sh
dune exec bin/chat_tui.exe -- --no-config --connect http://127.0.0.1:8787 \
  --bearer-token-file "$OCHAT_DEMO/admin.token" --new-daemon-session \
  --prompt hello --workspace project --detached
```

Or use the raw-envelope gateway over HTTP:

```sh
dune exec bin/ochat_agent_stdio.exe -- --connect http://127.0.0.1:8787 \
  --bearer-token-file "$OCHAT_DEMO/admin.token"
```

Initialize using [discover.ndjson](../../examples/agent-server/clients/discover.ndjson),
then list/attach as in the [stdio tutorial](stdio-client.md). The transport adapter
handles HTTP connection IDs and the connection notification SSE reader.
The compiled `docs_example client` demonstrates the same embedding API.

## Raw HTTP integration

With `curl` installed, the setup helper supplies a validated handshake and a
private curl credential config. Do not use verbose/header tracing with secrets:

```sh
curl --fail-with-body --silent --show-error --config "$OCHAT_DEMO/admin.curl" \
  -D "$OCHAT_DEMO/initialize.headers" \
  -H 'Content-Type: application/json' -H 'ochat-protocol-version: 1.0' \
  --data-binary "@$OCHAT_DEMO/initialize.json" http://127.0.0.1:8787/v1/rpc
OCHAT_CONNECTION=$(awk 'tolower($1)=="ochat-connection-id:" {gsub("\r", "", $2); print $2}' "$OCHAT_DEMO/initialize.headers")
curl --fail-with-body --silent --show-error --config "$OCHAT_DEMO/admin.curl" \
  -H 'Content-Type: application/json' -H 'ochat-protocol-version: 1.0' \
  -H "ochat-connection-id: $OCHAT_CONNECTION" \
  --data '{"jsonrpc":"2.0","id":"sessions","method":"session.list","params":{"limit":20}}' \
  http://127.0.0.1:8787/v1/rpc
```

In another terminal (with the same variables), observe connection notifications:

```sh
curl --no-buffer --silent --show-error --config "$OCHAT_DEMO/admin.curl" \
  -H "ochat-connection-id: $OCHAT_CONNECTION" http://127.0.0.1:8787/v1/events
```

That stream stays open until interrupted/closed; it may contain no session updates
until this logical connection subscribes through create/attach. For an independent
session observer after setting `OCHAT_SESSION` from the session list:

```sh
curl --fail-with-body --silent --show-error --config "$OCHAT_DEMO/observer.curl" \
  "http://127.0.0.1:8787/v1/sessions/$OCHAT_SESSION/snapshot"
```

Read `latest_event_sequence` from the snapshot into `OCHAT_SEQUENCE`, then:

```sh
curl --no-buffer --silent --show-error --config "$OCHAT_DEMO/observer.curl" \
  "http://127.0.0.1:8787/v1/sessions/$OCHAT_SESSION/events?after_sequence=$OCHAT_SEQUENCE"
```

After interrupting streams, close the operator logical connection deliberately:

```sh
curl --fail-with-body --silent --show-error --config "$OCHAT_DEMO/admin.curl" \
  -X DELETE -H "ochat-connection-id: $OCHAT_CONNECTION" http://127.0.0.1:8787/v1/connection
```

For an implementation independent of Ochat's client library:

1. Load the raw token from its private file without logging it.
2. POST the fixture's initialization envelope to `/v1/rpc` with bearer auth,
   JSON content type, and `ochat-protocol-version: 1.0`.
3. Capture `ochat-connection-id` from the response headers. Supply it on subsequent
   RPCs, `/v1/events`, and `DELETE /v1/connection`.
4. Open `/v1/events` for notifications or use the per-session snapshot/event
   routes for a separate read-only projection. Do not give connection notifications
   a durable replay guarantee they do not have.
5. Discover prompt/workspace IDs, create or attach a session, and retain attachment
   IDs for mutations. Follow the [protocol reference](../protocol.md), not the
   friendly catalog names used by TUI convenience flags.
6. Correlate response IDs while receiving events. A message acknowledgement can
   say deferred; wait for terminal operation/history events to report completion.

The complete [route/header and SSE reference](../transports/http.md) specifies
error handling, body/batch limits, and payload boundaries. Implement SSE parsing
at frame boundaries, not network chunks. Use a library that supports incremental
streaming rather than reading the entire SSE body before processing it.

## Second observer and reconnect

Quit the writer TUI, list sessions with its operator token, and assign the returned
ID to `OCHAT_SESSION`. In a second terminal:

```sh
dune exec bin/chat_tui.exe -- --no-config --connect http://127.0.0.1:8787 \
  --bearer-token-file "$OCHAT_DEMO/observer.token" --session "$OCHAT_SESSION" --read-only
```

The observer token has only transcript-read scope. Finalized transcript updates
are visible; protected tool/permission/provider-delta detail is not. Attempts to
write are rejected. A separate read-only attachment with the admin token would
still have that token's broader read scopes.

The generated operator and observer tokens represent the same principal with
different scopes. A token for a different principal cannot view this session
merely by holding transcript-read scope. Regenerate fixtures created by the old
helper that assigned separate principals. Each TUI initializes its own HTTP
connection; do not pass an operator connection ID to the restricted observer.

For a raw session subscriber, remember the latest durable sequence, disconnect,
then request events after that sequence. On `snapshot_required`, fetch/replace
the snapshot and retry after its new sequence. Do not reuse a connection ID after
server restart; initialize again. Unsent editor drafts belong to the client and
should not be resent automatically as new mutations during reconnect.

Close logical connections deliberately and quit clients before Ctrl+C in
Terminal A. Detached sessions remain in the private store for the next startup.
No public listener or real-model request is required to test discovery/attachment.
