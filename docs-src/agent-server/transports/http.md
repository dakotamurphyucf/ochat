# HTTP RPC, SSE, snapshots, and blobs

The stock listener is plain HTTP; use private loopback by default. An HTTPS
endpoint is supported by the client when a properly configured external TLS
endpoint fronts the daemon. No WebSocket or automatic browser/CORS integration
is promised. This is Ochat's protocol, not the legacy MCP HTTP endpoint.

## Authentication and logical connections

Every route authenticates the request. Use exactly one `Authorization: Bearer
TOKEN` header. The token has no whitespace. Static records contain SHA-256
digests, scopes, attributes and optional expiry; the raw token belongs only to
clients. Use [the generated example](../../examples/agent-server/README.md).

RPC requires exactly one `Content-Type: application/json` (UTF-8 if a charset is
specified) and `ochat-protocol-version: 1.0` (also accepts `1`). First POST an
initialization request without a connection ID; retain the returned
`ochat-connection-id` response header. Subsequent RPCs, connection notification
streams, and connection deletion use that ID with the same principal. This is a
logical connection across HTTP requests, not the current TCP socket.

Connection IDs bind the complete authenticated authority: principal ID,
authentication kind, scopes and attributes. A request with different authority
is rejected with `permission_denied`, even when the principal ID matches.
Initialize a new connection after changing authority. This also applies to the
connection notification stream and deletion; a rejected reuse leaves the original
connection intact. A rotated token with identical authority may reuse it.

## Routes

| Method/path | Contract |
|---|---|
| `POST /v1/rpc` | Protocol request or batch. Requires version/content type. First envelope on a new connection must initialize. |
| `GET /v1/events` | Notifications for the logical connection; connection ID required. No durable replay cursor contract for this queue. |
| `DELETE /v1/connection` | Close logical connection/attachments; detached sessions remain. Connection ID required. |
| `GET /v1/sessions/ID/snapshot` | Authorized principal-projected snapshot. Does not require an RPC attachment. Scoped ETag; matching `If-None-Match` returns 304. |
| `GET /v1/sessions/ID/events` | Creates a read-only session subscription; durable replay plus live events. Does not require a logical RPC connection ID. |
| `POST /v1/blobs` | Authorized bounded streaming upload; requires message-send scope and media type. |
| `GET /v1/blobs/ID` | Authorized complete blob download with content type/length and digest ETag. No HTTP Range implementation; use `blob.read` for bounded cursor chunks. |
| `GET /v1/health` | Authenticated health response; detail visibility depends on principal. |

Unsupported methods return 405 with Allow; unknown routes return 404. Failed
authentication returns 401. Protocol and route errors carry typed codes and
retryability; inspect the body as well as HTTP status. Session replay that requires
a replacement snapshot returns 409 with `snapshot_required`.

## Batches and limits

The stock listener accepts up to 16 MiB RPC bodies, 128 envelopes per batch,
16 concurrent dispatches and 1024 outgoing entries. Connection/idle limits are
configurable. Body rejection is not a claim that all HTTP buffering is streaming.

The first initialize envelope is handled before dispatch of remaining batch
work. Otherwise batch commands can execute concurrently; ordered collection of
responses does not serialize side effects. Await dependent responses before
issuing the next mutation. Zero replies produces 204, one reply a JSON object,
multiple replies an array. Correlate IDs, not array positions.

## SSE and replay

Parse SSE frames separated by a blank line, concatenating multiple `data:` lines
according to SSE rules. HTTP chunks and newlines inside one data field are not
event boundaries. Ignore heartbeat comments. Honor the stream's event type and
ID; only durable events advance the durable replay cursor.

For session replay provide `Last-Event-ID: N` or `?after_sequence=N`, a nonnegative
int64. If both are supplied their encoded values must agree. With neither, the
stream starts after its attach snapshot position: obtain a snapshot first when
building a complete projection, then request replay after that snapshot sequence.
This closes the snapshot/subscription race through the retained log.

If replay is unavailable, fetch a fresh snapshot, replace the projection and
subscribe after its sequence. Do not append the snapshot as extra transcript rows.
Ignore duplicate durable positions; preserve structural sequence even for hidden
events with redacted/empty payloads. Recoverable provider deltas require security
scope; transcript-only clients receive finalized history instead.

Slow subscribers are closed; they must reconnect/replay. Active connection SSE
streams keep that logical connection active. Configure proxies not to buffer SSE;
responses set `Cache-Control: no-cache` and `X-Accel-Buffering: no`. The per-session
stream has its own read-only attachment/heartbeat cleanup, not the RPC owner lease.

## Blob uploads and downloads

Upload headers are `Content-Type`, optional `ochat-blob-kind` (default `binary`),
`ochat-target-session`, `ochat-display-name`, `ochat-allowed-use` (default
`message_input`) and `ochat-sha256`. The store validates limits and optional digest,
then publishes metadata only after finalization. Uploaded temporary blobs expire
after one hour in this implementation. A target session must be authorized.

Use opaque returned IDs in message attachments. Never send a server filesystem
path in place of a blob reference. Export blobs bind the exact principal/scope
projection that created them. `blob.read` supports bounded chunks and continuous
cursors; validate length and digest before atomically installing a download.
See [blob codecs](../../../lib/agent_protocol/blob.mli) for complete metadata/input shapes.

## Proxy and security boundaries

Trusted reverse-proxy identity headers are accepted only from exactly configured
direct peer addresses. Missing/duplicated/invalid asserted identity or scopes fail
authentication; an untrusted peer cannot select its identity by adding headers.
Do not expose a trusted proxy hop to arbitrary local workloads without reviewing
that trust. OAuth IDs require a host-injected validator; stock config alone does
not provide one. Token files are loaded into immutable authentication state;
restart to replace it. See [configuration](../configuration.md).

Follow [the HTTP tutorial](../tutorials/http-client.md) for runnable commands.
