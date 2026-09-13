# Troubleshooting

Start with config validation, actual executable help, the connection status, and
the typed error. Do not log raw credentials, tool arguments or full transcripts
unless you have explicitly reviewed their sensitivity.

| Symptom | Check / action |
|---|---|
| Wrong TUI behavior or rejected local flags | Run with `--no-config`; inspect `--print-effective-args`; distinguish native/legacy/connected modes. |
| Native local history disappears on quit | Expected transient mode; use a durable daemon or supported persistent local host, not invented TUI data-root flags. |
| Socket refused/missing | Correct absolute URI; daemon running; private owned parent; inspect startup diagnostics. Do not remove a live socket. |
| Store already owned | Identify existing owner and shut it down normally; don't delete lock files or run two configs sharing a root. |
| HTTP 401 | Raw token versus hash, one valid bearer header, principal/expiry and loaded token file; restart after credential-file replacement. |
| HTTP connection ID missing/unknown | Initialize, capture response header and retain it across requests. After restart initialize again. |
| Protocol-version/content-type error | Exactly one accepted version header; JSON content type/UTF-8; correct `/v1/rpc` route. |
| Catalog ID rejected | List catalogs; use opaque returned wire IDs. TUI friendly config names are not raw protocol IDs. |
| Prompt validates but catalog reports unavailable | Artifact construction has additional source-closure/manifest checks. Keep captured imports/scripts beneath the root prompt directory; inspect the catalog diagnostic. |
| HTTP connection authority differs | Token scopes, attributes, authentication kind or principal changed. Initialize a new connection; do not reuse a connection ID from a broader credential. |
| Observer cannot see operator session | Transcript scope alone does not grant cross-principal visibility. Use the same creator principal with restricted scopes and a separate connection. |
| Materialized prompt tree verification fails | Preserve evidence and restore the complete verified artifact from a trusted backup, or publish a newly reviewed prompt revision. Do not edit digests to accept changed bytes. |
| Workspace unavailable or queued | Prompt/workspace pairing, canonical directory identity, exclusive conflict domain and root/job quota limits. |
| Permission denied on writable client | Credential scopes, actor attachment ownership/lease, invocation identity and current revision. Writable mode cannot grant scope. |
| Tool appears stuck asking approval | Authorized writable approver present? `approval_timeout none` can wait; inspect pending permissions/fallback and runtime policy. |
| Named reviewer/OAuth unavailable | Stock config IDs require injected implementations; select supported static auth/policy or embed the resolver. |
| Shell manifest rejected | Inspect exact expanded source, source/manifest hashes, host path context, trusted-source/signature and administrative ceilings. |
| Shell backend/resource helper unavailable | Install required helper/backend, verify trusted path/platform limits; do not silently fall back to direct execution. |
| Audit rows are redacted | Scope projection is deliberate; grants/audit/security require their own scopes. Remote audit is not filesystem access. |
| TUI shows connected and thinking after client loss | Connection detection/lease/idle timeouts differ from turn completion. Check daemon operation state; do not submit duplicates to unstick it. |
| HTTP events stall | Proxy buffering, SSE blank-frame and multi-data-line parser, independently draining notifications/stderr, body/queue limits. |
| Provider request stalls | Inspect daemon's `API_URL` override and credentials without printing secrets; isolate transport framing from model latency. |
| Duplicate transcript after reconnect | Replace snapshot instead of append; deduplicate stable IDs/sequence; don't mix operation sequence with durable cursor. |
| `snapshot_required` | Fetch scoped snapshot and resume after its latest durable sequence. Preserve client draft separately. |
| Pagination cursor invalid | Identity/query/collection/host changed; restart the listing rather than editing signed cursor data. |
| Revision conflict | Fetch current state; review intended mutation; don't remove the revision check to force it. |
| Crash leaves interrupted job/tool | External side effect may be unknown; inspect records and reconcile before retry. Persisted state is not a continuation. |
| Corrupt store or unsupported schema | Stop mutations, preserve backup/logs, inspect schema and recovery error; inspection is not automatic repair. |
| High memory/descriptors | Distinguish active actors/jobs/subscribers, retained streams/history/artifacts and runtime/allocator overhead; sample trends, not one RSS value. |
| Terminal broken after exit | Check actual terminal restoration; headless PTY checks don't replace every emulator's visual/input behavior. |

## Local stdio RNG initialization

Older builds could fail with “The default generator is not yet initialized.”
when `ochat-agent-stdio --local --prompt FILE` omitted `--data-root`. Transient-root
allocation requested an ID before the RNG was initialized. `Embedded.start` now
initializes the RNG first; a cold-executable test checks protocol initialization,
transient/process-bound metadata and temporary-root cleanup on EOF.

Rebuild or update if this error occurs before protocol initialization. It does not
indicate missing provider credentials or invalid ChatML. On an older build, a
dedicated private `--data-root` or daemon gateway avoids the affected path. That
root preserves data while liveness stays process-bound; do not reuse a normal
daemon's active data directory. See the [stdio tutorial](tutorials/stdio-client.md).

For a reproducible report include version/build context, host mode, transport,
redacted config, exact command, typed error, timestamps and whether an isolated
offline fixture reproduces it. Do not run load/soak or paid model tests merely to
collect a first diagnostic. See [testing](testing.md) and [operations](operations.md).
