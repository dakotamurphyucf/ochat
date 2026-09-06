# Library-reference audit remediation

Follow-up to the September 6, 2026 ten-finding review. Runtime corrections are
limited to OAuth token cache identity/publication and legacy snapshot publication.
Other findings reconcile guides/interfaces with supported behavior rather than
silently changing compatibility APIs. Verification is recorded below.

## Findings addressed

| Finding | Correction |
|---|---|
| 1 — OAuth identity | Versioned SHA-256 cache keys bind exact issuer, grant type, client ID, secret and requested scope. Old unbound issuer-only files are ignored, not deleted/imported. Credential-bearing cache helper APIs are explicit in a new interface. Exclusive per-writer temporary files prevent shared-temp corruption. |
| 2 — parallel tools | Removed invented environment/CLI controls, corrected default and host scope, Responses items, per-loop semaphore and deterministic output collection; no at-least-once guarantee or early next-model continuation. |
| 3 — legacy persistence | Reconciled prompt-session, Session and Session_store references with V5 identities, typed save errors, corruption behavior, actual IDs and checkpoint/export separation. Direct serialization was found to truncate: Session_store.save now publishes atomically via a private temporary file and rename. |
| 4 — Io | Corrected switches, rendezvous queue, API examples, data-URI module path, 0600 permissions, verbatim logs and non-atomic raw writes; cancellation/TLS limitations explicit. |
| 5 — vectors | Corrected independent global BM25 ranking/intersection, beta=1 scope, input preconditions, nonzero mock vectors and float64 memory estimate. |
| 6 — caches | Documented lazy expiry, physical stale entries, base hit statistics, callback/complexity rules and actual lookup APIs. |
| 7 — web fetch | Corrected header-based JSON handling and documented query/port loss, caught cancellation and post-decompression size checking. Implementation hardening remains separately identified below. |
| 8 — templates | Distinguished literal versus functor substitutions, fixed raw-string newline example, clarified loading and parser errors; reconciled interface. |
| 9 — tokenizer | Documented fixed regex, vocabulary responsibility, heap/rolling-hash implementation, allocations and valid-text chunking; removed unsupported compatibility/performance claims. |
| 10 — client excerpt | Included attachment accessor and aligned full excerpt with current interface. Docs checker now enforces full excerpts labeled “current callable contract.” |

## Compatibility and operational boundaries

OAuth cache_file/load/store now require credentials. Existing callers must pass
the same credentials as get; there is no unsafe compatibility fallback.
The first call using a v2 key acquires a new token. Public-client PKCE still has
no account selector; separate user accounts need separate private cache roots.
Cache writes are atomic visibility, not fsync durability or a credential vault.
Concurrent cache misses may still acquire independently.

Legacy snapshot writes preserve the prior file on write/rename failure, but
save-time locks are not multi-client revision ownership. Directory creation can
raise before save's Result boundary. Reset/rebuild still use minute-resolution
archives with best-effort rename, and are not rollback-capable operations.
These limitations do not describe agent_store or daemon administration archives.

## Separately tracked implementation limitations

- Web Fetch loses URL queries/ports, catches cancellation, and decompresses
  before checking expanded size. Future hardening needs URI-preserving requests,
  cancellation propagation, incremental bounded decompression and local HTTP
  fixtures for query/port, cancellation and highly compressible payloads.
- Template.load uses blocking filesystem APIs; an Eio-aware replacement needs
  an explicit capability/API migration. This task adds no new blocking I/O.
- Legacy reset/rebuild archive collisions and lack of rollback remain explicit;
  atomically publishing snapshots does not repair their whole maintenance flow.
- Generic Io wrappers retain their documented null TLS authenticator and broad
  exception conversion. They are not production transport-security primitives.

See the [broader audit](code-documentation-audit.md) for other previously tracked
limitations. No external MCP server, browser login, paid provider or soak is
required to verify this change.

## Verification

- Five new OAuth cases cover all key dimensions, stored identity selection,
  loopback reacquisition/reuse across six credential tuples, ignored old cache
  files and concurrent atomic publication/private permissions. Together with
  existing cases, the MCP/OAuth suite passed 43 cases.
- Two legacy snapshot regressions check that an already-open reader retains
  original bytes after save, and failed rename preserves the destination and
  cleans owned temporary/lock files. The fixture now uses a uniquely allocated
  temporary directory with Eio cleanup, avoiding collisions between repeat runs.
- The opt-in docs check now runs offline library behavior checks for Io, the
  worker pool, LRU/TTL, templates, a synthetic tokenizer, dense/hybrid retrieval
  and current/corrupt session decoding. It also checks all excerpts explicitly
  labeled as current callable contracts.
- The first combined rerun caught an overly broad excerpt-marker check matching
  explanatory prose. It was narrowed to actual declaration lines, with positive
  and negative regression checks; that failed run is not counted as a pass.
- Final combined `dune build @all @runtest @agent-docs-check` passed. The
  documentation gate checked 297 pages and 38 protocol methods, including the
  new offline checks. The normal tier includes all 43 MCP/OAuth cases and six
  legacy-store tests. Only non-fatal duplicate-library linker warnings remained.
- Navigation checks passed for both root READMEs: all 297 docs-src Markdown
  pages are reachable within three links. Formatting and `git diff --check`
  passed. No paid provider, browser, manual-terminal or new soak runs were used.

These finite checks are not proof that every historical code sample or all
runtime interleavings have been verified. The separate implementation
limitations above remain open, not silently counted as code fixes.
