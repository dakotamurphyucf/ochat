# Neutral transcript projections

Protocol 2 separates a client's transcript view from the session's canonical storage. The daemon, embedded client, HTTP, socket and stdio paths use the same public projection boundary. This is an intentional wire upgrade: clients negotiate version 2, and HTTP RPC requests declare `ochat-protocol-version: 2.0`. Version-1 connections are rejected. The version-1 storage documents introduced by the persistence migration retain their own independent codecs and version policy.

## Canonical data and disclosure

`History_entry.Payload` owns immutable semantic content and retained provider evidence. A public history row carries its actual host history ID and provenance, and one of three bodies:

- `Full` retains the neutral payload, including unknown JSON and provider evidence.
- `Visible` contains only whitelisted readable message fields or reasoning summaries. Developer remains distinct from System. Unknown message parts have explicit redacted positions; annotations, log probabilities and arbitrary provider metadata are omitted.
- `Redacted` discloses only an optional structural header.

Visible and redacted bodies cannot be converted into canonical input. Display placeholders never become fabricated assistant entries. Copy and edit eligibility depends on complete known content and the client's existing authority; the server continues to authorize every mutation using the actual history ID.

Transcript and security scopes are independent. A reader without transcript scope receives no history window, deferred transcript or history event content, even if it has security scope. Transcript-only readers receive visible messages/reasoning and redacted tool/unknown items. Full history and live tool content require both scopes. Other session, grant, job and permission scopes retain their existing policies.

Private `History`, `Snapshot`, `Event.Durable` and `Method_result` remain persistence/domain representations. `Public.History`, `Public.Snapshot`, `Public.Durable` and `Public.Result` are read projections. The server stores private idempotency successes, then projects each response and retry using the current principal. Export renders the selected public projection under that same authority; export blobs remain scope-bound.

Create and attach requests check the requested attachment mode against the current principal before consulting cached outcomes. Narrowing scopes cannot recover an owner attachment or reclaim credential through an otherwise identical retry.

## Streaming and authority

The pure `Transcript` library describes source, attempt, item and part identity without importing OpenAI, protocol operation IDs, Eio or UI code. Each actual inference attempt has a fresh scope. A partial item may have an unknown role or part index; consumers do not guess Assistant or index zero.

Provider adapters emit announcements and append/replace changes. A provider's output-item-done observation is provisional. Finalization occurs only after the actual history commit callback returns successfully, including the separate local tool-call and tool-result commit paths. Finalized content is the exact admitted host entry, including changes made during preparation or moderation. Nested entries remain transient in the parent's view and never become its canonical history.

Tool activity carries typed progress, outcomes and classification. A parent reference includes its actual source/attempt and call alias: a child may legitimately reuse the parent's alias in a different scope. Native trace observations without a direct child scope remain explicitly attributed to their emitting parent scope. They do not invent a child attempt or host invocation ID.

Strict typed observers propagate unexpected delivery failures. Cancellation preserves its original exception and backtrace, skips strict terminal delivery during cleanup, and relies on the authoritative operation terminal event to close live activity. A missing tool outcome is displayed as unavailable rather than inferred from overall operation success or cancellation. Existing protected observer APIs retain their previous behavior.

Transient publication does not save a moderator checkpoint: these callbacks can run while an asynchronous moderator invocation owns that checkpoint. Submission, model-input preparation, completion and owned invocation/event commits retain their explicit persistence boundaries. A live observation does not acquire authority to save another operation's moderator state.

## Ordering, limits and recovery

Durable sequence numbers and operation-local live sequence numbers remain independent. The client orders live data against its durable anchor, retains bounded receipts and future events, and fences completed operations. Exact retained duplicates are harmless; conflicting duplicates are errors. An older sequence whose receipt has already been evicted is obsolete, not a verified duplicate.

Received live envelopes retain their admitted original JSON so unknown fields and numeric spelling participate in duplicate comparison and byte charging. The client bounds both individual events and aggregate retained receipts, pending events, transcript drafts and activity. Append admission checks the prospective retained size before concatenation. These are encoded-content bounds, not a claim to measure exact OCaml heap usage.

The common admission profile allows 16 MiB per public envelope, depth 160, one million fields and two million nodes. The client retains at most 64 MiB of aggregate live state. Canonical payloads still pass their own stricter validation; envelope headroom accommodates wrappers and does not relax canonical limits. Snapshot and event admission reject duplicate history IDs, mismatched session owners and invalid replacement anchors. Native constructors and wire decoders apply the same child-domain checks.

A missing live prefix stays explicit. Reattaching with a fresh snapshot clears transient state; exact reconstruction of every in-flight partial delta is not promised. A complete committed entry reconciles a matching finalized root draft without rendering it twice. Replacement snapshots replace their authoritative activity content while retaining the ordering information needed to reject stale live traffic.

After an observed terminal operation, the client retains the latest operation's observed draft and tool activity as a frozen read view within the same byte budget. It does not count as active work. Missing tool outcomes remain unavailable, and later live traffic cannot alter the frozen view. A new operation, replacement snapshot or cleared projection retires it.

Active tool snapshots contain all running calls and the exact classified subset used by the agent view, including shell scripts. The host's ephemeral start-summary cache is bounded to 1024 summaries of at most 4096 encoded bytes each. Oversized summaries are omitted entirely; no truncation string is presented as real tool input.

A failed RPC attachment subscription emits a sanitized, attachment-scoped `session.stream_error`. The client marks the view stale and obtains a fresh snapshot before treating it as current. HTTP SSE reports the corresponding snapshot-required condition. Neither path fabricates a durable sequence or silently drops a failed durable projection. This does not introduce a new shared-connection notification multiplexer; that broader connection-lifecycle work remains separate.

## Implementation boundaries

`Openai.Responses_live` is the current legacy DTO ingress adapter. It does not claim that a reconstructed DTO is an actual provider capture. Provider runtime request/replay adoption remains a separate implementation step. Public read views and TUI state no longer decode provider response DTOs to recover their presentation model.

The TUI consumes semantic transcript drafts, actual canonical commits and public read rows. Attached views retain disclosure, stable host identity and provenance without populating the standalone model's canonical history with synthesized entries. The shared pure `History_chatmd` renderer handles canonical export; `Chatmd_export` adds public disclosure and provenance rendering at its owning boundary.
