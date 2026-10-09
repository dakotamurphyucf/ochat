# Neutral durable history

Canonical history contains application-owned `History_entry.Id` identities and
validated `History_entry.Payload` documents. `History_entry` depends on Core,
Jsonaf, the repository's pure JSON binary helper and the shared
`Document_schema` JSON validator, and does not depend on OpenAI. Provider IDs are metadata; they never allocate a host history identity.
The agent actor's existing canonical history list remains the sole committed
history owner. Host notification, authoring and moderator provenance stays in
its existing `Agent_protocol.History.entry` record.

Durable moderator insertions and replacements hold `History_entry.Payload.t`
directly. Their generic script state and queued events remain ChatML snapshots.
Effective-history projection transfers neutral payloads without a provider
reader or a JSON-to-script-value roundtrip, preserving opaque fields and numeric
spellings. Inserted host IDs cannot collide with canonical or deferred history;
replacement target IDs deliberately identify the existing occurrence.

A payload is a `history.payload` document in the `ochat.document` family, at
schema version 1. Its named payload fields contain a neutral semantic view and
an explicit representation. The domain retains the immutable complete JSON
value and a cached validated projection. Re-encoding a restored payload preserves
unknown envelope, payload, semantic, metadata and raw provider fields, including
field order, numeric representation, and absent-versus-null values. This is JSON
value preservation; it does not promise original HTTP lexical bytes.

Neutral semantics distinguish input/output messages, system/developer/user/
assistant/tool roles, text/refusal/image/unknown content, function/custom calls,
results, readable reasoning summaries and unknown provider kinds. Function
arguments and custom inputs remain exact strings. Canonical validation never
parses these strings as executable arguments and never invokes a provider
payload decoder. Raw JSON is bounded and checked for duplicate keys, invalid
UTF-8, invalid number literals and excessive depth/node count. The outer durable
reader applies its document byte limits before admitting the complete record.

Representations have distinct contracts:

- `Authored` contains a portable host-authored semantic value.
- `Captured` contains an actual provider capture, its nonsecret origin when
  available, and its immutable original JSON envelope. OCH-50 Wire captures enter
  through the explicit `Openai.Responses_history.of_wire_item` adapter. Unknown
  account, profile or model identity is not invented.
- `Reconstructed` retains the known-field serialization available from an older
  typed provider DTO. It is explicitly lossy and is not evidence of original wire
  capture or eligibility for opaque replay.

Local function/custom tool results enter through
`Openai.Responses_history.authored_output` at dispatch and recovery. Their text,
content parts, call metadata and host occurrence relation are authored semantics;
they retain no duplicate provider DTO body. Lowering reproduces the same legacy
request output item. General legacy DTO conversion retains its reconstructed
representation.

`History_entry.Payload.Origin.unavailable` expresses missing provenance without
inventing an endpoint, model, account or provider configuration. Origin identity
is a compatibility reference, not execution or replay authorization.

A result relation is either `Bound` to an actual host call entry identity or
`Unresolved`. Contextual bulk conversion and streaming/recovery bind results only
when an actual preceding call or recorded invocation supplies the occurrence.
Existing standalone and orphan outputs remain legal unresolved values. Retained
explicit bounds must follow a call of the same family and agree on shared known
provider call metadata. A missing call may have been retired by compaction, so
absence alone does not invalidate a retained result. Pair deletion follows an
explicit host relation first; unresolved values preserve existing nearest
family/provider-ID pairing semantics. Reused provider IDs in later turns do not
replace the earlier host occurrence.

`Agent_session.History_codec.to_canonical` and `of_canonical` preserve neutral
payloads and validate host classification/provenance. The existing internal
`to_protocol` and `of_protocol` spellings are canonical aliases. An explicit
`to_presentation` operation lowers compatible readable content for the temporary
legacy display boundary. Presentation values cannot overwrite canonical raw
captures, and redacted values cannot become canonical model input.

`Openai.Responses_history` is the explicit legacy runtime adapter. Reconstructed
DTOs are decoded only at that seam; their derived semantic projection must match
the committed neutral projection before use. A mismatch is an error. Actual
captured payloads deliberately require the neutral runtime adoption in OCH-56:
legacy runtime lowering rejects them rather than silently losing opaque reasoning
or unknown replay metadata. `to_presentation_item` may derive a readable known
view without authorizing replay or tool execution. Unknown kinds remain durable
and valid even when the legacy presenter cannot show them.

A semantic edit preserves the host entry ID and creates an authored replacement,
invalidating the previous capture's replay eligibility. Compatible bound result
relations survive ordinary result-content edits. Unrelated outer document edits
retain the existing immutable payload and its unknown fields. Outer history
arrays use host entry IDs to attach extension data through reorder and edits;
the storage owner's extension carrier rejects accidental loss of unknown fields.

Authoring guidance hashes bind the neutral payload that is actually committed.
Invocation checks use neutral call bytes, host occurrence bindings and recorded
outcomes independently of provider DTO readers. Canonical restore is therefore
separate from whether a current provider runtime can prepare that history.
Original persisted frame bytes remain the source of transaction/snapshot
integrity anchors; no neutral or provider re-encoding substitutes for those bytes.

Compaction archive writers admit the complete encoded state before writing.
Raw document writes also validate the archive digest, session owner, revision
and invocation dispositions before replacing any file. Admission preserves the
supplied complete document rather than substituting a typed re-encoding, so
unknown fields and their numeric spellings survive this boundary too.

Focused expect coverage is in `test/history_identity/neutral_payload_test.ml`.
Independent documents cover unknown-field/raw retention, exact call strings,
null metadata, malformed admission, actual opaque Wire captures, explicit replay
rejection, reconstructed semantic mismatch, unresolved outputs and repeated
provider IDs with host-bound deletion.
Local authored outputs cover exact function/custom DTO lowering for text and
images, preserved metadata and call relations, and absence of a duplicate body.

Embedded state tests additionally restore captured neutral history and opaque
moderator insertions/replacements, check exact payload/provenance/identity
retention, and reject redaction, false classification and inserted-ID collisions.

## Store document ownership and recovery

The agent store writes complete named `ochat.document` envelopes for
`session.snapshot` and `session.transaction`, each at envelope version 1.
Snapshots contain an explicit `session.state` child document; transactions
contain explicit `session.delta`, `session.event` and optional
`session.command_audit` child documents. Child documents keep their own kinds,
versions and preservation context. No serialized sexp or binary DTO is hidden
inside the envelope. This is the accepted beta format break: old complete bytes
fail closed and stay unchanged; no legacy reader or migration rewrite runs.

`Agent_store.Document_record` remains the framing and integrity boundary.
Frame version 1, payload checksums and terminal transport frames retain their
existing contracts. A decoded transaction retains both its original payload
bytes and their SHA-256 digest. Its hash never comes from re-encoding the current
domain projection. An authored transaction encodes its complete document once;
the commit writer appends those bytes and publishes that digest only after the
existing durable append succeeds. Functional transaction edits deliberately
create a new record and digest while retaining unknown fields. The last representable
transaction can be acknowledged; its writer then rejects further commits without
wrapping the sequence. Segment rotation validates its next ID before writing a
terminal or data frame whose append would require rotation.

Recovery first parses bounded generic documents and checks stored kind/version,
required semantics, owner metadata, sequence ranges, all retained checkpoint
anchors and the original-byte journal hash chain. Snapshot metadata must agree
with its embedded state identity, counters, prompt and workspace. Every complete
retained fallback and every transaction child receives current-domain validation
only after these stored checks. The existing recovery owner replays every retained
fallback tail and validates each recovered state, then returns the selected
checkpoint's recovered state. A newer checkpoint cannot hide an invalid
preservation transition covered by its counters. Physical crash-tail repair is its
final effect; any complete format, conversion, ownership, integrity or domain
error leaves the snapshots, journal and both CURRENT pointers unchanged.

Only a missing or physically incomplete current checkpoint permits descending
fallback selection. A complete unsupported or malformed checkpoint does not.
Recovery also rejects complete bad retained checkpoints that were not selected.
`Recovery.preflight` performs the same preparation without repair. Shared
preflight applies an older checkpoint's prefix before comparing its complete
state document with a newer checkpoint. Only exact equality, including unknown
field order and numeric spellings, permits reuse of that checkpoint's already
validated suffix. Unequal carriers continue ordinary replay.

Each loaded persistence owner also owns one `Recovery.Retention_preflight`
scope with fixed session, journal, limits and conversion policy. Every check
rereads and validates the original stored records and all anchors, restores every
retained checkpoint and validates every transaction. A route can resume from a
previously validated replay result only when the checkpoint's original bytes
still match and the freshly verified original-byte journal chain reaches the
exact previously certified head. New or changed checkpoints and unreachable
proof heads use full replay; newly appended transactions always replay normally.
Shared replay compares states at their effective anchors, so a cached head is never
mistaken for its older checkpoint's position. Every original checkpoint and final
fallback state still receives its normal validation, including bounded archive
reads. A failed check never publishes a new replay proof or repairs disk data.

The scope retains only the last successful check's immutable snapshot routes
and head; reopening a session constructs a fresh scope. Memory use therefore
scales with the retained routes and their bounded state documents. Reuse requires
pure deterministic replay and immutable state under that fixed policy; it does
not authorize skipping integrity, conversion, transaction or archive checks.
Ordinary loading keeps its sequential replay and original outer snapshot carrier.
Preflight cost still scales with stored bytes and new replay work, and reopening
requires complete validation again. This is not a production latency guarantee.
Snapshot pruning validates every complete retained document and CURRENT before
its first unlink. Journal pruning rejects incomplete segments. Neither pruning
helper introduces an alternative hash-chain or recovery owner.

`Session_persistence.Restored` owns both the state extension carrier and the
restored outer snapshot. Commits advance that carrier only after successful
publication; checkpoint replacement uses the same state carrier and
`Snapshot.update` retains the outer carrier. Each write first encodes and admits
the original native state, then prepares its next carrier from that complete
document. Publication retains the actual admitted field layout, including newly
present optional fields, so live updates and replay use the same preservation
basis. A failed write keeps the previous carrier.
Required limits follow the
configured snapshot and journal payload budgets rather than silently imposing
an unrelated default byte limit. Embedded codecs and outer store readers use one
durable-document profile: depth 256, one million object fields and two million
JSON nodes. These bounds apply to the complete envelope and its embedded
documents at writing, metadata projection and recovery. Configured byte budgets
remain in force, and generic JSON defaults remain unchanged. The idempotency
cache retains its own bounded metadata and outcome budgets.

Replay validates the complete unstamped state before applying transaction
timestamp and counter metadata. These edits replace only four existing scalar
fields through `Document.replace_payload_scalars`. Valid scalar edits that do
not increase compact escaped size preserve the original complete admission
proof; any growth receives full final inspection. The final state still passes
the same typed decoder and domain invariants.

Idempotency receipts are named `session.command_audit` documents. The durable
idempotency cache is a complete `store.idempotency_cache` envelope with named
keys, outcomes and stable record identities. Cache updates retain unknown
root, record and nested fields. Fresh receipt admission reserves the entire metadata candidate under the original
16 MiB, one-million-field, two-million-node profile before Pending is published.
Terminal Success and Failure outcomes retain their complete raw subtree in
immutable `store.idempotency_outcome` documents. The terminal base document is
bounded to 16 MiB. Original Pending unknown fields have separate storage-only
`pending_custody` at the envelope root, preserving even names that collide with
terminal `value` or `error`. That component validates as a separate same-kind
universal document under the same 16 MiB structural profile; the actual compound
artifact is bounded to 32 MiB, two million fields and four million nodes at depth
256. The terminal reply limit remains unchanged. The
atomic cache retains validated digest/size references, and lookup uses validated
cached outcomes without filesystem IO. Exact retry, Standard's existing 24-hour
expiration, and Protected retention remain unchanged.

Old legal near-full metadata can expand during completion, accepted-sequence
recording and timestamp/null normalization. Its derived compatibility profile is
38,877,216 bytes, 1.6 million fields and 2.7 million nodes (depth 256): at most 100,000
old legal rows, each reserving 221 bytes / six fields / seven nodes of remaining growth.
Existing receipts above the fresh budget remain readable and completable; no new
key is admitted until lawful expiration frees capacity. This finite bound is not
a filesystem quota or a disk-space guarantee: IO exhaustion can leave an uncertain
Pending receipt. New admission capacity maps to Resource_limit; actual write
failures preserve their existing persistence uncertainty.

Artifact publication precedes atomic metadata replacement. Unknown outcome
members and numeric lexemes move with the whole outcome; receipt/envelope
extensions remain in metadata. Expiration explicitly retires only the expired
record's preservation context. Blob-reference proofs validate disk and memory
metadata plus referenced artifacts under the existing mutex, including escaped
strings and unknown members; Pending, corruption or shared budget exhaustion
refuses result collection. Bounded owner cleanup retires only verified unreferenced
outcome artifacts after validating both views under a shared 2 GiB budget and
600,002-entry ceiling. Each sweep retires at most 128 verified unreferenced
artifacts or recognized atomic-writer temporary files. Unknown namespace names,
linked files or exhausted budgets fail closed; no cleanup lifetime is independent
of the receipt owner. Empty sweeps make no directory mutation or sync.

Focused store tests cover independently computed original-byte transaction
hashes, exact commit payloads, checkpoint anchors, parent and child extension
edits, unsupported fallback preservation, refusal before pruning, and cache
update/expiration ownership. Named fault fixtures retain the existing partial
write, activation, sync, rotation and sticky writer failure assertions. Generic
schema tests cover tagged variant ownership, tag-change conflicts, polymorphic
carrier edits, and the explicit empty-key identity policy for named dictionaries.

## Standalone sessions and nested runtime state

Standalone `Session_store` snapshots use the `standalone.session` version 1
document. The existing `snapshot.bin` filename is retained, but it contains
named-field JSON rather than an OCaml binary record. Older beta binaries have
no fallback readers: loading reports the unsupported format and leaves the
original file intact. Future supported changes use generic document conversion
before a current runtime decoder. The runtime record is not the storage schema.

The complete standalone record includes canonical history and its allocator,
moderator overlays and queued ChatML values, shell security state, task records,
metadata and prompt/VFS references. Typed optional storage fields are required
and explicitly nullable; missing data is a conversion decision, rather than an
implicit decoder default. Optional raw JSON, such as a subscription completion
schema, uses field absence for `None` and a present value for `Some`, preserving
an explicit JSON null. Wide counters and timestamps use decimal strings.
The reader validates host IDs, allocator bounds, overlay change watermarks,
nonnegative byte/audit counts and finite script values. Directory-scoped readers
check the stored session ID before conversion and verify it again after restore.

Each loaded standalone value carries immutable extension ownership metadata
through ordinary record updates. Embedded sessions keep their state and outer
snapshot carriers in the persistence owner. Nested moderator checkpoints and
queued event snapshots are structured values, with no sexp strings hiding the
old runtime representation. Identity-based moderator insertions and replacements
store neutral history payloads directly. Effective-history restoration applies
those payloads without decoding OpenAI items or passing captured JSON through
ChatML value conversion. Script-authored replacements create authored payloads;
restoring a captured overlay preserves its original evidence. Generic moderator
state and queued script events remain ChatML snapshots.

ChatML record fields and metadata dictionaries
retain unknown fields by their string keys, including an empty key. Host entry,
task and grant identities still require nonempty values. Tagged runtime cases
select their own ownership shape instead of claiming fields belonging to another
case.

Replacing or removing a container that owns unknown extension data fails with
an extension conflict. This applies to reset and compaction as well as ordinary
edits; the implementation does not silently clear preservation metadata. A
future explicit retirement/conversion policy may handle such data once its
semantics are understood. Fully owned captured JSON follows the existing
explicit content-editing/removal policy.

Standalone saves validate and merge the complete document before creating a
directory or taking a file lock. They write an exclusive temporary file and
rename it into place; this owner does not promise fsync durability. Reset and
rebuild validate before writing an archive, copy the old snapshot instead of
moving it, and replace the snapshot under the same lock. Changing the prompt
creates a new immutable local copy, so a failed snapshot replacement cannot
change the prompt referenced by the old snapshot. An interrupted commit can
leave an unreferenced prompt copy; it does not overwrite the old copy.

The tests replace frozen historical-layout fixtures with current independent
documents, unknown-field edit/reorder cases, malformed admission, pure decoding,
identity consistency, and filesystem failure/atomicity checks. Existing behavior
tests for session reset, shell trust clearing and moderator execution remain.

Journal replay transfers extension metadata from changed records into the
resulting state carrier. The merge checks the current owned projection, preserves
compatible unknown fields from both the checkpoint and journal, and rejects
conflicting values at a captured unknown-field boundary. Unknown object fields
are treated atomically: the reader does not infer how to combine their children.
Updating an existing field retains its position in the complete object, including
the state envelope and nested preservation paths.
This preservation step is separate from the immutable original transaction
bytes used for integrity verification.

Replay applies the transaction's checked timestamp and counters to four owned
scalar fields in the complete carried state document. It validates the original
native state and complete unstamped document before those updates, then checks
the complete stamped document and decodes the final state. Valid scalar edits
that do not grow reuse the original admission proof; any growth requires full
document inspection. Metadata cannot repair
an invalid inherited counter or hide an oversized intermediate document; a
transaction without events preserves the post-delta event counter.

## Remaining storage owners

This change migrates standalone session snapshots, embedded state/checkpoints,
transactions and deltas, durable replay events, command audit receipts, the
idempotency cache, moderator state, and compaction archives. Retention readers
that inspect those records use the same document boundaries.

The broader M3 storage work still owns prompt artifact records, delegation
ledger, job-result intent records, blob manifests/retention state, audit log and
migration bookkeeping. The session index, store metadata and lifecycle markers
are covered by the M3 baseline described below. Adding
named codecs for references embedded in a session does not migrate those
independent on-disk owners. They retain their existing validation and lifecycle
contracts until their own migrations are implemented.

## M3 store metadata and catalog projections

The M3 store baseline replaces root `schema.sexp`, session `metadata.sexp`,
`indexes/sessions.snapshot`, `ARCHIVED` and
`indexes/sessions.recovery-required` payloads with named documents, retaining
these filenames and their existing atomic file replacement adapters. The kinds
are `store.schema`, `store.session_metadata`, `store.session_index`,
`store.session_archive` and `store.session_index_recovery`. The root schema is
version 2; the other records retain version 1 independently.
There are no sexp or binary fallback readers for these beta records. Raw host
server IDs, locks and secrets remain outside this session-document boundary.
Operator migration inspection uses the same `store.schema` admission boundary.
It validates current payloads and reports admitted positive future versions
without interpreting or rewriting their payloads; malformed documents and
unsupported required semantics fail before any schema mutation.

Session metadata owns immutable prompt/workspace references and its validated
session summary. The index owns a rebuildable projection and scheduling hints;
its session identities are unique, and its entries retain nested unknown fields
by session identity across edits and reordering. Counts and wide session revision
counters use nonnegative decimal strings. Protocol optional fields retain their
absence policy; inference summary additionally retains explicit null. Archive
marker presence remains authoritative across index loss. Unknown required
semantics reject ordinary reads and rebuilds before replacement.

Generic metadata documents also admit supported transient embedded sessions on
temporary disk backing; rebuilding a persistent daemon root still rejects transient
layouts. Operator metadata retention reads the same codec and visits full unknown
JSON fields and decoded strings under its existing shared budget.

Canonical journal commits prepare a typed session projection capability after
pure admission and before first archive/journal effects. It owns the latest full
summary and scheduling hints across serial commits. Only matching metadata AND
index publication retires it; stale hints cannot clear a newer target. Tokens for
different sessions complete independently. Last successful token clears only its
new recovery requirement; inherited startup requirements survive. Startup completion
serializes with new commits and defers clearing active tokens without failing
otherwise successful hydration. Thus a crash after journal acknowledgement before
metadata/index publication forces eager replay even when the old index remains valid.
Private staging journals are unreachable until marked, synced atomic session layout
installation, so their persistence constructor has no live-root precommit callback.
Their initializer returns a validated Initial_projection carrying metadata and the
complete entry from the same canonical state. Directory installation publishes
those exact full hints in its recovery bracket; journal-bearing initializers cannot
silently select zero hints. Ordinary create_session explicitly selects defaults.

Metadata/index publication is serialized by the host's projection-update owner.
Both complete documents and extension carriers are validated before an intent
marker or authoritative metadata is published. The recovery marker now covers
both missing-index eager hydration and interrupted projection publication.
Metadata that committed before an index failure remains authoritative: the
Handle is refreshed from validated persisted bytes, never presented as rolled
back. An uncertain index replacement likewise refreshes its observed state.
Failed refresh makes checked reads and further mutations unavailable until
reopen. Production startup, retention, workspace authority and automatic start
consumers use these checked reads; the compatibility pure accessors expose only
the last validated observation.

A fresh successful paired publication may clear only its own recovery marker.
A preexisting eager-recovery requirement survives ordinary updates. A failed
live publication prevents that owner's recovery-completion call from erasing a
newer intent. Reopen reconciles stale or missing projections from validated
metadata and archive markers, preserving archived flags, and keeps the eager
hydration requirement until the existing startup owner completes recovery.
Reading/converting these records never launches work; operator-only hosts retain
their existing execution gate. Scheduling hints from a rebuilt index are unknown
until actor/journal hydration, with no invented pending initial start.

| Record | Writer and ordinary reader | Independent recovery/retention | Boundary |
| --- | --- | --- | --- |
| Root schema | `Session_store` / `Store_schema_document` | Store open before index work | Named document |
| Host organization | `Organization_store` / `Organization_document` | `Organization_root` upgrade and reopen | Named `host.organization` v1; atomic groups, tombstones and mutation receipts |
| Session metadata | `Session_store` / `Session_metadata_document` | Index rebuild and session open share decoder | Named document and private Handle carrier |
| Session index | `Session_index` / `Session_index_document` | Checked startup/maintenance/workspace reads | Named document and keyed entry carrier |
| Archive marker | `Session_store` / `Session_archive_document` | Index rebuild/archive reconciliation | Named v2 lifecycle authority; status and admission own archive/execution state |
| Projection recovery marker | `Session_projection_update` / `Session_index_recovery_document` | Store reopen/eager hydration | Named document; serialized recovery ownership |
| Blob metadata | `Blob_store` / `Blob_metadata_document` | Expiry, preparation protection and `Blob_retention` share decoder | Named carrier; original blob ID and raw content digest |
| Private result intent | `Job_result_intent` / `Job_result_intent_document` | Publisher restart and independent retention | Verified frame, full reference, exact temporary/durable metadata strings |
| Delegation intent | `Delegation_store` / `Delegation_document` | Admission resolution, revoke and independent artifact collection | Named current v6; original scoped filename and immutable admission hash |
| Prompt manifest | `Prompt_artifact_store` / `Prompt_manifest_document.Publication` | Revision rebuild and independent retention | Exact immutable bytes/digest before conversion; known inventory validation |
| Root audit evidence | `Audit_store` / `Audit_evidence_document` / `Audit_event_document` | Complete semantic admission before tail repair | Original embedded bytes/hash; verified canonical snapshot and signed pages |
| Host server ID, actor/daemon locks, secrets | Existing host owners | Existing validation | Intentionally outside session JSON |

Root schema version 2 requires the host organization authority. A version 1
root acquires its existing exclusive owner, admits or initializes the organization
document, synchronizes its directory, and publishes schema version 2 last. Missing
organization authority under version 2 fails closed. Schema conversion preserves
unknown fields and the original creation timestamp. Session metadata versions
remain independent of the root version.

Organization mutations publish groups, permanent ID tombstones and terminal
receipts in one bounded document. An uncertain publication makes the live owner
unavailable until reopen; it does not claim rollback. Failed startup releases
acquired organization, operator and daemon-lock resources, preserving the primary
error or cancellation even if cleanup also fails.

## M3 blob metadata and private result intents

Blob metadata retains existing `<blob>.sexp` filenames with `store.blob_metadata`
version 1 named payloads. Every ordinary reader, expiry/protection consumer and
retention scan uses that same validated owner. Handles carry nested/envelope
unknown fields across adoption. IDs, exact durable/temporary namespace, target
session, canonical SHA256 digest and nonnegative decimal length are validated;
optional fields omit absent values and reject explicit null.

Private `<blob>.frame` preparations contain `store.job_result_intent` version 1.
Original frame checksum/version/flags/EOF are validated before JSON admission;
original stored filename/session identity is checked before conversion. The full
reference plus exact temporary/durable named metadata publication strings fit the
unchanged 32768-byte payload bound. The publications differ only in durable flag,
including field presence and unknown fields. They are prepared and validated before
any intent/blob effect, retained across restart, and reused byte-for-byte for stage
publication and atomic temporary-prefix cleanup. Converted or current reencoded
JSON cannot replace this original evidence. Raw completion bytes and their length/
SHA256 remain independent; recovered publisher retries retain original content
bytes rather than reserializing the current typed completion.

Retention keeps its existing live storage capability and shared reader entry/byte
budget. It visits full named metadata/intent JSON strings, embedded publication
strings and unknown nested references (including escaped IDs), preserves existing
root/edge/self-reference policy, and rejects unsupported required semantics before
deletion. Staged cleanup validates all exact metadata and raw content evidence,
removes data before metadata, syncs their directories, and removes the private
intent last. Partial/uncertain acknowledgement retains proof for retry. Cancellation
propagates outside recoverable publisher/storage locks without poisoning them.

Blob handles retain one immutable observation containing a validated metadata
carrier and its actual locations. Availability is a single state transition;
metadata is derived from that carrier. The diagnostic last observation cannot
authorize a read or mutation. Adoption synchronizes a newly created blobs parent
and both affected directory owners before success. Failed publication conservatively
removes live authority, then verifies the exact old or new document and raw content
length/digest under the storage owner. Only a proven pair restores availability.
Protected rollback/reconciliation secondary failures never replace the primary
result or exception/backtrace; mutation cancellation itself propagates outside the
owner lock. A fresh verified reopen remains possible after uncertain acknowledgment.

## M3 delegation ledger

The private delegation owner admits only current `delegation.intent` version 6
named documents inside complete checksum-verified, zero-flag frames. The original
scoped key must match its deterministic existing filename before document
conversion or current admission validation. Filename identity continues to use
the current scoped key serialization; it does not admit historical ledger formats.
Every admitted record has a captured inference target. Readers never guess a
missing target from current parent policy or reinterpret older typed ledgers.

`Delegation_record` owns the actual domain shapes and current invariants;
`Delegation_document` owns the bounded carrier and immutable admission identity.
References hash the original admitted JSON subtree with its ordering and numeric
lexemes. Mutable stage, revocation and artifact-collection publication preserves
that subtree, optional presence, nested extensions and envelope extensions. A
changed immutable request or admission cannot reuse the carrier. Reservation,
replay, resolution, stage advancement, revocation and independent artifact
retention all use this same document owner. Record admission confers no authority:
current generation, parent policy, stop/revocation and invocation lifetime checks
still belong to the existing delegation coordination owner. Cancellation propagates
outside the ledger mutex with its original exception/backtrace.

## M3 prompt manifest publication

Prompt artifacts retain `manifest.sexp` and `manifest.sha256` filenames with a
`store.prompt_manifest` version 1 named document. `Prompt_manifest` owns the
current inventory invariants; `Prompt_manifest_document.Publication` holds the
validated carrier, exact immutable publication bytes and their original digest.
Load and independent retention verify that digest and the original revision ID
before document conversion and current validation. Root, source and materialized
tree files retain their independent raw byte digests and exact inventory checks.
A stored manifest is never reencoded to establish its admitted digest.

Authored manifest admission has an explicit 262,144-byte limit for the complete
inventory document, including envelope and metadata. This is a new beta admission
restriction, separate from the source capture owner's 256-file and 8 MiB limits;
long paths or extended metadata can exhaust the manifest allowance even when
source content satisfies its limits. Admission completes before staging effects.

Loaded and cached artifacts preserve unknown fields, optional presence and exact
manifest whitespace through immutable publication custody. Revision rebuilds
compare validated known source metadata and return the installed artifact with
its original publication, timestamp and digest. Independently retained delegation
admissions still pin that original digest; this comparison cannot substitute a
changed manifest admission. Installation flushes exact files and all staging
directories before final rename, then flushes the artifact parent before success.
Failure preserves the primary result or exception/backtrace; private cleanup can
remove only this invocation's created staging directory and never an installed
or preexisting destination. Original byte evidence remains verifiable on reopen.
## M3 audit evidence and recovery

`Audit_store` retains the existing journal filename, framing and hash-chain rule:
SHA-256(previous hash or empty string, NUL, exact embedded event bytes). Complete
frame payloads are now `store.audit_evidence` version 1 named documents containing
immutable `store.audit_event` version 1 byte evidence. Binary beta audit records
are unsupported. Stored chain hashes cover the original embedded bytes, including
whitespace, ordering and numeric lexemes; current conversion or projection can
never replace that evidence. Original previous/hash/byte fields are checked before
conversion and current event validation. Optional event IDs preserve absence and
reject null; the opaque supported event payload may itself be null. Unknown event
and evidence fields remain private on disk; public audit pages use only the
current protocol projection and existing signed ordered paging/filter rules.

Authored admission validates the complete escaped evidence envelope and frame
against the configured payload byte limit before journal effects. Recovery first
verifies framing, then all complete chain records and their current semantics,
and only then repairs a short tail. A complete semantic failure followed by a
torn tail leaves the entire file unchanged. Sequence overflow fails before effects.

The audit owner keeps one immutable verified snapshot inside an availability
variant. After uncertain append, protected reconciliation can install either the
fully verified old or new canonical journal snapshot; if this proof fails, both
reads and appends return the unavailable failure. Primary errors, exception
backtraces and cancellation survive secondary recovery failures. Exceptions are
captured inside the owner mutex and raised outside it, preventing poisoning.
Journal helpers return expected filesystem errors and propagate cancellation,
timeouts and unexpected exceptions. Aggregate segment scanning retains its
existing complete-file ownership contract; this slice does not add a journal-size
limit or invent another store. Cursor secrets remain raw host facts.

The shared session `Commit_writer` also owns asynchronous exception replies.
An interrupted journal commit marks the writer unavailable before resolving its
initiating caller with the original exception and backtrace. Accepted queued and
later commits fail without advancing or reusing uncertain journal counters.
Synthetic operation failures leave the worker alive to process failure replies
and close; genuine owning-context cancellation still stops it. Recovery uses a
freshly reopened journal and writer, preserving canonical acknowledged or
unacknowledged complete transactions and repairing only validated torn tails.

The writer's protected stopped signal owns bounded admission and reply lifetime.
Commit and close race their complete enqueue-and-reply waits against shutdown;
shutdown cancels blocked producers and releases external queued callers even
when they do not share the worker's cancellation scope. Already resolved normal
or exceptional replies always take precedence over generic closed failures.

## Session organization projections

Canonical session state version 6 requires the historical organization IDs.
Its structural version 5 conversion initializes an absent organization to empty;
current canonical records cannot omit the field. Changes share the metadata
revision with names and labels and preserve all unrelated canonical state.

Public session summaries admit absent organization as empty for existing summary
records. Metadata retains its independent version 1 boundary; the session index uses
version 2 with required lifecycle revision and admission fields;
this compatibility default is a projection, not permission to erase canonical
membership. Checked publication writes the full current summary from canonical
state. An unloaded catalog cannot distinguish an older empty summary from an
isolated missing organization field without reading canonical history.

Live catalog membership resolves historical IDs against checked host organization
authority and current principal visibility. Deleted IDs remain in canonical state
but disappear from effective membership. Neither representation grants access to
session content or changes execution workspace, prompt or provider authority.
## Archive lifecycle document preparation

`Session_archive_record` defines the pure M3 archive lifecycle state and its
accepted outcome proof. `store.session_archive` v2 keeps the existing archive
marker filename, session identity, lifecycle revision and explicit execution
admission. Active restoration requires explicit resume; an already active restore
is a no-op that preserves its current admission. Successful explicit resume sets
Automatic. Removed is terminal logical absence, including while physical cleanup
is pending. These pure types do not authorize a client, acquire actor ownership,
start work or implement the public restore workflow.

Original named v1 archive documents convert structurally to Archived,
Explicit_resume_required, revision 1 and an empty receipt inventory. Original
owner extraction precedes conversion. Colliding v1 fields must agree with those
original archive semantics; they are never overwritten or interpreted as a grant
of active execution. Current fields require validated presence and types. Unknown
root, payload and keyed receipt fields survive lawful edits and acknowledgement.
Unsupported required semantics fail admission.

One accepted lifecycle outcome and its principal/session/method/idempotency key,
request digest, canonical anchor and exact timestamps live in the same immutable
record. Replay requires current authorization by the eventual owning service.
Matching replay precedes stale lifecycle-revision checks; it never supplies new
execution admission. Outcomes cannot be changed by receipt acknowledgement.
At most 32 receipts and a 32,768-byte complete envelope are admitted. Receipts have
a 24-hour retained interval; unacknowledged outcome proof remains protected beyond
expiry until exact generic durable completion is proven. Only acknowledged,
expired receipt templates may retire. Capacity and byte limits reject a new
admission before effects instead of evicting retained proof or increasing budgets.

The host service reserves a session ID under the registry's short mutex before
performing actor or filesystem work. Loaded mutations acquire the issuing actor's
canonical fence and stop admission before retiring its runtime. Indexed targets
retain the existing store actor lock and read canonical state without constructing
an actor. Exact checked publication binds its current installed witness to the
owning Handle and canonical epoch. Closing, uncertain publication, or a later
canonical/lifecycle change invalidates that witness. A rejected foreign Handle
cannot invalidate the original owner's witness.

Registry reads capture a binding under the short mutex and hold a private read
lifetime while observing outside it. Successful reads recheck the binding rather
than substituting a replacement owner. Loaders reserve only their own session ID,
so loading a retained ancestor does not recursively acquire the global mutex.
Shutdown rejects new lifetimes, drains existing readers and reservations, then
joins the complete runtime graph before releasing stores. Failed provisional
cleanup retains its actual owner, issuing ID and both original diagnostics for
shutdown retry; that ID cannot admit replacement ownership meanwhile. Inactive
eviction uses the actor fence and closes outside the global mutex. Failed cleanup
after retirement keeps its closed owner discoverable; it never reopens execution.

`session.restore` publishes Active with Explicit_resume_required. `session.resume`
commits the Active/Automatic admission gate; it does not start a runtime or change
the session's desired state to Running. Startup, background collection, attachment
and delegated creation respect the gate. Catalog and inspection expose lifecycle
revision and admission so clients can construct an exact expected anchor. Neither
inspection nor archive restoration activates an actor. Workspace references and
workspace content survive archive, restore and permanent session removal.

Removed publishes terminal authority and index absence before physical cleanup.
The existing host-owned generic idempotency store retains each original protected
result before payload destruction. Cleanup moves the session into a recognized
`lost+found/deleted-<session>-<transaction>/payload` namespace, synchronizes both
rename parents, moves the terminal marker to the stable container root and then
removes only the payload. The marker survives interrupted recursive deletion; it
is removed only after payload absence and parent synchronization. Startup retries
from either recoverable location. Live delegation or registry obligations reject
removal; fully retired historical references alone do not. Cleanup never rolls
back namespace changes or deletes workspace content.

Generic outcome acknowledgement does not prove physical removal cleanup. A Removed
head must retain exactly one Applied Remove outcome at its current lifecycle
revision, even after the 24-hour interval and generic acknowledgement. Only final
physical cleanup of the owned terminal document retires that proof. No-op Remove
receipts and other acknowledged expired receipts can retire normally. Existing
`session.delete` Archive/Remove commands retain their original method, principal,
key and policy-bound params digest; the service validates the original delete
policy before selecting the action and retains existing deletion authorization.
Receipt aliases never turn deletion into restore/resume authority.

A checked absent outcome under the current session reservation, before any
publication attempt, permits recording the exact primary command rejection as
Failure in the retained generic owner. Completion occurs before the reservation
is released, so a same-key retry cannot publish between proof and failure caching.
Reservation contention, unreadable authority, retained proof or attempted
publication preserves Pending until recovery reconciles the original result.
Cancellation and unexpected exceptions remain exceptional control flow. A failed
rejection acknowledgement preserves the primary error and reports required
recovery with its secondary storage diagnostic. Definitive No_effect means the
exact generic Failure completed durably; runtime cleanup may still have occurred.

Cached lifecycle results and command receipts recheck current nonactivating
visibility while their target exists. The narrow absence exception is the bounded
original Protected successful Remove acknowledgement, with the original principal
and current deletion policy. Archive, restore and resume receipts never bypass
current visibility merely because the caller once held broad host authority.

## Pending input durable custody (OCH-169)

Named session state version 9 follows version 8's optional run lifecycle index.
Native state is version 24 and named delta version 6. The adjacent 8→9 structural
conversion wraps each original deferred entry without re-encoding its contents,
introduces a zero pending revision and empty retained dispositions, and captures
unknown legacy submitting provenance. It preserves unrelated raw run fields and
compatible unknown paths; same-name new semantic fields are rejected rather than
silently reinterpreted.

The existing queue remains the sole pending-input carrier. New admissions capture
host-authenticated submitting principal separately from receipt retention.
Adoption atomically transfers the exact queued entry into canonical history and
the exact wrapper metadata minus its entry into a declared storage-only
adoption-custody member. Compatible nested future fields, explicit nulls and
numeric spelling survive admission, journal replay and restart. Public DTOs
project only known timing and authorized public history, never private custody
or provenance.

Ordered admission keeps intermediate custody transfers private until the complete
candidate passes state validation, preservation checks, and comparison with the
ordered delta result. Persistence reuses that exact validated final encoding and
decodes it once to establish the next preservation basis before archive or journal
effects. This avoids a repeated encoding without bypassing native validation or
changing unknown-field custody.

Pending cancellation and replacement archive the exact prior admitted session
document before their journal record. Ordinary adoption does not archive full
history. The independent pending-outcome retention policy keeps at most 4096
records in both live admission and recovery; unrelated notification, delegation
and command receipt limits do not affect it. When bounded retained dispositions expire, the exact removed private
records are independently archived before the eviction journal record; rejected
archive/journal publication leaves state, events and worker launch unchanged.
Transient memory backends retain the same exact custody within their owned
lifetime. Expired public lookup remains unavailable rather than inventing a
missing or never-submitted outcome. Source reset/replacement and proved canonical
causal retirement preserve truthful explicit disposition reasons.

The offline `@test/agent_server_e2e/agent-e2e-pending` alias runs
`crash-matrix / pending.interrupted-root-recovery`; the same focused case is also
part of the required `agent-e2e-extensibility-pr` selection. It kills an owned child only
after the actual daemon acknowledges after-root input and graceful stop, then
reopens the physical store. Cold inspection cannot invent terminal proof; actual
activation records matching Interrupted evidence while the queue remains stopped.
Explicit resume admits one canonical occurrence, and a second reopen retains its
adopted outcome. SIGKILL exercises process interruption, not power-loss durability.

The `cross-transport-conformance / conformance.pending-inputs` case exercises the
four pending methods over Unix, HTTP, and both stdio gateways. It uses actual
persisted queued input with a cancelled predecessor and stopped session. Read-only
controls fail; replacement keeps identity and changes content revision once; stale
controls fail; cancellation and original-key retries preserve the truthful outcome.
No live inference provider is used.
