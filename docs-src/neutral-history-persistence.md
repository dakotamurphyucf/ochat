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
cache retains its own fixed byte budget.

Replay validates the complete unstamped state before applying transaction
timestamp and counter metadata. These edits replace only four existing scalar
fields through `Document.replace_payload_scalars`. Valid scalar edits that do
not increase compact escaped size preserve the original complete admission
proof; any growth receives full final inspection. The final state still passes
the same typed decoder and domain invariants.

Idempotency receipts are named `session.command_audit` documents. The durable
idempotency cache is a complete `store.idempotency_cache` envelope with named
keys, outcomes and stable record identities. Cache updates retain unknown
root, record and nested fields. The aggregate cache uses the same durable
structural profile with a fixed 16 MiB byte cap for writing, reading, updating
and reference scanning. Scan-specific disk-and-memory budgets still apply.
Expiration explicitly retires the expired
record's preservation context while keeping every other context. Blob-reference
proofs validate and scan both durable and in-memory receipts under the existing
mutex, including decoded escaped strings and unknown members; pending outcomes,
corruption or shared budget exhaustion refuse collection.

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

The broader M3 storage work still owns the separate session index and store
metadata, prompt artifact records, delegation ledger, job-result intent records,
blob manifests/retention state, audit log and lock/migration bookkeeping. Adding
named codecs for references embedded in a session does not migrate those
independent on-disk owners. They retain their existing validation and lifecycle
contracts until their own migrations are implemented.
