# Universal documents, integrity and field presence

OChat's new durable contract is independent of OCaml runtime layouts and provider SDK types. This covers outer containers and embedded records, not just history payloads. Retain one logical family with `format`, positive kind-specific `schema_version`, registered `kind`, object `payload` and optional non-null `extensions`. Suggested format marker: `ochat.document`; first committed schemas start at version1. Concrete kind payload schemas are downstream implementation deliverables. Frame, document, client protocol, replay-codec and application versions are separate.

Conversion targets are registered per kind; different record families need not advance together. Pure deterministic bounded generic-data transformations reach the selected kind's target version before current-domain construction. The current codec independently checks expected kind/version and validates IDs, ordering, counters, provenance and call/result relationships. Unknown fields/extensions are carried through supported edits/writes; unknown required semantics fail closed rather than becoming executable defaults. The existence of a generic JSON object does not prove domain validity. Restore returns an extension carrier; functional edits use with_value to retain its unknown fields/template. Domain_codec.encode consumes that carrier, validates known data and merges preserved fields at their original paths. A conflict with a newly owned field is a typed Extension_conflict, not silent overwrite/discard; a pure kind conversion must explicitly resolve promotion. New authored records use an explicitly empty carrier and must not replace a restored carrier during editing.

## Read and commit order

1. Existing Eio reader bounds bytes and validates frame magic/version/checksum.
2. Generic decode rejects duplicate keys and enforces depth/size/field limits; inspect envelope and stored-version structural metadata without current runtime construction.
3. Verify stored-version transaction chain/session/counter/fallback anchors using the original payload representation. Conversion cannot redefine an old digest.
4. Convert supported generic documents to their kind-specific target, preserving extension data and absence/null semantics.
5. Validate the current domain and authoritative references through existing owner validators.
6. Existing actor/recovery installs state; existing atomic checkpoint/commit machinery writes current documents only after successful validation.

For new transactions, hash the exact persisted document payload bytes before frame wrapping. Preserve these bytes/digest across in-memory conversion; a new checkpoint references the original journal head digest. The physical frame checksum remains independent. Do not hash upgraded JSON or change order-sensitive history equality by silently sorting object keys. The existing typed re-encoding hash path must be adapted explicitly, with independent stored-bytes and chain-anchor cases.

Current coupling is concrete: Session_persistence.restore_snapshot constructs Session_state.t before upgrade ([source:119](../../lib/agent_session/session_persistence.ml#L119)); transaction delta and events contain current-type sexp strings ([41](../../lib/agent_session/session_persistence.ml#L41), [138](../../lib/agent_session/session_persistence.ml#L138), [168](../../lib/agent_session/session_persistence.ml#L168)). Snapshot.Persisted and Transaction.Persisted have outer bin_io layouts ([snapshot:11](../../lib/agent_store/snapshot.ml#L11), [transaction:19](../../lib/agent_store/transaction.ml#L19)). Changing only their inner payload leaves this coupling intact.

## Record ownership ledger

Rows are required migration coverage, not separate new storage systems. Generic envelope/conversion is M1-T03a; domain integration is assigned below. Preserve existing framing, locks, atomicity and recovery owners. Referenced modules are current source starting points.

| Family and nested representation | Domain integration owner | Current writers/readers that must agree |
|---|---|---|
| Snapshot outer wrapper, metadata and Session_state payload | M1-T03b | agent_store/snapshot; session_persistence; recovery; retained_history fallback/retention scans |
| Transaction outer wrapper/counters/hash chain | M1-T03b | transaction; journal/journal_segment; commit_writer; recovery; retained_history |
| Session_delta, including Created state and embedded history | M1-T03b | session_persistence:55/141; session_delta; retained_history:211; replace sexp strings with registered documents |
| Durable event documents and embedded history/projections | M1-T03b, protocol projection M1-T03c | session_persistence:56/171; agent_protocol/event; replay and retention readers |
| Transaction command_audit embedded receipt | M1-T03b complete embedded receipt codec; M3-T06 separate side-store | command_handler:442; idempotency_store.Command_audit; transaction/reconciliation; do not keep an opaque current-type sexp escape hatch |
| Canonical/effective history and stable identity | M1-T03b | history_entry; history_codec; session_state; session_delta; legacy and generated child callers |
| Compaction archives and references | M1-T03b | compaction_archive:3/68/122; retained_history archive paths; recovery and blob-reference scans |
| Moderator checkpoint/embedded identity/effective history | M1-T03b | moderator_checkpoint:7; Session.Moderator_state.Identity_snapshot; runtime_builder snapshot/restore; not identity_snapshot_sexp forever |
| Standalone local Session/session_store, complete outer snapshot and moderators | M1-T03b | session_store:66/107/200; Session.V5/current; legacy CLI routes; same new document boundary, no per-surface frozen readers |
| Data-root/session metadata, catalog/index and organization fields | M3-T06 plus owning M3 catalog/grouping tasks | agent_store/session_store, session_index, data_root, migration; ordinary and administrative readers |
| Idempotency/protected receipts and command audit ledger | M3-T06 | idempotency_store, command_handler reconciliation and independent retention readers |
| Blob metadata/retention/reference documents | M3-T06 | blob_store, blob_retention, blob_reference_scan, retained_history; raw content bytes stay blobs |
| Job-result intents/completion artifacts | M3-T06 | job_result_intent, job_result_store and host job recovery/retention |
| Delegation/captured child intents | M3-T06 | delegation_store and runtime/host admission readers |
| Prompt artifact manifests and audit-chain documents | M3-T06 | prompt_artifact_store, audit_store, source pinning/chain/retention readers; raw source stays an asset |
| Goal/automation occurrence/package records, when introduced | CAP domain tasks using shared infrastructure | CAP owns domain contracts and records; M3 ledger tracks cross-links/retention; existing timers/jobs remain owners |
| Context windows/notes/hints/transition receipts, when introduced | CTX domain tasks using shared infrastructure | CTX owns domain records; same archive/history/authority and M3 retention ledger |
| Provider/MCP/daemon secret material; operational locks | Existing auth/transport owners, excluded | Never session documents; unsupported binary conversion is not permission to touch credentials |

All source modules above are under lib/. Implementation must enumerate the actual writer, ordinary reader, recovery/retention reader and tests per row before claiming universal coverage. M1 handles its provider/history-dependent complete containers; M3 implements independently owned side records. M1-T03b implements and validates the complete embedded command-audit receipt document, reusing existing idempotency invariant validation. It does not wait for M3-T06. M3-T06 subsequently reuses that receipt codec while converting the independent idempotency side-store/remaining administrative documents. This is a one-way prerequisite from shared infrastructure/history integration to side-store integration, not a cycle or current-type sexp escape hatch.

## Beta cutover and failure

The new schema begins the forward-compatible document conversion commitment. Supporting older versions from that family preserves old field meanings in pure transformations, not frozen old OCaml types or SDK readers. Unsupported pre-contract binary/sexp beta files remain untouched and return Unsupported_beta_format. Do not rename, delete, reset, rewrite in place or silently create an unrelated replacement session.

Current session_store tries V5 then V4/V3/V2/V1/V0 typed readers. Downstream new-format paths replace that cascade; retaining/importing old beta storage is not required here. CLI disposition of legacy import commands is explicit migration work, not an accidental delete of utilities. No automatic downgrade, external effects or job re-execution happens inside conversion. Existing recovery policy deals with interrupted work after successful domain restore.

## Field presence

Presence and nullability are separate. Independent decode inputs and exact emitted keys are required; round trips alone are insufficient. Generated option behavior is verified by the [expect probes](validation/presence_probe.ml), but profile-specific provider eligibility remains downstream.

| Field/contract | Missing | Null | Value | Encode policy |
|---|---|---|---|---|
| Envelope format/kind/payload/schema_version | Reject | Reject | Validate kind/type/positive version | Always present, no null |
| Optional extensions object | Absent | Reject | Validate/preserve object | Omit absent; preserve present fields |
| Required nullable chain anchor | Reject in current kind; an explicit old-version conversion may supply it | Genesis/no anchor if invariant allows | Validate digest | Present null or digest, never infer missing==null |
| Durable int64 sequence/revision/time_ns | Reject unless version rule supplies it | Reject | Canonical validated decimal string within domain range | Exact decimal string; no float roundoff |
| Optional non-null generation option | Omitted/default with provenance | Reject unless that profile explicitly supports null | Validate typed value and capability | Omit absence; preserve explicit supported value |
| Distinct optional nullable option/output format | Preserve absent provider omission | Retain only with explicit field/profile support | Validate; explicit Text differs from absent | Custom record codec controls absent/null/value |
| Provider request store | Adapter sets false | Reject | Only false in selected stateless path | Always false |
| Provider metadata/usage/error fields | Field-specific wire policy; unknown counts stay unknown | Retain nullable meaning only where declared | Validate, preserve raw detail | Do not blanket-apply option attributes |
| Config update clearing | No change only where an update contract defines it | Explicit clear only where selected/defined | Explicit replacement | M3 defines mutation semantics; not automatically every field |

`[@jsonaf.option]` admits absence and emits omission but rejects explicit null for a non-null element. A plain string option requires the key and accepts/emits null. Neither alone preserves all three distinct states. An explicitly presence-valued record codec must inspect membership and emit keys intentionally. `allow_extra_fields` accepts and drops extras: capture raw before narrowing and retain an explicit carrier. Duplicate keys must reject before generated decoding. Tests use constructed new-schema data, not historical application binaries.
