# Local context windows and recoverable history

An agent should be able to start a fresh model input and recover evidence
from earlier work. The session owns that evidence. A citation can survive a
client disconnect or model change. What the agent may read depends on the
current session policy and the current visibility of the cited revision.

This is a design for future recovery tools. OChat already has neutral history,
stable occurrence IDs, edit revisions, checksummed pre-change archives, bounded
conversation search and an instruction asset for context recovery. The existing
asset supplies guidance. The window ledger, note store, scoped recovery service
and protected model delivery described here require implementation. OChat will
own the records and their authorization on the runtime host. OpenAI can receive
a newly prepared model request without providing a history, notes or compaction
service. [History interface](../../lib/agent_protocol/history.mli),
[compaction archive](../../lib/agent_session/compaction_archive.mli),
[conversation search service](../../lib/agent_server/search_service.mli),
[recovery instruction asset](../../lib/builtin_instructions/assets/context-recovery.md).

## What a window identifies

| Object | Identity and purpose |
| --- | --- |
| Transcript occurrence | `History.Id` identifies one application-owned occurrence. Equal text and repeated provider IDs can belong to distinct occurrences. |
| Content revision | `History.Content_revision` identifies the content of that occurrence. Editing a user message preserves its ID and advances its revision. |
| Recovery window | A session-owned, durable identity for a period of work. Its ordered membership points to exact occurrences and revisions. A committed rollover records its predecessor. |
| Effective history | One admitted history projection reflects the moderator overlay. Insertions and replacements have their own effective versions even when the public row preserves canonical identity. |
| Prepared request | A preparation binds selected effective history to the actual model, tool schemas, instructions and other host contributions in its own order. |
| History page | `History.Window` is a bounded response to a pagination request. Reading more pages has no effect on the active recovery window. |

The provider response ID, allocation sequence, session generation and journal
revision each have other jobs. In particular, sorting occurrence IDs cannot
recover chronology. Window order comes from admitted membership. A citation
includes the owning session, window, occurrence, content revision and view.
For effective evidence it includes an exact effective member version. The
prepared-input record separately binds its ordered membership and projection basis
to the prepared request. The host verifies membership before resolving payloads.

### Proposed state interfaces

The names below describe domain interfaces. Typed ID and revision modules use
explicit equality and comparison. Wire and durable document versions are
admitted through validating decoders. A caller can supply a syntactically valid citation selector. The actor or
its validated reader resolves the selector and constructs the admitted
citation under current authority.

```ocaml
module Recovery_owner : sig
  type t
  val session : t -> Agent_protocol.Session_ref.t
  val generation : t -> Session_generation.t
end

module Effective_item_version : sig
  type t
  val overlay : t -> Overlay_incarnation.t
  val change : t -> Effective_change_origin.t
  val canonical_base : t -> Agent_protocol.History.Content_revision.t option
  val digest : t -> Payload_digest.t
end

module Recovery_citation : sig
  type version =
    | Canonical of Agent_protocol.History.Content_revision.t
    | Effective of Effective_item_version.t

  type t
  val owner : t -> Recovery_owner.t
  val window : t -> Window_id.t
  val occurrence : t -> Agent_protocol.History.Id.t
  val version : t -> version
end

module Recovery_window : sig
  type origin =
    | Root of Window_origin.t
    | Successor of
        { previous : Window_id.t
        ; operation : Agent_protocol.Id.Operation.t
        }

  type state =
    | Open
    | Sealed of Manifest_ref.t
    | Expired
    | Retired of Retirement_reason.t

  type t
  val id : t -> Window_id.t
  val owner : t -> Recovery_owner.t
  val origin : t -> origin
  val ordinal : t -> Window_ordinal.t
  val state : t -> state
end
```

The session actor admits one open recovery window per live generation. Window
identity and order survive sealing. A member contains an occurrence, version,
origin or carry provenance, manifest ordinal and a private locator. A locator
addresses an immutable live-state observation or an archived document plus its
source-local ordinal. For archived bytes it binds owner, generation, exact
session revision, archive kind, operation and digest. Dereferencing checks the
located entry's occurrence and version as well as the source identity. Window
ordinals and member ordinals are monotone within their respective orders.
Duplicate exact members within a window fail admission. Imports and prepended
items retain their admitted positions. ID allocation order is insufficient.

Append advances the open window's observation. An unchanged exact member keeps
its citation when its validated location moves from live state into retained
storage. A sealed window can use a manifest spanning multiple exact sources.
Summary compaction inside a tracked window relocates removed eligible members
into its checked prestate archive, then adds the summary with its own occurrence
and derived-content provenance. It creates no successor window. Only a
committed rollover installs the successor and its transition receipt.

## Why an archive alone is insufficient

Suppose a first window contains `[a, b]`. A later edit archive contains
`[a, b, c]`. Reading both snapshots as a stream would deliver `a` and `b` twice
and would give the old `c` a position in the wrong window. A window manifest
selects exact ordered members. A validated archive supplies bytes to resolve
those members. A pre-existing session without a manifest begins tracking from
its current admitted view and reports earlier recovery coverage as unavailable.
Old edit, delete, pending-input, reset and upgrade archives provide prestate
evidence under their existing contracts. Earlier recovery window membership
requires direct evidence.

Retiring a window and deleting an occurrence have different effects. Rollover
keeps authorized earlier members recoverable. An edit of `b@0` to `b@1` retires
the old content and the edit's discarded suffix from recovery. Ordinary deletion
retires the IDs returned by `History_deletion`, including a paired tool occurrence
when applicable. Search, listing, reads, hints and paging continuations all
consult the same visibility decision. A stale citation cannot select the newer
payload by accident. Failure to authorize a session or grant reveals no member
metadata.

The existing `Compaction_archive` writer checksums the exact admitted document
and syncs its file and directory. Build on this archive-before-reference order.
A visible manifest pointing at unavailable bytes produces a typed failure. Empty
history is a successful result only when the manifest genuinely has no visible
members. A retained opaque provider envelope stays in the universal document.
Recovery exposes only supported readable projections.

### Resolving an exact reference

The key for a canonical member is `(History.Id, History.Content_revision)`
within its source session. For a moderator replacement, the current public
history projection can retain both the canonical ID and its content revision.
Two replacements of the same canonical text can therefore have the same pair.
An effective version binds its overlay incarnation and exact change origin,
the canonical base revision when present, and a digest of the immutable neutral
projection. Incarnation covers rebuilds and prompt upgrades that can reset
change allocation. An unchanged member can keep its exact effective version
across unrelated overlay changes. A legacy projection without durable change
identity can only be cited with its exact source revision. The exact prepared
request separately binds prepared membership, overlay basis and the selected
model, tools and instruction inputs. A moderated transcript alone establishes
which content was available in that projection. A claim about submitted input
requires a record from the preparation and submission boundary.

Validate a retained artifact in two steps. Resolve the visible member and its
part first, then authorize the referenced asset and check its identity against
the retained metadata. A `blob` identifier embedded in arbitrary text does not
establish attachment membership. A model-supplied image URL does not establish
local custody. An unavailable attachment can have a readable caption if the
caption has independent provenance and passes the same visibility checks.

For a canonical read, the host binds the source Store and live session handle
before selecting a member. The reader resolves the member at its recorded
source ordinal, checks the archive digest and source generation, and checks
the current visibility again before releasing the projection. A locator that
moves during the read can be retried. Inline image reads accept original
image parts carrying bounded PNG, JPEG, GIF or WebP data. The reader checks
canonical base64 and the media signature. Text that happens to contain an
image URL remains text. [Canonical source reader](../../lib/agent_session/recovery_source_reader.mli),
[inline image reader](../../lib/agent_session/recovery_image.mli).

An effective read starts with an exact durable preparation and citation.
Ordinary summary compaction qualifies that preparation's still-visible
members against the checked prestate. It commits the archive identity and the
verified source positions with the history replacement. Positions in the
prepared request stay fixed even when later overlay insertions change the
source positions. Earlier verified archive locators keep their original
custody. An exact member without a verified position is unavailable after
transfer.

The reader applies current scope and visibility before opening the archive
and again before disclosing the result. The archived prestate must contain
the original preparation, and the selected position must reproduce its
exact occurrence, effective version and digest. A preparation record and its
archive locator establish the prepared source.

The actor binds that preparation to the actual foreground root allocation
before dispatch. The binding uses the exact prepared configuration on the
live host and records the operation, scope and attempt ordinal. An
acknowledgement marks the point after which a backend could have started.
The runtime terminal or interruption records whether submission definitely
did not occur, could have occurred, or produced a confirmed response. If the
host stops first, reopening can distinguish an allocation that never reached
acknowledgement from an acknowledged attempt with an unknown submission outcome.
The terminal record survives retirement of detailed inference accounting
without retaining request bodies or replaying execution authority.

### Logical visibility and physical custody

Commit the visibility change and the edit, deletion, redaction, or window
fence in the same actor transaction. Check current visibility before selecting
payloads and again at disclosure after the last operation that can yield. An
active moderator tombstone suppresses both default recovery views. Lifting it
can restore eligibility only while the underlying occurrence and exact revision
remain eligible. Redacted or withdrawn members supply no text, snippets, counts,
artifact handles or raw length. General transcript inspection and export have
their own authority.

| Outcome | Recovery decision after caller and scope admission |
| --- | --- |
| Exact, currently eligible member | Return the selected projection or range from its verified source. |
| Old version, removed member, or an overlay suppression | Report stale or withdrawn through an exact read when authorized to disclose that status. Omit it from lists, search and hints. |
| Confirmed policy expiry | Report expired from the committed expiry record. |
| Referenced bytes unexpectedly absent | Report missing. |
| Wrong owner, revision, identity, digest, or malformed frame | Report corrupt for an otherwise authorized promised reference. |
| Unsupported projection or asset | Report unsupported. Preserve opaque payloads, including provider reasoning envelopes, in their original storage. |
| Unknown or never-admitted ID | Report unavailable within an authorized scope. A caller without scope receives denial without an existence signal. |

A source owner with an active writer attachment and grant-management scope can
commit expiry for a sealed window. Exact citations in that window subsequently
report expired. The open successor and its independently admitted citations keep
their own status. The durable fence retains membership so retries, restart and
later reads can identify the expired revisions without reopening an archive.

Before collecting visibility metadata, prove that every affected citation or
window is expired or guarded by a retained fence. Keep validated membership,
manifests and artifact descriptors as typed roots under the existing store and
blob retention coordinator. Protect active reads, preparations and receipts as
applicable. The collector charges bounded shared work and retains bytes if it
cannot prove their absence from the admitted roots. A raw blob reference scan
provides candidates, whose custody and authorization still require proof.
The archive writer and retention scanner share the same filename decoder,
including the pending-input archive kind. [Retained-history interface](../../lib/agent_session/retained_history.mli),
[archive writer](../../lib/agent_session/compaction_archive.ml#L45-L60),
[retained scan](../../lib/agent_session/retained_history.ml#L256-L274),
[blob reference scan](../../lib/agent_store/blob_reference_scan.mli).

### Image and document parts

An artifact descriptor is minted under the source session's custody. It binds
the exact citation, part ordinal, immutable blob identity, asset kind, media
type, length and digest. Verify adoption before publishing a descriptor and
verify the typed source and blob reference at read time. Return bytes through an
approved bounded image or document result. A text part produced by document
conversion can be read as captured text. Recovering original file bytes
requires a separately retained descriptor. Inline images need bounded
validation and admission of exact bytes. Ordinary recovery initiates no remote
fetch, OCR or automatic image resubmission.

Lists, search and hints expose neither an asset URI nor a generic blob handle.
Blobs reserved for recovery are private even to the session creator through
HTTP download and the transport-independent blob read command. The public
HTTP upload route rejects reserved use tags before opening an upload.
A call through the general blob route for other content requires its own authority. Qualify that
route's treatment of recovery-only metadata on each enabled transport before
using it to store protected assets. Revoking a recovery citation affects future
recovery disclosures, while independent grants for the same bytes follow their
own policy. [Blob protocol](../../lib/agent_protocol/blob.mli),
[blob store](../../lib/agent_store/blob_store.mli),
[blob authorization](../../lib/agent_server/principal_projection.ml#L380-L392).

## Bounded reads and search

`list_windows` follows committed lineage. Within a window, list and search in
manifest order, then in content-part order. Filter by role, item kind and view
before producing hits. Reuse the existing `Search_term` convention of a
nonempty UTF-8 literal up to 256 bytes, ASCII case-insensitive, without Unicode
normalization. Search parts separately so a match cannot cross an image or a
tool boundary. Document and image references have typed metadata. A tool result
can expose its admitted readable content without exposing its invocation grant
or opaque provider fields.

Use half-open UTF-8 byte ranges relative to a particular part and revision.
Both endpoints must be scalar boundaries. For `aβc`, `[1,3)` reads `β`. The range
`[1,2)` is invalid. A truncated read ends on a scalar boundary and supplies the
next byte offset. A search page bounds positions scanned as well as hits and
bytes read. A page with zero hits may still carry a continuation. Enforce a
total response bound before emitting the page, including metadata.

Bind a continuation to its principal, scope, source revision, visibility basis,
filters, order and next source position. Authorization runs again on each page.
Stale continuations require a fresh search. A rebuildable index may supply
candidates. Verify every candidate against current visibility and exact content
before returning any snippet, identifier, count or hint derived from it.
Conversation search already has a narrower audience and a different selection
of readable parts. Share its literal matcher and range checks while preserving
those existing permissions and results. [Literal matcher](../../lib/agent_server/search_text.mli),
[transcript projection](../../lib/agent_server/search_entry.mli),
[host-local cache](../../lib/agent_server/search_cache.mli).

### Retrieval service and accounting

```ocaml
module Recovery_page : sig
  type coverage =
    | Complete
    | Incomplete of Coverage_reason.t

  type 'a t
  val rows : 'a t -> 'a list
  val next : 'a t -> Recovery_cursor.t option
  val coverage : 'a t -> coverage
end

module Recovery_error : sig
  type t =
    | Denied
    | Unavailable
    | Stale
    | Withdrawn
    | Expired
    | Missing
    | Corrupt
    | Unsupported
    | Invalid_query
    | Invalid_range
    | Budget_exceeded
    | Refresh_required
    | Cursor_expired
end

module Recovery_reader : sig
  type t
  type authority

  val list_windows
    :  t
    -> authority:authority
    -> Window_query.t
    -> (Window_summary.t Recovery_page.t, Recovery_error.t) Result.t

  val list_items
    :  t
    -> authority:authority
    -> Item_query.t
    -> (Item_summary.t Recovery_page.t, Recovery_error.t) Result.t

  val read_item
    :  t
    -> authority:authority
    -> citation:Citation_selector.t
    -> selection:Item_selection.t
    -> (Item_read.t, Recovery_error.t) Result.t

  val search
    :  t
    -> authority:authority
    -> Search_query.t
    -> (Search_hit.t Recovery_page.t, Recovery_error.t) Result.t
end
```

The host issues a scoped authority value for the current call. Window queries
select the current owning session and admitted generations, filter by state
and order by lineage ordinal. An item query selects one window and an explicit
canonical or effective family. It may narrow the admitted position range and
roles, kinds or provenance. Lists return safe metadata. Search uses an
explicit history, owned-note or exact-share scope. Default history search
covers selected semantic user and assistant text and explicitly admitted
developer and tool-result text. Host instructions, generated summaries or
hints, raw invocation data, unknown payloads and opaque reasoning are excluded.
A filter can narrow the projection.

First-slice search walks the selected visible positions in ascending window,
member and part order. Take one hit per exact owner, occurrence and version
within the selected family, attributed to its earliest eligible window in
scope. Listing either window still shows its genuine membership. Overlapping
sources alias the same exact version and conflicting bytes at that version are
corrupt. Determine the earliest eligible member from a bounded structural
membership lookup at the same pinned basis. This lookup contains references
and ordinals. It can be derived from validated manifests and updated at the
actor boundary. Charge lookup and visibility work. Incomplete deduplication
coverage is an explicit failure or partial result. It cannot be repaired by
accumulating all prior hit IDs in the cursor. Use the first literal match
within the first matching allowed part. Expose its original part index and
byte range. An item read selects a bounded
UTF-8 range in one immutable original part or a separately authorized artifact
bounded in raw bytes. The matcher's snippet remains a window into its part.

Use the following initial ceilings for qualification. Stricter host document
and transport limits apply.

| Quantity | Initial ceiling per call |
| --- | --- |
| Returned rows or hits, and examined source positions | 50 rows or hits and 256 positions, including filtered or unresolvable candidates |
| Query and encoded reply | 64 KiB each, including response metadata and continuation. Literal term 256 bytes and snippet 512 bytes. |
| Selected plaintext | 4 MiB aggregate examined bytes, 1 MiB selected text and 4,096 parts per history item. Text read defaults to 4 KiB with a 16 KiB hard reply cap. |
| Source work | 8 bounded source opens and 64 MiB cumulative source work, including encoded bytes read and decoded output. Charge validation, decompression, hashing and rereads under the same allowance and host deadline. |
| Assets | Existing blob admission and transport limits, charged to the same source and encoded reply budgets. |

Charge work from the first lookup through final visibility checks and result
encoding, with cooperative cancellation. A permanently oversized first
candidate produces a typed limit or unsupported result. If a later candidate
would exceed the remaining work budget, continue from that candidate on the
next page. Make progress through examined nonmatches, even when the reply has
no hits. If a promised source is missing or corrupt, report incomplete coverage
or fail the bounded operation explicitly. A successful end of data requires
complete coverage of the promised scope.

Bound continuation size before decoding. A signed, versioned continuation
binds this host and purpose, the caller
and authority epoch, owner and generation, query, filters, order and limits,
the selected membership basis and the visibility watermark. A share query
also binds the grant revision. Use source position for progress, including
filtered items. Tampering, a different principal or a revoked grant cannot
resume a read. A changed admissible basis requires refresh. Process restart
expires continuation tokens. An exact citation with a still-admitted revision
can remain valid. A cache or index is a rebuildable candidate source, with
keys that include scope, family, exact source basis and visibility. Start with
a bounded scan and disposable cache. Store no plaintext index on disk. A
partial index cannot certify a complete miss. Revalidate candidates against
the exact source and current authority before output.

## Note ownership and sharing

Each session owns its notes and source-owned outgoing grants. A virtual path
is an exact case-sensitive key in a session catalog, such as
`recovery/decisions.md`. It has at most 240 ASCII bytes and eight slash-delimited
components, each 1–63 bytes drawn from ASCII letters, digits, period, underscore
and hyphen. Reject dot and dot-dot components, absolute or drive-prefixed
paths, backslashes and NUL. A component prefix selects whole components. An
owned listing sorts paths by component bytes and then by note ID. The path
never reaches a filesystem open. Delete and recreate allocate a fresh never-
reused note ID. The initial contract makes a note's path immutable. A note
version combines host-qualified owner, note ID and monotone content revision. The catalog also has its own revision. Exact reads
pin a note version or an explicit share.

```ocaml
module Note_path : sig
  type t
  val of_string : string -> (t, Note_error.t) Result.t
  val to_string : t -> string
end

module Note_version : sig
  type t
  val owner : t -> Agent_protocol.Session_ref.t
  val id : t -> Note_id.t
  val revision : t -> Note_revision.t
end

module Recovery_notes : sig
  type t
  type reader
  type writer
  type share_manager

  type selector =
    | Owned of Note_version.t
    | Shared of Note_share_ref.t

  val read
    :  t
    -> reader:reader
    -> selector:selector
    -> range:Utf8_range.t
    -> (Text_read.t, Note_error.t) Result.t

  val write
    :  t
    -> writer:writer
    -> Note_write.t
    -> (Note_receipt.t, Note_error.t) Result.t

  val append
    :  t
    -> writer:writer
    -> Note_append.t
    -> (Note_receipt.t, Note_error.t) Result.t

  val delete
    :  t
    -> writer:writer
    -> Note_delete.t
    -> (Note_receipt.t, Note_error.t) Result.t

  val create_share
    :  t
    -> manager:share_manager
    -> Note_share_request.t
    -> (Note_receipt.t, Note_error.t) Result.t

  val revoke_share
    :  t
    -> manager:share_manager
    -> Note_revoke.t
    -> (Note_receipt.t, Note_error.t) Result.t
end
```

Creating a note compares the catalog revision and requires an absent path.
Replacing, appending and deleting compare the exact note revision. Append
performs one concatenation with the supplied UTF-8 text, with no implicit
newline. Empty replacement is valid and empty append fails validation. The
single source actor commits the change and a body-free receipt together. The
mutation key is scoped to owner, operation and authenticated writer domain.
Its receipt binds a canonical request digest and the result identity and
revision. Recheck authority and look up that receipt before evaluating a fresh
revision precondition. A same-key same-request retry within the admitted
receipt lifetime observes the same result. A different request under that key
conflicts. A fresh stale write fails before mutation. Once the retry horizon
ends, reject old mutation admissions using a validated expiry or durable fence.
An uncertain commit must be recovered before claiming definitive success. A
successful ACK gives subsequent authorized reads that commit or a later state.
List and search also observe the current committed basis when started afresh.
[Commit writer](../../lib/agent_store/commit_writer.mli),
[idempotency store](../../lib/agent_store/idempotency_store.mli).

The initial profile admits up to 128 live notes, 64 KiB per UTF-8 body,
2 MiB total live note text per session, 128 active outgoing grants and a
seven-day maximum share TTL. Bound the combined ledger, retained receipts,
tombstones and storage roots as well. Check encoded transaction and snapshot
costs before commit, preserving headroom for terminal and recovery state.
Reject the mutation if its receipt cannot be retained within the admitted
horizon. Citation fields carry typed same-session references that remain inert
until independently resolved. Arbitrary note attachments are deferred.

An owner read uses a currently authorized connection bound to that session
or the exact installed native execution scope. A mutation also needs current
writer or native effect admission. The source session may explicitly grant one
recipient read access to the plain text of one pinned note revision. A typed
citation field, write operation or source catalog requires its own authority.
The recipient gets a reference which selects policy, and the host rechecks the
recipient's authority on each use. Revocation, expiry, deletion or replacement
of the pinned revision ends future access. An authorized retry of a lost
response observes the original receipt under current disclosure policy.

Share creation requires explicit source-owner management authority, an exact
current note version and the current catalog basis. The grant pins one
same-host recipient session and its authorized incarnation, the source
incarnation, the share revision and expiry. Resolve the actual recipient
binding through the host. Shared listing takes explicit share selectors and
reveals only independently authorized entries. Unknown, expired, revoked and
mismatched grants give the recipient the same safe unavailable response.
The source retains one ledger. A separately authorized owner may write its
own independent text under the destination catalog, without transferring any
source authority. Check both scopes at disclosure. Gather recipient witnesses
before entering the source mailbox and recheck them without yielding at commit
or disclosure. A cross-session actor call while holding that mailbox can
deadlock opposite-direction shares. Catalog visibility and selected native-tool
access do not confer share-management authority.
[Authorization](../../lib/agent_server/authorization.mli),
[session registry](../../lib/agent_server/session_registry.mli),
[native tool dispatch](../../lib/agent_session/native_tool_dispatch.mli).

Revocation controls future host disclosures. Bytes an authorized recipient has
already received may have been quoted or used in a decision. Durable host
receipts, automatic transcripts, search indexes and recovery hints must not
republish a protected tool result after its source becomes unavailable. Preserve
a typed reference and materialize it under current authority at the point of
disclosure. A string resembling a citation inside a note is ordinary authored
text and cannot substitute for host provenance.

## Protected native delivery

A ChatMD capability must be installed and selected for the current execution.
The actual native invocation and its current session authority are both
required. Importing a prompt or helper grants neither. Recovery content is
data even when it resembles instructions, a receipt or a tool call.

Ordinary native calls currently persist invocation arguments and outcomes.
Their canonical tool outputs must agree with persisted outcomes for replay
and regeneration. For protected recovery, preserve that integrity with a
body-free call projection and a safe result stub. Keep validated live
arguments outside the durable invocation, including on rejection and
redirection. Persist a body-free mutation receipt separately. Replay can
render the stub without repeating the read or writing a note again.
[Native dispatch](../../lib/agent_session/native_tool_dispatch.ml#L142-L157),
[invocation history](../../lib/agent_session/invocation_history.ml#L118-L143),
[invocation recovery](../../lib/agent_session/invocation_recovery.ml#L126-L163).

Only the trusted admitted invocation can issue a private, bounded delivery
plan. Bind it to the committed output occurrence, exact source/version and
range, visibility basis, recipient and immediate next submission attempt.
Pass it through a typed live channel. The host reauthorizes it at the
submission edge after transport waits and invalidates it on cancellation,
turn retirement or process exit. Keep materialized bytes out of invocation
state, event replay, ordinary history, export, request diagnostics and
reusable provider-prefix state. Each permitted retry performs fresh
preparation and authority checks. A host or backend that cannot guarantee
the delivery contract reports the protected read as unavailable. A read
whose final check preceded revocation may already have disclosed data.
[Submission guard](../../lib/inference_runtime/inference_runtime.mli#L3-L22).

The session dispatcher recognizes the trusted native registration's
`Protected_recovery_v1` contract. It displays `{}` as the recorded call
arguments. A successful invocation records a JSON null outcome. The live
handler receives the validated original arguments under its admitted native
scope. A second result selection, an expired invocation, or a request with
an unprojected canonical call fails. A host-side redirect cannot transfer a
protected call's arguments to an ordinary tool.

After the actor acknowledges the exact canonical output, the dispatcher can
hand the next preparation a process-local plan bound to that output. The
plan supplies its result in the original tool-output role and preserves the
provider call ID. A stored null outcome can be replayed independently of
that live handoff. Native code that needs to acknowledge a mutation without
disclosing content can choose an acknowledgement without a plan.
[Session native result](../../lib/agent_session/recovery_native_result.mli),
[transient provider delivery](../../lib/inference_runtime/protected_delivery.mli).

## Host lifetime and recovery

- A durable on-demand host can close and later reopen the same store. A
  reopened host admits reads against current retained state without starting a
  session or provider. Durable window identity, note revisions, visibility
  fences and valid grants survive when their exact owner and authority can
  be verified. Old page tokens and protected delivery plans expire.
  Explicit Resume consumes the current host/session/generation/revision
  anchor and reconciles interrupted operations. It starts from committed
  intent and state. [Native session lifecycle](../agent-server/sessions-and-workspaces.md#save-a-session-and-resume-work).
- A long-running host retains custody if one client detaches. Reattachment
  renews connection authority. A citation's identity is independent of the
  attachment. Changing model or client starts a new preparation when
  necessary and preserves exact eligible evidence. A reset or rebuild
  that deliberately discards evidence commits the required visibility fence.
  Same-owner generation changes retain the local note catalog. An authority
  incarnation change closes the previous grants. Session removal ends access.
- A transient actor retains records and receipts for the admitted actor
  lifetime only. Disposal ends access. Enable recovery here only after
  qualifying that host's commit and delivery paths. First-slice grants are
  limited to durable same-host owners. Reject a transient fork which has no
  admitted recovery owner. [Memory backend](../../lib/agent_session/memory_backend.mli),
  [native dispatch admission](../../lib/agent_session/native_tool_dispatch.mli).
- A branch, seed, child or restore-as-new receives its own owner,
  recovery root and empty note catalog. Copied neutral material is admitted
  by the seed contract with explicit provenance. A parent citation or share
  has its original authority and cannot be retargeted simply by moving it
  into the child. A new share requires the source owner to approve that
  exact recipient. The runtime host owns records and provider credentials.

## Acceptance walks

1. **Overlapping archives.** Window W0 admits `P,U1,A1`. Summary compaction
   retains `P` and adds derived `S`. Its prestate supplies `U1,A1` at their
   original positions. A later prestate contains `P,S,U2,A2`. Commit W0
   with manifest order `P,U1,A1,S,U2,A2`. W1 carries `P`, then admits
   `U3`. By window, list each genuine `P` membership if allowed. Across the
   two windows, search finds the exact version of `P` once when `P` is
   eligible. An imported member retains its original admitted position.
   An orphan archive provides no membership.
2. **Stale, edited and deleted records.** Edit `U@2` to `U@3` and retire
   its discarded suffix in the same commit. An allowed exact read of the
   old citation reports stale or withdrawn with no payload. Old search
   continuations refresh. Even if the old text remains in two archives,
   search and hints can use only the current eligible version. Delete a
   bound tool result and retire its exact call as well. Another call pair
   with the same provider call ID stays independent. Check the final
   authority fence by pausing a candidate read before revocation.
3. **Moderator version.** Keep canonical `X@0` and admit an effective
   replacement of `X@0`. The citation selects the exact effective bytes.
   Reuse a change counter after a prompt upgrade. An old effective citation
   must not alias the new overlay incarnation. An overlay tombstone
   suppresses ordinary recovery and never reverses a committed deletion.
4. **Images, documents and missing data.** Adopt an image with a verified
   local blob and a captured document with text plus an optional independently
   admitted byte descriptor. Seal the window before pruning. Read the
   exact image through its visible part and verify length, digest and media
   type. Search, hints and listing supply no raw asset URI. Exercise
   a missing promised file, corrupted bytes, committed expiry, remote-only
   image and document text without an admitted attachment. Each produces
   its corresponding bounded result. Revocation while an asset read is paused
   discloses no bytes after the failed recheck.
5. **Note races and sharing.** Race two appends to revision 1. One commits
   revision 2. A same-key retry after a lost ACK observes revision 2, a
   changed body under that key fails and the other append gets a stale-write
   failure. A fresh read observes the winning append once. Share revision
   2 with a specific child. The sibling, a stale attachment or a caller
   presenting only the note ID receives no protected metadata. The child
   cannot mutate the note or use its citations to cross into source history.
   Pause a warmed-cache read and revoke the share. Its subsequent snippet
   and model delivery fail current authorization.
6. **Crash, reopen and fork.** Crash after syncing an archive and before its
   ledger commit. Recovery finds no newly admitted window. Crash after
   commit and before its reply. Reopen to the same window and transition
   receipt. The prior cursor expires. A still-eligible citation resolves.
   Explicit Resume reestablishes the live execution scope. A child starts
   with no inherited note/share authority. Incomplete custody proof blocks
   collection. A missing promised manifest is an explicit recovery failure.
7. **Limits and capture.** The UTF-8 byte boundaries of `aé🙂\r\nZ` are
   0, 1, 3, 7, 8, 9 and 10. A read of `[1,7)` returns `é🙂`.
   Reject `[2,7)`. Preserve CRLF and stop a truncated result at a scalar
   boundary. Test a zero-hit incomplete page, an oversized first candidate,
   and budget exhaustion before a later candidate. For protected delivery
   check the distinctive test body appears only in admitted canonical note
   storage and the authorized immediate request. Check durable invocation,
   output, event replay, export, index, reconnect, restart, fallback and
   transport diagnostics. A receipt-shaped note remains literal data.

## Implementation and qualification boundaries

The session state owner adds the lineage and membership ledger, archive
admission, and the note/version/grant/receipt records through the universal
document and journal boundary. The retention owner covers typed live and
retained roots, atomic visibility fences, all archive kinds and concurrent
read custody. One archive per prestate transaction is the current safe
assumption until archive reference comparison and loading can distinguish
multiple references with an identical session revision.
[Session persistence](../../lib/agent_session/session_persistence.ml#L204-L250).

The host recovery service consumes those typed interfaces for bounded
list/read/search and artifact authorization. The tool and inference owners
qualify protected capture and materialization through the actual selected
transports. The window transition and budget design owns exact request
preparation records, rollover, fallback and reminder policy and uses this
service for bounded hints. Stable reference tests, archive crash/expiry
tests, cross-session and provider-edge revocation races and measured
legacy-source cost are required before claiming the corresponding support.
Exact historical coverage, storage retention profiles and supported
host/transport combinations remain qualification outcomes.

## Public reference

The November 20, 2026 public Codex client models current and previous window
IDs and offers direct retrieval, discovery and notes tools. Its history and
notes adapter forwards authorized requests to hosted endpoints. Its public
code can establish request shapes and window handling. Server authorization,
indexing, retention and live account eligibility remain outside that
evidence. The September 8 client advertised history ordering and unknown
window behavior in its tool description. The November description instead
directs the caller to discover valid window IDs. Treat both as pinned client
evidence. [November window state](https://github.com/openai/codex/blob/51f4b8af01db1fb04e0cc6235444573ecceda716/codex-rs/core/src/state/auto_compact_window.rs#L33-L101),
[November recovery tools](https://github.com/openai/codex/blob/51f4b8af01db1fb04e0cc6235444573ecceda716/codex-rs/ext/history-notes/src/requests.rs#L19-L25),
[November adapter](https://github.com/openai/codex/blob/51f4b8af01db1fb04e0cc6235444573ecceda716/codex-rs/ext/history-notes/src/backend.rs#L83-L162),
[September tool description](https://github.com/openai/codex/blob/44ab72674e0f0d8fc5f87a01389912d617793206/codex-rs/ext/history-notes/src/tools.rs#L24-L28).

The OChat adaptation uses exact local references, bounded discovery, notes
for progress and next steps, and small host-assembled hints. Guidance can
arrive through a built-in import or module. Only selected local tools and
verified current authority can read or commit recovery state.
