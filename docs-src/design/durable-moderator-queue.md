# Durable moderator queue provenance

A moderator checkpoint may retain a named `queue_provenance` child beside its
identity snapshot. The authored queue remains an ordinary list of ChatML values;
its value ABI does not change. Historical checkpoints without this child remain
loadable and have unavailable provenance. A genuine later capture can assign
structural ordinals to an existing queue, but its retained old rows still have no
known producer or inference origin.

A queue lifetime binds the exact captured moderator source, generation and
installation epoch. Its rows occupy the contiguous interval `[head, next)`.
Keeping the queue preserves the head; consuming one actual head advances it once.
New values receive fresh ordinals from the previous `next`, including a value
equal to one just consumed. Empty queues retain the frontier. Payload equality
checks preservation of the retained suffix; it never identifies an occurrence or
proves which callback ran. The live transaction supplies the actual consumption
action and must commit each callback separately.

Each row retains a payload digest, optional producer correlation and optional
immutable inference origin. Known producers name an actual retained moderator
execution, invocation, completed Job attempt, Schedule delivery, or source setup
fence. Complete-state validation checks the referenced facts, source and origin
after all ordered deltas. A queued reference keeps its terminal producer record
from being pruned. A Run can remain named after terminal disposition; the claim
still grants no execution permission. Historical missing origin never becomes
positively unbound or a choice of the current Goal.

The universal document carrier owns unfamiliar row fields by ordinal. Fields on a
retained row follow that occurrence through a head removal or equal-value
append. Consuming a row with protected unfamiliar fields fails closed rather than
discarding their meaning. Replacing or removing a captured queue lifetime is also
currently closed: the live source/reset integration must provide explicit carrier
handling, rather than move old unfamiliar data onto a new source's ordinal zero.
This intermediate boundary includes Clear/reset paths that would retire the
positive metadata. The live source/reset integration must provide explicit
protected-carrier handling before enabling this facility as a complete lifecycle
feature. This restriction is temporary. Ordinary historical absence remains
unaffected, and metadata does not become mandatory on historical queues.

The checkpoint owns its complete raw preservation carrier, including unfamiliar
outer fields and identity-snapshot fields as well as queue metadata. Its parent
state and delta documents retain this validated child as a complete value. A
native update must already contain every compatible retained field; persistence
does not silently repair a stripped Actor or Memory candidate. Native state,
immutable delta replay and the reopened Store therefore expose the same raw
checkpoint, including field order and unfamiliar number spellings.

To preserve this contract, live owners must update the actual previous checkpoint
through the validated checkpoint operation. Keeping the queue preserves its absent, null or present
form. Replacing known snapshot or queue state uses the existing structured
universal codec to capture incoming fields and adopt retained custody. A first
metadata bootstrap may preserve same-observer historical extensions, without
inventing a prior installation epoch. A known source/lifetime change cannot move
retained extensions; incompatible deletion and ordinal reassignment fail before
commit. This data operation grants no Manager resource, source admission or
execution permission, and does not relax the source/reset restriction above.

The initial metadata profile admits at most 256 rows and a complete encoded child
of at most 1 MiB, including unfamiliar fields. Hashing accepts a bounded raw queue
of at most 8 MiB, with finite depth and node limits. Existing whole-session and
transaction profiles remain authoritative. These are admission bounds, not a
promise that arbitrary future callback output will fit.

Positive queue metadata or stored Schedule creator provenance requires the outer
`runtime.queue-provenance.v1` semantics marker on the actual state and delta
documents. Older readers that do not understand these facts reject the document
instead of treating the new fields as ignorable extensions. Historical absence
and null retain their existing compatibility behavior. The enclosing Store
transaction and snapshot readers explicitly recognize this marker only on their
state/delta children; unfamiliar child markers and marked outer envelopes still
reject.

A Schedule can separately retain its exact original source and optional inference
origin from creation through claim, delivery, cancellation and recovery. This
claim is immutable, including absence. A Run origin must agree with an actual
retained Schedule occurrence in that Run; an unbound claim must not contradict
retained Run custody. Public positive receipts use Schedule wire version 4,
including nullable ownership; older public event and replacement-snapshot readers
reject that version. This evidence neither creates a Schedule nor authorizes its
callback, cancellation or Model effects.

These modules qualify durable facts and preservation. Live producer capture,
current original-authority handoffs, exact queue claim acknowledgement, and Model
allocation remain separate integration requirements. Reopening a stored queue
restores facts; it does not reconstruct an expired proof or implicitly authorize
a continuation.

The implementation separates pure data from actual owners:

- `Queue_producer` validates finite producer correlations; `Moderator_queue` owns
  checked frontiers, payload digests, row codecs and preservation shape.
- `Moderator_checkpoint` keeps the optional named child beside the existing
  identity snapshot. `Queue_provenance_validation` checks the complete final
  facts and admissible before/after relation.
- `Session_transition` and `Session_document_transition` validate complete ordered
  mutations. The universal state carrier also validates original/adopted/encoded
  values; raw child bounds include retained unfamiliar fields. Intermediate
  structural delta leaves do not grant authority.
- `Queue_provenance_semantics` owns the conditional outer feature fence. Existing
  whole-document, transaction and capacity owners continue to impose their fixed
  configured limits.
- `Agent_protocol.Schedule_provenance` retains Schedule source/origin facts. Its
  protocol codec grants no execution permission; final carrier validation resolves
  any named Run against its actual retained Schedule occurrence.


## Consumed occurrence receipts

A queued moderator Event can retain a `queue_occurrence` capture: the original
source installation, nonnegative ordinal and nullable original inference origin.
The physical `operation_id` remains the operation that drained the queue. If
operation A drains an occurrence originally attributed to Run B, the completed
Event and its emitted child retain B. Explicitly unbound stays unbound, and an
explicitly unavailable captured origin stays unavailable. Neither case is
reclassified from the drainer. A capture does not create Run-owned work, grant a
live callback borrow or authorize a Model request.

Positive captures use Event receipt wire version 5. That version requires a
capture; missing or null rejects. Older receipt encodings remain absent and keep
their existing causal lookup. A capture cannot be added to an existing receipt,
removed or reassigned. Its structured universal child preserves compatible
unfamiliar fields, bounded together with known fields to 4 KiB and depth 8.
State and journal deltas retain the receipt version, so an older reader cannot
silently discard the new capture and fall back to the physical drainer.

Fresh Running admission checks the exact previous source, head ordinal, nullable
origin, payload and known checkpoint digest. If the previous queue has no
metadata, structural bootstrap numbers only its actual old values; all their
original captures remain unavailable. Incoming fields cannot certify their own
origin. Ordered completion saves the completed receipt, consumed frontier and
resulting checkpoint together; equal reemission receives a fresh ordinal. The
complete before/after validators accept this ordered Running-to-Completed batch,
while standalone snapshot validation also rejects contradictory dispositions.

In the current source lifetime, a Running or unretired failed/interrupted receipt
must still identify the actual head and its exact origin and payload. A completed
or retired receipt must precede the current head. Later checkpoint edits and
consumption do not require an old receipt's checkpoint digest to equal the new
checkpoint. A strictly later retained installation epoch permits truthful
terminal history, including after source removal; a Running receipt cannot
survive as live work from an older installation. Future epochs and same-epoch
source or generation mismatches reject. A named Run must remain retained in the
same session and exact original source lifetime, even after it becomes terminal.

A known Running receipt cannot disappear as implicit termination. An unretired
failure cannot be erased while its occurrence is still reclaimable. Terminal
history may follow ordinary retention once a later actual installation makes the
old occurrence unavailable, or once completion/retirement has consumed it. Queue
producer references still prevent pruning a referenced receipt. These rules
apply to whole-state replacement and document adoption as well as ordinary
ordered deltas; protected unfamiliar fields retain their existing safeguards.

Disk qualification uses the existing physical journal, commit writer and snapshot
recovery adapters, including save rejection and an acknowledged commit whose
response is lost. Those tests qualify durable facts and exact preservation.
Actual opaque Manager claim stamping, live emitted-child custody, current
authority and provider execution remain separate integration gates. Reopening
these receipts restores evidence without reviving a permission or replaying an
effect. The existing positive source/reset metadata pause remains in force until
the live retirement integration handles it explicitly.
