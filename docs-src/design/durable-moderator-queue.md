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
discarding their meaning. A known queue can be removed or replaced only through
an actual source/reset transition: its original full source must match the
retained installation, that installation must advance exactly one epoch, and the
session generation must stay unchanged or advance once. Existing Run retirement
and immutable receipt rules still apply. The destination checkpoint and any new
queue must match the actual new installation. A new queue starts at ordinal zero
and independently validates its payloads and retained producer references; an
epoch change alone cannot certify arbitrary rows. A prior Running queue occurrence
cannot be retired through this path. Truthful terminal history retains its
existing later-epoch rules. Ordinary historical absence remains unaffected, and
metadata does not become mandatory on historical queues.

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
execution permission. Protected unfamiliar checkpoint data still prevents a
source/reset transition from removing it or moving it onto a new lifetime;
known-only retirement does not weaken this preservation rule.

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

The legacy callback boundary selects one actual queue head under the Manager's
execution gate. Its opaque callback token belongs to that exact compiled Manager
and expires when admission returns. The actor checks the original saved
checkpoint before constructing a prospective update; matching source text alone
does not make a stale Manager current. Callback services are scoped to execution
and restored on success, error or cancellation. Standalone embeddings retain
their explicitly supplied services.

The same installed boundary can admit an ordinary lifecycle or foreground event,
or an event under an exact claimed Job attempt. An internal event must use the
actual queue head. The actor retains fresh live admissions separately from
historical absence: a genuine legacy admission without an original authentication
object can return `None`; missing historical custody rejects. An expired original
object also rejects rather than becoming `None`. A Start admits at most one
pending startup/resume continuation, consumed only after its claim is acknowledged.
It does not lend credentials to unrelated queue entries. The existing operation
cell and Job scope supply their actual ownership; a saved identifier cannot
reconstruct that custody.

Ordinary claims validate the original saved checkpoint, stage the actual parent
loan, and recheck the installed Manager, source and physical owner before save.
After acknowledgement the actor installs the callback loan and consumes any
pending lifecycle continuation before publication. A failed or uncertain save
discards speculative custody. Returned proofs expire with the callback and its
Manager/source binding. Worker replacement rejects while a callback holds its
borrow. After that callback returns, its loan is already expired; clearing the
installed Manager also prevents a new callback under the old resource, even while
the physical operation remains running. This boundary currently
keeps Run actions unavailable; producer and runtime integration must supply the
remaining Run-specific services.

The first actor claim may atomically install source facts, structural queue
metadata and its execution receipt. This creates no Run, Goal episode or inferred
unbound origin. The source installation logic is shared with Goal metadata and
reuses an existing matching epoch. Queue consumption and state installation
require the checkpoint acknowledgement; failed or interrupted claims prevent
automatic replay of their external effects.

Until a producer supplies genuine continuation custody, this boundary treats its
queue scope as unavailable. It cannot borrow the physical drainer's credentials,
wake a Run from a completion-looking payload, obtain Run actions, or request an
inference turn or compaction. A pure callback can still update its state and
consume its actual head. Fresh suffix values name the actual completed Event
and preserve its original nullable inference origin. An admitted callback stages
one independent live loan per new ordinal before saving; only the checkpoint ACK
installs these loans, before publication. Failed or uncertain saves discard the
speculative loans. A publication failure after ACK retains acknowledged children.
The next exact queue claim transfers and consumes only its matching live row.
Pending follow-up custody uses the same ACK boundary for Event checkpoints,
returned native invocations and moderator observations. Its speculative loan is
prepared before save and installed before publication. A publication exception
may terminate the actual actor owner; it does not undo the committed receipt or
turn a saved receipt into restored permission after reopen.
Historical and reopened facts do not recreate this inventory, and an unavailable
callback cannot lend custody to its emitted children. The inventory is bounded by
the existing queue limit and expires with consumption, source retirement or the
installed Manager resource. Event-produced Job/Timer lookalikes never borrow Run
wakes or action services. Job handoffs, constructor carryover and full runtime
routing remain integration work. These lower-level APIs alone do
not provide those features.

The callback-bound Schedule service provides immediate legacy timer admission
and cancellation without using constructor defaults. Each call checks the exact
live callback and original admission. Creation keeps the existing nonnegative,
checked wall-clock delay and raw payload semantics, and saves creator provenance
with an independent process-local loan. The loan installs after ACK and before
publication; failed saves discard only the speculative loan. Cancellation still
accepts any retained Scheduled ID after checking the caller, including historical
timers without provenance. Private `Legacy_schedule_custody` keeps at most one
loan per retained timer and retires it with cancellation, source or runtime
retirement. Restoring a timer never restores the loan.

Legacy Schedule delivery uses the actual installed Manager and its prepared
checkpoint append. The actor compares the exact saved Delivering record and old
checkpoint, preserves every old queue occurrence, and atomically saves Delivered
with one captured ordinal. Positive creator provenance must match the current
full source lifetime; it cannot be relabelled as a new source or unavailable.
Omitting the Manager cannot select a historical fallback when positive creator
facts or queue metadata already exist. The v1 owned Timer-frame path retains its
separate strict validation.

The completed-fact handoff stages independent custody before Delivered retires
the old timer loan. A known expired admission is carried as an explicit negative
queue cell so the truthful delivery can persist while later execution rejects.
Missing historical or reopened custody stays unavailable. These cases never
become a fresh legacy None admission. Ordinary new-work selection and emitted
children still require a current parent. General host callback routing requires
its actual execution owner; data provenance alone does not provide it.

Active Job custody uses one private `Job_custody` inventory keyed by the exact
acknowledged Job record and attempt. Genuine initialization and Run-constructor
owners can admit a fresh loan, including an actual legacy admission with no
original proof. Historical absence remains unavailable. A captured moderator
source may have no installed Manager during construction; that source fact does
not create a Manager loan, and a host without a moderator can still run Model
work.

Staged Jobs retain independent speculative loans in the existing staged-work
container. The real result owner prepares one candidate against the current
inventory for all selected Jobs, then repeats the exact-record, source and
original-admission checks before persistence. After ACK the actor installs every
selected loan before negative reconciliation, capacity publication or durable
event publication. Failed saves and cancelled selections close speculative loans;
outer cleanup cannot retire an acknowledged selected loan. The genuine claim,
retry and deferred-completion writers preserve the same admission across their
validated phase changes. Raw Job edits only retire inconsistent custody.

Genuine terminal writers retain an exact pending completed record in that same
inventory. Each completed Job uses its own admission, including recursively
cancelled dependencies; artifact-backed results bind only after their final
stored representation is known. An independent completed-fact continuation can
be current, known expired, or absent. Expiry does not prevent saving truthful
outcomes. Failed saves close only speculative children, and terminal custody
cannot admit another active invocation. Actual Stop commits retire all live Job
custody, including unchanged Graceful Jobs. Historical recovery restores facts
without live loans. Private constructor handoff plans preserve the same permission
object while changing only an owned resource binding; negative resource plans
never adopt a source-only row. Preparation-owned Job selections require the
actor's private owner and advance only at real creation/checkpoint ACKs.
The active initialization scope or Run preparation supplies only its own
acknowledged constructor subset. Its actual checkpoint ACK can bind a first
source or physical Manager without changing the permission object. Completed
no-delivery work may have no live row and stays unavailable. A matching bare
Manager snapshot reuses the original full checkpoint when its validated known
state is unchanged; explicit metadata or changed state still passes through the
strict carrier rules. Failed ACK leaves the old subset and binding intact.

Runtime replacement reconciles captured Job loans once against the intended
final physical Manager, before publishing the new resource. An intermediate
cleared slot does not discard an already acknowledged new binding. Plain
retirement removes old physical-resource loans; source-only rows never gain a
Manager by matching IDs. Actual legacy Job callback admission and final checks
require the exact source and captured physical Manager. Ordinary Model work
without a moderator keeps its separate execution topology.

The legacy Job delivery adapter reuses the existing Model completion event or
Async background frame. It compares the original known checkpoint separately
from complete queue metadata, requires one exact append, and stages the terminal
Job's independent continuation before marking it Delivered. The queue loan
installs after ACK and before publication. Known expiry records an explicit
expired continuation while preserving the truthful Delivered fact; missing
recovered custody cannot become a fresh legacy admission. A present terminal row
must match the exact physical Manager and full source. A named original Run must
remain retained under that exact source; a missing or conflicting Run rejects.
Historical Model records with no provable full source epoch append unavailable
capture metadata and retain their original stored inference origin. Source-free
Async completions cannot inherit a later moderator. Qualified v1 frame and
checkpoint validation retain their existing strict path. Actual delivery and
provider qualification remain separate from these adapter contracts.

The implementation separates pure data from actual owners:

- Private `Execution_custody` owns concrete original-proof epochs and admission
  cells; closing a lifecycle admission differs from retiring its contained proof.
  Private `Legacy_queue_custody` owns the bounded exact Manager/source/Entry
  inventory. The actor retains physical admission decisions and every ACK install.
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
  any named Run against its retained session and full source identity. Causal
  attribution does not create a Schedule work obligation; any actual retained
  Schedule owner must agree with it.


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
effect. Known-only queue retirement follows the source/reset validation above;
protected unfamiliar data still rejects incompatible retirement. Live custody
retirement remains separate from the persisted receipt evidence.

## Constructor checkpoints

Pending initialization and authenticated Ready resource construction share one
opaque constructor scope. Ready construction requires current original custody
or its genuine Run preparation; rebuilding resources without that owner does not
create a new effects admission. Completing or failing Pending initialization is
not permitted through a Ready scope. Both purposes retire through the same owner
cleanup.

An owned Ready checkpoint hands off the initialization scope's acknowledged Jobs
together with any actual Run-preparation subset. It preserves their existing
permission objects and binds only the intended runtime resource. Missing completed
Jobs that require no delivery remain unavailable. An unchanged checkpoint without
a binding change performs no write. Rejected persistence leaves the prior basis
and bindings intact; accepted persistence installs them before reconciliation or
publication.

The original control and inference Selection basis stays immutable. A private
acknowledged projection records only the moderator checkpoint, Run index and Event
receipts from the exact owned checkpoint. It is checked before persistence and
installed after that acknowledgement; unrelated changes still invalidate the
constructor. This does not authorize callback execution or grant a missing
Manager. The constructor consumer claims only its actual Session_start or
Session_resume callback, through the same receipt and callback CPS as installed
legacy execution. Claim ACK atomically installs source facts, unavailable
structural rows and the Running receipt. Emitted suffixes and immediate timers
record only that owner's acknowledged selected keys over the existing loan
inventories; restored rows are never selected. Final Pending/Ready handoff binds
only that actual Manager without renewing permission. Unhanded cleanup and a
failed resource install retire only the constructor's selected loans, leaving
truthful durable effects intact.

An executed callback failure can settle its exact owned receipt without asking
for new Run-preparation authorization; its ACK closes that preparation. Foreign
control or source changes still reject the constructor basis. The native Tool
adapter reuses that same callback and Event. Its exact claim and finish ACKs
advance only the owned Invocation row, preserving every unrelated Invocation and
control field. Fresh effects recheck original authority; a truthful returned
outcome does not acquire new permission. The legacy bridge uses the actual
host-prepared native ceiling and protocol value bounds, plus the standard
100-attempt host call-scope guard. It does not infer authority from required-tool
names or impose a v1 script budget. Same-Manager callback services select Tool,
Model and Schedule together, without falling back when the selected epoch expires.
Actual host routing, interactive permission ownership and the paired Model
provider/accounting integration remain separate consumers.

Queue and timer loan retention checks the actual full source installation and
unchanged retained facts separately from execution eligibility. A constructor
checkpoint while initially Stopped does not itself retire those facts. Resource
replacement retains only loans already tied to the intended final physical
Manager; removing or replacing that resource retires mismatched loans. Neither
operation restores missing custody or renews known expired custody. Callback and
delivery effects still require their exact current owner and resource checks.
An explicit successful Stop retires these loans before publication, including
Stop while already Stopped; rejected persistence preserves them. Constructor
selected-subset transfer and failed-resource cleanup still require their actual
owner integration.

The actual host constructor supplies an explicit CPS port to both ordinary and
qualified legacy builds. Tool, immediate Schedule and constructor Model services
share that exact actor callback. Ordinary builds obtain their native services from
the same host policy construction as qualified builds; they do not invent a v1
definition. A genuine constructor can run while initially Stopped, but its callback
still checks the actual halt/failure flags, original admission and exact scope.
Interactive approval retains its existing running-owner requirement.

Cold loading a Ready session invokes `Session_resume` with the explicitly supplied
current actor through the same constructor adapter. It preserves the old queue
prefix and source installation; any emitted suffix belongs to the new Resume
Event. Existing queued data does not acquire fresh authority from that load.
Ensuring an already loaded runtime does not replay Resume or append its effects.
Host builders without the authenticated construction port reject a supplied actor
on cold load rather than discarding it; this limitation does not apply to the
Factory's legacy ChatML constructor.

A successful callback records its exact full checkpoint as acknowledged data before
publication. Reading it after commit neither re-encodes Manager state nor reuses
closed effect authority. Repeated startup calls return that same checkpoint. Owned
startup preserves acknowledged queued suffixes for installed per-head delivery and
never drains them with constructor defaults. Standalone embeddings that explicitly
supply no construction port retain their existing behavior. Installed host routing
uses the separate callback services described below; its actual foreground and
idle execution qualification remains a distinct gate from constructor tests.

The shared legacy callback bridge returns the actual committed outcome together
with that acknowledged checkpoint. Its explicit notification list commits in the
same Event acknowledgement as the full prospective queue and request intent;
consumers must not publish or schedule those facts again. An empty selected queue
returns no outcome without claiming an Event. The bridge itself does not drain a
batch or install foreground or idle routing.

Installed callback services use a separate Factory claim and the Actor's exact
callback Model admission port. Every admission rejects constructor scopes and
rechecks the actual callback, installed Manager, source and original custody.
Ordinary synchronous child Jobs retain their existing claimed execution and
inference lifetime. Constructor services keep their genuine initialization owner;
a missing installed owner cannot fall back to it.

The host-backed foreground adapter binds the actual operation capabilities before
any submitted-item event. The stream hooks and native `Pre_tool_call` moderation
use that same handler factory; native boundaries read current canonical history
from the Actor rather than a captured operation history. Each selected callback
uses its exact installed claim, never the constructor's closed services. The
provider-before-model hook acknowledges the retained turn intent. Foreground
compaction and dependent-turn requests follow the same durable consumption policy
as v1; notifications are removed from returned outcomes only after their Event ACK
has already published them. Worker checkpoint boundaries confirm agreement with
that same durable checkpoint: under the actual registered foreground operation
and installed source, an unchanged bare Manager snapshot reuses the full retained
carrier without writing. An exact full carrier is also unchanged; altered queue
metadata, extensions or known state are rejected. Terminal completion retains the
same owned full carrier before releasing foreground registration. The unowned
embedding checkpoint path keeps its existing strict transition behavior.

The runtime exposes a separate idle drain with no operation-ID argument or
foreground provider hook. Runtime_owner checks the actual retained legacy source
and its failed/interrupted claims before every head, reads current canonical
history and invokes one per-head ACK under its cancellable runtime lease. It
clamps the batch to the existing idle callback budget, stops on empty queue, halt,
End_session or failure, and consumes durable follow-up once after the batch. It
does not acquire an aggregate idle callback or publish notifications twice.
Standalone embeddings without the explicit host adapter retain their existing
aggregate compatibility path. Legacy source identity does not become a v1 native
observation capability; that separate delivery path remains unchanged.

Constructor `Model.call` and `Model.spawn` use the dedicated initialization
services. The ordinary installed callback port does not accept constructor
custody and must not substitute for that path.

Deferred native observations retain a separate process-local admission component
within the existing bounded continuation inventory. The completed native result
ACK installs its loan before publication; the exact observation claim ACK transfers
it once. This preserves the producer's original admission when another operation
drains the observation. The handler follow-up component has an independent lifetime.
Historical absence and known expiry remain explicit negative facts: they can settle
truthful outcomes but cannot authorize a new Model call, Turn, or Compact. Stop,
source retirement, owner shutdown, and recovery never reconstruct these loans.

A constructor's native permission wait retains the actual dispatched Invocation,
its Event borrow, callback and initialization scope. Opening or resolving that
selected permission advances the existing initialization acknowledgement only
when the exact permission rows, optional new grant and waiter-derived lifecycle
are accepted in the same durable transition. The acknowledgement is installed
before registering or fulfilling the waiter. Ordinary constructor checkpoints do
not absorb unrelated permissions, grants or lifecycle changes; those changes
still invalidate the constructor basis. The same selected-data validation covers
native-result cancellation of its pending permissions. Failed writes advance
neither the acknowledgement nor the waiter, and approval data never renews the
original authentication or revives a retired constructor. Existing Run preparation
ownership advances through the selected successful ACK; unavailable or expired
owners keep the existing failure/retirement behavior.

Selected denial and timeout resolution are factual settlement, not new effect
admission. They retain exact physical constructor and Run-preparation basis
checks while allowing an expired original to remain expired; successful ACK
advances only that same preparation's owned revision. Opening and approval keep
their currentness checks. Native-result cleanup likewise retains its existing
settlement-only checks, so a negative outcome can unblock its waiter without
renewing authentication or reviving a closed scope.

Stop retires an actual in-flight constructor in both Graceful and Cancel modes.
Its existing durable transaction records the exact owned Running Event as
Interrupted and its still-unresolved native children as Cancelled, preserving any
already resolved winners and unrelated metadata. These outcomes mean retirement
before result acknowledgement; external effects may already have occurred.
Graceful does not acquire physical cancellation or rollback, and its existing Job
and Schedule policies remain unchanged. After ACK and before publication, the
actor installs a distinct terminal-disposal receipt, closes the selected child
custody and retires that exact constructor. A failed Stop write changes none of
those live owners. The exact closed native execution cells remain owned until actual callback
unwind, where existing cleanup releases their provisional resources and the
borrow without another terminal write. Graceful therefore retains that owned
resource lifetime until the callback returns; it does not claim immediate
external cancellation. Disposal never counts as a successful Manager checkpoint or
refreshes the constructor's acknowledged basis.


Actual Model Job completion joins the owning Run's immutable attempt evidence in
the same Job transaction. The raw Model result remains unchanged and is validated
through the existing terminal-result adapter. A failed attempt and its queued
retry successor are distinct owned occurrences; the unresolved successor prevents
premature completion. Operation and Job completion use one aggregate settlement
law and create at most one next Run revision with its canonical terminal receipt.
This handles both a synchronous child finishing before its root and a spawned
child finishing last. Waiting still requires its actual callback; successful
Workflow requires a retained Finish, while quiescent Single_turn completes.
Existing failure precedence includes retained failed attempts.

A Job-driven terminal Run uses a genuine paired wall/monotonic completion sample
and installs its live timing plan only after the same durable ACK. An invalid live
sample does not erase a truthful completion or become a recovered wall anchor.
An actual initialization Model owner advances its exact acknowledged Run projection
only with that ACK, retaining its original authentication and Run preparation;
failed persistence cannot freshen that constructor basis.


Owned Model cancellation and interruption join that same transaction law. A
queued cancellation records its exact planned next attempt without inventing an
execution; a running cancellation or interruption records the actual current
attempt. Either may settle the owning Run when all work is known terminal. The
Job, custody retirement, Run proof and any terminal receipt share one ACK, with
the genuine clock pair and existing Run preparation forwarded by the actual
writer. Late success cannot overwrite a terminal attempt, and cancelling an
already Interrupted Model is a no-op preserving that winner. These statuses do
not promise rollback of external effects.

The existing queued Async_tool policy is preserved: an exact Wait retires with
truthful cancellation evidence, while non-waiting queued cancellation retains
its Run lifecycle. Actual completed Async_tool attempts use the shared aggregate
settlement described above.
