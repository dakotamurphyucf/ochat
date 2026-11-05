# Durable recurring automation

Status: accepted engineering design with implemented timing, bounded pure due
planning, and durable controller metadata. The `ochat.recurrence_rules` library implements validated timing rules,
finite zone models, clock arithmetic and resumable cursors. The session actor owns
validated captured definitions, keyed control receipts and lifecycle retirement.
The configured daemon host publishes bounded due plans through the existing live
scheduler and actor persistence owner. Execution admission, target dispatch and
public user controls remain integration work. The retained-root timezone loader
supplies bounded acquisition under explicit host root policy. Passing the pure
timing suite does not qualify host scheduling or target execution.

A recurrence belongs to a durable controller session. Finishing an occurrence or
disconnecting a client leaves future occurrences enabled. Explicitly stopping the
controller durably pauses future launches; restarting it does not silently resume
them. Deleting the controller retires its definitions and revokes unstarted
launch intents, subject to existing live-work deletion rules. This lifetime policy
is approved. Client attachment, controller lifetime and occurrence execution are
separate concerns.

## Execution and ownership

Use the existing actor persistence, one-shot schedule service, shared Run
lifecycle, job capacity and session factory. Extend the same
`Schedule_scheduler` dispatcher; recurrence does not introduce a second scheduler
or a polling fiber per definition. A scheduling notification is a hint to inspect
current actor state, rather than authority to execute.

The following identities must remain distinct:

| Identity | Meaning |
| --- | --- |
| Controller session | Current source, installation, principal and host authority owning definitions. |
| Recurrence and immutable revision | Rule, target capture, limits and policy being planned. |
| Slot and occurrence | One selected nominal instant under that revision, with immutable execution origin. |
| Alarm and callback | Existing delivery machinery, which can enqueue work but cannot prove its completion. |
| Target session and Run | Actual execution and its custody, admission key and terminal receipt. |

An occurrence has one durable association to its actual Run. A fresh target also
has its exact captured child and protected creation reservation. A completed
callback, delivered timer, installed child or disconnected client is not a
successful occurrence. Only actual Run evidence can establish its outcome.

Existing primitives establish these boundaries:

- [Schedule](../../lib/agent_protocol/schedule.mli) is explicitly one-shot.
  Delivered schedules must not have their deadlines rewritten to represent new
  occurrences. [Schedule delivery](../../lib/chat_response/schedule_delivery.ml)
  commits callback enqueue; it does not commit business completion.
- [Schedule_scheduler](../../lib/agent_server/schedule_scheduler.ml) owns dispatch,
  recovery and joined shutdown. Actor-owned claims retain current source and
  callback validation.
- [Extension_clock](../../lib/agent_session/extension_clock.mli) distinguishes
  process-local elapsed anchors from restart wall reconciliation.
  [Extension_stop](../../lib/agent_session/extension_stop.mli) owns existing
  graceful-stop and cancellation behavior. Recurrence adds its explicit durable
  controller pause rather than inferring it from UI visibility.
- [Session_factory](../../lib/agent_server/session_factory.mli) owns authored and
  generated creation; the [delegation store](../../lib/agent_store/delegation_store.mli)
  owns stable reservations and parent linking. The existing
  [Generated_definition](../../lib/agent_session/generated_definition.mli) path
  accepts a restricted generated-child subset. Complete captured authored programs
  require their actual authored preparation path and a qualified protected
  recurrence creation/link owner; the subset is not a substitute.
- [Run_start_service](../../lib/agent_server/run_start_service.mli),
  [Run_start](../../lib/agent_protocol/run_start.mli) and
  [Run_receipt](../../lib/agent_protocol/run_receipt.mli) own execution admission
  and outcome evidence. [Run_retirement](../../lib/agent_session/run_retirement.mli)
  handles interrupted custody without inventing success or replaying effects.

The checked lifecycle source helpers compose explicit component obligations and
at most one continuation or exact wait. Their Run adapter stages existing native
Run decisions from a genuine actor callback. They do not create a headless caller,
a recurrence ledger, an execution scope or a new Run. Shared actor-authorized
headless controller admission is a separate required integration boundary.
The shared [Run start service](../lib/agent_server/run_start_service.doc.md) now
supports an internal authenticated headless host command; it does not yet mint
authority from a goal or recurrence record.

## Timing contract

[Pure recurrence rules](../lib/recurrence_rules.doc.md) document the implemented
API. Domain constructors and versioned JSON readers validate inputs; abstract
values have no unchecked generated sexp reader. Periods use positive integer
milliseconds and timestamp arithmetic checks representable endpoints.

Fixed-rate intervals retain a phase: slot `n` follows the original phase plus
`n * period`, regardless of callback duration. Live due counts use injected
elapsed time through `Clock.due_count`, then advance the cursor arithmetically
with `Cursor.Interval.consume_count`. Advancing a million missed slots must not
require a million iterations. The actor must commit the consumed cursor and
matching live phase together. A wall-clock correction cannot change a live
fixed-rate due count.

Fixed-delay intervals are the authoring default. Their next deadline follows
actual committed terminal completion plus the period. Timer enqueue and callback
return cannot substitute for completion. A wall rollback before completion does
not invalidate a real completion timestamp; slot identity still increases.

Process monotonic origins do not survive restart. Recovery uses the saved wall
deadline and the new process clock, then follows elapsed time again. Overdue work
is subject to the saved missed-work policy. This discontinuity is explicit;
recovery does not reconstruct an unavailable historical monotonic clock. The
pure `due_through` helper is for wall-based restart reconciliation, not the live
fixed-rate clock path.

Calendar rules select an explicit local time on daily dates, selected weekdays or
selected monthly days, between an inclusive start and optional end date. Missing
monthly dates are skipped. Advance civil labels rather than adding 86400 UTC
seconds. A named zone is mandatory; ambient `TZ`, local machine settings and
ambiguous abbreviations do not supply defaults. These are OChat rules without a
cron or RRULE language claim.

Calendar defaults skip nonexistent local times and select the earlier repeated
instant. `At_gap_end`, `Later` and `Both` are explicit policies. Repeated candidates
have distinct fold indices. If different labels select the same UTC instant,
coalesce them to the earlier label. Emitted instants strictly increase.

The pure resolver admits offsets within plus or minus 24 hours and at most 4096
strictly ordered transitions in explicit inclusive coverage. It inverts admitted
offsets and validates actual forward mapping. It does not assume unambiguous
midnight, an hour-long change or at most one shift per day. It requires complete
coverage for the 24-hour window on both sides of the requested local scalar;
otherwise `Out_of_coverage` preserves uncertainty, including near representable
timestamp endpoints.

Calendar enumeration retains one earliest UTC candidate while scanning a bounded
number of new civil dates. Future candidates are bounded below by their local
scalar minus the maximum offset. Emit only when that bound proves the retained
candidate is earliest. Then restart at the earliest date possibly following the
last emitted instant and filter consumed timestamps. This handles repetitions
interleaved across civil dates without an unbounded candidate queue. `Continue`
is unfinished planning and must be persisted or resumed, rather than treated as
an empty schedule.

A retained candidate is revalidated against the actual rule and zone before
emission: date pattern, local time, fold or gap policy, UTC mapping, provenance
and position after the emitted frontier. Matching provenance alone is not proof.
Changed zone provenance discards speculative lookahead. Cursor decoding accepts
partly representable endpoint days and one exhausted-date sentinel without
requiring a representable midnight. The durable owner binds every cursor to its
immutable definition revision; a local decoder cannot establish that ownership.

## Timezone loading boundary

The pure `Zone` value is not a TZif reader or an installed IANA database. A separate
host adapter must acquire trusted configured zone bytes through explicit Eio
filesystem capability, bound input sizes and parsing work, validate the zone
name and record byte provenance. Unknown zones and unavailable data are explicit
errors. No blocking Unix/Stdlib channel loader or implicit ambient-zone fallback
belongs inside pure planning.

The pinned Core conversion helper assumes unambiguous midnight, and its
nanosecond implementation also assumes at most one shift in a local day. It
cannot alone qualify general skipped-date and midnight-transition behavior.
The pinned filename-based timezone parser also requires a separate IO review;
calling it from a pure-looking function does not remove blocking IO.

The pure `ochat.recurrence_zone_data` library decodes bounded exact TZif v2/v3
bytes and projects a fully requested finite interval into `Zone`. It validates
both block structures and the final type/footer association. Type 0 governs
before the first transition; with no transitions and no footer it governs all
time. With transitions but an empty footer, coverage ends strictly before the
last transition. Undefined `-00` intervals are unavailable. There is no fallback
or silent coverage clipping. Leap-bearing data and other file versions are
explicitly unsupported.

Self-contained POSIX footers support standard/daylight offsets, quoted names,
Julian/ordinal/month-week-day rules, default offsets/times, southern and negative
DST, nonhour differences, perpetual daylight and signed v3 transition times up
to 167 hours. A bounded Gregorian 400-year pattern supports future dates and
validates raw signed 64-bit table timestamps without narrowing them into
`Time_ns`. Implementation-defined external/default DST rules and effective
offsets beyond `Zone`'s 24-hour domain fail explicitly. The parser admits at most
1 MiB, 4096 transitions per block/projection, 256 types, 65536 designation bytes
and 4096 footer bytes, checking count-derived sizes before allocation.

Provenance hashes the exact input string once. Its bounded source label describes
the host-selected data set; TZif itself does not authenticate an IANA release
name. The library retains bounded parsed data, performs no IO and grants no
execution authority. Its fixture suite uses committed bytes generated from
pinned IANA source/reference tooling and independent calendar/offset oracles.

`Agent_session.Recurrence_zone_loader.load` supplies the Eio acquisition boundary.
It borrows an already-open, explicitly admitted closeable directory with an empty
Eio path; the caller keeps that root alive. Name, source label and ordered finite
coverage are validated before acquisition or checkpoints. It reads once using
the decoder's shared byte cap through `Chatml_local_source.with_root` and `read`,
then closes its joined native duplicate before pure decoding. The caller's
original root remains usable, and a returned zone contains no native resource.
The final checkpoint and cancellation check precede result publication.

The reader opens every directory component and final file with no-follow checks
and verifies a regular file before reading. The loader requires canonical files:
it rejects all symlink aliases, including aliases whose targets lie inside the
root. Deploy canonical copies of the selected zone files beneath an admitted root;
there is no alias resolver or fallback to another path. This boundary does not
rely on `Eio.Path` confinement flags, because some supported backend fallbacks can
follow symlinks. Original finite filesystem operation/code errors are retained,
while decoding and coverage errors remain distinct. Cancellation/checkpoint
failures return no partial zone and preserve resource ownership.

The reader observes file identity/revision changes around the exact bounded read;
it does not promise an atomic snapshot against metadata-preserving concurrent
rewrites. An explicit later load can select new bytes and provenance. An earlier
returned immutable zone and already materialized deadlines retain their selected
values. Actor scheduling, root admission policy and refresh selection remain
integration responsibilities; the loader creates no schedule, cache or registry.

Persist selected UTC time, nominal label, fold/adjustment and admitted zone
provenance with a materialized occurrence. Updated data may affect future
unmaterialized planning, while existing materialized occurrences retain their
selected deadlines. Missing or unsupported data pauses affected planning with an
inspectable reason. Provenance identifies data; it grants no execution authority.

## Durable controller metadata

The session actor owns one optional `Recurrence_catalog` inside the existing
session state, delta and snapshot carriers. Absence and explicit null retain
their historical meaning. Definitions, controls and cursors have separate typed
identities and revisions; a control command cannot advance a due cursor. The
initial owner produces no due occurrence, alarm, protected creation or execution
association. Those require their actual scheduler, factory and Run owners.

Each recurrence retains its immutable captured revisions, creator, controller,
source installation and lifecycle birth. Edits append one captured revision and
start it paused with its initial cursor. A retired ID remains a tombstone and
cannot be edited or enabled. The initial finite limits are 16 retained IDs,
eight revisions per ID, 256 immutable keyed command receipts and a complete
32 MiB catalog, including compatible unknown fields and control growth reserve.
These bounds do not enlarge the enclosing host document or journal profile.
There is no eviction at saturation. A host can impose smaller limits.

The policy defaults are serial execution, coalescing, a 60,000 ms lateness grace,
one pending occurrence, 64 detailed history entries and 256 calendar steps per
planning pass. Supported maxima are 32 pending occurrences, 256 detailed history
entries, 4096 calendar steps and catch-up counts from 1 through 32. Grace must
convert exactly to a nonnegative int64 nanosecond duration. These are saved
constraints; this metadata owner does not implement misfire planning or history
compaction, and grants no permission to exceed current host ceilings.

Original operator proof and current host write policy precede receipt disclosure
and fresh mutations. Capture, edit and enable also compare exact catalog/control
frontiers, current source and lifecycle expectations. They require a genuine
Active/Automatic observation from the actual store, retained Handle and complete
current canonical/index projection. Stored IDs, receipts and selectors cannot
reconstruct this authority. Preparation may yield; the actor rechecks original
proof and currentness immediately before persistence.

Safe operator pause and retirement remain available without an installed
moderator. Source replacement or generation reset holds every enabled definition
for source review while preserving an existing pause. Disconnect and occurrence
completion do not change controller control. Explicit Stop, in either mode and
including an already-stopped session, commits a bulk recurrence pause before
ordinary Stop effects in the same serialized command. If a later Stop step
fails, the recurrences remain paused; no external cancellation or completed Stop
is claimed. The pause does not accept the original Stop audit. A successful final
Stop transaction retains the original audit, including the already-stopped path.

All nonretired definitions share the controller's current birth lifecycle epoch.
The existing lifecycle publication is authoritative. Archive or deletion changes
that permanent revision; old definitions are effectively retired immediately,
including read-only archived projections and failed reconciliation writes.
Restore and explicit Resume never revive them, even after old command receipts
expire. Before a new capture/enable can acquire target authority, the actor must
persist old-birth retirement; changed CAS expectations require a fresh request.

Crash recovery can legitimately have pending canonical metadata/index repair.
Safe retirement uses the existing genuine opaque lifecycle `Observation` under
the owned recovery Handle, re-reading and comparing it before planning and
commit. Reconciliation checks the original observation before acknowledging
retained lifecycle receipts, then obtains a fresh genuine observation with the
same session, lifecycle revision, status and admission. Acknowledgement can
replace receipt documents and invalidate the earlier Handle epoch; it never
permits a stale observation to authorize recovery. This recovery-only evidence authorizes no create, enable, runtime or
target admission. Retirement joins the existing recovery transaction before any
recurrence runtime or target source/execution admission. Normal metadata/index
repair then permits the stricter `Current` observation. Inert controller artifact
decoding and eligibility checks need no broad factory reorder.

Known control changes reserve actual complete-carrier growth and numeric
frontiers before admission. Enabled controls retain two bulk steps, paused
controls one and retired controls none. The catalog takes the maximum across its
same-epoch controls; the host adds that obligation to independent Run and other
owner credits. Compatible unknown fields, JSON overhead and counter growth are
charged. Private future JSON probes are capacity accounting only and are never
decoded or installed as lifecycle evidence. The actual persistence owner checks
combined Run, Goal and recurrence recovery records in its transaction/journal
profile, including sessions without Runs. General disk failure and unrelated
future Stop/job payloads remain ordinary persistence errors.

Both target variants are immutable selection data. Existing-session targets pin
exact session/source/configuration and bounded ordinary user input. Fresh targets
retain a complete validated `Captured_prompt_artifact`: exact manifest
publication, authored sources and both private program/content payload pools,
plus the original bounded prompt definition, parent configuration selectors and
capability pins. Nested carriers own their compatible unknowns and remain opaque
to enclosing codecs. Captured `root_file` is provenance and never a request to
reopen mutable files. Stored permissions, workspace/profile/runtime selectors and
native pins remain constraints; actual runtime admission rechecks current host
and native policies before initialization.

Authorized metadata views omit captured input, artifacts, workspace paths and
runtime configuration. Execution, authenticated target associations, due alarms,
protected occurrence retention and controller activation remain separate
integration boundaries. Unknown required semantics or unsupported execution
variants fail closed; control retirement alone never proves cancellation of
external work.

## Durable domain and policy

The durable domain must use validated types for controller scope, definition
revision, target capture, occurrence identity, associations and transitions.
Conceptually, definitions have `Enabled`, `Paused reason` or `Retired` state;
occurrences move through due/planned admission, associated execution and immutable
terminal outcome. Admission uncertainty remains explicit and consumes capacity.
Protocol DTOs cannot bypass cross-record ownership validation.

A definition freezes the principal, controller/source generation and installation,
rule, target capture, selected inference target, limits and policy. An occurrence
freezes the definition revision, monotone slot identity, selected time and capture
digest. The final execution gate must recheck current enabled state, revision and
authority even if an earlier scheduler check succeeded.

The selected defaults and configurable limits are separate:

| Concern | Default behavior | Host-configured bounded values |
| --- | --- | --- |
| Interval authoring | Fixed delay; fixed rate explicit. | Minimum/maximum period and definitions per controller. |
| Calendar discontinuity | Skip gaps, earlier repeat; invalid monthly date skipped. | Finite zone coverage, transition and scan-work ceilings. |
| Missed work | Coalesce to one latest eligible occurrence; record skipped ranges. | Finite lateness grace and catch-up count when explicitly selected. |
| Overlap | Serial execution with at most one pending coalesced occurrence. | Active/global job ceilings; parallelism only where target admission supports it. |
| Interrupted execution | Pause with recovery required; preserve uncertainty. | Inspection and explicit resolution controls. |
| History | Compact terminal rows plus aggregates and consumed high-water evidence. | Finite retained-row, byte and protected-creation capacities. |

Host configuration must supply validated finite limits before enabling dispatch;
there is no implicit unlimited grace, catch-up queue or history. `Skip`, bounded
`Coalesce` and bounded `Catch_up` are distinct missed-work policies. Existing-session
single-turn targets remain serial. Busy targets wait only within configured
pending capacity and grace. Uncertain admissions and nonterminal associated Runs
count against concurrency until reconciled; losing a receipt does not free a slot.

Pause atomically disables future admission and invalidates unadmitted alarms or
launch intents. Already-admitted work remains visible. Cancelling current work is
a separate explicit operation through existing Run/session cancellation owners.
Resume reauthorizes current scope and applies saved missed-work policy without
silently resetting consumed identities.

Edits use compare-and-swap revision checks. Rule, target or source changes create
a new immutable revision and invalidate unadmitted old-revision work. Already
associated Runs retain their original source and receipt identity. A retry can
resolve their original admission without reexecuting it; current authority still
gates disclosure. Source replacement requires explicit recapture/migration rather
than silently rewriting frozen bytes. Target deletion or generation mismatch
pauses with a typed reason instead of following a replacement by name.

Permission, profile, workspace and runtime-policy changes are rechecked before
claim, creation and activation, after yielding preparation, and through existing
parent-authority checks during child execution. Revoked or changed authority
pauses future work. Credentials remain on the execution host and are not copied
into definitions to preserve stale privileges.

Terminal history may be compacted only while retaining enough high-water and
tombstone evidence to reject old-slot replay. Nonterminal, admission-uncertain or
ambiguous-effect records cannot be pruned to make room. Detailed transcripts use
existing session/blob retention. Protected generated-creation reservations are
not ordinary history and currently have no automatic TTL: enforce a finite
creation quota and pause with capacity evidence. Long-lived reclamation requires
separately qualified tombstone/compaction support.

## Bounded due planning

[Recurrence_due_plan](../../lib/agent_session/recurrence_due_plan.mli) computes a
candidate from the exact captured definition, entry cursor, planning frontier,
injected clock pair and optional admitted zone. It performs no IO and launches no
work. [Recurrence_entry.apply_due](../../lib/agent_session/recurrence_entry.mli)
accepts that candidate only against its original basis while enabled. Cursor and
frontier advance together; the control revision stays unchanged. The enclosing
catalog advances once without manufacturing a user-command receipt. The future
actor adapter must persist this complete candidate before installing its returned
process-local phase or exposing selected work to target admission.

For intervals, live due counts and lateness use the original elapsed phase.
After restart, missed work is reconciled against wall time before a new elapsed
anchor is established. Long missed ranges use checked arithmetic, not a loop for
every elapsed slot. Due-time and grace equality are inclusive.

| Missed-work policy | Selection before target admission |
| --- | --- |
| `Skip` | Keep existing Ready work; when idle, select only the latest eligible nominal. |
| `Coalesce` | Replace older unadmitted Ready work with the latest eligible nominal; retain withdrawal evidence. |
| `Catch_up n` | Select earliest eligible nominals up to the bounded catch-up and remaining pending capacity. |

A calendar pass captures one wall cutoff and performs at most its configured
number of one-step cursor searches. New candidates remain private until search
proves that cutoff complete. Existing Ready work remains Ready while a later
batch is collecting. A committed future nominal retains its exact selected UTC
time and zone provenance even if subsequently admitted timezone bytes change.
Only the search for unmaterialized future nominals uses the new zone. Missing zone
configuration or insufficient coverage produces an inspectable issue; neither
silently substitutes UTC nor clips coverage. Target dispatch still rechecks current due/grace eligibility and authorization
after planning; a backward calendar wall correction cannot cause an early launch.

Fixed delay selects its initial occurrence once and waits for actual completion
ownership to rearm it. If that initial deadline is already outside grace, the
planner records `Initial_delay_missed` without consuming its slot or inventing a
completion. An explicit new definition/first due time reschedules it. Repeated
unchanged ticks leave durable state unchanged.

[Recurrence_frontier](../../lib/agent_session/recurrence_frontier.mli) retains
per-definition high-water marks and lifetime consumed, omitted and withdrawn
counts. Ready rows, private candidates and retained nominal details preserve
complete compatible JSON, including unknown fields. Pausing or retiring the same
definition withdraws unadmitted work and retires its alarm, but retains any
unconsumed lookahead as inert cursor-associated metadata. A new definition clears
that old lookahead together with the old cursor. Re-enabling creates a fresh alarm
identity; retired definitions cannot be enabled.

Occurrence-detail history and inert control-metadata history each obey the
captured `max_history` limit (1–256). Unknown fields on retired alarm or batch
containers move with the complete raw container into that control history.
Pruning details never resets high-water marks or totals. The complete frontier,
including unknown data and reserved control growth, is bounded to 2 MiB; enclosing
entry, catalog, host and journal limits can be stricter. Unsupported execution
variants reject instead of being interpreted as disposable unadmitted work.
Historical beta entries without a frontier remain inspectable and can be paused
or retired, but require a new recurrence identity for planning; an initial-looking
cursor is not evidence of zero historical consumption.

Fresh-child creation now has an actor-owned reservation boundary. The actor moves
the exact Ready nominal, including its complete compatible JSON, into immutable
protected custody before any child effect. The reservation fixes the original
child and transaction identities, occurrence, control revision, captured artifact,
target/source/birth bindings and request digest. Retrying under fresh current
authority returns those same identities; the stored original principal is audit
data. The actor requires genuine current `Create_sessions` authority and current
host target policy. The planning-only automation configuration described below
cannot supply that authority.

Only the actual reservation commit acknowledgement allows the actor to issue its
process-local creation permit. Metadata/index repair and a fresh genuine lifecycle
observation precede that publication. A lost acknowledgement or actor termination
publishes no permit; the existing journal writer sequence/hash and complete carrier
fences prevent stale follow-up writes from replacing uncertain durable identity.
Recovery reopens that owner and reauthorizes the same retained reservation.

An actor RPC reply does not make the caller's next effect atomic. After yielding
preparation, the factory refreshes through the issuing actor and invokes the
returned checked view's synchronous final check immediately before its own
mutation. That check repeats original proof/current nonyielding host policy,
actual actor liveness/runtime, lifecycle/source/control, exact retained row and
grace. The captured definition getter alone grants no authority, and a successful
check grants none across a later yield. Live interval grace retains the actual
selected elapsed anchor through reservation, including fixed-delay waiting;
calendar and recovered interval selections use their documented wall-time basis.

Protected reservations consume pending/serial capacity and cannot be replaced or
pruned by pause, edit, source retirement or detail-history compaction. Their
presence requires the supported `recurrence.child-reservation.v1` semantics marker;
an older reader must not reinterpret them as unadmitted gaps. They remain bounded
by the existing frontier, entry, catalog and complete host/journal profiles.
Before accepting a reservation, those owners reserve the complete immutable row
plus a bounded future association envelope of at most 4 KiB, depth 24, 64 fields
and 128 nodes. The concrete child owner must validate its complete actual envelope
against that quota before effects; the capacity witness is never stored or
interpreted as reference proof. Numeric credits sum one independent association
step per retained reservation in addition to the maximum bulk-control sequence
and the other session owners' obligations. Retirement does not spend unfinished
association credits.

The reservation owner does not install an artifact, create/link a child or admit
a Run. Those execution owners must validate actual same-store reference/origin
evidence and preserve uncertainty at every cut. Actual terminal associations must
eventually release pending/serial occupancy without discarding their retained
immutable evidence; retaining completed history must not permanently stall a
serial recurrence. Until those owners are integrated, protected reservations are
unsettled and continue to consume that capacity.

The pure tests compare bounded interval enumeration with arithmetic planning,
calendar chunked and whole searches, live wall corrections versus restart, exact
selected provenance across timezone changes, control dispositions and complete
carrier growth near the frontier limit. These establish planning/storage
invariants, not target execution, persistence acknowledgements or host scheduling.

## Existing and fresh targets

An existing target pins exact session identity, source generation and relationship.
A normal occurrence admits one `Single_turn` input through the existing input
parser/history allocator and Run preparation, without fabricating a writer
attachment. An optional existing-workflow wake mode needs its own typed target
constructor and a genuine exact Run wake; it cannot be inferred from a copied
continuation plan. Current one-unfinished-Workflow-per-source rules remain intact.

A fresh target retains the complete validated captured artifact, original prompt
definition, workspace/profile selectors and selected inference target once per
revision. The artifact includes exact manifest publication, authored sources and
program/content payload custody. Reuse that captured closure per occurrence;
never reopen the definition’s provenance-only root path. The captured-child
factory uses the existing captured parser/program/content replay and artifact
owners, then establishes current-authorized parent linkage and reciprocal
occurrence origin before activation. It does not reinterpret a full captured
program as the restricted generated-definition subset. Ordinary creation cannot
bypass these protected admission requirements.

The lower delegation ledger can retain a bounded immutable captured origin:
the exact original reservation and prompt-definition JSON plus the initial
session Spec digest with only its delegation locator normalized to absent.
The locator separately binds the full admission digest, avoiding a hash cycle.
This custody does not validate upper recurrence or prompt semantics, authorize
creation, install an artifact, link a child or initialize execution. The creation
owner must validate those original values with their actual domains and perform
fresh current-authority checks. Artifact payloads remain with the existing
artifact owner rather than being duplicated in each ledger record.

A fresh child is a new session and transaction; its captured source may be
identical to the controller's source. Only the captured-recurrence source kind
permits the same parent and child artifact revision. The creation owner must
verify that exact admitted revision and complete manifest, rather than fork or
replace the already reserved artifact. Session identities remain distinct.

The stopped-child creation boundary accepts an actor-issued reservation permit,
not decoded reservation data. After yielding preparation, it refreshes the
controller's original proof and current host policy, then performs a synchronous
final check immediately before each owned mutation. The reservation's original
principal remains immutable audit data; a different freshly authorized caller
may reconcile the same identities. Current creation scope, workspace/profile,
configuration, selected inference target and native pins must still match. The
planning-only automation principal does not supply target creation authority.

Artifact and child-directory publishers repeat that synchronous check after
all yielding staging and projection preparation, immediately before their atomic
rename. A denial cleans only staging acquired by that call. Any already
published destination remains durable evidence after a later failure; no denied
caller may remove it. If child publication already retained a projection intent,
the existing projection and index owners conservatively require recovery before
another attempt. A fresh authorized host may reconcile the same reservation and
identities; the old denied proof supplies no retry authority.

Native-pin and parent-moderation inspection borrows only an already installed
runtime through its existing owner gate and cancellable lease. Unloaded,
unloading or retiring parents reject without constructing a runtime. The short
borrow returns only validation and an immutable policy fingerprint; no native
resource or callback escapes. A captured Config preview likewise replays only
retained bytes, including per-script project and content custody. It grants no
installed-tree assurance; the complete installed inventory and exact publication
are verified again through ordinary captured reparse.

The factory reserves the existing lower delegation record, installs or reuses
the exact admitted manifest, and creates or reopens its one stopped child. The
new child's canonical history and allocators remain empty, runtime initialization
remains pending, and automatic initial start is false. No initializer, provider,
tool effect or Run is admitted. Recovery preserves that dormant state only when
the exact captured origin, initial Spec and reference match and all history,
Run, inference, host-turn and uncertainty evidence is pristine. Ordinary
interrupted initialization retains its existing failure behavior. Ordinary
execution remains fenced until reciprocal occurrence Run admission is supplied
by the execution owner. Sending a message requires an already loaded runtime
before allocating a history ID, so a rejected message cannot consume the
dormant child's history reservation or prevent its later captured recovery.

A genuine same-store child actor observation supplies the immutable reference
and a synchronous currentness check. The parent commits the exact association
before the lower record becomes Linked and before registry publication. An
association is stored beside its protected reservation in the same frontier;
`recurrence.child-association.v1` is required whenever it is present. Complete
association unknowns remain within the reserved 4 KiB profile. Capacity reserves
its outer array, required semantics, structural counters and coexistence with an
existing alarm, as well as independent control obligations. An association is
not terminal evidence and does not release serial pending capacity.

Failures or uncertain acknowledgements preserve original child, transaction and
artifact identities under the existing writer/recovery fences. Retrying verifies
the same actual effects; a missing child after its installed stage is not replaced.
Legacy generated-child recovery leaves captured records protected rather than
loading a parent or inventing reciprocal linkage. A fresh current permit is
required to reconcile them. This boundary creates and links stopped children;
actual target dispatch, Run admission, activation and terminal reconciliation
remain separate execution work.

Captured origins require the supported
`delegation.captured-recurrence.v1` document semantics marker. Older readers must
reject that meaning instead of treating it as ordinary generated creation.
Retry cannot substitute generated, authored-tool and captured recurrence source
kinds or replace the exact captured origin. Compatible unknowns, original field
presence and numeric lexemes remain immutable through stage/revocation updates.
An absent captured origin is omitted from newly authored admissions, preserving
the existing generated/authored admission encoding and reference hashes.

The existing 256 KiB complete delegation-record profile also bounds the captured
origin and its metadata; this is an explicit admission limit, not support for
every possible full prompt-definition size. Under the existing ledger mutex, a
new key charges its complete framed candidate and all retained records' positive
remaining known stage/revocation/collection growth beyond actual framed bytes
read. The preflight includes every preserved unknown byte and rejects a candidate
without room for those later known dispositions. With exclusive ownership,
supported schema and the same or larger scan policy, this preserves the admitted
future scan allowance. It is not a disk quota or a guarantee against external
mutation, arbitrary future schema growth or policy reduction. Atomic temporary
names still consume the existing scan entry budget. A matching historical retry
reflushes its original identities without a new candidate debit or a retroactive
claim that old records reserved missing headroom.

Both target kinds need one shared actor-authorized headless Run admission path.
Its typed caller binds host-issued current scope and original principal/key/digest,
while preserving normal preparation, CAS, custody and receipt semantics. It must
not fake a client attachment or manufacture an `Authored_start` already consumed
by an initialized session. Fresh startup is genuine and has exactly one activation
owner; it cannot race an independently persisted pending-initial-start path.

A fresh child's actual Run association must be reciprocal and durable before
execution. Parent linkage alone does not attribute its inference usage to a goal.
If a composed workflow charges a goal, a host-issued goal delegation must bind
the child before dispatch. Goals remain objective/control metadata with linked
Run episodes; recurrence remains the sole timed scheduling owner.

## Atomicity and recovery

Actor transactions and protected factory reservations are the linearization
points. Commit cursor advance, occurrence selection, skipped-range evidence and
alarm changes together. Never consume a slot before reserving its durable outcome.
Cross-actor creation/admission is a staged protocol with reciprocal evidence,
not a claimed distributed transaction. Preserve existing lock order and do not
hold one actor's mailbox while awaiting another.

| Failure cut | Required reconciliation |
| --- | --- |
| Before due-plan commit | Original cursor remains; recompute the same plan. |
| Due-plan save or acknowledgement lost | Recover the complete batch or old state through the persistence owner. |
| Alarm claimed or callback enqueued | Existing claim/frame evidence and occurrence key prevent a second launch; delivery is not completion. |
| Creation reserved before installation | Retry the exact protected child/artifact/transaction identity under current authority. |
| Child installed before link/association | Validate stopped child origin and reservation; repair exact association or retain revoked/stopped evidence. |
| Run admission acknowledgement lost | Resolve original key/digest to its actual receipt; uncertainty retains capacity and cannot allocate another Run. |
| Activation occurred before process death | Shared Run recovery preserves terminal winners or records Interrupted/Unconfirmed; never replay prior external effects. |
| Terminal receipt committed before controller import | Import that exact outcome idempotently; fixed-delay next-arm and import commit together. |
| Stop, edit or revocation races admission | Final gate rejects stale intent, or preserves an already committed association and applies explicit cancellation. |

Recovering a prepared host-owned launch with its original idempotency key is
separate from rerunning an interrupted script/native handler. Neither an absent
acknowledgement nor a surviving child directory proves an external effect safe
to repeat. All retries retain actual current authority checks.

## Host and completion requirements

A local daemon can configure bounded planning authority through
`server.recurrence_automation`. For example:

```lisp
(recurrence_automation
 ((principal_id pri_recurrence_planner)
  (controllers (ses_controller))
  (expires_at 2050-01-01T00:00:00Z)
  (zone_directory
   ((directory /srv/ochat/zoneinfo)
    (source tzdata-2025b)
    (from 2025-01-01T00:00:00Z)
    (until 2050-01-01T00:00:00Z)))))
```

The dedicated principal has planning permission for only the exact listed
controller IDs on this server. Its original proof belongs to the current daemon
instance and expires with the configured deadline or host closure. Stored creator
IDs, prior receipts and copied configuration do not restore a proof. These grants
do not authorize target visibility, child creation, Run admission or execution.
Those owners must obtain their own current policy authorization. Server policy
changes use the existing server-configuration restart boundary.

Omitting this configuration leaves recurrence metadata and stop/pause controls
usable; an authorized actor read reports `Not_configured`. Omitting the optional
zone directory, or failing to open a declared directory, leaves interval planning
usable and calendar search unavailable. The embedding administrator can inspect
the finite `Daemon.recurrence_zone_status`; no filesystem path is disclosed by
that projection. Malformed present configuration rejects before opening roots.
The explicit directory is opened component by component without following
symlinks. Only canonical regular zone files are supported, including when a
timezone database normally provides an internal symlink alias. Reads use bounded
exact bytes, declared finite UTC coverage and provenance; there is no ambient
timezone fallback or last-offset extrapolation.

The existing scheduler owns the sole timer. It starts at most one bounded
recurrence sweep per registered actor independently of legacy callbacks. The
actor acquires original host authorization and a genuine current lifecycle
observation before releasing bounded zone IO and pure planning outside its
mailbox. A serialized finish rechecks that original proof and exact source,
definition, control, cursor and frontier before committing through the existing
persistence owner. Successful ACK installs state and its process-local phase
before publication. A failed save installs neither. A genuinely new enable
captures its adoption clock pair before persistence and installs it only after
its control ACK; retained receipts and
already-enabled controls never reset it. Recovery starts with no old monotonic
phase and reconciles the retained wall deadline at the first authorized pair.
Daemon closure revokes planning, cancels and joins sweeps, then closes borrowed
timezone resources. Termination of the actor while protected acquisition or
cleanup waits is an unavailable owner, never an acknowledgement or retry permit.

### Explicit execution grants

Planning configuration alone cannot launch work. A separate optional
`server.recurrence_execution` grants exact local controller-to-target edges and,
independently, permission to create captured children:

```lisp
(recurrence_execution
 ((principal_id pri_recurrence_executor)
  (controllers
   (((session_id ses_controller)
     (existing_targets (ses_controller ses_worker))
     (captured_children true))))
  (expires_at 2050-01-01T00:00:00Z)))
```

Self-targeting requires the controller ID in `existing_targets`. An empty target
list is useful for a captured-child-only controller; a row granting neither
kind is rejected. Omit `expires_at` for authority limited to the current host's
lifetime. The entire configuration is bounded to 64 KiB, 256 controllers,
256 targets per controller and 1,024 total target edges. Duplicate or malformed
IDs are rejected on construction and decoding; there are no wildcard grants.

[`Recurrence_execution_host`](../../lib/agent_server/recurrence_execution_host.mli)
belongs to the actual daemon or embedded composition. A controller must be
registered and selected for execution in that composition. Listing a controller
or target in configuration does not select it. Operator-only mode creates no
execution host. Closing the host revokes its original proofs before worker
teardown; it does not alter another host's proofs. Configuration changes use the
existing server restart boundary.

Changing execution grants does not erase older occurrence records. An unresolved
attempt may still name a target removed from the current allowlist, so that
allowlist is not an inventory of outstanding work. Serial-dispatch checks must
account for those retained attempts. If the host cannot read the facts needed to
establish that a target is free, it must defer new conflicting execution rather
than assume absence. Factual reconciliation remains distinct from permission to
launch new work.

For an existing target that is not selected, the host can acquire its metadata
through the Factory. This opens its actual session owner for validation, reads
and control without constructing the runtime, running `Session_resume`, migrating
its inference configuration or starting queued work. Acquisition can persist
necessary recovery changes; it is not a promise of zero storage writes. Normal
provenance, lifecycle, profile, workspace and capacity checks still apply. If
capacity is unavailable, acquisition returns an error rather than queuing an
ordinary session start.

The acquired target stays in the existing registry. Repeated acquisition reuses
the same Actor, Handle and runtime owner, including retry evidence retained after
a fully joined source failure. A source preview returns the selected source and
current revision only after releasing its temporary source and parent loans. It
runs no initializer or provider request and grants no execution permission; the
later Run service must select and validate the source again.

Fresh background execution remains excluded until the target is explicitly
selected or its actual authorized Run handoff is acknowledged. Reads, attachment,
control, factual job completion and shutdown remain available. A durable receipt,
configuration entry or uncertain admission alone cannot select execution. An
acquired target also cannot act as a recurrence controller until selected. This
keeps on-demand hosts from starting every session mentioned in configuration.

When explicit trusted-host selection rebuilds a runtime, it carries the caller's
current authentication into initialization. A legacy `Session_resume` callback
uses that actual initialization owner; a saved creator ID or a recovered receipt
cannot supply the missing authority.

The selected controller proof has message and transcript scopes, adding session
creation only for an explicit captured-child grant. Target proofs have message
and transcript scopes. The original absolute expiry remains unchanged through
source preview, intent selection, retries and reconciliation. Stored creator IDs
and retained receipts cannot recreate these proofs.

The host delegates to the existing child-creation and recurrence Run services.
A captured target requires the actual Factory-produced child and matching
controller reservation, association and target delegation record. The host
creates no alternate executor or permission registry. Lower owners still enforce
current source, control, admission, reciprocal acknowledgement and cleanup.
The existing scheduler invokes a bounded dispatcher in the same per-controller
sweep. It visits at most 32 distinct pending or ready occurrences per pass, using
a rotating cursor so a busy target cannot permanently hide independent targets.
It reconciles an occurrence's actual Run before retrying it and performs at most
one activation or retry per pass. A nonterminal Workflow remains occupied even
when it has no foreground turn.
Execution intents retain the exact stored Ready occurrence, including its status
and compatible unknown fields. The typed nominal view is for inspection;
serializing that view cannot replace the complete stored record.

Before new work, the dispatcher excludes concurrent preparation for that target,
reads current occupancy, and checks the target again after source preview at the
revision used for admission. The exclusion is scheduling bookkeeping, not an
execution grant; the actual Factory and Actor still enforce authorization,
source identity, revisions and writer acknowledgements.

Only actual joined attempts can retain live retry evidence, bounded by the
retained occurrence frontier. An initial source failure before recording an
attempt may re-enter with its exact original request when its live no-effects
evidence remains valid. A failed attempt with no joined retry evidence remains
retained while its occurrence is pending; the next pass cannot silently issue a
replacement request. A recorded attempt uses the existing checked retry path.
Restarted hosts reconcile saved records; they cannot recreate that live evidence
from a key, receipt or stored identifier.

A live daemon may continue after client disconnect. Embedded execution requires
its controller to remain selected/open in the host; store reopening does not
implicitly select every recurrence. Operator-only mode does not execute. Durable
records do not imply OS wake, a launch agent, automatic daemon installation or a
remote hosting service. Host/provider availability and existing resource ceilings
remain mandatory.

The pure execution domain keeps selected input and nominal custody once per
occurrence, with at most eight immutable admission attempts. Attempts have
contiguous ordinals and fresh keys; each retains its original caller, CAS and
canonical request digest. Rejection and uncertainty remain inspectably pending.
Only a separately authenticated no-Run/no-effects resolution permits an actual
owner to rotate an attempt. Its stored reference data and decoder grant no such
permission. A pause/resume must recheck current control and policy separately
from the intent's immutable original control revision. An uncertain attempt or
contradictory late admission cannot be discarded to make room for another.

Reciprocal Run origins bind complete intent/attempt digests to exact admission
evidence without copying captured artifacts. Compact terminal references retain
an exact receipt and cached outcome; the complete result stays in the target Run
owner. Every live import must authenticate the actual retained admission and
terminal payload and verify its canonical digest. A decoded Completed tag is
unresolved data. A definitive terminal reference releases pending capacity,
including Interrupted; automatic fixed-delay rearm is a separate atomic owner
decision and remains held for Interrupted review.

Each complete execution row is bounded to 512 KiB. Its current attempt reserves
mandatory terminal or resolution growth before acceptance, including compatible
unknown fields and audit slots. Complete origin and terminal-reference quotas
are 8 KiB each; the mutually exclusive resolution reference is at most 4 KiB.
Already present nullable slots avoid late slot-key insertion. The reservation
does not promise capacity for all optional retries: each next candidate admits
its full retained history and remaining mandatory growth. These row limits do
not enlarge the enclosing frontier, catalog, host or journal limits. Fresh
targets retain an explicit Single_turn or Workflow mode; historical missing
mode stays manageable and cannot execute by inference.

Pure contracts alone grant no target observation or execution authority. The
actual target observation and controller import owners authenticate evidence and
join terminal, cursor and alarm changes to the existing persistence transaction.
Effect-deferred startup, reciprocal admission before effects, activation and host
target dispatch remain required execution integration boundaries.

The implementation proceeds through independently verifiable boundaries: pure
timing; Eio-owned timezone acquisition; actor ledger/history and atomic alarms;
shared headless admission; reciprocal captured-child creation; scheduler/target
and Run-outcome reconciliation; native/operator/authored controls; and real
composed-workflow qualification. Goal functionality is required by the final
combined goal/recurrence fixture, rather than by independent timing or recurrence
persistence foundations. Builtin instructions and discoverable source exports
likewise gate authored integration, rather than pure arithmetic.

The pure timing suite exercises independent UTC-forward candidate oracles,
interleaved folds, gaps, non-hour/midnight/skipped-date changes, leap/month labels,
chunk-and-codec equivalence, cached-candidate validation, clock corrections,
restart anchoring, large arithmetic ranges and timestamp/coverage boundaries.
Those observed pure checks do not establish actor or full-host qualification.

Feature completion additionally requires a reference transition model for
create/tick/pause/edit/admit/finish/restart/revoke, actual persistence fault cuts,
both target kinds, finite capacity/history behavior, installed-zone loading and
future coverage errors, real embedded/daemon selection semantics and native plus
operator controls. A real authored goal/recurrence fixture must exercise isolated
component checkpoints, stable handler order, one continuation, exact wait/wake,
explicit root finish and authenticated child accounting where selected.

Count actual initializer, model and tool effects with deterministic fake
capabilities across due-plan, creation reserve/install/link, association, Run
admission, activation, terminal receipt and pruning cuts. Prove that every
consumed slot has outcome or summary evidence, every execution has one exact
association, terminal outcomes stay immutable, uncertainty is inspectable and
resource usage stays bounded. Full integration checks, documentation qualification
and publication gates remain necessary after these component proofs.


The target Run index retains a reciprocal origin under the existing bounded Run
and receipt retention policy. A new origin can enter only with its new
revision-zero admitted Run and exact new Admission receipt in one candidate;
there is no independent late-attach operation. Terminal updates, source
replacement and recovery retain that origin alongside the existing Run. The
complete index still has its existing 1 MiB and occurrence limits, and admission
also reserves the actual terminal projection in bytes, fields and nodes.

A positive origin table requires `recurrence.run-origin.v1` on the enclosing
stored state and change document. This fence composes with existing Schedule and
queue provenance semantics. Snapshot and transaction readers recognize these
markers on their state and delta children, while their own outer envelopes do
not accept them. Historical missing or null origin fields remain readable and
retain their wire presence. Native state updates, carrier adoption and delta
application enforce the same association timing and immutable retention rules.
These storage contracts grant no current target authority, execution permission
or terminal observation. Actual actor observation and import authenticate their
retained evidence; deferred activation remains separate runtime work.


The target actor can issue an opaque read-only observation of that retained
origin, Admission and actual terminal Run. It validates the canonical terminal
payload against the exact retained receipt and repeats current transcript
visibility, original proof and genuine Store lifecycle checks after observation.
A synchronous final check is required after any later yielding preparation and
before a separate owned effect. The observation loads no runtime and grants no
execution permission; a missing request is not a no-effects or retry proof.
The controller actor imports this evidence through its settlement owner;
observation alone grants no deferred activation permission.


The frontier can retain one immutable original execution request per selected
occurrence, with bounded attempts sharing its input. Initial recording moves the
exact complete Ready nominal for an existing target or binds the exact protected
reservation and linked child. Repeating the original record changes no revision.
Every retained intent remains associated with its captured historical definition;
current policy and authority are separate from its original audit fields.

Pending capacity counts the occurrence union across Ready, unresolved executions
and protected children, including older revisions. A child and its execution
count once. Lowering a policy prevents additional selection while preserving
older obligations. The existing complete frontier profile may saturate before
its defensive inventory ceilings. Positive execution tables require the
frontier's `recurrence.execution.v1` semantics; missing and null history retain
their original wire distinction.

Before accepting a record, storage-only projections reserve mandatory proof,
compact cursor-basis, archival-wrapper and alarm growth. Their complete child
profiles include maximum depth, so the existing frontier, catalog, host and
journal encoders can reject insufficient capacity before effects. Projections
preserve the original unknown metadata and are never installed as evidence.
The controller actor records the initial request through its existing transaction
and returns immutable data after ACK and fresh Store/policy observation. Both new
records and exact retained requests require current Send_messages and current
host policy for the retained definition. Only new work additionally requires the
running ready controller, current automatic birth/source, enabled control,
selected-occurrence grace and the actual caller as attempt principal. Replaying
historical data after pause or source changes writes no transaction and grants
no target capability. Live selection anchors remain associated with unresolved
execution rows under their original source/birth/control/definition checks.
A separate actual actor import authenticates the observation and commits atomic
terminal-plus-successor-or-hold settlement. This record path admits no Run,
initializes no target and issues no retry or activation permission.

Settlement data distinguishes an imported fact, exact retained evidence and
detail that is no longer retained. A consumed high-water plus lifetime counts
cannot certify that a particular missing coordinate previously completed; an
unavailable-detail result changes no counts and never rearms. Whole recognized
terminal bundles may retire under the declared history policy independently of
slot order, while every unresolved or uncertain request remains protected.
Compatible metadata stays complete while its identity is retained.

Checked timing transitions can represent later fixed-delay slots, but their
timestamps need the enclosing frontier's exact prior completion basis. They are
not a definition-only periodic phase. The pure Frontier/Entry/Catalog candidate
combines terminal data, one checked lifetime settled increment, whole-bundle
retention and a completion-derived successor or hold. Its restore fence
recomputes the complete candidate. A new ordinary-pause enable may consume the
saved definitive basis once; duplicate enable does not. Actual actor import
authenticates the target evidence and commits this whole candidate, then installs
its prepared completion phase after ACK. These data values issue no target
observation or activation permit.

The lower process-local timing bridge can retain a signed completion position
relative to a receiving controller's monotonic origin. A target completion may
precede that origin. Checked translation into elapsed time since completion
preserves the receipt-derived wall deadline across wall corrections, and rejects
backward or overflowing live time. It reuses the existing delay clock. A missing
process anchor keeps the existing wall-lateness and misfire reconciliation; an
invalid live anchor is explicitly present and cannot silently choose recovery.
The runtime's bounded completion plan validates exact reciprocal Run and receipt
facts before persistence and is installed only after the corresponding ACK.
The target actor captures the wall and absolute monotonic pair before each
definitive Run terminal producer and passes that same wall to the receipt. Its
existing persistence path prepares the complete map before committing, then
installs it after state assignment and before publication. An opaque target
observation privately retains any such sample and validation of saved extended
references against the actual terminal payload. The sample grants no rearm or
execution permission.

The lower controller settlement admission helper freezes one exact catalog
basis under fresh controller visibility and optional automatic-rearm policy.
Its phase plan distinguishes contradictory evidence from unavailable live
timing. Only the latter can select an evidence-plus-review-hold candidate;
missing process timing instead preserves pending wall-time adoption. Phase maps
are prepared before persistence and installed by assignment after ACK. The Actor
import operation authenticates an opaque same-Store target observation, retains
validated saved reference annotations, and commits the whole evidence and
successor-or-hold candidate. After all policy callbacks, it checks both original
proofs and exact owner/current evidence bases without invoking further policy.
Nonyielding callbacks can revoke another proof, so ordering callbacks alone is
insufficient. It then installs any phase before publication and
refreshes controller visibility without reusing a self-target's stale witness.
An invalid live clock returns a typed hold diagnostic with acknowledged data;
it cannot disguise contradictory evidence or a failed persistence attempt.
Physical Store/Actor checks cover self-target ACK, separate target ownership,
current-policy revocation, exact retained-reference duplicates, recovered wall
lateness and lost-reply journal reopening. An in-flight caller re-raises an
unexpected owner exception or cancellation; queued and later calls instead return
`Server_shutting_down`. Neither reply permits replay of an effect whose
acknowledgement is uncertain. These checks do not qualify execution admission or
target activation. Actual producer-to-importer live-phase
qualification still requires genuine held admission, reciprocal controller
acknowledgement, effect-deferred startup and central activation fences. Host
target dispatch remains a separate required integration boundary.

### Controller activation custody

The controller actor owns a revocable process-local permit for an exact retained
execution attempt and its acknowledged reciprocal target Run. Acquisition requires
a genuine current same-Store observation of an `Admitted` Run, current guarded
host policy, enabled current definition/source/control, automatic controller birth
lifecycle and nominal grace. Stored creators and original request principals are
audit data. This permit is not the target's held admission or startup authority.

A checked view contains fresh actual Store lifecycle evidence. An unrelated
controller ACK may stale that view; refreshing a still-live permit obtains new
evidence. Pause, Stop, edit, source/lifecycle changes or terminal association
close affected permits before the normalized controller candidate begins its
write. Failed writes never revive them. Runtime retirement, lifecycle reservation
and owner shutdown close custody before external effects. A successful runtime
reconfiguration with an existing worker also closes pending permits; initial
worker installation alone does not reopen or revoke prior closed custody.

The checked view exposes separate policy, original-authentication, fresh timing
and pure basis checks. Original guards and supplied clocks execute trusted host
code, even when they do not yield. The complete check first rejects stale or
closed custody, runs both controller and target-read policies, repeats both
originals, samples fresh wall/monotonic time for nominal grace, then checks both
pure owner/evidence bases. Those final bases invoke no policy, original guard or
clock. A callback closing custody cannot escape that final check.

The eventual target consumer must retain its own genuine held owner and compose
all policies, all original guards, fresh timing, and both pure target/controller
bases after yielding preparation, persistence, history reconciliation and
scheduling. No callback or yield may intervene between the final bases and the
actual first effect on the same Eio domain. The basis alone grants neither
authentication nor held ownership. Current source-level custody does not qualify
that deferred startup/activation integration. Release neither cancels an already
activated Run nor claims that an uncertain effect did not occur.

### Source selection before deferred activation

`Runtime_builder.Source` retains an explicit `Authored` revision or `Generated`
definition, rather than manufacturing an ordinary revision for generated code.
Authored selection uses the existing bounded parser/program restoration owner. Its
explicit `Legacy`, `Extension` and `No_moderator` outcomes separate source identity
from versioned invocation capability. A legacy script's genuine identity does not
become a versioned observer or handler receipt. Single-file source identity and
complete linked-program identity use the same recipe as the live Registry.

This immutable selection performs no Manager construction, Agent-content
conversion, history allocation, native shell initialization or MCP connection.
It grants no execution authority. Native resource discovery and compilation
requiring a newly discovered tool ABI still belong to the real activation path
after the held Run and reciprocal controller acknowledgements. Generated
definitions can statically restore and compile against their actual already
loaded parent's inherited bindings; the source retains that genuine generated
owner and its compiled programs. The existing Registry binds their exact identity
without constructing or initializing a Manager. A source selection alone cannot
authorize an inert admission or bypass the target's physical runtime lifetime.
The held admission and first-effect owner integration
remains a separate qualification boundary.

`Session_factory.with_prepared_run_source` verifies the actual target's complete
retained publication and materialized inventory before binding or any held Run
write. Authored cache hits use the Catalog's own artifact store and exact retained
manifest digest; cached revision identity alone is insufficient. Captured
recurrence children retain their exact private admission and captured replay.
The source witness distinguishes ordinary authored, captured recurrence,
authored-tool child and generated families. Each child carries its actual
retained child association; sharing the parent's artifact does not make the
parent a valid target. The child's absent catalog identity does not erase the
original artifact's defining metadata or relax ordinary source matching.
Authored-tool children resolve their genuine defining policy and source through
the loaded parent's existing resource index. Generated children restore their
exact recorded artifact and inherited capability pins through the existing
generated owner. Neither branch consults original files or repairs stored bytes.

The Factory acquires the actual no-load parent lease before parent policy lookup
and retains it through the prepared callback when inherited bindings are needed.
Only already-realized inherited bindings are selected. A cold parent or unrealized
bindings fail without creating tools, connecting MCP or running an initializer.
Ordinary eager loading remains available through its existing authorized path;
source staging does not invoke it covertly.
The prepared callback still owns current command/delegation checks, reciprocal
acknowledgement and final effect admission. It cannot retain the borrowed resource
lifetime after returning, and artifact verification grants no execution permission.

The actor can bind that source to its exact authenticated Run preparation.
`Session_actor.Source_binding` owns only this source/preparation custody; it does
not establish a runtime lease. `Runtime_owner.with_prepared_source` separately
reserves the actual owner without loading a runtime, and lends a `Prepared`
lifetime with the actor-issued binding. Competing leases and eager loading are
excluded while it is reserved. Original authentication, pinned selection, actor
basis and runtime resource identity remain independently checked.

An ordinary validated persistence attempt closes bound preparation before its
write yields; an explicitly owned preparation acknowledgement may advance its
same basis. Accepted lifecycle/runtime retirement and replacement close custody
before cancellation or cleanup joins. Closing either lifetime never reopens it
or proves rollback.

The actor's inert admission path uses that exact source binding and original
headless preparation. It validates the complete occurrence intent/attempt and
commits the revision-zero `Admitted` Run, canonical Admission receipt and
reciprocal origin together. The source-installation candidate precedes that
combined commit; origin cannot attach later to an ordinary admission. Ordinary
Run admission retains its existing initialized-checkpoint and operation rules.
The inert path requires scripted orchestration, including an actual legacy
source identity; a no-moderator selection cannot admit a Run.

This acknowledgement allocates no input Operation or history, invokes no source
initializer or native setup, and installs no normal Run authority. Only the
continuously live actor-issued `Held_run` owns the admitted startup; activation
also requires the separate genuine `Runtime_owner.Prepared` lifetime and verified
retained artifact. An exact receipt retry returns data, never recreated held
custody. Closure preserves the last write disposition and durable evidence.
Prewrite rejection, attempted-write uncertainty and confirmed admission remain
distinct, including when custody closes during persistence or publication raises.

For a controller targeting its own session, the exact prepared-record operation
keeps the original target request, CAS, key and digest while its controller record
ACK advances only the actual owned preparation. The held association import then
requires the same live `Admitted` Run, canonical Admission and immutable origin.
Its reciprocal controller ACK advances that same preparation before publication.
Retained data neither advances nor reattaches custody; unrelated ordinary writes
still close it. Neither operation accepts a different actor or server-qualified
target. The controller must already own its legitimate installed source and
checkpoint; these operations create no cold checkpoint.

Both owned paths finish their controller policies after the central source-binding
policy callback. Fresh selection grace, where required, precedes the final
original proofs and callback-free controller and source/held bases immediately
before the sole writer attempt. Own ACK rotates the actual Store witness, so returned
data refreshes controller visibility without reusing the stale self-target read.
The pure custody basis alone remains neither authentication nor runtime authority.

The deferred host, actual reciprocal activation barrier and no-effects retry
issuer remain separate qualification boundaries. Factory must verify the full
retained artifact, installed tree and selected program/content custody before
binding or held admission; equal revision IDs alone do not establish that proof.
Activation must consume the same genuine `Prepared` and `Held_run` owners through
a one-shot phase, without widening staged quiescence or accepting an arbitrary
runtime replacement. This inert admission seam starts no deferred effects and
issues no no-effects proof.
