# Durable recurring automation

Status: accepted engineering design with implemented timing, bounded pure due
planning, and durable controller metadata. The `ochat.recurrence_rules` library implements validated timing rules,
finite zone models, clock arithmetic and resumable cursors. The session actor owns
validated captured definitions, keyed control receipts and lifecycle retirement.
Publishing due plans through the live scheduler, execution admission, target
dispatch and public user controls remain integration work.
The retained-root timezone loader supplies bounded acquisition; host root policy
and schedule integration remain explicit owners. Passing the pure timing suite
does not qualify those features.

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
never reopen the definition’s provenance-only root path. The future creation owner
must use actual authored preparation with protected creation and linking, then
establish current-authorized parent linkage and reciprocal occurrence origin
before activation. The existing restricted generated-definition path does not
accept every authored program or captured project closure. Ordinary creation
cannot bypass these protected admission requirements.

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

A live daemon may continue after client disconnect. Embedded execution requires
its controller to remain selected/open in the host; store reopening does not
implicitly select every recurrence. Operator-only mode does not execute. Durable
records do not imply OS wake, a launch agent, automatic daemon installation or a
remote hosting service. Host/provider availability and existing resource ceilings
remain mandatory.

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
