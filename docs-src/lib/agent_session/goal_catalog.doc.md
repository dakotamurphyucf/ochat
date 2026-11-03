# Actor-owned goal metadata

`Session_actor.goal_command` is the internal authenticated operator-command
boundary for persistent goal metadata. It creates an objective, edits objective
or progress, changes a budget, holds, explicitly adopts the current source, or
abandons an objective. New goals start held for source review. These commands
never enable, resume, schedule or cancel a Run. Abandonment records objective
disposition; outstanding Run work still belongs to its existing execution owner.

The actor is the sole writer. `Goal_catalog` is an immutable optional child of the
existing `Session_state`, journal delta and snapshot pipeline. There is no new
store. Each goal retains its complete `Goal_document` envelope, including compatible
unknown fields and the existing settlement reservation. Ordinary messages,
compaction and generation reset retain identity, creator, adopted source,
objective, progress, Run associations and immutable command receipts.
Introducing a goal through a decoded mutation or carrier adoption checks the
same held revision-zero provenance and matching new command receipt as a command
commit. Loading a complete historical snapshot remains a separate operation.

## Command identity and authority

`Goal_command` is bounded data. It carries session identity, current generation,
catalog CAS, per-goal CAS for existing goals, an idempotency key and the exact
mutation. Source-dependent commands carry an exact source installation fence.
Safe Hold and Abandon carry no source claim, so an operator can control a retained
objective after moderator removal. They still require current generation and
both applicable CAS frontiers.

The host supplies its actual session reference and original opaque
`Operator_authorization.t`. Mutation requires current original authentication,
`Send_messages` and the host's non-yielding current write/control policy. Reads
require current original `View_session_transcript` and host visibility. Stored
principal IDs, goal IDs, decoded records and callbacks returning success cannot
substitute for that proof. Protocol/native wrappers must capture the real host
session reference rather than accepting a client claim as node authority.

Current authorization and target admission precede receipt lookup. The actor
computes a disjoint versioned canonical digest, including operation, source and
CAS fields. An identical principal/key/digest retry returns the exact immutable
acceptance receipt without writing. Another input under that principal/key
conflicts. Historical receipts may reconcile after source or generation changes;
they confer no current source or execution authority. A separate read returns the
current objective rather than a duplicated historical mutable goal snapshot.

Fresh source-dependent commands select the actual moderator checkpoint identity
and shared durable Run installation metadata. Before the first Run, goal creation
may materialize that installation in the same atomic delta as the catalog and
receipt. This creates no Run, runtime, callback scope or worker. Ordinary edits
require the goal's adopted source to match. Only explicit Adopt_source changes
that association, and adoption leaves admission held. Safe Hold/Abandon bypass
source selection entirely.

The mailbox serializes fresh checks and mutation preparation. Original authority
and host policy are checked again immediately before the ordinary persistence
boundary. Success is acknowledged only after the complete carrier commit and
state installation. A definite failed save installs neither the catalog nor its
receipt. An uncertain acknowledgement must be reconciled through the retained
durable result and normal recovery; it is never permission to replay effects.

## Finite retention and capacity

Initial explicit limits are sixteen retained goals, 256 retained user-command
receipts and 8MiB for the complete catalog including reserved child growth. The
existing per-goal 128KiB owned projection, 256KiB full carrier and depth limits
remain in force. The configured complete session document may impose smaller
limits. Admission charges preserved unknown data and positive reserved child
byte, field and node growth against both the catalog and the complete host
document. Enabling an imported goal also preflights its eventual source-review
hold against its protected child carrier.

There is no receipt eviction, silent goal deletion or unbounded retention. At
saturation, fresh user-command keys fail closed, including fresh Hold/Abandon
keys. Exact retained retries and authorized reads remain available, and the
existing session stop remains callable. Internal source/reset/recovery holds do
not consume user-command receipt slots. Future authenticated terminal settlement
must likewise use a receipt-independent internal transition, preserving the
existing Run proof and child reservation. Automatic metadata holds can still fail
explicitly on exhausted revision or incompatible forward-schema protection;
evidence must never be dropped to force admission.

## Source changes, reset and recovery

Actual source replacement/removal and generation reset hold enabled goals for
source review in the same existing session transaction. Previously held goals,
including an operator pause, keep their exact reason. Their adopted source and
historical Run evidence remain unchanged until explicit adoption. Reset planning
preflights these holds before preparing the replacement. Recovery uses the
existing Run recovery command to retire unfinished Run custody and hold enabled
objectives atomically, without constructing a runtime or scheduling work.
Repeating reconciliation is a no-op.

Snapshots expose validated rendering-neutral goal values, with historical absence
defaulting to empty. Principal projection discloses them only with transcript
visibility. Private receipts and protected carrier extensions stay with the
session owner.

The remaining execution integration owns actual enable/resume, episode binding,
achievement settlement, cumulative accounting and model/native control policy.
Explicit session-stop to active-goal pause belongs to that control boundary.
Neither this catalog nor a stored goal reference grants a second scheduler or an
execution permit.
