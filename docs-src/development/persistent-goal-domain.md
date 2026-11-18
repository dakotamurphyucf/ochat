# Persistent goal domain

Status: pure domain and universal-document codec. Actor mutation, goal tools,
continuation admission and cumulative usage collection are separate integration
work. These types do not themselves run or authorize anything.

`Agent_protocol.Goal` represents one session-local objective with a nominal
`Id.Goal`, immutable session/creator identity, adopted source, progress, admission
policy and budget. The stable goal identity is its accounting scope: editing the
objective does not create a new scope. There is no duplicated Run lifecycle or raw
inference usage in this record.

An open objective is enabled or held with a typed reason. A closed objective is
achieved with receipt associations or abandoned with a reason, and has no enabled
flag. Closed metadata cannot be edited or resumed. Every accepted pure mutation
advances its nonnegative int64 revision once; overflow rejects. The future actor
must authenticate the caller and enforce model/operator distinctions when using
pure resume or budget operations.

Goal text is preserved verbatim. Objectives are nonblank and bounded to 8 KiB;
progress may be empty and is bounded to 16 KiB; reasons/evidence are nonblank and
bounded to 4 KiB. Bounds count UTF-8 bytes. Malformed UTF-8, NUL and Unicode control
characters other than tab/newline are rejected. Budget ceilings are nonnegative
int64 counts; zero permits no new admission. Observed-token limits select an
existing usage component and do not claim hard billing limits. This slice stores
policy, not cumulative counters or provider observations.

`Goal_episode` records an immutable actual Workflow admission. Binding a replacement
episode requires the matching old Run and its terminal receipt. One bounded
immutable predecessor settlement is retained; a later verified episode replacement
may rotate that proof. The existing Run index owns other retained evidence under
its existing retention policy; the goal does not guarantee indefinite history.
If request and predecessor settlement slots refer to the same Run ID, both must
reuse the exact same immutable settlement, including admission and terminal receipt.
The next admission session revision cannot precede its predecessor terminal;
equality allows both records in one actor transaction without assuming monotone
wall time. Action and Terminal Run revisions strictly follow their Admission; an
immediate terminal may share its Action revision. Source adoption
advances installation identity without rewinding generation and leaves the goal
held; it retains old episode/request source references verbatim. The actor proves
that supplied evidence belongs to the actual authoritative Run index.

`Goal_achievement.Request` binds an explicit Completed Finish decision to its
actual Action receipt. It retains the exact action and checks the canonical action
SHA-256. Accepting that request leaves the objective open. Settlement retains the
exact episode and Terminal receipt, with no copied outcome payload or authoritative
adverse classification. Its live constructor validates that receipt against the
supplied actual terminal Run. The actor still authenticates that Run and receipt.

Achievement recomputes canonical `Run.Terminal.Completed(requested_result)` from
the retained request and checks its hash against the exact Terminal receipt. A
Failed receipt cannot become achieved merely by changing a stored tag. This also
checks exact episode/source/principal and ordered receipt revisions. A matching
actual Completed Run is required by the live achievement constructor. Goal wire
state stores only an achieved marker; decoding reconstructs its proof from the one
outer request/settlement pair. It cannot create achievement from the marker alone.

New or replaced requests must belong to the current episode; an unchanged settled
historical request can survive a later episode binding. Actual failed, cancelled,
limited or interrupted Runs can retire a request, preserving the immutable receipt
association and any existing hold, especially operator pause. Previously enabled
work becomes held for review. A later episode cannot satisfy an old request.
Request pending/settled disposition comes from associations and actual Run evidence,
not another stored execution enum. Stored evidence never grants current authority.

Goal construction, mutation and decoding enforce a 128 KiB owned JSON projection
at depth 64. Existing Run limits allow 4096 work occurrences and a 1 MiB
action/document, so an independent request can exceed the goal embedding budget.
Request admission also reserves capacity for all later bounded objective/progress,
hold or abandonment reason, budget, adopted source and revision values, plus maximum
current/predecessor episode and Terminal receipt slots. The immutable request is
charged exactly once. This bounds completion, retirement and next-episode binding
without copying large Finish relinquishment or result references into closure state.

`Goal.reserved_capacity` reports encoded bytes, object fields and JSON nodes for
storage accounting; it is neither a hypothetical Goal nor execution authority.
Its conservative witnesses include JSON escaping: admitted goal text costs at most
two encoded bytes per input byte, source script IDs at most six, nominal IDs at
most 96 bytes, keys 256 bytes, counters 19 decimal digits and timestamps 64 bytes.
The future actor must preflight the complete carrier before staging the Run Finish
in its owner transaction. Oversize requests reject at admission, rather than first
failing when the Run actually settles. Public JSON decoders bound traversal before
recursive codecs or serialization, including unknown subtrees. Goal/creator/session
IDs are validated at construction as well as decoding.

`Agent_session.Goal_document` is a pure codec over the universal
`Document_schema.Domain_codec` and `Extension_carrier`, with kind `ochat.goal`,
version 1 and a complete-document ceiling of 256 KiB at depth 64, with 8192
fields and 16384 nodes. At create/decode/update/adopt/encode, it charges the exact
complete document plus any positive growth between current owned fields and the
reservation. Retained unknown bytes, fields, nodes and envelope overhead therefore
cannot consume settlement headroom. Unknown variant/container ownership conflicts
remain explicit guards; reservation does not authorize discarding unknown data.
The actor must preflight future-schema compatibility before effects as well as
quota, rather than discover a protected variant conflict after staging a Finish.
The general store transaction must still handle its own capacity and atomicity.
Compatible
unknown envelope and nested fields survive edits/adoption. Unsafe variant changes
or removal of containers with protected future data reject. Known optional fields
are required and explicitly nullable; absence is malformed, and unknown absence
and null remain distinct in the carrier. Decimal int64 fields use canonical
nonnegative strings. No Eio operations or second persistence writer are introduced.

The grouped `test/goal_domain` suite checks receipt/episode evidence, source
adoption, malformed typed IDs and deeply nested JSON, projection size at proof
embedding/closure, checked revisions, independent bounded command
sequences, and extension preservation/conflicts. It does not qualify a running goal
capability or a composed authored workflow.
