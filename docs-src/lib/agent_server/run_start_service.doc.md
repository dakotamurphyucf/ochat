# Shared Run start admission

`Run_start_service` admits explicit Run starts through the actual retained runtime.
It does not create another executor, scheduler or session store. Public
`start` retains the existing writer request and wire digest. Internal
`start_headless_command` accepts a validated intent and the original authenticated
`Operator_authorization.t`, with no fabricated client attachment.

A headless intent contains session identity, generation, original expected
revision, mode, input and idempotency key. It is data, including after successful
validation. Only the target actor can issue a preparation for its private
process-local owner. There is no wire codec or controller-ID constructor for that
permit, and a retained receipt cannot reconstruct it.

For a headless command the actor checks original authentication currentness,
`Send_messages` and the installed non-yielding host policy before retained receipt
lookup or runtime construction. The same original proof and policy remain in
preparation custody and are checked after yielding constructor/parser work and
at final commit. A principal record with the same ID does not replace the
original proof. Host policy must validate the actual target and current visibility;
a callback that merely returns success cannot replace the original proof/scope
checks. Authorization exceptions and cancellation retain their original behavior.

The shared path preserves these boundaries:

1. Current authorization and target admission precede original key/digest receipt
   lookup. An exact retained admission returns before CAS, runtime construction,
   history allocation or parsing. Reusing a key with another digest rejects.
2. New admission checks the original generation and revision before constructing
   a cold runtime. One actor-issued `Run_preparation` exclusively owns this
   attempt. Only its acknowledged constructor/history changes can advance the
   captured revision; unrelated state changes invalidate it.
3. `Runtime_owner.with_prepared_run_runtime` borrows the real runtime through its
   existing joined lease. The callback executes outside the runtime mutex. Its
   compiled manager supplies the actual observer and pending-startup capability.
4. A user input reserves one history ID through that preparation and uses the
   runtime's existing parser. Final actor admission creates the actual starting
   input operation and commits history, Run and receipt before worker handoff.
5. `Authored_start` requires a genuinely stopped target and its actual unconsumed
   startup capability. It cannot restart an initialized source. Workflow mode
   still permits only one unfinished Workflow for the current installed source.
6. Actor enqueue and acquisition wait are protected together until their decision
   is received. Cleanup covers the first cancellation check before any cold
   constructor/parser work; a cancelled queued command cannot orphan custody.
   A retained reply also observes pending cancellation. Every normal, error,
   cancellation and uncertain-write exit releases custody.
   A final persistence attempt without a known accepted receipt returns
   `Uncertain`; retry/reconciliation uses its original key and digest without
   replaying effects. Already committed admission evidence wins a lost reply.

Live Run authority records `Writer attachment_id` or `Headless_command` as
provenance, alongside the same current source-bound scope. Client detachment is
separate from continuation authorization. Recovery cannot recreate that scope
from an admission receipt; the shared Run recovery policy remains authoritative.

Writer request digests remain the canonical encoding of the original public
`Run_start` DTO. Headless command digests use a disjoint canonical envelope. A
caller cannot switch between attached and headless requests under one original
key/digest. Neither encoding carries current authorization.

This internal host-command API is the implemented shared foundation. Actual goal
and recurrence controller admission remains a separate integration boundary:
its real actor owner must prove the committed record, exact revision, current
hold/enabled and capacity policy, and original durable origin before minting a
controller reservation. Same-actor goal association must join the Run admission
transaction. Cross-actor recurrence needs reciprocal origin evidence and the same
admission fence for final dispatch, stop, edit and revocation. Stored controller
IDs, pure decoded records and generic success guards cannot provide those proofs.
See the [durable recurrence design](../../design/composable-agent-recurrence.md).

The focused actor fixture exercises a real compiled moderator lifecycle, a cold
runtime owner and ordinary Run/input persistence, with no writer attachment or
provider credentials. It checks retained retries, startup consumption, parser and
worker counts, current policy/expiry after yielding preparation, exact custody,
source displacement, failed save, Workflow exclusion and queued cancellation
before construction for both caller kinds. These checks do not
qualify future controller association or automation dispatch.
