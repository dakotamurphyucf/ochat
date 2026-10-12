# Last successful script project selections

`Prompt_project_baseline` records a successful prompt definition's exact artifact
and the selected input for each stable script slot and actual invocation role.
It stores references to captured program records, not copies of project locks.
Retrieving a prior lock does not restore or compile the old graph and does not
read the original project directory.

The host passes the original immutable `Prompt_revision_builder.Capture`, the
actual plans used by source preparation, and its returned program custody to
`Prepared.create`. Only `Success.finalize` can turn that association into a new
successful entry. Finalization checks the revision's exact complete custody
manifest and private bytes. The host must durably install the artifact before
publishing the baseline, and include its exact revision in pruning protection.
Finalization itself is not a durability receipt.

`Prompt_project_preparation.capture` connects this history to the existing source
batch. It uses the actual capture's original receipt, loads and reconciles the
exact prior artifact, and selects each operation by stable slot and authentic
invocation role. Build uses the retained lock; a known new slot or Provided input
uses the Local-only no-prior operation. Only authoritative first admission allows
Initial. Update is an explicit host request and does not increase source grants.
The host still owns authorization, candidate reservation and durable publication.

Source preparation retains all requested plans for execution metadata and exposes
`captured_plans` for baseline association. Unconfigured contexts without supplied
graphs intentionally retain no program graph; they continue through the lightweight
implicit path. Every configured context remains in `captured_plans`, so absent
configured custody is still an error. Unknown history rejects configured project
requests before acquisition while allowing these ordinary implicit scripts.

A stable slot contains the registration Root, declaration input role/path and
qualified script ID. It excludes byte ranges, source revisions and compiler
contract hashes. Roles are selected by the authentic planner: standard targets,
explicit legacy UI/headless invocation, or the actual native hook and entrypoint.
The saved named index is the historical role-to-program-key authority.
Reconciliation independently checks its exact artifact revision, original
manifest digest, original operator receipt, key and stable slot. It does not infer
historical roles from today's compiler contracts. A deliberate coherent rewrite
of the saved association changes that authority; this is not a separate artifact
signature scheme.

The version1 `ochat.prompt-project-baselines` document retains unknown metadata.
Definition rows align by definition ID and association rows by a checked
length-coded slot/role digest. Potentially arbitrary script/entrypoint bytes use
hex fields. Explicit limits cover the complete envelope, definition/reference
counts, identity bytes and cooperative work, including nested custody walks and
unknown-field handling. Artifact manifest limits remain independent of index
limits. Successful replacement retains every other definition; failure, disable
and removal supply no deletion authority.

`Prompt_project_baseline_store` borrows an opened metadata directory and existing
host-exclusive coordination for its switch lifetime. A known initialized index
with no definition proves first admission. A known entry with no current slot
proves a new slot; a Provided record is also a known historical state. Missing or
invalid historical documents are Untracked, while ordinary filesystem failures
remain typed store failures. None of these states silently selects Initial.
The host's project gate decides which known new/Provided requests may build a
Local-only closure and which require explicit Git update.

Snapshots and publication candidates are owner-affine. The persisted index
counter is independent of a Catalog's pending-candidate generation; integration
must seal both. The owner bounds and compares exact expected file bytes, performs
its final cancellation/checkpoint/cancellation poll, then enters the protected
`Durable_file` replacement with file and directory flushing. It marks itself
Uncertain before entering the writer. Only delivered successful writing installs
the preallocated next state. Writer errors, exceptions or cancellation before
acknowledgment leave Uncertain, with no automatic retry or rollback. There is no
postcommit progress/deadline callback.

A daemon-owned private worker switch allows normal owner close to revoke,
cancel and join admitted operations before releasing the borrowed directory.
Operation completion uses a worker promise even when callback entry is skipped;
startup admission also settles when the lifetime closes before daemon entry.
Callbacks run without a held owner lock and preserve their original exception
and backtrace. Concurrent/reentrant operations return Busy.

The daemon opens this history from its actual session-store creation/open branch.
Only successful creation of the owned data root permits a new empty index; a
missing index in an existing root remains Untracked. Catalog preparation captures
configured projects through the administrative source host, then seals the prompt
and factory candidates before durable publication. Reload joins preparation,
publication and cleanup under one owner; shutdown joins that owner before closing
source and session-store resources. Pruning uses the same publication admission
and retains last-successful baseline artifacts. Unknown history disables pruning.

The publication owner's history observation is immutable metadata: a tracked
generation, Missing, or Invalid. Startup obtains it from a bounded actual snapshot;
subsequent status reads perform no filesystem or source work. A prepared candidate
exposes its authentic previous and next history. Only final in-memory catalog
installation updates the displayed generation. A successful disk write followed
by interrupted installation therefore leaves the old displayed generation and
uncertain publication; that old observation does not describe current disk bytes
or authorize a retry. The existing publication tests check this distinction at
the real writer boundary, and the daemon fixture verifies configured-project
startup, restart after a local edit, and refusal to recreate a missing old index.

Explicit dependency updates and configuration reload share the same watcher
admission and publication hooks. An update prepares against the exact installed
configuration without reading a staged configuration edit or consuming its
modification time. Its response is derived from the prepared history and actual
target outcomes before the durable write. On-demand and operator-only hosts keep
this owner available for explicit operations while leaving automatic polling and
runtime activation disabled.

The command handler acknowledges a Protected Pending receipt in the existing
idempotency store before entering the host. It releases the idempotency mutex
before cancellable preparation; original operator authorization is checked at
admission and preparation checkpoints. Known rejection or acknowledged success
can complete the receipt. Uncertainty or an exception leaves Pending, including
when the catalog committed but result delivery failed. Receipt completion records
an already admitted effect and does not reclassify a committed update when its
operator is subsequently revoked. Replay and receipt reads still require current
authorization. The receipt and dependency index are separate durable writes.
