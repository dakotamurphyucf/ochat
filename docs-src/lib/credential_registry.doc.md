# Shared host credential lifecycle

The [registry interface](../../lib/credential_registry/credential_registry.mli)
borrows one explicitly provisioned private directory and immutable secret backend
for a host and OS principal. The host must select this authority outside session
roots and keep its borrowed resources alive until the registry closes. Opening an
existing authority never initializes missing or corrupt metadata, imports another
cache, discovers HOME, or infers a login from account metadata. PRIVATEFILES is the
initial backend; this implementation does not access Keychain or supply an OAuth
browser, listener, provider endpoint, or remote revocation route.

## Authority and publication

A single bounded nonsecret document contains the registry incarnation, validated
host-qualified identities, authorization epochs, authoritative secret revisions,
candidates, rotating-token intents, removal progress and operation receipts.
Identity includes the declared provider/method/account and, for OAuth, verified
issuer/client/resource/subject and required scopes. Raw grant absence/null/value
is retained beside explicitly verified effective scopes, expiry and provenance.
Missing expiry never silently means unlimited validity. Trusted provider policy
owns cryptographic verification and qualified preservation of omitted grant
fields; structural constructors still reject inconsistent material and grants.

Login candidates preserve the working active login. Immutable secret creation
precedes pointer publication. Authorization epochs advance on replacement and
local disable; a successful refresh changes the secret revision without changing
the authorization epoch. The host synchronizes the immutable metadata snapshot into its
inference registry before capturing a fresh context. Synchronization reads no
secret and invokes no environment callback; Ready means configured metadata,
not probed credential availability. Explicit status and admission require the
selected host authorization before probing credential material. Credential lookup must not
reauthorize an already captured context. Attempt guards check current epoch,
revision, refresh certainty, expiry and any supplied configuration guard. Each
Admission carries its exact immutable identity; consumers compare it against the
approved profile/binding before borrowing material.

Protected material has no public serializer or equality API. Its bounded private
envelope contains ownership markers for registry incarnation, binding and
operation, plus access/refresh tokens and optional opaque provider continuity
proof. Trusted provider code can borrow OAuth material through `with_oauth`.
Continuity can preserve original nonce/authentication-time proof across restart;
provider policy verifies its meaning. Refresh supplies complete continuity and
cannot silently omit or null-discard an existing value. No token or proof bytes
appear in public status, receipts or diagnostics.

Global revision reservations include every active, staged and retired reference
across bindings. A foreign, corrupt or pre-existing revision is quarantined and
never activated or deleted. Owned inactive revisions can be cleaned after their
private markers and global inactivity are checked. A missing revision is confirmed
through the backend's own retained-directory check and sync before cleanup is
marked complete; syncing a separate metadata directory is insufficient. Errors
retain pending or uncertain cleanup. Quarantine remains an explicit
operator-visible condition. Logical deletion is not forensic erasure.

## Coordination and lifetimes

Each registry instance belongs to one Eio domain; sharing its mutable cache or
operation list across OCaml domains is unsupported. Independent instances and
processes coordinate through native locks. The stable global metadata lock M is
held only for bounded local state work. Each
binding has a shared attempt fence G and exclusive rotation lock R. Dispatch
uses G, then R when renewal is needed, then short M transactions. Background
refresh uses R and short M transactions. External calls never hold M, and no M
transaction waits for G or R. Different bindings do not share a network lock.
Kernel locks coordinate independent processes; a daemon mutex is insufficient.

Each registry owner explicitly selects metadata admission at open. Nonblocking
admission reports Busy. Production hosts use a validated monotonic wait budget
of at most 60 seconds; expired lock admission reports Timed_out. Waiting retries
only kernel lock acquisition, before metadata load or any effect. A metadata
callback and filesystem publication execute once after admission. Attempt and
rotation lock waiting uses the explicitly supplied monotonic clock and duration;
wall-clock expiry uses the clock borrowed at open. These are not claims of hard
native I/O timeouts. Native operations are bounded and joined through the private
storage boundary. Trusted renewal/revocation ports must honor their switch,
qualified response/time bounds and cancellation, and join their work before
returning or raising. They must not start detached exchanges. Unexpected errors
and cancellation propagate with the durable intent intact.

Registry close rejects new calls and joins admitted owned work. A finite call
limit rejects before side effects. Attempt G fences belong to caller switches;
idle transport channels do not hold those fences or keep registry close waiting.
The registry does not close its borrowed directory or secret backend.

## Uncertainty, logout and recovery

A durable possibly-sent intent precedes external rotating-token exchange. Missing
or ambiguous outcomes never authorize an automatic retry. Exact operation receipt
readback distinguishes a committed pointer, pending intent, rejected publication
and unavailable historical proof. Unavailable never means presumed rollback.
Metadata acknowledgment loss is reconciled against the real authoritative
pointer; an older revision is not restored as a fallback.

Local disable first publishes a tombstone and advances the authorization epoch,
then releases M before waiting for exclusive G, R and owned cleanup. Pending drain
is persisted and survives restart. Missing runtime state never proves drain
completion. Replacement activation waits for local drain and retired cleanup.
A stale login or refresh cannot publish over the tombstone.

Remote revocation is optional and requires a qualified explicit port. No port
means Not_requested, including the local-only Codex route. Its possibly-sent
journal precedes the external call; uncertainty cannot undo local disable.
Local erasure deliberately abandons remote token retries and retains nonsecret
uncertain evidence. Bounded unresolved revocation history reserves capacity before
enrollment so a later local disable is not blocked by optional remote work.
Terminal operation receipts can age out; unresolved intents and outstanding
removal proofs cannot be silently evicted. Environment removal disables this
binding without modifying process environment bytes.

A trusted host can inspect the pending candidate's nonsecret original operation
ID and explicitly cancel that exact candidate after restart. Service consumers
must authorize the flow owner before exposing cancellation. Existing active
credentials remain; only operation-owned staged cleanup is scheduled. Generic
reconciliation never replays login or external rotation.

## Focused qualification

Ten pure model expect cases and ten native lifecycle expect cases passed on
macOS with synthetic owned private directories. Native cases include actual child
processes for serialized refresh/logout and process death after rotating intent,
foreign-existing quarantine without deletion, pre-external capacity rejection
with zero provider calls and successful full-capacity logout, restart drain recovery, canceled
exchange joining, explicit environment guards, private continuity recovery, and
real pointer publication followed by acknowledgment loss. The acknowledgment hook
runs after successful native directory sync; pre/post-sync native fault coverage
belongs to the private-storage tests. These results do not qualify Linux,
real-provider routes, Keychain, browser acquisition or the complete host/CLI bridge.
Repository documentation gates and broader integration checks remain separate.

Prospective revision slots are reserved before enrollment or renewal calls. This
reserves known revision resources, rather than every possible future grant size.
The bounded whole document is validated at each transition. A larger valid
provider response can exceed the remaining document budget; storage or publication
can also fail after an external effect. These outcomes retain uncertainty and
require exact-operation reconciliation or explicit recovery. They never authorize
automatic exchange retry or fallback to a stale credential revision.
