# Private-file provider secret revisions

OCH-162 uses explicit private files initially. Keychain storage, access enrollment
and qualification are deferred. This boundary does not discover HOME, read an
existing authentication cache, import credentials, or choose a fallback.

The [provider interface](../../lib/provider_secret_store/provider_secret_store.mli)
stores immutable secret revisions under a caller-provided namespace. The
[storage interface](../../lib/private_storage/private_storage.mli) supplies secure
directory, publication and advisory-lock primitives below provider/session code.
Neither library owns login, refresh, metadata CAS, authoritative pointers, logout,
or dispatch fencing; those policies belong to the credential lifecycle.

## Filesystem and authority

The host supplies a trusted Eio directory capability. Opening retains that
capability's descriptor identity rather than reopening an absolute path. All
descendants use descriptor-relative no-follow admission; opened directories must
belong to the effective OS principal, have mode 0700 and no extended ACL. Files
must be regular, owner-only 0600, singly linked and ACL-free. Nonblocking admission
rejects special files before reading or mutating them. Directory admission is
rechecked for each operation. Unsupported and remote filesystems are rejected;
macOS local-filesystem support and selected Linux implementations still require
their platform qualification. Same-user advisory coordination is required.

Permissions protect access; this is not encryption at rest. Arbitrary hostile
same-user or root readers, manual restoration, and forensic erasure are outside
the contract. Public errors contain finite codes, operation tags and publication
status, without paths, secret material, token hashes or native exception text.
Secrets have no public serializer or equality API. Trusted consumers explicitly
borrow their string contents and must not log them.

## Publication and cancellation

Secret input is copied and bounded to a positive maximum of 256 KiB. The lower
storage boundary bounds reads and all writes to 1 MiB. Immutable create uses an
exclusive owned temporary, a full bounded write, file sync, atomic no-replace
rename and directory sync. An existing revision returns Exists without comparing
secret contents. Namespace encoding is length-prefixed to avoid separator
collisions. Metadata replacement is separate and intended only for nonsecret
caller-coordinated metadata.

Mutation errors distinguish Not_published from Published_durability_unknown.
A directory-sync failure after rename or unlink never reports Not_published.
Deletion is logical removal, not forensic erasure. The lifecycle must not publish
an authoritative reference until immutable creation returns success. Native work
owns copied buffers/descriptors, releases the OCaml runtime while blocking, and
is joined under Eio cancellation protection. Cancellation is re-raised with its
original backtrace. Create attempts cleanup only of its own newly created,
unreferenced target, proven by its retained descriptor/inode; an Exists target is
never removed. Cleanup failure can leave an unreferenced revision for lifecycle
reconciliation. Cancellation after metadata replacement or deletion is ambiguous
and requires rereading the authoritative metadata; it does not imply rollback.

## Lifetimes and locking

Each backend borrows its Directory; closing the backend joins its operations and
does not close the directory. The host keeps Directory alive until backend close.
Closing Directory joins active operations and rejects new ones. Lock leases own
separate descriptors and switches, so a directory close does not release a lease.
Every acquisition opens a distinct stable lock-file descriptor, including within
one process. Shared and exclusive flock admission is nonblocking; Busy leaves
polling, deadlines and lock order to the lifecycle. Lock files must never be
unlinked or replaced. Owner death releases the kernel lease.

## Qualification scope

Synthetic tests are in
[private_storage_test.ml](../../test/private_storage/private_storage_test.ml),
with a separate [lock process](../../test/private_storage/lock_probe.ml). They
cover actual native pre/post-publication faults, immutable retention, cancellation
ownership, unsafe descriptor admission, cross-process shared/exclusive locking,
owner death, stable inode identity, failed-switch adoption, anchor replacement,
borrowed lifetime and redacted errors. On macOS, all nine native expect cases
passed through the isolated private-storage test alias, including actual Darwin
extended ACL rejection and empty ACL admission. This qualification uses owned
synthetic directories and separate lock-holder processes. Linux native behavior
has not been executed or qualified. No actual credentials, provider requests,
Keychain access, or broader host integration are covered by this result.
