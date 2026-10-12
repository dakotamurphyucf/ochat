# Exact Git catalog capture

`Chatml_git_catalog.Reader` converts exact raw objects from one scoped
`Chatml_git_host.Repository` into the existing immutable `Chatml_catalog.t`.
The host owns deployment, explicit transport/authentication grants, acquisition,
private storage, process limits and the whole-bracket deadline. This library owns
Git object verification, selected tree traversal and catalog capture. Neither
captured declarations nor catalogs grant filesystem, network or execution access.
There is no checkout, initializer, compiler, package installation or dependency fetch.

A caller first reads a declaration with a validated package-root prefix and logical
project member. The Reader hash-verifies the pinned commit, traversed trees and
selected blob. It projects the authored namespace/version from those exact bytes
using `Project.Namespace`, chooses an explicit override or that authored namespace,
then creates the final captured source and fully decodes the same bytes. An override
cannot repair invalid authored namespace/schema. Full decoding still rejects unknown
fields. Bootstrap errors have no invented source owner; subsequent failures retain
their exact captured declaration context. Reading another package does not change
which owner belongs to a saved declaration's capture.

`package_root` translates only raw tree lookup. Source IDs remain effective package
namespace plus package-relative portable member path. A saved declaration is affine
to its Reader. The shared Catalog Discovery/Plan defines own environments, source
directories, module names, explicit overrides, aliases, entries and optional companions.
The Reader supplies actual observed direct children and exact selected bytes. It does
not independently rediscover modules or compose dependency catalogs. Canonical aliases
retain the same module/source identity. Selected or traversed symlinks, gitlinks and
unsupported modes reject; unrelated links remain inert. Executable regular blobs have
exact raw bytes. Optional absence means a missing final tree entry; bad ancestors,
missing objects, framing/hash/type failures and unsuitable modes are errors.

Git object IDs are explicitly SHA1 or SHA256. Verification hashes
`kind + " " + decimal(body byte length) + NUL + body`, checks the requested header ID,
kind, size, framing and trailer, and then decodes raw tree names/modes/binary IDs.
Tree order uses unsigned raw bytes and the directory '/' terminator. There is no
Unicode repair or locale normalization. Commit interpretation obtains one exact root
tree; it does not verify signatures, traverse parents or claim complete `git fsck`.
Hash verification establishes content identity, not author authentication or source
trust. Captured source SHA256 remains a separate digest with the existing Source API.

One Reader shares verified object/tree and captured-source caches across package
captures in the same repository bracket. Accounting counts unique verified object
bodies, decoded tree entries and unique exact Source IDs/contents. Equal blobs at
separate Source IDs charge separate captures; repeated aliases/reads do not charge
those bytes again. Raw native queries and semantic work have separate ledgers.
The returned accounting receipt is cumulative for the entire Reader, not a reset per
package or an estimate of the host's process/network work.

Every actual checkpoint admits one semantic work step before the next bounded chunk,
element, comparison or stage. Byte copying/hashing/scans use at most 4096-byte chunks;
raw tree names are bounded to 1024 bytes before copying. Full translated member byte
and component limits are admitted before object queries. Counts and byte additions
are checked against remaining allowance before effects/copying. Object query wire
allowance checked-adds 128 bytes of header/trailer reserve to remaining body allowance.
A valid framed over-limit body is a semantic limit; truncated/invalid framing is a
protocol error; exceeding native output allowance remains a host limit. The shared
JSON parser is byte/depth/node bounded and polled before/after parsing, without a
claim that arbitrary parser CPU work can be preempted internally. Checkpoints also
poll the same host bracket's live state, deadline and Fiber cancellation. Work steps
are cooperative admission units, not bytes, CPU cycles or wall-clock guarantees.

Operations run in one switch-owned worker. Concurrent or reentrant calls reject
Busy and poison successful completion. Reader callback return with pending work
rejects and cancels/joins that work before releasing borrowed state. Waiting caller
cancellation also joins its worker first. Close revokes admission before cancellation;
only after joining does it clear callbacks, repository and caches from the escaped
Reader. Caller exception instances and original backtraces survive cleanup. Expected
work/deadline aborts use a fresh bracket-local tag and become the typed Reader result;
no partial catalog/accounting result is admitted. A consumer must propagate that
result through the enclosing host callback to prevent cache publication.

Online acquisition uses the host's exact full pin, never HEAD/ref/tag fallback.
A server may refuse or lack an exact full pin, including an unadvertised historical
commit; acquisition then fails. The moving-HEAD test first acquires the advertised old
pin, moves the actual branch, verifies the cached old pin and explicitly acquires the
new pin. Offline use has no fetch port and re-verifies exact cached objects with this
same Reader. OCH203 capture-only lock restoration is a separate pure source-inventory
operation; this library does not claim that persisted catalog metadata validates
uncaptured or historically absent directory entries.

The tests combine actual qualified Git 2.56 repositories in both object formats,
authenticated controlled HTTPS, exact source/scopes/alias expectations, raw malformed
fixtures, aggregate boundaries and deterministic checkpoint-gated cleanup/cancellation.
Pure malformed fixtures supplement native-produced object bytes; they are not a
second catalog planner or an encoder/decoder roundtrip used as the sole oracle.
