# Local captured catalogs

`Chatml_local_capture.capture` prepares one selected own-package catalog from an
already decoded declaration and an already open host-admitted directory. Source
paths are portable root-relative members. Dependency locator resolution, repeated
package composition, compiler compatibility and executable admission belong to the
host composition layer. The implementation does not parse or execute source code.

Discovery is nonrecursive. Explicit names replace conventional candidates before
ambiguity/orphan checks; exclusions suppress discovery and guessed companions.
All raw non-dot directory entries count, including discarded candidates. All
remaining catalog implementations are captured, even when unused by the selected
entry. Other entries are future launches and are not opened. An entry also exposed
as a module shares its source and module identity. Each selected path is read once;
source identities do not collapse equal bytes or equal inodes.

The native descriptor owner enforces component-relative no-follow acquisition and
joined cleanup. Only an exact missing optional leaf is omitted. Missing parents,
explicit interfaces and all other failures reject. Current local bytes are fresh
for each capture; previous captures remain immutable. Acquisition is not an atomic
snapshot across all selected files and cannot prove absence of metadata-preserving
concurrent rewrites.

## Work reservation model v1

The host explicitly supplies all caps. Work steps are conservative **stage-weight
units**, not an exact instruction, CPU, syscall or wall-clock bound. Each charge
is checked with overflow-safe arithmetic before its stage. Polls check cancellation
and progress; ordinary budget rejection is a Result, never a checkpoint exception.
No unused allowance is refunded. A failed capture publishes no accounting or catalog.

Let `H(n) = ceil(log2(max(1,n)))`, `Q(b) = ceil(b/4096)`, and
`O(n,b,p) = p * n * (H(n)+1) * (Q(b)+1)`. Paths are bounded to 1024 bytes.
The maximum comparison key `K` is the greater of 1024 and actual override-name
lengths. Catalog validation also includes the effective namespace length in `K`.

| Stage | Reservation |
|---|---|
| Declaration inventory and metadata | `Q(declaration bytes)+1`, plus actual declaration bytes for metadata visits |
| Discovery lookup sets and override sorting | `O(overrides + exclusions + source_dirs, K, 6)` |
| Canonical source-directory ordering | `O(source_dirs, 1024, 2)` |
| Native root bracket | 6 |
| Each native directory open | `2 * directory components + 6` |
| Each enumerated candidate callback | `8 + 4 * H(overrides + exclusions + source_dirs)` |
| Pure candidate planning | `O(observations + overrides + 1, K, 16)` |
| Each selected member read | `byte allowance + 2 * member components + 6` |
| Each newly captured member hash | `Q(actual member bytes)+1` |
| Immutable catalog validation | `O(captures + planned members + observations + overrides + 1, K, 16)` |
| Exact declaration revision comparison | `Q(declaration bytes)+1` |

A callback unit covers bounded basename/component scanning; its logarithmic term
covers the directory, exclusion and override lookups. Multipliers cover the several
sorts, balanced-map/set passes and canonical walks in each coherent pure stage;
this model does not equate one reservation unit with one comparator invocation.
The byte reservation for reads permits one-byte partial read operations plus EOF,
not an optimistic assumption that every read fills a 4096-byte buffer. Native
thread operations remain bounded by their admitted entry/component/byte allowances,
with no deadline guarantee for an individual OS call.

A member's byte allowance is `min(per-source cap, remaining aggregate bytes)`.
When no source slot remains, an optional member uses a zero-byte probe: exact leaf
absence is still allowed, while any present source fails the source-count limit.
The supplied declaration counts toward source/byte admission and is seeded rather
than reread. A host composing several catalogs must pass the remaining attempt
allowances, not reset the original caps for each dependency.

## Shared pure boundary

`Chatml_catalog` owns transport-neutral conventional selection, override precedence,
portable collision checks and immutable captured facts. Its public factory validates
inventory, exact declaration revision, required presence, namespaces and alias
association agreement. Optional omission remains an acquisition-owner provenance
claim. Local descriptor/work/error ownership remains here; object-backed Git
acquisition can consume the same pure plan without a checkout or filesystem adapter.

The artifact is not `Chatml_prepared_program.t`. Imported module resolution,
complete file scopes, lock reconciliation, compiler target/tool compatibility and
compiler source-role eligibility still require host composition and admission.
