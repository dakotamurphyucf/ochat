# Complete captured prompt custody

`Captured_prompt_artifact` retains a complete `Prompt_artifact_store.Artifact`
inside an immutable universal document. It holds the exact original manifest
publication bytes and digest, authored root and dependency bytes, and both private
program and content payload pools. This preserves captured project/program and
document editions that a raw ChatMD source bundle cannot represent. It performs
no filesystem access, publication, compilation, native initialization or execution.
The stored value establishes consistency and custody; the actual selected source,
host policy, native contracts and factory remain responsible for execution.

All byte strings use canonical padded base64, including the original publication.
The decoder checks the complete JSON profile, every encoded length, alphabet and
padding bits, and the **combined** inventory before decoding any bytes or hashing.
The root, authored sources, program pool and content pool together have the existing
256-item and 8 MiB limit. The original manifest has its separate 262144-byte limit.
Missing, surplus, duplicate or changed private payloads and mismatched revision,
publication digest or source inventory reject through the existing artifact owners.
A different pool containing equal bytes still consumes its own inventory item.

`Artifact.restore` is the pure restoration boundary: it restores private bundles
against the original publication inventories, checks the shared payload budget and
matches the exact root/source hashes and metadata. It does not reauthor a manifest.
Compatible unknown publication metadata remains in its original bytes, and the
universal carrier retains compatible unknown envelope and payload fields. The
carrier has no public mutation, authority or artifact-GC interface.

The complete carrier is bounded to 16 MiB of encoded JSON, depth 64, 4096 object
fields and 8192 nodes, including unknown fields. Its enclosing durable owner must
also preflight the complete embedded document against that owner's host limits
before saving or effects; this carrier's bounds do not enlarge a session quota.
A legal artifact may therefore fail a smaller carrier or host budget.

The focused storage tests cover joint program/content pools, arbitrary binary
bytes, exact publication and future metadata retention, tamper rejection and the
shared inventory boundary. Existing parser/program and captured-document owner
fixtures also pass their actual artifacts through the pure carrier. These checks
do not establish runtime authority or replace the actual authenticated replay and
factory qualification.
