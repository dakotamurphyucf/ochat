# Universal durable documents

`ochat.document_schema` supplies a pure named-field document boundary, explicit
version conversions, domain validation, and unknown-field preservation.
`Agent_store.Document_record` connects this boundary to existing Frame v1 and
original persisted-byte SHA-256 anchors. Neither library performs I/O.

This is the storage foundation. Concrete adoption by snapshot, transaction,
session state/delta, event, audit, archive, moderator and legacy prompt-session
owners belongs to OCH-52. Existing `Snapshot.Persisted` and
`Transaction.Persisted` binary codecs and `Session_persistence` sexp payloads
have **not** been migrated by adding this library.

## New baseline envelope and wrappers

The complete frame payload for the new baseline is one JSON object:

```json
{"format":"ochat.document","schema_version":1,"kind":"session.snapshot","payload":{}}
```

The format marker identifies the family. Each registered kind has its own
positive target version. Frame version, client protocol version, application
version and provider replay version evolve independently. Optional `extensions`
is a non-null object. Optional `required_semantics` is an array of unique,
nonempty names; the domain codec rejects names it does not understand before
constructing domain values. Unknown envelope members are retained.

OCH-52 must put **outer** snapshot metadata and **outer** transaction counters,
chain/session identifiers, audit, delta and events into named fields too. Placing
this envelope inside the old derived binary wrapper does not establish the new
boundary. Wide counters use validated decimal strings in each concrete payload
schema. Embedded records must use their own registered document contracts, not
opaque sexp strings containing current runtime types.

Pre-baseline beta sessions may be incompatible by accepted policy. Unsupported
binary/sexp document payloads return `Unsupported_beta_format`; damaged/newer
frames and malformed/newer documents return their distinct typed errors. Reader
owners must leave files, journal bytes and CURRENT pointers unchanged on failure,
without reset, repair, destructive fallback or legacy type guessing. The pure
boundary itself cannot mutate them.

## Read, convert, validate, commit

1. The owner bounds its Eio read and calls `Document_record.decode_frame` for a
   journal entry or `decode_file` for a complete snapshot. Existing Frame v1
   verifies its header/version/length/checksum first.
2. An optional expected SHA-256 digest is checked against the **exact persisted
   frame payload bytes**, before document parsing or conversion. JSON decoding
   then enforces depth, byte, node and field limits and rejects duplicate keys.
3. Before conversion, the owner checks stored-version session/counter/chain and
   snapshot/fallback anchors on generic named fields. Structural metadata access
   must not construct current runtime state. `Document_record` supplies bytes
   and digest; it does not invent a second chain/recovery owner.
4. `Conversion.upgrade` checks a registered kind and applies an explicit contiguous
   adjacent version chain to its target. Field operations preserve absent/null
   distinctions. `Step.of_function` supports pure structural conversions such as
   array restructuring and counter representation changes. Each callback obeys
   a pure, deterministic, bounded-work contract. The engine bounds invocations
   and output values; it cannot sandbox arbitrary OCaml code. Unexpected callback
   exceptions propagate instead of masquerading as malformed persisted data.
5. `Domain_codec.decode` independently checks its kind, exact target version and
   required semantics, projects owned fields and validates the current domain.
   Existing owners validate IDs, counters, order, provenance and call/result
   relationships. No current binary/sexp reader participates before conversion.
6. The existing actor/recovery installs validated state. Existing atomic commit
   owners persist the complete new envelope only after validation succeeds.

The original verified record is immutable. `upgrade` returns a separate logical
document, leaving `stored_bytes` and `stored_digest` unchanged. A recovered
transaction's digest must never be recomputed from upgraded or normalized JSON,
nor from a current typed re-encoding. A new checkpoint anchors the original
journal head digest. New transactions hash the exact payload bytes actually
written before frame wrapping; the frame checksum also covers the frame header
and remains independent. `Document.to_string` preserves value/field order but
may normalize whitespace and string escaping, so it is not an original-byte
integrity operation.

## Editing and retaining extensions

A `Shape` declares named-field ownership. `Value` owns a whole subtree (useful for
an immutable captured raw provider envelope); `object_` declares owned child
fields. `nullable` permits null without surrendering ownership of non-null
structured values. A missing key remains absent, and a present null remains
null. Each domain encoder must explicitly model its concrete field's presence
policy; an ergonomic `option` alone cannot distinguish absent from null.

Restoring returns `'a Extension_carrier.t`. Use `with_value` for functional domain
edits and pass the carrier to `Domain_codec.encode`. The encoder validates the
replacement known data by decoding its projection, then merges unknown data
at original paths while preserving original object order. Both unknown envelope
members and nested payload members survive. The carrier records field ownership
at restore time: a newly owned field cannot silently overwrite an inherited
unknown field. This returns `Extension_conflict`, even if the values happen to
match. Schema promotion must happen explicitly in a conversion followed by
current decoding.

Array shapes can declare an owned, unique, nonempty string identity field. Host
IDs then attach unknown fields to the correct entries through reorder and edits;
array order follows the replacement domain. Removing an entry with unknown data
fails with `Extension_conflict`. A carrier also retains its original identity
policy: changing between keyed/unkeyed arrays or changing the identity field
while unknown data exists fails with `Extension_conflict`. Without identity, an
array with unknown data can
be emitted only when its known projection is semantically unchanged: object key
order may normalize, but array order, numeric lexemes and absence/null may not.
The conservative check prevents unknown data from attaching to a different
entry. Identity lookup uses maps/sets rather than an unbounded quadratic scan.

New records deliberately use `of_authored_value`, with an empty carrier. Ordinary
edits must never discard a restored carrier using this constructor. When a
storage owner intentionally retires unknown-bearing entries, it must make that
loss an explicit domain operation: either a versioned pure conversion with a
reviewed retention policy, or deliberate reconstruction as a new authored record
under the existing owner's authorization. That is a new record, not an automatic
conflict fallback. The generic encoder always fails closed on accidental loss.

Limits are validated positive values, with maximum depth 256. Input receives a
lexical depth guard before Jsonaf parsing. In-memory values receive a bounded
traversal that counts exact compact JSON bytes, including string/key escaping,
without allocating encoded output; invalid UTF-8 keys
and strings and malformed numeric lexemes reject. Validation and conversion
boundaries use immutable JSON trees; the carrier retains the converted document
and unknown tree for its lifetime. Callers should release carriers with their
own loaded record lifecycle.

## Verification

```sh
opam exec --switch=default -- dune build lib/document_schema lib/agent_store --root . --build-dir _build
opam exec --switch=default -- dune runtest test/document_schema --root . --build-dir _build
```

Constructed independent documents cover renames, defaults, structural callbacks,
version chains, null, unknown nested/envelope fields, array identity/order,
extension conflicts, domain failures, wrong kinds/targets, required semantics,
duplicate/malformed input, limits and original-byte integrity. A stored-byte
SHA-256 expectation was independently computed with Python `hashlib`, and a
Quickcheck property exercises 100 arbitrary Unicode extension-preserving edits.
No frozen historical binary fixture or runtime reader is introduced.
