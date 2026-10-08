# Credential registry metadata model

[Credential_registry_model](../../lib/credential_registry_model/credential_registry_model.mli)
is the pure validated document and transition layer used by the
[shared lifecycle](credential_registry.doc.md). It performs no network, native
storage, environment lookup or secret decoding. It preserves unknown universal
and nested document fields while validating required semantics and bounded
identity, grant, epoch, operation and revision invariants.

The document is limited to 1 MiB, 128 bindings, 64 operation receipts and 64 joint
active/staged/retired revision reservations per binding. Only terminal unreferenced
receipts may age out. Unresolved intents, current removal proof and unresolved
revocation history remain reserved. Revision ownership is globally unique across
bindings. Capacity rejects before a staged backend effect; tombstoning transfers
existing reservations rather than requiring extra secret capacity.

Candidate publication uses expected epoch, revision and operation identity.
Refresh publication preserves the authorization epoch, changes the immutable
revision, and requires the original rotation intent. Local disable advances the
epoch and invalidates both publication paths. `reject_refresh` validates the same
original intent, epoch and protected revision before an authoritative
renewal rejection can disable the binding; a stale network result leaves a
replacement unchanged and does not assert that the exchange was unsent.
Persisted removal drain is monotonic;
restart cannot infer completion. Quarantined foreign revisions remain explicit
and are never treated as eligible owned cleanup.

Raw grant presence is distinct from verified effective values. Declared raw scopes
and expiry must match the effective values; omitted/null fields require explicit
qualified provenance and an unknown-expiry policy. Qualified_token_claim may
supply effective scopes only from an explicitly qualified authenticated token
claim when the actual response scope is absent. It cannot replace an explicit
null/value or infer expiry; canonical authenticated token expiry is Value/Declared. Constructors and document
decoding apply the same invariants. Provider verification remains a trusted policy
boundary, not a claim made by this metadata codec.
