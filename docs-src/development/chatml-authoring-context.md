# Immutable authoring contexts (OCH-278)

`Chatml_authoring_context` is the portable, pure revision store for M5 authoring.
Its host keeps the latest immutable store value and supplies an already admitted
`Chatml_project_build.t` from the source host or a validated bundle. The store
performs no filesystem, network, compiler, runtime or transport work. Identity
labels bind principal, host incarnation, connection lease and a host-minted
context nonce for comparison; none is a permission. The host rechecks the
current actor and source grant for every request and cancels/joins work before
dropping a context.

The selected base build fixes the environment, compiler contract and base graph
fingerprint. At admission the host passes exactly the source IDs it permits the
editor to change. The store rejects unknown, duplicate and generated-lock IDs;
the host determines origin-specific edit rights. `replace` accepts full exact
bytes for an editable captured source, with no UTF-8 or newline repair. It
preflights document, effective-input, context-source and aggregate-source byte
caps before hashing/indexing the new source. The store charges base prepared
source bytes plus all open overlay bytes, including shared immutable strings;
this is a conservative source-byte ledger, not an exact heap measure.

Every edit or document close advances the **whole context generation**. A
version issued for document B becomes stale after a successful edit to A, even
if B's bytes did not change. Clients obtain a fresh version before retrying.
Editor revisions for one source strictly increase across overlay close within
one context. Closing a document restores the exact base bytes and retains its
last editor revision. Refresh is explicit and uses a new host-minted nonce; it
either rejects open overlays or discards them by named policy. It never merges
them, and cannot change the selected environment or compiler contract in place.
Closing can fail without changing the context if restoring the base bytes would
exceed the effective-input cap after edits to other open documents.

`Inputs` captures exact selected bytes at one generation. If overlays are open,
it is pending source data, not a prepared graph: OCH-264 must validate the
complete closure through the existing project catalog and composition path.
OCH-278 retains only one analysis key and clears it on edit, close, refresh or
context removal. OCH-258 owns the single immutable semantic payload and applies
its real structural publication limits. The pure store's ledger bounds only
the current contexts and source bytes it owns. A host must bound the lifetime
of old store values and `Inputs` snapshots held by in-flight responses.
