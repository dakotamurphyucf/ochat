# Original prompt sources

An immutable prompt artifact records the code and inputs already captured for
that revision. `Prompt_source_receipt` additionally records the operator-selected
prompt definition ID and exact absolute root filename. Its fingerprint identifies
that selection, independently of file contents. The receipt is bounded metadata;
it cannot open a directory or grant tool, network or execution permission. Older
artifacts without a receipt remain restorable.

`Prompt_catalog.prepare_owned` admits current operator definitions and retains a
`Chatml_local_source` directory owner before publishing an owned candidate. Source
availability is separate from captured revision availability. A missing live root
can therefore leave captured restoration usable while fresh source reads are
unavailable. The catalog never reopens a path read from a stored receipt.

Owned candidates are catalog- and generation-affine, with one pending candidate
at a time. Discard or failure closes candidate descriptors outside the catalog
mutex. Installation rejects foreign, discarded and stale candidates. Unchanged
selection and immutable limits reuse the installed policy cell, so old runtime
leases and newly installed owners observe later removal or revocation together.
Discarding a candidate does not revoke that shared policy. Different limits for
an unchanged installed selection reject before acquisition.

`Prompt_source_lease` gives an actual runtime one independent duplicate of the
admitted directory. Private and authored descendants borrow that loan through the
ancestor's resource lifetime. `Session_factory` selects it using the artifact's
original receipt ID, including for artifacts whose generated definition ID differs.
`Runtime_builder` admits attachment once and closes the owned loan after runtime
cleanup. Descriptor closure and source-policy revocation do not alter captured
code. Captured code never substitutes for a denied live-source grant.

`Prompt_source_owner.with_scope` permits sequential capture and source preparation.
It reuses the existing Builder/parser walk through a retained filesystem loader;
no second loader or ambient path fallback exists. The original Eio path is only a
diagnostic label. The retained Local owner remains usable after that Eio directory
switch closes or the original path is renamed. Leases clone the same native
directory; a different directory installed at the old path does not replace it.

Scope workers are joined on callback return, failure and cancellation. Concurrent
or reentrant operations reject as Busy and poison that scope. Scope closure
revokes admission before cancellation and joining. User callbacks preserve their
original exceptions and backtraces. Explicit Owner and Local close unregister
cleanup hooks, join native operations, and safely repeat during switch cleanup.
Owners, policy cells, scopes and loans remain on one Eio switch domain; immutable
capture and receipt values may cross domains.

A capture exposes routes only for its actual parser input identities. Relative
project locators use the declaring input's parent, while the project member stays
package-relative. `Scope.prepare_sources` validates capture affinity, privately
borrows the same Local owner through `Anchor.of_local`, and calls the shared project
preparation API. Additional anchors and Git grants remain explicit host inputs.
The result contains immutable source custody and required tool names; source-only
preparation neither admits live tools nor executes initializers. No native owner
or root getter escapes the scope.

Receipts have a 4096-byte schema ceiling for root paths. Source limits validate
positive path-component ceilings and nonnegative runtime lease counts; callers
supply project preparation's separate aggregate limits. The daemon currently
admits at most 1024 original roots, 512 path components and 4096 runtime leases per
selection. Root counts and lease capacity are checked before acquisition, and
policy checks surround yielding checkpoints and publication.

This boundary supplies original-source ownership. Per-launch File selection,
retry reservation and document-content capture are separate integrations; these
APIs do not change existing captured execution or introduce authored selectors.
