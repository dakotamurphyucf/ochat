# Host project preparation

`Chatml_project_prepare` connects explicit host authority to the existing Local
capture, pinned Git catalog, and pure project-composition libraries. The host
supplies the root selection, admitted directory anchors and exact Git grants, the
actual compiler contract, and the already admitted tool registry. Authored project
configuration requests dependencies and tool names; it cannot create authority.

## Capture and selection

`prepare` produces one immutable `Chatml_project_build` and its final accounting.
`prepare_and_publish` additionally selects that build's exact lock through a
borrowed publication owner. The result retains captures and metadata, without
native resources, callbacks, tool implementations or a mutable ledger. Compilation,
initialization and runtime admission remain separate operations.

Initial preparation accepts authored full Git pins. An ordinary Build captures
current Local files and preserves previously selected Git code. Environment and
namespace changes are permitted at the same repository, object format, full commit,
package root and project member. New or changed Git code selection needs an
explicit Update. The pure selection gate checks every incoming previous-owner
route before acquisition or alias reuse; final composition repeats the checks.
Changing an alias cannot conceal a changed nested Git dependency.

Declaration traversal follows the actual selected catalog scopes, not every
package mentioned in a configuration file. Packages use their own selected
environments. Repeated acquisition identities share captured catalogs only when
namespace and environment selections agree. Declaration cycles can reuse captured
nodes; executable source cycles remain the compiler's responsibility.

Local identity comes from retained directory owners and the exact project member.
Relative dependency locations are interpreted from the declaring configuration's
directory. Git identity includes the exact repository, format, full pin, package
prefix and member. Relative Local dependencies inside Git stay within that pinned
repository. Native Local access still requires a host anchor.

One local-root bracket retains identities through composition. Git acquisition
brackets run sequentially because the store owns one active operation. Packages
known in an active repository bracket share its Reader and verified-object memo.
A different repository can later reveal a new package in a previously visited
repository. That uncaptured package requires a new bracket with new accounting;
already captured catalogs remain reusable. No reader or repository outlives its
bracket, and no nested acquisition or implicit authority fallback occurs.

## Bounds and failure

An attempt owns one aggregate ledger and one monotonic deadline. Literal backend
ceilings remain authoritative. Before entering a backend bracket, preparation
reserves its allowances; nested stages see the capacity left after all outstanding
reservations. A successful final receipt replaces that reservation with observed
usage. Every scoped Git Reader and native acquisition contributes a receipt,
including revisits. Failure aborts the attempt; catching an error cannot enable a
retry, refund or successful completion.

Accounting distinguishes retained sources, native request/output bytes, Git
objects/tree entries, local directory entries, and documented adapter work units.
Acquisition storage counts admitted peak-payload reservations, not operating-system
disk quota. Work units do not measure native instructions or CPU consumption.
Cooperative checkpoints preserve cancellation and unexpected exceptions; joined
cleanup can exceed the operational deadline.

## Selecting a lock

`Publication.open_owned` borrows an opened directory and the caller's exclusive
coordination. Keep both alive until the owner's switch has revoked admission,
cancelled and joined active work. The publication owner has no manual close or
automatic retry/reconciliation operation.

The shared deadline starts before the first expected-lock read. The owner checks
its exact expected bytes, prepares the complete candidate, and rechecks the old
selection before writer entry. The candidate, final lock bytes and next expected
capture exist before the final checkpoint. The existing `Durable_file.replace_in`
performs protected replacement with file and directory synchronization.

Before writer entry, failure leaves the previous lock selected. Once the writer
starts, a failure or an undelivered success leaves the owner `Uncertain`; it never
claims rollback or exposes the old lock as current. Only successful delivery
installs the new expected capture and restores `Available`. There is no progress
or deadline callback after commit. Host code exposes its new runtime or queue only
after successful publication, under its existing coordination.

Compiler queries and captured restoration do not call this library. Running
sessions retain immutable code. A new independent file-backed launch performs a
new preparation unless its caller explicitly selects retained captured input.

## Graph execution handoff

A successful preparation feeds `Build.prepared` directly to
`Chatml_compilation.compile_prepared_with_policy`, using the actual selected target
and existing host policy. Compilation performs no source reads, initialization or
tool execution. `Chatml_execution.run_prepared` then initializes the selected
value graph and invokes the standard role under the current runtime budget and
native admission. Persistent sessions use the existing prepared Host session API.

The runnable example in `docs-src/examples/chatml/projects/shared-counter` is
qualified by `test/chatml_project_execution`. It exercises a vendored Local
catalog through aliases, companion visibility, direct/open diamond reads,
independent runs, entry/catalog overlap, alternate environment selection and
capture-only restoration after all original files disappear. Native Git,
publication and aggregate admission remain qualified by their owning suites.
