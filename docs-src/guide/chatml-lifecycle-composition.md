# Composing lifecycle policy

The ordinary [lifecycle project](../examples/libraries/lifecycle/chatml.project.json)
provides checked source helpers. It adds no language syntax, scheduler, mutable
state store or native registration. Select `Lifecycle` from its `pure` environment
for pure author policy. The separate [lifecycle-run project](../examples/libraries/lifecycle-run/chatml.project.json)
exports `Lifecycle_run` from its `moderator` environment and requires `moderator_v1` or `delegated_moderator_v1`.
These ordinary source projects are included in the installed `lifecycle` resource.
Use [installed library discovery/export](chatml-installed-libraries.md) to obtain
the complete tree without a checkout.

A component returns an explicit ID, its `Active` or `Finished` obligation, and
`No_action`, `Continue` or `Wait(wake)`. `Lifecycle.compose` preserves authored array
order. It admits at most 64 components with unique IDs matching
`[a-z][a-z0-9_-]*` of 1..64 bytes. Invalid IDs, duplicate IDs, a finished component
requesting more work and incompatible actions return typed errors with no partial
plan. Empty input gives an empty neutral plan. Inputs are never mutated.

`No_action` is neutral. Identical continuations coalesce. Waits coalesce under the
explicit pure equality policy passed to the generic helper. The moderator adapter's
`Lifecycle_run.compose` first preflights every wait under aggregate limits of 4096 JSON nodes,
65536 string/key bytes and 32 nested edges. Cyclic and oversized values return
`Wake_limit_exceeded` before comparison; generic policy errors are wrapped in
`Composition`. It then supplies exact structural JSON equality, including ordered
object fields and duplicate keys. It is conservative: differently represented
requests can conflict even if a host decoder would normalize them. Equality is
not wake authorization. The actor still validates each actual run, source,
installation, occurrence and current callback.

Keep component state in explicit typed fields, as in the
[two-component example](../examples/learning/lifecycle-composition/moderator.chatml):
`counter` and `review` own distinct fields beside the shared schema/config version.
The helper does not hold handlers, callbacks or component state in a generic
registry. Each component is a separate ordinary project dependency with its own interface,
namespace and selected environment. The example calls counter then review in a stable order and updates
both state fields only through the successful callback transaction. Its explicit
legacy migration preserves the old counter field and initializes the new review
field. Source/config replacement must deliberately choose a migration or reset;
an incompatible schema/config fails before handling an event. A library import
cannot automatically reinterpret an older persisted checkpoint.

`Lifecycle_run.stage(plan.action)` stages zero or one native Run request. Component
completion does not finish the root: active peer obligations remain in the plan.
`Lifecycle_run.finish(plan, explicit_decision)` separately requires no active peer
and no pending action, then stages the supplied finish decision unchanged. It
never creates a successful terminal status, result or receipt. Plans are transparent
author data, not a trusted admission proof. The actual Run owner validates finish
against owned work and evidence; settlement can remain pending until the real
worker joins. These tasks require an actor-owned run callback scope. Compiling
or importing the adapter does not install that scope; a host without it rejects
native Run requests. The two-event example requires a host-admitted `Workflow`
run so the run remains open between its continuation and explicit finish.
See [scoped run decisions](chatml-authoring-runs.md).

Helpers do not retry or replay an already committed action. Resume restores the
existing actor checkpoint and receipts; it does not repeat startup or recreate a
run from a copied plan. Current native staging, catch rollback, commit and source
custody tests retain those runtime guarantees. The lifecycle tests add a bounded
independent decision oracle, checked module interfaces on both moderator surfaces,
and a two-component callback that stages one continuation and one separate finish.
