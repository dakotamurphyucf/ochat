# Capturing project sources before session admission

`Chatml_project_prepare.prepare_sources` captures and composes an ordered batch of
project requests before a live session tool registry exists. The result is a
`Source_batch` containing immutable `Source_preparation` values and one aggregate
accounting receipt. It grants no tool authority and runs no compiler, initializer
or script.

This is intentionally distinct from `prepare` and `prepare_and_publish`. Those
strict operations still admit the selected lock's required tools in their original
attempt and deadline before returning a completed preparation. A session using a
source-only batch must admit `Source_preparation.required_tools` against the actual
registry available to each handler before initialization.

The batch shares one existing attempt, deadline and source/native/work ledger.
Each composition receives the remaining canonical-input allowance; successful
prepared inputs debit their actual `Prepared.Accounting.input_bytes`. Independent
requests count separately even if they read identical files. Failure returns no
partial batch or budget refund. Checkpoint exceptions and cancellation propagate
after native cleanup, and an empty batch has an empty receipt.

`Request.Entry` distinguishes declared entries, captured fragments and captured
files. A host may supply a file's `root_member` only after establishing its actual
relationship to an admitted project root. If that member exists in the selected
root catalog, its length, digest and bytes must agree with the captured file.
Agreement reuses the catalog source identity, so the entry and a reference to that
module do not initialize separate copies. Disagreement returns
`Entry_revision_conflict`; it never silently picks one version. If the member is
absent, the original captured identity remains independent. Inline fragments are
always independent, even when their text matches a catalog member.

Neither a serialized path nor a working directory establishes that relationship.
Original source ownership remains with the host. Existing `Request.create` callers
retain their captured-fragment behavior; `create_with_entry` exposes the explicit
file distinction without an extra backend read.

The public preparation tests exercise real capture, composition, compilation and
execution for an overlapping entry. Compact invariants cover strict versus
source-only tool admission, aggregate source/canonical-input limits, interruption
at every observed checkpoint and subsequent reuse of the host.
