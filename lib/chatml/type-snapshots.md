# Immutable inferred type snapshots

`Chatml_typechecker.Type_snapshot` freezes already settled compiler types into
an immutable graph. It does not perform inference, execute initializers, or
certify a partially checked program. The compiler caller owns the raw types
exclusively during freezing and must finish companion and host-contract checks
before publishing a snapshot.

Each `Input_group` represents one actual quantification boundary. Put related
roots of one scheme or joint contract in the same group; use separate groups for
independent schemes. Equal generalized names share a quantifier only within a
group. Free inference cells instead share by physical identity across all
groups. An unquantified variable alone does not establish weak mutable storage;
that fact requires the owning compiler's binding information.

The graph preserves n-ary functions, tuple and variant payload argument lists,
record/module/field-receiver distinctions, row layers and equality exclusions.
Bound links remain explicit and may form cycles. Recursive references identify
their lexical binder, including when the same bound cell occurs beneath
different shadowing binders. Printed names are display labels, not identities.

IDs belong to one snapshot. `find` and `recursive_binder_name` reject foreign
IDs; equal integer offsets from unrelated snapshots do not establish semantic
equality. Published data contains no inference references, mutable tables,
callbacks, machine state, native implementations, or runtime values.

The caller supplies positive bounds for nodes (including groups, quantifiers
and recursive binder metadata), root/child edges, and retained label bytes
(including equality exclusions). Worklist traversal, physical-identity searches,
label/set copying and publication cooperate with the supplied checkpoint.
Cancellation preserves the callback exception and backtrace, leaves inputs
unchanged, and returns no partial snapshot. Physical-identity caches currently
use bounded linear searches; the output limits and checkpoints bound admission,
but no constant-time identity lookup or linear overall complexity is promised.

The focused tests compare a bounded independent structural model, exercise
quantification and free-cell sharing, lexical recursive cycles, deep traversal,
budget rejection, and every observed checkpoint. They also freeze types emitted
by the real checker for recursive aliases and `Json`. These establish the stated
projection invariants; they do not claim universal type-system soundness or a
complete semantic query service.
