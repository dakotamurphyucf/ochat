# Durable goal accounting custody

`Goal_accounting` is the pure cumulative partition for one stable Goal and
session. `Goal_usage` combines six independent components while preserving
Actual, Estimated, Unknown and Untracked classifications. The canonical
`Inference_ledger` owns the retained observations, states and opaque admitted
handles. Accounting stores no second provider event stream.

`Goal_accounting.Stored` contains validated compact identity claims and aggregate
counters. It contains no live handle or execution authority. The context-free
codec supports ordinary delta decoding; `Goal_accounting_document.resolve`
subsequently looks up every exact canonical row and compares its complete handle
identity. Missing rows, mismatched session/generation/scope or accounting/context
identities fail. Strings never allocate a missing historical handle.

`Goal_accounting_document` preserves a complete universal document for one
partition. Applying a pure Plan checks its original accounting identity and
frontier before preserving the carrier. Retired bindings fold the final ended
before-ledger contribution exactly once. Retained authoritative revisions remain
live reads, including after a Goal closes. Final ordered-batch validation derives
settled changes from the actual removed canonical rows; decoded totals alone do
not prove original execution attribution.

`Goal_accounts` is an owned optional child of the existing `Session_state` and
`Session_delta` persistence flow. It has no separate store. The parent treats the
complete child envelope as one value; the child alone preserves unknown fields
inside its collection, wrappers and account documents. Native admission and
decoding share the same projection validator. At most sixteen partitions and
256 globally unique retained binding ordinals fit within a 4 MiB collection;
each partition has a 256 KiB complete document limit. Structural limits and the
actual enclosing host profile also apply. Preserved unknowns count toward all
limits.

Positive byte, field and node growth for future aggregate folding is reserved at
partition, collection and complete host admission. These credits add to Run,
Goal-disposition and instruction-clear reservations. Private maximal JSON probes
measure capacity and preservation compatibility only; they are never decoded as
accounting or receipts. Retained bindings reserve one accounting revision for
retirement. Binding containers with protected future data cannot be removed:
`can_retire` returns false only for the real codec's extension conflict. Other
errors remain errors. The actual ledger admission owner must consume this
eligibility before publishing a retirement; this storage layer does not install
that owner or silently discard future data.

Historical missing accounting remains missing, including absent versus explicit
null field presence. It is not a lifetime-zero report. A new held Goal before its
first episode may bootstrap zero counters at the exact current ledger frontier.
An existing historical episode cannot acquire zero accounting by losing its
retained Run link or by changing source. A migration, if needed, requires an
explicit future contract. Resume, reset, user messages, metadata edits and Goal
closure never reset an existing partition.

Final validation checks immutable Goal/session scope, exact Run-to-Goal binding
membership, complete canonical handles and checked admission totals. Across
partitions, admitted turn/attempt and Untracked advances cannot exceed the same
canonical advancement. Unrelated canonical work need not be Goal-attributed.
Snapshots also bound aggregate counts by canonical lifetime coverage. These are
storage consistency checks, not runtime permission or evidence that an authored
Goal executed.

Actual execution-owner attribution, budget enforcement, atomic inference
admission/retirement and final provider submission guards remain integration
work. The controller and authored `Goal.finish`/worker-join qualification must
use those actual owners; storage fixtures do not satisfy that execution gate.
