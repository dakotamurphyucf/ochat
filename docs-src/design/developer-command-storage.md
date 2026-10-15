# Developer command storage

Status: the durable storage primitive is implemented. The hosted developer-command
service and its execution policy remain separate integration work.

A top-level check, fixture run or live run has its own command receipt. Child-job
results and Root phase checkpoints remain separate evidence. Commands live in
the existing developer-run document and use its sole Store writer, complete
before/after transaction images and pending-write ownership.

## Identity and phases

A command key is unique across every epoch of one stable principal/run. Its
immutable identity includes its original owner, capture, command kind and request
fingerprint. The host computes that fingerprint over the entrypoint, inputs,
fixture selection and execution/output policy; the Store compares its exact
identity and does not inspect those request fields. The Store allocates the command ID only after proving
the key absent; an exact retry returns the persisted ID. Historical reads retain
the original epoch and never authorize new execution.

All kinds begin Reserved. Only Live_run may enter Waiting_permission or
Execution_started. Check and Fixture_run remain inert with respect to host tools.
A live success requires the execution barrier; failure, limits, interruption or
cancellation may finish before it. The barrier records that execution could
begin, not proof that an external effect occurred. Terminal receipts are
immutable: repeating the same terminal is idempotent, and changing it conflicts.

Receipts preserve structured diagnostics, fixture failure categories, logs and
results. Stored source ranges are bounded comparison coordinates; they do not
grant source access or authenticate bytes. Result JSON may contain null, while an
optional diagnostic location is omitted when absent. Explicit null is rejected
for that optional location.

## Bounded recovery

Command limits bound row count, the aggregate encoded size of complete rows and
individual terminal size. The actual document also enforces its existing byte,
depth, field, node and shared work limits. Fixture logs use the aggregate budget;
storage adds no smaller per-log byte ceiling.

Encoding reserves enough space for interruption of all nonterminal commands and
all live inference attempts together, including their combined document revision
increments. Checking those two groups independently would not guarantee that
both could be recovered in a nearly full document. This reserve is a size check,
not a persisted recovery decision. Arbitrarily large successful results still
require their own bounded admission.

The unreleased developer-run layout advances to version two with the command
array. A version-one layout is rejected explicitly rather than interpreted using
new assumptions. This does not introduce historical OCaml runtime type copies.

## Execution and uncertain writes

The command interface distinguishes a newly acknowledged Start from an observed
stored Start. Only the first uninterrupted successful Start commit may issue its
process-local acknowledgement, which is consumed once. Reconciliation, repeated
commit and restart return observations only. Actual Root, actor, source and
provider admission are still required before effects.

An uncertain write retains the exact selected Pending in the existing Store map.
A key miss or cached absence does not release it. Repreparation requires a fresh
complete before-image comparison under the writer and preserves the same selected
row and terminal bytes. A proven absent admission may be retired after
cancellation; frozen terminal selections cannot be discarded that way. Recovery
joins or reconciles owned work before choosing a truthful terminal, and never
replays an effect to discover its result.
