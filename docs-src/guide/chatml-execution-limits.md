# Execution limits, cancellation and persisted state

ChatML's language rules and its execution policy are separate. A valid program
may run forever, allocate too much data, or request effects its caller cannot use.
The executing host chooses resource policy and checks tool authority independently.
Use this guide with the selected [entrypoint contract](chatml-authoring-runtime.md)
and [task semantics](chatml-task-effects.md).

## Compilation does not execute the program

Compilation parses, resolves and typechecks source, then checks the exact target
entrypoints. It does not evaluate top-level initializers, call tools, start a
session or test whether a provider accepts a model configuration. An initializer
can therefore compile successfully and fail when execution begins. Read the
validation report's deferred checks before relying on it.

The compiler's source-size and elapsed-time budgets are separate from interpreter
execution limits. Ochat calls the native compiler in an Eio-managed domain with
invocation-local mutable compiler state. There is no compiler helper executable
or serialized compiled-program subprocess protocol. Cancellation/time checks run
between stages and within inference traversals; cleanup is joined. Work between
checkpoints may overrun the configured duration. This is cooperative cancellation,
not a hard deadline or an operating-system memory sandbox.

## What an execution budget measures

Initialization, pure evaluation, task continuations and controlled input/output
conversion participate in an owned execution scope. These limits are useful for
bounding generated computations, including code that never reaches a tool call.

| Control | Meaning |
|---|---|
| Fuel | Interpreter/checkpoint work, including controlled value traversal; not a portable count of source statements. |
| Elapsed time | Time since scope entry, including waits. Cancellation is observed at cooperative boundaries. |
| Spawned tasks | Spawned host effects and owned job starts; ordinary `let*` sequencing is not itself a new background job. |
| Tool calls | Charged call/start attempts, shared with nested execution; failure does not refund an attempt. |
| Invocation depth | Nested ChatML execution levels; a native dispatch wrapper alone does not add a level. |
| Value size, array size and depth | Bounds checked during construction/projection and at controlled input/output/state boundaries. |
| Allocation | Estimated allocation charged by the evaluator, builtins and value conversions; not measured process RSS or an OCaml heap cap. |

A tool result or a large context can exhaust a budget during projection even
when the script itself is short. Tool schemas, encoded-result limits, retained
work quotas and durable-state limits also apply; passing one does not bypass
the others. Keep aggregates bounded and return authorized references when the
selected tool's result contract supports them.

Numeric ceilings belong to the selected configuration, not the language grammar.
ChatMD script limits and `run_chatml` request limits have different public fields;
do not copy internal interpreter field names into a ChatMD declaration. Read
[declarations](chatmd-authoring-definitions.md) and the
[native request fields](chatml-native-requests.md). An agent-facing one-off request
can lower its host's ceilings, not select unrestricted execution.

## Nested work shares the caller's remaining budget

A nested script call retains its active ancestors' budgets and remaining depth.
Starting another interpreter or crossing a host domain boundary does not reset
the caller's counters or deadline. If two nested computations share an ancestor,
their charged work consumes that shared allowance. Exhausting an ancestor cannot
be hidden by returning a successful child result or catching a task failure.
Captured execution context expires when its owner finishes; retaining the context
does not grant a longer lifetime or reusable authority.

Independent session executions have their own scopes. A persisted child session
is not a continuation trapped inside its creator's expiring script scope: later
turns use the child's admitted resource policy and inherited authority. Durable
jobs similarly have owned attempts and captured ceilings. This separation allows
concurrency; it does not provide dedicated CPU, memory or disk capacity. See
[child lifetime and authority](chatml-authoring-children.md) and
[background ownership](chatml-authoring-background.md) before detaching work.

Trusted embedding code can choose a custom bounded policy or `Unrestricted`.
The latter adds no new interpreter budget and cannot remove an active ancestor's
ceilings, extend an expired scope, or grant tools. Without any bounded ancestor,
there is no interpreter budget to stop an unproductive pure computation; do not
assume the bounded runner's polling guarantees apply. Compiler unrestricted policy
still retains its cooperative cancellation checkpoints. These host policies are
not options a generated script can enable through its source.

## Failure, cancellation and later events

Budget exhaustion is a control failure, not an ordinary recoverable tool error.
`Task.catch` cannot turn its exhausted execution into success. A caller with
remaining budget can handle a separately bounded child's failure, but cannot
resume the exhausted child's scope or hide exhaustion of a shared ancestor. Caller cancellation
also escapes the task recovery path. Local effects, prospective invocation
resolution and moderator state must pass validation before their owning commit;
failure prevents that local update. Already executed shell, file, provider or
remote effects are not undone. Reconcile their results before retrying.

Error codes can distinguish source/compile timeout, execution fuel/time,
allocation/value limits, tool-call or spawned-task limits, excessive nesting and
expired scopes. Host adapters may wrap these in invocation diagnostics. Inspect
the structured diagnostic instead of matching only human-readable text. Static
validation cannot prove termination, tool success or recoverability of external
effects.

A persistent moderator receives a fresh configured execution scope for each
initialization/event. A failed callback does not poison every future event's
budget. Access to its mutable runtime is still serialized; sharing a runner is
not permission to execute two handlers against the same state concurrently.

## Persist data, not live execution

Retained moderator state supports finite primitive values, arrays, records and
variants containing other serializable values. It rejects refs, functions,
modules, builtins and task objects. The snapshot codec retains ChatML structure;
it is not the same representation as an agent-facing JSON result.

The manager checks and copies the old state before a versioned handler runs,
then validates the new state and its encoded size before committing it. Returned
state must fit the declaration's value/depth/array limits as well as the active
execution budget. Its encoded snapshot also must fit `max_value_bytes`; tool
output has its own separate limit. A small in-memory value can have a larger encoded
snapshot. State validation also has a 100,000-node traversal ceiling; choosing
unrestricted interpreter policy does not remove this serialization boundary.
Store IDs, cursors and bounded workflow data rather than closures or
unbounded result histories. Failed validation keeps the previous checkpoint;
mutable globals are outside this state rollback contract.

The behavior suites cover [nested and independent budgets](../../test/chatml_execution_budget_test.ml),
[compilation without effects](../../test/chatml_compilation_test.ml), and
[moderator initialization, projection, rollback and cancellation](../../test/moderation/moderator_execution_budget_test.ml).
They exercise actual compiler/interpreter paths offline. They do not establish
a hard process-memory boundary or exactly-once external execution.
