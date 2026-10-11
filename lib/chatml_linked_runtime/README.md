# Executing a linked program

`Chatml_linked_program` contains checked captures and immutable resolved code.
`Chatml_linked_runtime` initializes that code with an explicitly authorized
runtime surface. It retains an execution-owned entry environment and selected
binding values, rather than retaining the prepared inventory or compiler engine.

Only host entrypoint names selected during compilation may be invoked. A
compile-only artifact with no selected requirements cannot start execution.
Before any initializer, the runtime compares the caller's actual static builtin
metadata with the linked contract: global schemes, module exports and type-alias
bodies are included. This check cannot establish that a native implementation
honors its declared scheme or grant operation authority. The host supplies and
authorizes implementations and operations separately.

Prepared host invocation also validates roles before initializers. Registered
one-off/tool contracts use their fixed `main`/`run` role; moderator contracts use
the fixed `initial_state`/`on_event` session roles. Native contracts may select
custom names, with an explicit fixed-arity task-function requirement. A Native
session requires three on_event arguments and the same static state encoding in
its initial-state requirement, event state input and task result. Shared type
variable names are part of that grouped contract. This preflight checks declared
host metadata; it performs no runtime inference or source alias solving. Generic
initialization through this library still permits selected value contracts.

Initialization uses the dependency-first order of the entry's value-reference
closure. Type references and configured `open` statements do not initialize a
module by themselves. A value reference inside an uncalled function does select
its dependency, as does a compiled host binding imported through an `open`.
Available but unused catalog entries have no runtime effect.

For example, if `Left` and `Right` both call `Counter.next()`, they receive the
same completed public `Counter` instance. If the entry also opens a configured
alias of `Counter`, `next()` reaches that same instance. A private `ref(0)` inside
Counter advances across all three calls. A second standalone run starts with a
fresh counter; events in one persistent session keep the existing counter.

Each unit receives fresh builtin tables, its captured source and completed maps
containing its direct value dependencies. It receives no importer-local values.
The initializer evaluates a body once and applies checked visibility before
publishing its canonical public module. Closure environments retain private
values; aliases share the public module and its mutable values. A companion
restricts external exports while compiled local host entrypoints use the entry's
private lexical environment.

Failure stops later initializers and returns no partial runtime instance. Native
effects which have already happened are not rolled back. A native callback which
deliberately retains a value owns that escaped value. Deferred failures retain
the defining unit's source identity, digest and byte range.

`Chatml_host_runtime.run_prepared_entrypoint_detailed` shares the existing task
interpreter and operation/transaction behavior. The prepared session constructor
returns the existing session type, so event and UI-continuation APIs reuse the
initialized graph. `Chatml_execution.run_prepared` wraps installation,
initialization, invocation and result conversion in one execution scope. Units
do not open fresh fuel, allocation or invocation-depth budgets.

A persistent owner creates an execution runner, passes `runner_control` while
initializing inside `run_scoped`, then enters `run_scoped` for every event. All
dependency closures use that same proxy and consult the event's current scope.
Using them outside the scope fails. The owner must serialize mutable session
access; runtime instances are not domain-transfer artifacts.

Legacy text/single-file compiler and execution APIs retain their original
surface compatibility and arbitrary entrypoint-name behavior. Capturing current
FILE bytes, restoring exact bundles and routing artifact variants belong to
the host custody/composition adapters, not this runtime initializer.
