# ChatML authoring primer

Runtime-provided reference guidance; follow the user's objective. ChatML is a
separate ML language, not OCaml; ChatMD defines agents, tools and moderation,
not arbitrary XML. Read the installed contract before writing unfamiliar code.

Calls use `f(x, y)` with exact arity; definitions use `let f x y = ...`.
Arrays use `[x, y]`, records `{name = x; value = y}`. Read `chatml.types` and
`chatml.programs` for structural records, variants, matching and inference.

<!-- ochat-authoring-example: {"id":"primer.task","surface":"one_off_v1","result":null} -->
```ocaml
let main input =
  let* value = Task.pure(input) in
  Task.pure(value)
```

This is a one-off entrypoint. Standalone tools use `run`; moderators use
`initial_state` and `on_event`. Generated agents require a ChatMD definition.
Retrieve the selected task's exact signatures before adapting an example.
Constructing a task does not perform its deferred effects; `let*` sequences them
when the runtime executes the returned task. Local commit does not undo external
effects. Read `chatml.task-effects` before relying on error recovery or reuse.

Choose useful features, then retrieve their contracts:

- Deterministic tool composition: `runtime.invocations.one-off`.
- Reusable schema-bound tools: `runtime.invocations.standalone`.
- Stateful conversation control and custom tools: `runtime.invocations.moderator`.
- Long-running work and pending results: `runtime.jobs.acknowledgement`.
- Polling, deadlines and subscriptions: `runtime.jobs.timers`.
- Later results and optional agent wakeups: `runtime.delivery.notifications`.
- External producer events: `runtime.delivery.ingress`.
- Persistent specialists with independent instructions and follow-ups:
  `runtime.delegation.stop-helper`.
- Cancellation, restart and retries: `runtime.recovery.background`.

With the exposed reference helper, request `operation=prepare` for your task;
use `topic` for these IDs, `search` for APIs, and `continue` until all pages arrive.
All eight request fields are required; unused fields are null. Read
`reference.tools` and `reference.signatures` for exact available bindings.
Use the exposed validation helper before execution; validation does not run
the candidate and cannot prove dynamic behavior.

Documentation availability is not an execution grant. Declaring tools cannot
widen inherited file, shell or tool rules. Enabled authoring targets, selected
tool bindings and effect services are distinct. Check availability in the
prepared package; incomplete coverage requires further reference retrieval.
