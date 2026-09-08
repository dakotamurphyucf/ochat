# ChatML orchestration in agent hosts

ChatMD declares the prompt/tools and optional moderator script. ChatML handles
events and returns typed tasks through the shared moderation runtime. The daemon
session actor/controller, not a connected TUI, owns semantic work and durable
state. Client detachment therefore does not disable an idle moderator listening
for a completion or timer.

See the [moderator language/runtime reference](../guide/chatml-moderator-runtime.md)
for full builtin signatures, event constructors, task syntax and helper modules.
The older phase documents describe shared or compatibility hosts; their statement
that no generalized job API existed must not be applied to the newer agent host.

## Async work and timers

`Model.call` awaits a registered recipe; `Model.spawn` starts background work and
returns an ID; `Model_job_succeeded(id, recipe, result)` and
`Model_job_failed(id, recipe, message)` events allow continuation-style dispatch.
There is no ChatML `Model.await` builtin. External clients inspect jobs with
`job.get`/`job.list` and subscribe to events. The agent host records jobs and delivery state.
`Process.run` is shell-runtime-backed process access, not unrestricted subprocess
creation. Required permissions, quotas, limits and cancellation still apply.

`Schedule.after_ms` and `Schedule.cancel` are ChatML scheduling primitives. The
agent protocol also exposes durable one-shot schedule create/get/list/cancel.
Schedules specify due time/delay, payload and misfire policy: deliver once
immediately, skip if expired, or fail. They are not a cron-expression service.
Job and schedule methods require message-send scope, and mutations require a
writable attachment. There is no arbitrary external `job.create` wire method.

Global/principal/prompt/workspace/session/kind/nested-depth capacity bounds work.
Jobs are claimed before execution, record terminal state, and release capacity on
terminal/failed-start paths. Reviewers also use durable, non-redeliverable jobs.
Unknown external effects after restart require reconciliation; persisted records
and pending delivery are not serialized OCaml stacks or running child processes.

## Safe points and state

The moderator's state/effects commit transactionally. Canonical history,
effective overlay, safe-point changes, deferred steering and internal wakeups
are separate surfaces. The host drains wakeups while idle and serializes changes
through the actor. UI capabilities are host-provided; a headless host is not
guaranteed to have an interactive approval widget or local TUI callback.

The extensibility-v1 [event ownership and persistence internals](extensibility-foundations.md#actor-and-worker-handoff)
describe the separate queued-event receipt, actor borrow and checkpoint handoff.
That internal path is still being integrated; new model-visible extension tools
remain disabled pending runtime and permission qualification.

Instruction helpers retain compatibility names but construct developer messages:
`Item.system_text`, `Turn.prepend_system`, notice helpers, and
`Item.input_text_message(..., "system", ...)`. `Item.role` reports developer;
compatibility predicates recognize both old system and developer entries. Existing
history/raw `Item.create` values are not rewritten wholesale.

Budget policies and admission limits bound configured work, but are not a hard
dollar billing cap across arbitrary providers/tools. Unattended `ask` behavior
must be intentional; select timeout/fallback/reviewer policy before leaving an
agent disconnected. Cancellation/stop must propagate to owned work rather than
only hiding a loading indicator.

Try the [offline timer tutorial](tutorials/background-agent.md). For a model-spawn
example, the [background E2E scenario](../../test/agent_server_e2e/scenarios/background_scenario.ml)
shows the complete `agent_prompt_v1` recipe payload and completion events against
a deterministic provider, including interruption/cancellation tests.
