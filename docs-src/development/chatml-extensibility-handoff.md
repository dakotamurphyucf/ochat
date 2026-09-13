# ChatML extensibility: implementation and maintenance

Ochat supports deterministic tool-using scripts, stateful script-defined tools,
owned background work and independently addressable child conversations. The
default daemon and embedded runtimes share the session implementation. A ChatMD
author selects the tools and workflow contract; declarations and retrieved
documentation do not grant authority beyond the host's admitted capabilities.

## Choose an execution form

| Form | Entry point and lifetime | Guide |
|---|---|---|
| One-off computation | `run_chatml` compiles `main(input)` and runs its returned task with selected existing tools. | [Execution contracts](../guide/chatml-authoring-runtime.md) |
| Standalone tool | A `kind="tool"` script exports `run(ctx, input)`; each invocation receives fresh script globals. No moderator is required. | [Standalone tools](../guide/chatml-authoring-runtime.md#standalone-tools-and-explicit-outcomes) |
| Stateful custom tool | An extensibility-v1 moderator receives `Tool_invoked` and executes `Invocation.resolve`; state and the initial outcome commit together. | [Moderator tools](../guide/chatml-authoring-runtime.md#moderator-tools-and-session-owned-state) |
| Continuing specialist | `agent_create` returns a persisted session ID; send/read/status/wait/stop use that scoped identity and retained receipts. | [Child sessions](../guide/chatml-authoring-children.md) |
| Authored specialist | A named agent tool can declare optional or fixed persistence; explicit session IDs select an existing instance. | [Authored persistence](../agent-server/extensibility-foundations.md#authored-agent-tool-persistence-contract) |

Temporary built-in `fork` conversations retain native, standalone and moderator
tools, including recursive forks. They inherit parent moderation and permission
checks through an expiring invocation borrow. Their transcripts remain separate
from the root history. They are distinct from persisted child sessions and do not
acquire the latter's lifecycle API merely by returning a fork result.

## Ownership and effects

The session actor owns durable invocation, job, subscription, timer, delivery and
child-management records. Execution borrows are scoped to the actual caller;
they are not ambient authority attached to a tool name. Native, script, child and
configured-helper calls revalidate current bindings and permissions, including
after a yielding approval wait. Generated descendants retain the parent's file,
shell and tool restrictions while choosing their own supported model, reasoning
settings and instructions. Private authored dependencies stay private.

The moderator's handler returns a task, and `let*` sequences deferred effects.
An initial `Complete`, `Pending` or failure outcome is separate from later work
completion. Pending results identify owned work; later notification is framed
conversation data, and requesting a new turn is an independent policy decision.
Local state/receipt commits cannot undo an external shell, file or network effect.
Restart retains interruption or uncertainty rather than blindly repeating it.

`Job`, `Schedule`, `Subscription`, `Notification` and `Ingress` provide the
[background composition primitives](../guide/chatml-authoring-background.md).
The complete [example bundles](../../test/chatml_extensibility_fixtures/README.md)
include a response watcher written with timers and polling. Its optional confined
shell-helper version uses the same scoped services without requiring native
lifecycle tools. There is no native cross-daemon orchestration or child-response
push subscription in this implementation.

## Hosts and compatibility

| Host | Supported lifetime and boundary |
|---|---|
| Unix/HTTP daemon | Client-independent work, durable records and restart reconciliation. Child creation requires an admitted durable service. |
| Local embedded/stdio | The shared script/moderator services run while the host process lives. A durable data directory does not keep execution alive after exit. Transient child creation fails explicitly. |
| Stdio gateway | The connected daemon owns work; gateway EOF detaches the client. |
| Legacy batch/file-backed host | Existing contracts remain supported. New declarations that require absent services fail rather than silently losing ownership or delivery. |

Legacy moderator event contracts remain separate from `api="extensibility-v1"`;
adding new events does not force old exhaustive matches to change. Independent
child lifetime requires an explicit host grant. Original-owner stateful dependencies
or legacy bindings that cannot be safely delegated reject during admission.
An allowed unrestricted shell is not converted into an OS sandbox by delegation.

Compilation runs through the native compiler on Eio workers. Cancellation is
cooperative, including inference checkpoints and joined cleanup; it is not hard
preemption or subprocess isolation. Trusted hosts can select unrestricted language
execution without expanding tool authority. Protocol/storage limits remain
separate. Optional process resource limits and private-channel cleanup are linked
into the host's [child process setup](../lib/shell_access/process_spawn.doc.md);
Ochat does not self-execute or require a resource-runner executable for this setup.
The external session helper is an optional selected shell integration.

## Authoring guidance and validation

The [authoring context tool](../guide/authoring-context-tool.md) provides one strict
request schema with search, topic, prepare and continuation operations. Automatic
policy supplies an initial primer for selected authoring capabilities; manual and
preloaded policies give the ChatMD author explicit control. Ordinary agents receive
no authoring additions. Prepared packages include semantic prerequisites and
reflect installed version, execution surface, selected tools and host limitations.

Validation compiles candidates and checks static contracts without executing
initializers, creating children or calling tools. It cannot prove future dynamic
behavior. Paged reference receipts survive compaction without retaining all prose;
fresh pointers and queries restore compatible guidance when needed. Contributor
instructions for shared sources, signatures and the six reviewed coverage
inventories are in [DEVELOPMENT.md](../../DEVELOPMENT.md#maintaining-the-installed-authoring-reference).

## Qualification and upgrades

Normal tests cover compiler, actor, authority, persistence, composition and
authoring behavior. The required framework gate additionally runs PR-safe process
E2E. Wider runtime, permission, security, TUI and crash aliases exercise affected
host integration. [Testing](../agent-server/testing.md) describes the tiers;
the [source bundle README](../../test/chatml_extensibility_fixtures/README.md)
maps the eleven complete composition examples.

```sh
dune runtest --force
dune build @agent-docs-check @check @install
dune build --force @agent-e2e-pr
dune build @test/chatml_extensibility_fixtures/bundle
```

Qualification uses fake providers, controlled clocks and real local processes,
storage and crash injection where required. The authoring evaluation harness
checks eight tasks under three documentation conditions: 24 deterministic cases.
Its estimates and repair scores establish harness behavior, not model quality,
provider billing or actual model token usage. Optional live evaluation requires
separate authorization; see its [measurement contract](../../test/authoring_evaluation/README.md).

Before upgrading, follow [operations and migration](../agent-server/operations.md#inspection-migration-and-legacy-import).
Keep a matching binary and complete stopped-store backup for rollback. Store
container and session-state schemas are separate; do not edit versions to force
compatibility. Captured prompt sources stay pinned until explicit administration,
and changing a source file is not an application-state migration. Inspect
interrupted effects and refresh process-bound output cursors after restart.

Use the protected pull-request workflow for integration. Local tests, a draft PR
and a passing remote gate are distinct evidence; none alone means the branch was
merged or the website deployed. Keep broader platform, performance and optional
live-provider results explicit rather than inferring them from offline checks.
