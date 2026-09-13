# ChatML: programmable agent workflows

ChatMD describes an agent. ChatML adds program logic to its workflow: respond
to lifecycle events, decide when another turn should run, coordinate tools and
model calls, or arrange work that completes asynchronously.

You do not need ChatML for an ordinary question-and-answer agent. Start with
instructions and tools; add a script when the workflow needs explicit decisions
that should not depend only on the model following prose instructions.

## What belongs in a script?

A documentation workflow can read a tutorial inventory, run the supported checks,
send failures to specialists, and assemble their findings. A conversational
assistant can acknowledge a slow check immediately and return to the user when
the result is ready. ChatML makes those coordination rules explicit.

Use deterministic code for calculations, result transformation and repeatable
decisions. Let models supply judgment where it helps: explaining an ambiguous
failure or reviewing a proposed change. Tools connect both to the operations
the author and host have made available.

## Understand the two layers

Choose the smallest execution form that fits the work:

| Need | Use |
|---|---|
| Let an agent write deterministic logic over its existing tools | [One-off report program](../tutorials/chatml-program.md) |
| Expose a reusable script as a tool | [Reusable check-summary tool](../tutorials/chatml-tool.md) |
| React to conversation events and control progression | [Three-turn conversation moderator](../tutorials/workflow.md) |
| Implement a tool that depends on conversation state | [Stateful review ledger](../tutorials/stateful-workflow.md) |
| Return promptly and notify the agent when work finishes | [Background documentation checks](../tutorials/background-results.md) |

A continuing specialist is another **agent session**, not another script form.
Use the [delegation guide](../guide/subagents.md) to choose its lifetime, then
coordinate it through the tools available to the script.

Standalone tools get fresh script globals for each invocation. For a custom
tool that must remember workflow state, use a moderator-handled tool: its
`Tool_invoked` event is handled by the conversation's moderator, which resolves
the specific invocation. Declaring a tool alone does not implement its handler.

An agent can learn these contracts from the installed
[authoring documentation tool](../guide/authoring-context-tool.md), then validate
its candidate without executing it. The
[complete source bundles](../examples/README.md)
include runnable learning projects with scripts, schemas and sample inputs.
The [runtime contract reference](../guide/chatml-authoring-runtime.md) and
[background API reference](../guide/chatml-authoring-background.md) explain exact
behavior once you have tried the corresponding lesson.

- **Language:** values, functions, pattern matching, task syntax, and parsing
  rules are defined by the [ChatML language specification](../guide/chatml-language-spec.md).
- **Agent runtime:** entrypoints, events, context, and capabilities such as
  `Turn`, `Tool`, `Model`, `Process`, and `Schedule` are explained in the
  [moderator runtime guide](../guide/chatml-moderator-runtime.md).

The term *moderator* here means workflow logic participating in the agent loop;
it does not necessarily mean a human approval step. Some capabilities require
a suitable UI or host. Check host support before moving a script between modes.

## Who decides what happens next?

| Participant | Responsibility |
| --- | --- |
| Model | Interpret the task, request available tools, assess evidence and explain results. |
| Script | Apply deterministic rules, sequence selected tools, retain supported state and request actions. |
| Runtime | Execute admitted tasks, deliver supported events, check permissions and manage work/session lifecycles. |

ChatML task operations describe work for the runtime to execute. `let*` makes
dependent operations read in sequence without nesting task expressions. Returning
a task is different from a script having unrestricted access to the host.

In a **conversational coordinator**, the user asks a question, the model requests
a check, and the moderator records its state and arranges the next step. A later
result can become a new input for the model to explain.

In an **unattended coordinator**, the initial input/configuration starts a
workflow and the moderator may perform all coordination through tools and child
agents. It need not call its own model. This is an agent workflow whose script
does the coordination, not a separate ChatMD file type. It still depends on the
actual declared tools and supported host services.

For example, a report task can sequence file reads as a one-off program. A named
report tool can reuse that logic. A moderator can retain several report jobs and
notify the agent as results arrive. Start with the form that owns the state and
lifetime you actually need.

## Background work and persistence

An initial tool acknowledgement, eventual work completion and a notification
that requests another model turn are separate events. A pending result should
identify the work being tracked; a notification should connect the later evidence
to that request. Cancellation and unsuccessful work also need explicit handling.

For child responses, the current
[living documentation lab](../applications/documentation-lab.md)
uses timers and lifecycle polling. It does not introduce a native push
subscription for arbitrary child conversations.

For work that must continue after you close a client, use an appropriately
configured daemon session. A script running in a process-bound local host does
not keep executing after that host exits.

Durable recovery preserves supported state and classifies interrupted work; it
does not save and resume an arbitrary executing continuation or guarantee that
an external effect happens exactly once. The
[server orchestration guide](../agent-server/chatml-orchestration.md) explains
the persistence boundary, jobs, timers, and restart behavior.

## Read and try

See the scripts combined in complete applications: the
[engineering assistant](../applications/guarded-engineering.md) uses custom shell
decisions and report processing; the [review team](../applications/persistent-review-team.md)
collects specialist evidence; the [documentation lab](../applications/documentation-lab.md)
owns background checks, stateful tools, response watchers and staged rechecks.

1. Start with the [report program](../tutorials/chatml-program.md) and
   [reusable tool](../tutorials/chatml-tool.md) to sequence useful file operations.
2. Follow the [moderator lesson](../tutorials/workflow.md), then the
   [stateful review ledger](../tutorials/stateful-workflow.md) and
   [background check workflow](../tutorials/background-results.md).
3. Use the [runtime guide](../guide/chatml-moderator-runtime.md) to choose events
   and capabilities for your own script.
4. Consult the [language reference](../guide/chatml-language-spec.md) and
   [parsing and diagnostics guide](../guide/chatml-parsing-and-diagnostics.md)
   while writing it.
5. For library work, read the
   [implementation architecture](../guide/chatml-implementation-architecture.md).

For precise host behavior, see the [session-controller contract](../chatml-host-session-controller-contract.md),
[safe points and effective history](../chatml-safe-point-and-effective-history.md),
[budget policy](../chatml-budget-policy.md),
[async completion lifecycle](../chatml-async-completion-lifecycle.md), and
[UI capabilities](../chatml-ui-host-capabilities.md). These documents distinguish
shared/legacy controller behavior from the new agent host.

Language implementers can continue to [match semantics](../guide/chatml-match-semantics.md),
the [interpreter](../lib/chatml/chatml_lang.doc.md),
[parser](../lib/chatml/chatml_parser.doc.md), and
[resolver](../lib/chatml/chatml_resolver.doc.md). The
[`dsl_script` demo](../bin/dsl_script.doc.md) runs a built-in example; it is not
a replacement for launching a ChatMD agent.

Return to [ChatMD](../chatmd/README.md), [examples](../examples/README.md), or
the [documentation home](../README.md).
