# ChatML: programmable agent workflows

ChatMD describes an agent. ChatML adds program logic to its workflow: respond
to lifecycle events, decide when another turn should run, coordinate tools and
model calls, or arrange work that completes asynchronously.

You do not need ChatML for an ordinary question-and-answer agent. Start with
instructions and tools; add a script when the workflow needs explicit decisions
that should not depend only on the model following prose instructions.

## What belongs in a script?

Examples include stopping after a chosen number of turns, requesting a review
at a particular stage, reacting to a completed background job, or scheduling
a later event that starts more work. A script can maintain workflow state and
use the capabilities its host makes available.

The script lives with the ChatMD definition through its script declaration.
The host delivers events and executes requested effects. This keeps the workflow
definition separate from whether you interact through a TUI or a daemon client.

## Understand the two layers

- **Language:** values, functions, pattern matching, task syntax, and parsing
  rules are defined by the [ChatML language specification](../guide/chatml-language-spec.md).
- **Agent runtime:** entrypoints, events, context, and capabilities such as
  `Turn`, `Tool`, `Model`, `Process`, and `Schedule` are explained in the
  [moderator runtime guide](../guide/chatml-moderator-runtime.md).

The term *moderator* here means workflow logic participating in the agent loop;
it does not necessarily mean a human approval step. Some capabilities require
a suitable UI or host. Check host support before moving a script between modes.

## Background work and persistence

For work that must continue after you close a client, use an appropriately
configured daemon session. A script running in a process-bound local host does
not keep executing after that host exits.

Durable recovery preserves supported state and classifies interrupted work; it
does not save and resume an arbitrary executing continuation or guarantee that
an external effect happens exactly once. The
[server orchestration guide](../agent-server/chatml-orchestration.md) explains
the persistence boundary, jobs, timers, and restart behavior.

## Read and try

1. Follow the [background agent tutorial](../agent-server/tutorials/background-agent.md)
   for a concrete timer-driven workflow and its host setup.
2. Use the [runtime guide](../guide/chatml-moderator-runtime.md) to choose events
   and capabilities for your own script.
3. Consult the [language reference](../guide/chatml-language-spec.md) and
   [parsing and diagnostics guide](../guide/chatml-parsing-and-diagnostics.md)
   while writing it.
4. For library work, read the
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
