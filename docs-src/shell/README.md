# Shell access and permissions

Shell access lets an agent use command-line programs: inspect a repository,
run a test, or interact with a configured process. It is powerful enough to
deserve an explicit design, not just an instruction telling the model to be
careful.

Ochat's shell declarations describe runtimes and tools in ChatMD. The host
decides which authority it will admit, and approval policy determines when an
operation needs another decision. Begin with a narrow command and expand only
when the task requires more access.

## Start with a complete example

Follow the [shell-agent walkthrough](../agent-server/tutorials/shell-agent.md).
It uses a limited `pwd` tool and explains how to run it with the appropriate
authorization. The [declaration examples](../guide/chatmd-shell-examples.md)
cover more patterns, but are not all standalone prompts or universal policies.

Local and daemon hosts do not use interchangeable authorization switches.
Read [host modes and authorization](../guide/chatmd-shell-host-integration.md)
before copying a command from one execution mode to another.

## How the pieces fit together

| Piece | Question it answers | Guide |
|---|---|---|
| Runtime declaration | Where and under what execution settings do commands run? | [Runtime reference](../overview/chatmd-shell-runtime.md) |
| Tool declaration | What command interface does the agent see? | [Shell tools](../overview/chatmd-shell-tools.md) |
| Authority and confinement | What resources and effects are actually permitted? | [Security guide](../guide/chatmd-shell-security.md) |
| Host authorization | How does this local runner or daemon admit the declared access? | [Host integration](../guide/chatmd-shell-host-integration.md) |
| Review and approval | Which requests need a decision, and who or what makes it? | [Extensions and reviewers](../guide/chatmd-shell-extensions.md) |
| Durable records | What is retained, audited, or interrupted across restarts? | [Persistence and audit](../guide/chatmd-shell-persistence-and-audit.md) |

The workspace supplies a location, not a security boundary by itself. Review
the declared filesystem and network access, the actual confinement backend,
and approval policy together. Unattended agents need a deliberate noninteractive
policy; removing prompts for human approval does not reduce the consequences
of the commands they can run.

## Operate and extend

- [Management CLI](../cli/shell-runtime-management.md): inspect and manage shell
  authorization and related state.
- [TUI guide](../guide/chat_tui.md): security views, approvals, and navigation.
- [Server permission profiles](../agent-server/permissions-and-security.md):
  session and client permissions alongside tool policy.
- [Child process setup](../lib/shell_access/process_spawn.doc.md): optional OS
  limits and descriptor isolation.
- [Runtime internals](../guide/chatmd-shell-runtime-internals.md): implementation
  details for contributors.

Return to [tools](../tools/README.md) or the [documentation home](../README.md).
