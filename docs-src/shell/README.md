# Shell access and permissions

Give an agent the command-line capabilities its work requires. Define reusable
shell runtimes that control commands, files, environment, network access,
approvals and resource limits. Add ChatML logic or reviewer agents when decisions
depend on project-specific context.

A repository assistant might inspect source with one runtime, run builds with
another, and send unusual requests through a custom reviewer. The model sees
useful tools; the author defines the capabilities and guardrails behind them.
The host then admits and enforces the supported configuration.

## Start with a complete example

Follow the [shell-agent walkthrough](../agent-server/tutorials/shell-agent.md).
It reads Lantern's actual setup tutorial through a fixed command and a reusable
read-only runtime. Continue with [separate checker capabilities and approved report writes](../tutorials/shell-guardrails.md)
to run real checks against that project, then [customize review decisions](../tutorials/shell-customization.md)
with ChatML state and a separate model-review variant. These lessons include complete source
bundles you can inspect in the browser. The [declaration examples](../guide/chatmd-shell-examples.md)
cover more patterns, but are not all standalone prompts or universal policies.

Local and daemon hosts do not use interchangeable authorization switches.
Read [host modes and authorization](../guide/chatmd-shell-host-integration.md)
before copying a command from one execution mode to another.

## How the pieces fit together

| Piece | Question it answers | Guide |
|---|---|---|
| Runtime declaration | Where and under what execution settings do commands run? | [Runtime reference](../overview/chatmd-shell-runtime.md) |
| Tool declaration | What command interface does the agent see? | [Shell tools](../overview/chatmd-shell-tools.md) |
| Input/output schemas | What structured data does a tool accept or return? | [Tool declarations](../overview/chatmd-shell-tools.md) |
| Authority and confinement | What resources and effects are actually permitted? | [Security guide](../guide/chatmd-shell-security.md) |
| Host authorization | How does this local runner or daemon admit the declared access? | [Host integration](../guide/chatmd-shell-host-integration.md) |
| Review and approval | Which requests need a decision, and who or what makes it? | [Extensions and reviewers](../guide/chatmd-shell-extensions.md) |
| Durable records | What is retained, audited, or interrupted across restarts? | [Persistence and audit](../guide/chatmd-shell-persistence-and-audit.md) |

A schema describes data, not permission. A tool declaration exposes an operation;
its named `<shell_access>` runtime supplies the command capability and policy.
The configured backend determines which boundaries the operating system can
enforce. Review all three when adapting a shell example.

## Design access around the work

| Operation | Configuration to consider | Why it matters |
| --- | --- | --- |
| Inspect repository state | Fixed arguments, required read locations, no unnecessary write capability | The tool exposes a useful bounded operation. |
| Run a build or test | Explicit targets, output locations, environment, network and child-process needs | Build scripts execute project code and can have effects beyond the initial command. |
| Produce generated documentation | Declared inputs and writable output directory | The result can be checked without granting unrelated file modification. |
| Use a project-specific command | Structured arguments, appropriate policy and review hook | The interface and decision rules stay understandable as the capability grows. |

One agent can expose tools backed by different runtimes. This makes an inspection
capability distinct from a build capability instead of giving every shell tool
the same broad configuration. Versioned built-in profiles are starting
declarations; they are not evidence that every requested operation is authorized
or that a backend is installed.

## Customize decisions with scripts and agents

Build [custom shell decisions](../tutorials/shell-customization.md), then use the
[extension contracts](../guide/chatmd-shell-extensions.md) to adapt the pattern:

- A **matcher** recognizes a command/effect pattern for policy selection.
- A **ChatML reviewer** applies a deterministic project-specific decision.
- A **reviewer agent** adds contextual judgment through a configured, bounded
  request and structured response.
- A **before interceptor** can handle or rewrite a request within the contract;
  an **after interceptor** controls supported result transformation/disclosure.
- **Effect analysis and audit filtering** support the corresponding capability
  and audit decisions.

These extension kinds have different inputs and return contracts. Shell ChatML
hooks receive a narrow normalized context; do not assume they can call arbitrary
general tools or make model/network requests. A model reviewer is a separately
configured model request. The stock adapter uses a fixed tool-free reviewer
prompt; its `agent` field is an identity label, not a named ChatMD file lookup.

Custom decisions remain within the admitted authority. A hook cannot turn a
hard denial into broader permission. Rewritten requests go through the applicable
resolution, policy and verification checks again.

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
