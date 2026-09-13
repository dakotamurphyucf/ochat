# Ochat documentation

Ochat defines agents in text files: ChatMD holds instructions and tools, with
optional ChatML scripts for orchestration. Start by opening a prompt in the
local TUI. The same agent core also supports headless workflows and a durable,
multi-client daemon when you need one.

## Start here

Follow [installation](agent-server/quickstart.md), use
[build troubleshooting](guide/build-troubleshooting.md) when needed, then
[run your first local agent](agent-server/tutorials/local-tui.md).
No daemon is required for a first conversation. The
[learning paths](tutorials/README.md) build from instructions and file tools to
specialists, shell capabilities and programmable workflows.

Read [how Ochat's pieces fit together](concepts/how-ochat-works.md) for the
relationship between a ChatMD definition, the model, tools, scripts and sessions.

## Explore by topic

Choose the capability your next workflow needs. Each introduction explains its
purpose, the available patterns, and where to find exact contracts and examples.

| Topic | What you will find |
|---|---|
| [ChatMD: agents in text files](chatmd/README.md) | A first prompt, composition, execution choices, and the language reference. |
| [Examples and walkthroughs](examples/README.md) | Local agents, tool patterns, workflows, and complete hosting tutorials. |
| [Tools and integrations](tools/README.md) | Ochat's built-in capabilities, specialist agents, shell tools, and maintained MCP integrations. |
| [ChatML workflows](chatml/README.md) | Scripted decisions, runtime events, asynchronous work, and language documentation. |
| [Subagents and agent teams](guide/subagents.md) | One-off, persistent and dynamically generated specialists, shared lifecycle tools, and follow-up conversations. |
| [Shell access](shell/README.md) | Custom command capabilities and guardrails, reusable runtimes, ChatML hooks and reviewer agents. |
| [Agent server](agent-server/README.md) | Sessions, workspaces, daemon connections, Unix/stdio/HTTP, and operations. |
| [TUI guide](guide/chat_tui.md) | Interactive editing, tool inspection, approvals, and terminal controls. |
| [Search and indexing](guide/search-and-indexing.md) | Preparing code and documentation for retrieval. |
| [Commands and executables](bin/README.md) | TUI, completion, hosting, search, refinement, and helper command references. |
| [Project overview and direction](overview/project.md) | Design principles, architecture, repository layout, OCaml integration, and roadmap. |

For a specific declaration, go directly to the
[ChatMD language reference](overview/chatmd-language.md) or
[built-in tool catalog](overview/tools.md#built-in-catalog-code-correct).

## Author prompts and tools

A useful agent starts with a small tool set. A larger workflow can combine a
file reader, a configured test command, a specialist review and a script that
collects evidence. Choose the pieces deliberately:

- [Choose a delegation pattern](guide/subagents.md): decide whether a specialist
  needs one answer, a retained conversation or a role generated for the task.
- [Share tools and narrow authority](guide/delegated-tools.md): give children
  existing capabilities without expanding the parent's restrictions.
- [Choose a ChatML execution form](chatml/README.md): distinguish a one-off
  program, reusable tool, conversation moderator and stateful custom tool.

For exact syntax and behavior:

- [ChatMD language](overview/chatmd-language.md)
- [Built-in tools, file roots, and maintained MCP tools](overview/tools.md)
- [ChatML moderator runtime](guide/chatml-moderator-runtime.md)
- [ChatML language](guide/chatml-language-spec.md)
- [Build script tools and moderator handlers](guide/chatml-authoring-runtime.md)
- [Create persisted child agents](guide/chatml-authoring-children.md)
- [Compose background work and notifications](guide/chatml-authoring-background.md)
- [Give agents installed authoring documentation](guide/authoring-context-tool.md)
- [ChatML execution limits, cancellation and state](guide/chatml-execution-limits.md)
- [Session lifecycle and workflow recovery](guide/chatml-session-lifecycle.md)
- [Background agent tutorial](agent-server/tutorials/background-agent.md)
- [Interactive TUI keys and views](guide/chat_tui.md)
- [File-backed completion CLI](cli/chat-completion.md)

## Shell access

Give inspection and build tools different runtime configurations. Select their
commands, file access, environment, network behavior, limits and approval policy.
Use a ChatML hook for a deterministic project rule or a reviewer agent when the
decision benefits from contextual judgment. The
[shell introduction](shell/README.md) explains how these pieces work together.

- [Host modes and authorization](guide/chatmd-shell-host-integration.md)
- [Runtime declaration reference](overview/chatmd-shell-runtime.md)
- [Shell tool declarations](overview/chatmd-shell-tools.md)
- [Security and confinement](guide/chatmd-shell-security.md)
- [Extensions and reviewers](guide/chatmd-shell-extensions.md)
- [Persistence and audit](guide/chatmd-shell-persistence-and-audit.md)
- [Examples](guide/chatmd-shell-examples.md)
- [Management CLI](cli/shell-runtime-management.md)
- [Child process setup](lib/shell_access/process_spawn.doc.md)
- [Complete shell-agent walkthrough](agent-server/tutorials/shell-agent.md)

## Build a complete application

Combine the individual capabilities in a [guarded engineering assistant](applications/guarded-engineering.md),
a [persistent review team](applications/persistent-review-team.md), or a
[living documentation lab](applications/documentation-lab.md). Each project has
complete inspectable source, precise setup, evidence of its intermediate work and
an explanation of what happens when work fails or is cancelled.

## Operate and develop

- [Operations, backup, recovery, and migration](agent-server/operations.md)
- [Troubleshooting](agent-server/troubleshooting.md)
- [Tests and evidence](agent-server/testing.md)
- [OCaml embedding](agent-server/embedding.md)
- [Library documentation](lib/README.md)
- [Architecture specification](design/ochat-agent-server-spec.md)
- [Implementation specification](design/ochat-agent-server-implementation-spec.md)
- [Documentation coverage ledger](development/documentation-coverage.md)
- [README content and navigation audit](development/readme-content-audit.md)
- [Code-to-documentation audit and known implementation gaps](development/code-documentation-audit.md)
- [Documentation verification worklog](development/documentation-worklog.md)
- [Search and indexing](guide/search-and-indexing.md)
- [Meta-prompting](lib/meta_prompting.doc.md)

These are repository Markdown documents, readable directly in a Markdown viewer.
Generated odoc documentation comes from OCaml interfaces; Markdown sidecars are
not automatically included by odoc. See [development instructions](../DEVELOPMENT.md)
for the separate API-documentation and search-index workflows.

The [old MCP prompt server](bin/mcp_server.doc.md) is compatibility functionality,
not the agent-server transport. MCP-backed tools declared in ChatMD remain maintained.
