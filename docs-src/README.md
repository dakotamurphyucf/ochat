# Ochat documentation

Ochat defines agents in text files: ChatMD holds instructions and tools, with
optional ChatML scripts for orchestration. Start by opening a prompt in the
local TUI. The same agent core also supports headless workflows and a durable,
multi-client daemon when you need one.

## Start here

- [Install and choose a host](agent-server/quickstart.md)
- [Run the TUI locally](agent-server/tutorials/local-tui.md)
- [Run a daemon and connect the TUI](agent-server/tutorials/unix-daemon.md)
- [Integrate a stdio client](agent-server/tutorials/stdio-client.md)
- [Integrate an HTTP client](agent-server/tutorials/http-client.md)
- [Agent-server reference index](agent-server/README.md)

## Explore by topic

These introductions explain the main ideas, suggest a starting path, and link
to the detailed guides and references.

| Topic | What you will find |
|---|---|
| [ChatMD: agents in text files](chatmd/README.md) | A first prompt, composition, execution choices, and the language reference. |
| [Examples and walkthroughs](examples/README.md) | Local agents, tool patterns, workflows, and complete hosting tutorials. |
| [Tools and integrations](tools/README.md) | Ochat's built-in capabilities, specialist agents, shell tools, and maintained MCP integrations. |
| [ChatML workflows](chatml/README.md) | Scripted decisions, runtime events, asynchronous work, and language documentation. |
| [Shell access](shell/README.md) | Command capabilities, host authorization, confinement, approvals, and audit. |
| [Agent server](agent-server/README.md) | Sessions, workspaces, daemon connections, Unix/stdio/HTTP, and operations. |
| [TUI guide](guide/chat_tui.md) | Interactive editing, tool inspection, approvals, and terminal controls. |
| [Search and indexing](guide/search-and-indexing.md) | Preparing code and documentation for retrieval. |
| [Commands and executables](bin/README.md) | TUI, completion, hosting, search, refinement, and helper command references. |
| [Project overview and direction](overview/project.md) | Design principles, architecture, repository layout, OCaml integration, and roadmap. |

For a specific declaration, go directly to the
[ChatMD language reference](overview/chatmd-language.md) or
[built-in tool catalog](overview/tools.md#built-in-catalog-code-correct).

## Author prompts and tools

- [ChatMD language](overview/chatmd-language.md)
- [Built-in tools, file roots, and maintained MCP tools](overview/tools.md)
- [ChatML moderator runtime](guide/chatml-moderator-runtime.md)
- [ChatML language](guide/chatml-language-spec.md)
- [Background agent tutorial](agent-server/tutorials/background-agent.md)
- [Interactive TUI keys and views](guide/chat_tui.md)
- [File-backed completion CLI](cli/chat-completion.md)

## Shell access

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
