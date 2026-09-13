# Tools and integrations

Tools let an agent act on the world outside the conversation: read a file,
propose an edit, search documentation, ask a specialist, or use an external
service. Declare the tools in the agent's ChatMD file so its capabilities are
visible alongside its instructions.

Start with the smallest useful set. A repository explainer may only need file
reads; a coding agent may also need patching and carefully configured commands.

## Choose a kind of tool

| Kind | Useful for | Reference |
|---|---|---|
| Built-in tools | Files, patches, web ingestion, search, and other packaged capabilities | [Available built-ins](../overview/tools.md#built-in-catalog-code-correct) |
| Agent tools | Delegate a focused task to another ChatMD prompt | [Tool declarations and agent composition](../overview/tools.md) |
| Shell tools | Run configured commands or interactive processes | [Shell access](../shell/README.md) |
| Standalone ChatML tools | Reuse deterministic logic and sequence selected existing tools | [Script-tool contracts](../guide/chatml-authoring-runtime.md#standalone-tools-and-explicit-outcomes) |
| Moderator-handled tools | Implement a custom operation that uses retained workflow state | [Stateful tool contracts](../guide/chatml-authoring-runtime.md#moderator-tools-and-session-owned-state) |
| MCP tools | Connect to tool catalogs served by external MCP servers | [MCP selection, authentication, and discovery](../overview/tools.md) |

Agent tools can make one-off requests or keep specialist conversations. A parent
can also create a task-specific child from a generated definition. The
[subagent decision guide](../guide/subagents.md) explains those choices and the
[inherited-tool boundary](../guide/delegated-tools.md).

For command-line integrations, design the shell tool and its runtime together:
the model-facing operation can be convenient while the runtime specifies custom
capabilities and guardrails. For script tools, choose whether state belongs to
one invocation or to the conversation; [ChatML execution forms](../chatml/README.md)
explain that distinction.

MCP tool integration is maintained. It is distinct from Ochat's deprecated MCP
prompt-serving host, and from the new Ochat agent-server protocol.

## A small read-oriented tool set

Add these declarations to a ChatMD prompt with your own instructions:

```xml
<tool name="read_dir"/>
<tool name="read_file">
  <read id="project" path="${workspace}" description="Project files"/>
</tool>
```

The second declaration configures the file reader's root explicitly. Do not
assume that this root also confines other tools: each tool has its own behavior.
The catalog also distinguishes declaration names from model-visible names—for
example, `read_dir` exposes `read_directory`.

Before adding a writing or command tool, read its access and execution semantics.
Tool availability, filesystem authority, and approval policy are different
questions. A confirmation dialog does not itself create a sandbox, and a
configured workspace does not automatically restrict every tool to that folder.

## Learn by task

- [Full tool reference](../overview/tools.md): built-in catalog, read roots,
  agent tools, MCP connections, and extension points.
- [Search and indexing](../guide/search-and-indexing.md): prepare searchable
  documentation and connect the corresponding tools.
- [Agent workflow example](../guide/general-agent-workflow.md): a larger composed
  assistant; review its tools and paths before adapting it.
- [Shell walkthrough](../agent-server/tutorials/shell-agent.md): a narrow command
  example with host-specific setup.
- [Server permissions](../agent-server/permissions-and-security.md): configure
  automated or interactive decisions for hosted agents.
- [Custom OCaml tools](../lib/gpt_function.doc.md): typed results, progress and
  nested traces, with a compiled offline example.
- [ChatMD introduction](../chatmd/README.md) and [documentation home](../README.md).
