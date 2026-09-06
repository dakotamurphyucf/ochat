# ChatMD: agents in text files

ChatMD is where you define an Ochat agent: what it should do, what tools it can
call, and, optionally, how a script coordinates its work. It combines readable
Markdown with a small set of tags. You can keep these files alongside your code,
review changes in version control, and compose reusable pieces into new agents.

You do not need a server to get started. Write a prompt and open it in the local
TUI; hosting it in a daemon is a separate choice later.

## Start with instructions

A minimal prompt can be just a developer message. Save this as `helper.chatmd`:

```xml
<developer>
Help me understand unfamiliar code. Explain the main idea first, then work
through an example. Ask for missing context rather than inventing details.
</developer>
```

With Ochat installed and your provider configured, run:

```sh
chat-tui --no-config --local -file helper.chatmd
```

This prompt has no tools: paste the code you want to discuss. The
[local TUI walkthrough](../agent-server/tutorials/local-tui.md) covers setup;
the [TUI guide](../guide/chat_tui.md) covers editing and navigation.

## Build up the agent as you need it

- **Instructions and context:** use messages and Markdown to describe the task,
  constraints, and information the agent needs.
- **Tools:** add declarations for the capabilities the model may call. Start
  with the [tools introduction](../tools/README.md); declarations are not a reason
  to skip reviewing each tool's access rules.
- **Composition:** import shared declarations or expose another ChatMD prompt
  as a specialist tool. Keep reusable roles in their own files.
- **Orchestration:** add an optional [ChatML script](../chatml/README.md) when
  instructions alone are not enough to express your workflow.

The workspace supplies the `${workspace}` location used by configured tools.
It is not a blanket filesystem sandbox or an automatic access grant. The
language reference explains the other path variables, including `${prompt_dir}`
and `${source_dir}`, and how imports affect resolution.

## Choose how to run it

Use the [local TUI](../agent-server/tutorials/local-tui.md) for interactive work,
the [completion CLI](../cli/chat-completion.md) for a file-backed input/output
workflow, or the [agent server](../agent-server/README.md) when you need
independent session lifetimes and client connections. The prompt remains the
agent definition; the host supplies execution and lifecycle behavior.

## Go deeper

- [ChatMD language reference](../overview/chatmd-language.md): supported tags,
  configuration, imports, embedded content, and path resolution.
- [Examples and walkthroughs](../examples/README.md): starting points to adapt.
- [Tool reference](../overview/tools.md): declarations and built-in behavior.
- [ChatML workflows](../chatml/README.md): programmable orchestration.
- [Documentation home](../README.md).
