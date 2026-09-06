# Examples and walkthroughs

Start with a local prompt, then add the capabilities your task needs. You do
not need a daemon, shell access, or an orchestration script to try Ochat.

## Your first agent

- [ChatMD introduction](../chatmd/README.md): a minimal prompt you can save and
  open locally.
- [Local TUI walkthrough](../agent-server/tutorials/local-tui.md): setup and
  execution without a daemon.
- [File-backed completion](../cli/chat-completion.md): run a prompt with input
  messages and write the resulting conversation to a file.

## Add useful capabilities

The [longer prompt examples](prompt-patterns.md) preserve the minimal,
refactoring, and moderator examples from the detailed project README, including
the `Item`, `Tool_call`, and `Context` helper walkthrough.

| Try this | Example or walkthrough |
|---|---|
| Read project files or add editing tools | [Tool declarations and catalog](../overview/tools.md) |
| Compose a larger assistant from prompts and tools | [General agent workflow](../guide/general-agent-workflow.md) |
| Search code and documentation | [Search setup](../guide/search-and-indexing.md) and [illustrative output samples](../guide/search-examples/README.md) |
| Give an agent a narrow command | [Complete shell tutorial](../agent-server/tutorials/shell-agent.md) |
| Explore other shell configurations | [Shell declaration patterns](../guide/chatmd-shell-examples.md) |
| Respond to a scheduled background event | [ChatML background agent](../agent-server/tutorials/background-agent.md) |
| Implement a custom OCaml tool with progress | [Tool registration guide](../lib/gpt_function.doc.md) and [compiled offline example](tools/custom_tool.ml) |

The [prompt example collection](../../prompt-examples/readme.md) contains larger
prompts to study and adapt. Treat these and the workflow examples as starting
points, not a promise that their paths, services, models, or permissions match
your environment. View ChatMD as source/code so its tags remain visible.

For an actual run rather than a template, see the
[documentation-update session](../../real-world-example-session/update-tool-docs/readme.md),
which links both the full transcript and its compacted version. These historical
artifacts illustrate the workflow; they are not current setup instructions.

## Host an agent or build a client

- [Unix daemon and TUI](../agent-server/tutorials/unix-daemon.md): create a session
  and reconnect to it independently of the terminal client.
- [Stdio client](../agent-server/tutorials/stdio-client.md): use the Ochat protocol
  through a local host or daemon gateway.
- [HTTP client](../agent-server/tutorials/http-client.md): authenticate requests
  and consume live updates.
- [Tracked server examples](agent-server/README.md): reusable prompts, client
  inputs, and a helper that generates private demonstration configuration.
- [Example configuration notes](agent-server/config/README.md): where generated
  configurations and credentials belong.

The tracked examples include a [tool-free prompt](agent-server/prompts/hello.chatmd),
a [timer script](agent-server/prompts/timer.chatmd), and a
[narrow shell prompt](agent-server/shell/pwd.chatmd). Use the accompanying
tutorials for setup rather than assuming each file runs without configuration.

## Before running an example

Check the host mode, provider configuration, tool access, and required services.
Live model calls may cost money; generated demo credentials are for isolated
local demonstrations, not a production authentication policy. Use a disposable
workspace when trying tools that can change files or execute commands.

For explanations behind the examples, see [ChatMD](../chatmd/README.md),
[tools](../tools/README.md), [ChatML](../chatml/README.md), and
[agent hosting](../agent-server/README.md). The
[documentation home](../README.md) links the full reference set.
