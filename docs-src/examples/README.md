# Examples and walkthroughs

Start with a local prompt, then add the capabilities your task needs. You do
not need a daemon, shell access, or an orchestration script to try Ochat.

## Learn in order

1. [Run your first local agent](../agent-server/tutorials/local-tui.md): setup, one request, and clean exit.
2. [Give it a file tool](../tutorials/file-tool.md): a complete read declaration and a named sample file.
3. [Add a specialist reviewer](../tutorials/specialist.md): a parent, companion prompt, and observable delegation.

Then choose the capabilities your project needs:

| Learning path | Progression |
| --- | --- |
| Shell access | [Inspect a real project](../agent-server/tutorials/shell-agent.md), then [run checks with separate runtimes and approved report writes](../tutorials/shell-guardrails.md). |
| ChatML | [Compute over reports](../tutorials/chatml-program.md), [package a reusable tool](../tutorials/chatml-tool.md), then [control a conversation](../tutorials/workflow.md). |
| Agent teams | [Choose a delegation pattern](../guide/subagents.md), from a one-off specialist to persistent or generated children. |
| Run and operate | [Batch requests](../cli/chat-completion.md), [durable daemon sessions](../agent-server/tutorials/unix-daemon.md), and [background scheduling](../agent-server/tutorials/background-agent.md). |
| External clients | [Local stdio](../agent-server/tutorials/stdio-client.md) and [authenticated HTTP](../agent-server/tutorials/http-client.md). |

Hosting and transport lessons are optional branches, not prerequisites for every
shell tool or ChatML program. Persistent child workflows still require their
documented durable host.

Each tutorial records its host, prerequisites, verification scope, and persistence.
Read the associated ChatMD and companion files directly in each tutorial’s source
reader or in the website catalog. Download a bundle when you are ready to run it
in your configured local environment. The website does not execute agents in your browser.

## Choosing a source example

**Complete examples** include their local source/data dependencies; Ochat installation
and any stated provider credentials remain prerequisites. **Configurable templates**
need the additional host, backend, library, or connection setup described in their
linked tutorial. **Illustrative output** is a reading sample, not executable input.

On the website, expand **View source** in the catalog to read complete files with
syntax highlighting. The entrypoint is shown first; expand other filenames to
inspect companion prompts, ChatML scripts, sample data, build files, and notices.
The reader also works without JavaScript. Downloads provide exact-byte source
files and complete `.tar` bundles. Extract a bundle into a new directory with `tar -xf FILE.tar`;
its top-level directory matches the example ID. Preserve its relative paths and
license. Download every listed companion or use the bundle before running a multi-file
example. In a
checkout, the following links open maintained sources directly.

## Add useful capabilities

The repository-only [longer prompt examples](prompt-patterns.md) preserve the minimal,
refactoring, and moderator examples from the detailed project README, including
the `Item`, `Tool_call`, and `Context` helper walkthrough. They remain deferred from
website publication pending example-level review; use the complete tutorials above
for the supported learning progression.

| Try this | Example or walkthrough |
|---|---|
| Read project files or add editing tools | [Tool declarations and catalog](../overview/tools.md) |
| Compose a larger assistant from prompts and tools | [General agent workflow](../guide/general-agent-workflow.md) |
| Search code and documentation | [Search setup](../guide/search-and-indexing.md) and [illustrative output samples](../guide/search-examples/README.md) |
| Give an agent a useful fixed command | [Complete shell inspection tutorial](../agent-server/tutorials/shell-agent.md) |
| Separate inspection and check permissions | [Complete shell guardrails tutorial](../tutorials/shell-guardrails.md) |
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
