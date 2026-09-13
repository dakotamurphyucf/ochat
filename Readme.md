# Ochat — build your own AI agents in text files

**Your instructions. Your tools. Your workflow.**

[Website and documentation](https://ochatlabs.com/) · [Run your first agent](https://ochatlabs.com/docs/start/first-agent/) · [Explore applications](https://ochatlabs.com/applications/)

Ochat lets you define an AI agent in a text file and run it against your project.
Use it to understand unfamiliar code, review changes, update documentation, or
build a development assistant that works the way you do.

Start with instructions and a few tools. Reuse another agent as a specialist,
or add a script to control a longer workflow. Those definitions stay in files
you can edit, share, and version-control alongside your code.

For larger workflows, let agents write tool-using scripts, create persistent
specialists, and receive results from background work. ChatML gives you explicit
control over that coordination, including custom tools whose behavior depends
on the conversation's state. [Explore programmable workflows](#build-programmable-agent-workflows).

The usual starting point is the **local terminal interface**: open a ChatMD
file in the repository you want to work on. No server setup is needed.
You can also run agents headlessly from scripts or host them as a background
service when the workflow calls for it.

Ochat is written in OCaml, but you do not need to write OCaml to create and use
agents. Your project can use any language.

See the terminal workflow in action (click to play):

<div>
<a href="https://asciinema.org/a/gIelV4eAeA0LKvG7" target="_blank"><img height="700" width="900" src="https://asciinema.org/a/gIelV4eAeA0LKvG7.svg" alt="Ochat terminal workflow recording — click to play on Asciinema" /></a>
</div>

The recording shows an earlier terminal workflow; follow the quick start below
for current commands.

## Why use Ochat?

- **Make an agent your own.** Put your project's conventions, review checklist,
  and available tools in its definition instead of repeating them in every chat.
- **Build with tools already included.** Ochat provides file reading and editing,
  web-page ingestion, documentation search, image loading, and prompt-refinement
  tools. Choose what your agent needs and declare it in the same text file.
- **Keep the workflow with the project.** Review changes to an agent just like
  changes to code, and share the same definition with your team.
- **Build from small, reusable agents.** Give a main assistant a specialist
  reviewer, researcher, or documentation writer to call when needed.
- **Start simple, add control later.** Use a plain prompt for everyday tasks;
  add scripted decisions, follow-up work, or background events as you grow.
- **Give agents programmable tools.** Combine existing tools in a ChatML script,
  or let an agent generate a small program to transform data and coordinate calls.
- **Keep specialists involved.** Create a persistent child agent, send follow-up
  work, and inspect its status and output without rebuilding its conversation.
- **Choose how to run it.** Work interactively in your terminal, run one request
  from a script, or connect several clients to a long-running agent.

For example, this is a complete read-oriented repository assistant:

```xml
<config model="gpt-5.6-sol"/>

<tool name="read_file">
  <read id="project" path="${workspace}" description="Files in this repository"/>
</tool>

<developer>
You help me understand this project. Read the files I ask about, explain how
they fit together, and refer to filenames in your answers. If you need another
file, ask for its path. Do not invent details you have not checked.
</developer>
```

That format is **ChatMarkdown (ChatMD)**: ordinary text plus tags for
instructions, tools, and model settings. `${workspace}` means the directory
you launch the local agent in. The optional scripting language is **ChatML**;
you can ignore it until you want more than a prompt.

The `read_file` tool above comes with Ochat—there is no tool implementation to
write. Combine [built-in tools](docs-src/overview/tools.md#built-in-catalog-code-correct)
to make a documentation researcher, code reviewer, or editing assistant, then
add specialist agents or MCP integrations when you need more. Some tools need
extra setup, such as building a search index; the
[tools guide](docs-src/tools/README.md) helps you choose and configure them.

## On this page

- [Run your first local agent](#run-your-first-local-agent)
- [Make it your own](#make-it-your-own)
- [Build programmable agent workflows](#build-programmable-agent-workflows)
- [Run a request without the terminal interface](#run-a-request-without-the-terminal-interface)
- [Optional: keep agents running in the background](#optional-keep-agents-running-in-the-background)
- [What else can you build?](#what-else-can-you-build)
- [Documentation and contributing](#documentation-and-contributing)

## Run your first local agent

You'll need an OCaml/opam installation, this repository checked out locally,
and an OpenAI API key with access to your chosen model. Model calls incur
provider charges. The examples use `gpt-5.6-sol`; change the `model` setting
if your account uses a different supported model.

### 1. Build and install Ochat

Open a terminal in the **Ochat checkout**. If you have not installed OCaml and
opam, follow the [OCaml installation instructions](https://ocaml.org/install#linux_mac_bsd)
first. Current project requirements include OCaml 5.1+ and Dune 3.21+.

If you do not already have a suitable opam switch, create one here:

```sh
opam switch create .
```

Then install dependencies and build the command-line tools into your active
opam switch:

```sh
eval "$(opam env)"
opam install . --deps-only
dune build @install
dune install
```

Keep this terminal open: its environment now lets you run `chat-tui` and `ochat`
from another project directory. Check the installation with:

```sh
chat-tui -help
```

If you hit a build problem, see [build troubleshooting](docs-src/guide/build-troubleshooting.md),
including the Apple Silicon/OpenBLAS notes. You only need to do this setup once
for the selected switch, not for every agent.

### 2. Set your API key

If `OPENAI_API_KEY` is already set, keep it. Otherwise, in bash or zsh, run the
following command, paste the key into the hidden input, and press Enter:

```sh
read -r -s OPENAI_API_KEY
export OPENAI_API_KEY
```

For direct OpenAI access, set:

```sh
export API_URL=api.openai.com
```

Keep credentials out of prompt files and Git. This sets them for the current
shell; use your usual private environment setup for future terminals.
[Provider configuration and limitations](docs-src/agent-server/environment.md)
are documented separately.


**Additional LLM providers**  
Today Ochat integrates with OpenAI; future work is intended to support additional backends while keeping ChatMD and tool contracts stable.
You can use a proxy server that maps the Openai api Response endpoint format to your preferred providers format. Example: [LiteLLm](https://docs.litellm.ai/) and set the enviorment url API_URL to the proxy url. 
For a governed OpenAI-compatible proxy, `API_URL=https://api.tuningengines.com/v1`
can route the same workflow through Tuning Engines while Ochat keeps owning
the local workflow artifact and tool execution.

### 3. Save the agent in your project

Change into the repository you want to explore. For a first try, you can also
stay in the Ochat checkout itself.

```sh
mkdir -p agents
```

Using your editor, save the complete ChatMD example above as
`agents/explorer.chatmd`. The `agents/` name is just an example; agent definitions
can live wherever you prefer. Check your project's ignore rules before committing
them, and never put credentials in the definition.

### 4. Open it and ask about a file

From that project's root directory:

```sh
chat-tui --no-config --local -file agents/explorer.chatmd
```

`--local` runs the agent in this process without a daemon. `--no-config` keeps
saved TUI preferences from changing the example's launch behavior.

Try a request that names a file in your project. In the Ochat checkout:

> Read Readme.md and explain what this project does and where I should start.

The agent can call `read_file` to inspect the file and stream its explanation.
If a tool approval appears, review the request before allowing it. This example
has no editing or shell tools.

The essential keys are:

| Action | Keys |
|---|---|
| Start typing when in Normal mode | `i` |
| Add a new line to the draft | `Enter` |
| Send the draft | `Alt+Enter`, usually `Option+Enter` on macOS |
| Inspect tools while they run | `Ctrl-G` |
| Return from the Agent page | `Esc` |
| Quit from the Chat page | Leave Insert mode with `Esc`, then type `:q` and press `Enter` |

If your terminal does not deliver Alt/Option+Enter, leave Insert mode and use
`:w` followed by Enter to submit. See [TUI keys](docs-src/guide/chat_tui.md) for
search, scrolling, and other shortcuts.

**Local mode ends when you quit and does not automatically save a resumable
session.** The prompt file stays on disk. For saved conversation output, use the
[headless example](#run-a-request-without-the-terminal-interface); for persistent
interactive session options, see [local TUI sessions](docs-src/agent-server/tutorials/local-tui.md).

You now have an agent whose behavior you control by editing a file.

## Make it your own

Close the TUI, edit the ChatMD file, and reopen it to use the updated definition.
Here are a few ways to build on the explorer.

### Give it your project's habits

Change the developer instructions to describe how you want it to work:

```xml
<developer>
You are my project guide. Read the files I name before answering.
Explain unfamiliar concepts with small examples. Keep answers concise and
finish with one suggested next step. When discussing code, refer to filenames.
</developer>
```

Replace the original `<developer>` block rather than adding a second copy.
Try the same question again and compare the behavior. This is also a good place
for your team's naming conventions, documentation style, and review checklist.

### Let it make a small improvement

Copy `agents/explorer.chatmd` to `agents/editor.chatmd` using your editor and
add this declaration beside `read_file`:

```xml
<tool name="apply_patch"/>
```

Update its instructions to ask for small, reviewable changes, then launch:

```sh
chat-tui --no-config --local -file agents/editor.chatmd
```

For a first task in a disposable checkout, try:

> Read Readme.md. Propose one wording improvement without changing its meaning.
> Explain the change first and wait for my confirmation before editing.

After reviewing the proposal and any tool approval, let it apply the change and
inspect the result with `git diff`.

Adding `apply_patch` gives the agent an editing tool. Instructions to “ask first”
are guidance to the model, not a security boundary: enforce approval with the
host's tool policy where required. The read root on `read_file` does not also
restrict other tools. Start with narrow tools and a disposable checkout.
See [tools and permissions](docs-src/overview/tools.md) before broadening access.

### Give it a specialist agent

You can turn another ChatMD file into a tool. For example, save this complete
specialist as `agents/docs-reviewer.chatmd`:

```xml
<config model="gpt-5.6-sol"/>
<developer>
Review the documentation text supplied by the caller. Identify unclear setup
steps, unexplained terms, and missing examples. Return three actionable
suggestions. Review only the supplied text; you have no file or editing tools.
</developer>
```

Add the following declaration to `agents/explorer.chatmd`, beside `read_file`:

```xml
<tool name="review_docs" agent="docs-reviewer.chatmd" local/>
```

The relative path points to the specialist beside the parent prompt. Reopen the
explorer and ask:

> Read Readme.md, send its introduction to review_docs for feedback, and tell me
> which suggestion would help a new user most.

The main agent reads the file, supplies text to the specialist, and uses the
returned feedback in its answer. The specialist has its own instructions and
tools; it does not automatically inherit the parent's conversation. Additional
model work incurs additional cost.

This pattern works for planning, test review, research, and writing—not just
documentation. See [agents as tools](docs-src/overview/tools.md#agent-tools--turn-prompts-into-callable-sub-agents).

### Add a little workflow logic

Sometimes instructions are not enough: you want Ochat itself to react to events.
**ChatML** scripts can control follow-up work, inspect tool calls, and coordinate
other agents. They are optional; ordinary prompts do not need a script.

As a small example, append this script to a *copy* of the explorer prompt named
`agents/three-turns.chatmd`:

```xml
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Turn_end ]
  let initial_state = 0
  let on_event : context -> state -> event -> state task =
    fun ctx st ev ->
      match ev with
      | `Session_start -> Task.pure(st)
      | `Turn_end ->
        let completed = st + 1 in
        if completed >= 3 then
          let* () = Runtime.end_session("Three-turn session finished") in
          Task.pure(completed)
        else Task.pure(completed)
</script>
```

Run it with:

```sh
chat-tui --no-config --local -file agents/three-turns.chatmd
```

After three completed agent turns, the script asks the host to end the session.
That is workflow control executed by Ochat, not an instruction asking the model
to remember when to stop. You can still quit the client normally. A turn can
include several tool/model calls, so this is **not a dollar spending cap**.

Larger scripts can dispatch work to specialist agents and react to their
completion, or wait for timers and other events. Explore the
[ChatML guide](docs-src/guide/chatml-moderator-runtime.md) and
[background workflow tutorial](docs-src/agent-server/tutorials/background-agent.md)
when you need that level of control.

### Add shell commands deliberately

Ochat also supports shell tools for builds, tests, and other project operations.
Start with a fixed command rather than a general-purpose shell. Save this
complete example as `agents/working-directory.chatmd`:

```xml
<config model="gpt-5.6-sol"/>
<developer>When asked, use working_directory to report the current directory.</developer>
<shell_access id="readonly" extends="builtin:workspace-readonly@1">
  <policy default="deny" merge="replace">
    <rule id="pwd-only" action="allow"><basename value="pwd"/></rule>
  </policy>
</shell_access>
<tool name="working_directory" type="shell" mode="fixed" runtime="readonly">
  <command program="/bin/pwd"/>
  <arguments mode="none"/>
</tool>
```

Inspect the requested access before authorizing it:

```sh
ochat shell inspect agents/working-directory.chatmd -canonical
```

If inspection succeeds and you agree with that access, the existing local
interactive authorization path is:

```sh
chat-tui --no-config -file agents/working-directory.chatmd --authorize-shell-manifest
```

Ask “What directory are we working in?” The tool can run only its fixed command,
not an arbitrary command supplied by the model. Platform support and runtime
policy still apply; do not weaken a required sandbox to bypass a startup error.

This authorization flag selects the older file-backed local mode, so it is
intentionally **not combined with `--local`**. The complete
[shell tutorial](docs-src/agent-server/tutorials/shell-agent.md) explains local
and daemon authorization, adding build/test tools, and available safeguards.

## Build programmable agent workflows

ChatML now supports three distinct uses: a one-off program over selected tools,
a reusable custom tool, and a stateful moderator that coordinates a conversation.
You can write these yourself or expose authoring tools so an agent can generate
programs when the task calls for them.

| Capability | What you can build | Guide and examples |
|---|---|---|
| **Tool-using programs** with `run_chatml` | Read several reports, validate their contents, and aggregate results with explicit program logic in one tool invocation. | [One-off computations](docs-src/guide/chatml-authoring-runtime.md#one-off-tool-using-computations) |
| **Custom ChatML tools** | Package reusable logic behind input/output schemas, or let a moderator handle a tool using retained workflow state. | [Standalone tools](docs-src/guide/chatml-authoring-runtime.md#standalone-tools-and-explicit-outcomes) · [Invocation contracts](docs-src/guide/chatml-authoring-runtime.md) |
| **Background jobs and later notifications** | Start a build or review, continue the conversation, then deliver its result and optionally request an agent turn. Combine timers and subscriptions to watch for new output. | [Background work and delivery](docs-src/guide/chatml-authoring-background.md) |
| **Persistent child agents** | Let an agent define a specialist's instructions, model and moderator, then create, message, read, wait for, inspect and stop that session by ID. | [Child-session lifecycle](docs-src/guide/chatml-authoring-children.md) |
| **Persistent agents as tools** | Keep a user-defined reviewer available for several rounds of feedback, with optional persistence chosen by the calling model. | [Agent-tool persistence](docs-src/guide/chatmd-authoring-definitions.md#existing-tools-versus-new-tool-definitions) |
| **Built-in authoring guidance** | Let an agent query the installed language and runtime docs, then validate generated source before execution. | [Documentation and validation tools](docs-src/guide/authoring-context-tool.md) |

For example, the `review_docs` specialist above can offer both one-off calls and
persistent conversations by changing its declaration to:

```xml
<tool name="review_docs" agent="docs-reviewer.chatmd" local persistence="optional"/>
```

The model can call it with `mode: "persistent"`, then reuse the returned
`session_id` for follow-up feedback. Calls default to one-off behavior. Persistent
children require a durable session host; see the lifecycle guide for setup,
ownership and restart behavior. Generated children select from the parent's
delegable tools and retain their file and shell restrictions, while choosing
their own instructions and model settings.

One application is a **living documentation lab**: a coordinator assigns tutorials
to persistent reviewers, runs approved example commands as background jobs,
collects failures, and asks a writer to propose corrections. A moderator tracks
which checks and reviews are complete and delivers updates as results arrive.
These are building blocks for your workflow; the lab itself is an application
you can build with them.

Start with the [ChatML workflow overview](docs-src/chatml/README.md) or inspect
the [complete example bundles](test/chatml_extensibility_fixtures/README.md).
Authoring tools are opt-in through ChatMD declarations. By default, declaring
tools such as `run_chatml` or `agent_create` also supplies a shared authoring
primer and documentation/validation helpers; authors can choose manual guidance
or preload selected topics. Validation checks source without executing it.

## Run a request without the terminal interface

The same ChatMD authoring model works in scripts and CI. For a single request,
copy the original explorer definition into `agents/explain.chatmd` and append:

```xml
<user>
Read Readme.md and summarize the project in five bullets for a new contributor.
</user>
```

Use a filename that exists in your project. Then run from the project root:

```sh
mkdir -p agent-runs
ochat chat-completion \
  -prompt-file agents/explain.chatmd \
  -output-file agent-runs/explanation.chatmd
```

Open `agent-runs/explanation.chatmd` to inspect the resulting conversation.
ChatMD can hold the transcript—including tool calls and results—not just the
starting prompt. You can review, diff, and branch that text as your workflow evolves.
This does not require the TUI or a daemon. Use a fresh output filename for each
independent run; existing output files participate in the file-backed resume
workflow. Keep generated transcripts private if they contain project information.

For unattended work, configure tools that can run under your intended approval
policy; don't assume a human approval dialog will be available. See the
[completion CLI](docs-src/cli/chat-completion.md) for its additional options.

If you are building a client that exchanges messages with a running agent,
instead of executing one request, use [local stdio](docs-src/agent-server/tutorials/stdio-client.md).

## Optional: keep agents running in the background

Local TUI use is enough for everyday repository work. The **agent server** is
there when you want an agent to keep running after you disconnect, receive
remote input, or share updates with several clients.

The agent is still defined in ChatMD and ChatML. The server supplies hosting,
workspace configuration, permissions, and persistent sessions.

To try it without using your normal project or session store, open Terminal A
in the Ochat source checkout and create a private demo configuration:

```sh
OCHAT_DEMO=$(mktemp -d /tmp/ochat-docs.XXXXXX)
dune exec docs-src/examples/agent-server/clients/docs_example.exe -- \
  setup "$OCHAT_DEMO" gpt-5.6-sol
ochat-agent-server -config "$OCHAT_DEMO/unix.sexp"
```

The helper prints the demo directory and creates a no-tool prompt, an empty
workspace, configuration, and generated credentials. Leave Terminal A running.

In Terminal B, activate the same opam environment and set `OCHAT_DEMO` to that
printed directory. Then connect:

```sh
chat-tui --no-config --connect "unix://$OCHAT_DEMO/agent.sock" \
  --new-daemon-session --prompt hello --workspace project --detached
```

The daemon's environment—not Terminal B—needs the API key for model requests.
Quitting this TUI disconnects the client but leaves the detached session in the
server. To find it again:

```sh
chat-tui --no-config --connect "unix://$OCHAT_DEMO/agent.sock" --list-sessions
```

Replace `SESSION_ID` below with an ID from that listing:

```sh
chat-tui --no-config --connect "unix://$OCHAT_DEMO/agent.sock" --session SESSION_ID
```

For a second viewer, add `--read-only` to the attach command. When finished,
quit the clients and press Ctrl+C in Terminal A to shut down the demo daemon.
Its saved state remains in the demo directory; remove or archive only that
directory if you no longer want it, after the processes have stopped.

The [daemon tutorial](docs-src/agent-server/tutorials/unix-daemon.md) covers
publishing your own prompts and stopping individual sessions. Use the
[HTTP tutorial](docs-src/agent-server/tutorials/http-client.md) for authenticated
HTTP clients or the [stdio tutorial](docs-src/agent-server/tutorials/stdio-client.md)
for a subprocess gateway. Hosting is optional, not a new agent-definition format.

## What else can you build?

| Workflow | What Ochat provides | Explore |
|---|---|---|
| A project-specific coding assistant | Custom instructions, file tools, patches, reviewed shell commands | [Tools](docs-src/overview/tools.md) |
| A research or documentation assistant | Web ingestion and search over local documentation | [Search and indexing](docs-src/guide/search-and-indexing.md) |
| A team of specialist agents | Prompt-as-tool composition and scripted coordination | [Agent workflow guide](docs-src/guide/general-agent-workflow.md) |
| An assistant using external services | MCP-backed tools declared in ChatMD | [MCP tools](docs-src/overview/tools.md) |
| A prompt-improvement workflow | Generate, evaluate, and refine prompts with `mp-refine-run` | [Prompt refinement](docs-src/lib/meta_prompting.doc.md) |
| A background automation service | Persistent sessions, jobs, timers, multiple clients | [Agent hosting](docs-src/agent-server/README.md) |
| Your own OCaml application | Libraries for embedding agents and building clients | [Embedding](docs-src/agent-server/embedding.md) |

MCP tool integration is maintained. Only the older server that exposes ChatMD
prompts through MCP is deprecated; existing users can find its
[compatibility documentation](docs-src/bin/mcp_server.doc.md).

Optional [draft suggestions](docs-src/guide/chat_tui.md#type-ahead-availability-and-privacy)
work in local and connected TUIs. They default off; enabling them sends unsent
draft text to your provider and may incur additional charges.

## Documentation and contributing

Explore the [full documentation](docs-src/README.md), or pick a topic below.
Each topic index introduces the ideas and points you to examples and references.

| Start with… | To learn how to… |
|---|---|
| [ChatMD: agents in text files](docs-src/chatmd/README.md) | Write instructions, add tools, and compose prompts; includes the [language reference](docs-src/overview/chatmd-language.md). |
| [Examples and walkthroughs](docs-src/examples/README.md) | Try a local agent, explore workflows, or follow a complete server tutorial. |
| [Tools and integrations](docs-src/tools/README.md) | Choose from the [built-in tool catalog](docs-src/overview/tools.md#built-in-catalog-code-correct), call specialist agents, or connect MCP tools. |
| [Using the TUI](docs-src/guide/chat_tui.md) | Edit messages, inspect tool calls, handle approvals, and navigate the terminal interface. |
| [ChatML workflows](docs-src/chatml/README.md) | Add scripted decisions, coordinate work, and respond to events. |
| [Shell access and permissions](docs-src/shell/README.md) | Give an agent deliberate command access with appropriate limits and approvals. |
| [Agent server and transports](docs-src/agent-server/README.md) | Run background agents, connect the TUI to a daemon, or integrate Unix, stdio, and HTTP clients. |
| [Search and indexing](docs-src/guide/search-and-indexing.md) | Help agents find relevant code and documentation. |

Looking for a particular executable? See the [command index](docs-src/bin/README.md).
For design principles, repository layout, and the roadmap, see the
[project overview](docs-src/overview/project.md).



Contributing code? Start with [DEVELOPMENT.md](DEVELOPMENT.md). From the source
checkout:

```sh
dune build
dune runtest
```

Documentation and end-to-end checks are separate, explicit tasks. See the
[testing guide](docs-src/agent-server/testing.md) for `@agent-docs-check` and the
E2E suites; they are not required for every normal test run.

Ochat is actively evolving. Expect some API and tool-schema changes, review
generated edits, and choose permissions that fit your project. Bug reports,
examples, documentation improvements, and contributions are welcome.

## License

Original source code is licensed under [LICENSE.txt](LICENSE.txt).
