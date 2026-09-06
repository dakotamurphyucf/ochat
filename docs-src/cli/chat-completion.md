# ochat chat-completion – script-friendly cousin of chat_tui

Host scope: this is the existing file-backed completion/utility CLI. New durable
agent sessions use [ochat-agent-server](../agent-server/README.md), while daemon-free
native TUI uses [the local guide](../agent-server/tutorials/local-tui.md).
`ochat shell` store administration targets legacy sessions, not daemon IDs.

`ochat chat-completion` runs a ChatMarkdown prompt non-interactively from the
command line. It is the script- and CI-friendly counterpart to the
interactive `chat_tui` UI.

---

## 1 30-second smoke-test

Run a single command that verifies **ChatMD parsing → tool-calling → OpenAI
round-trip** before you start wiring Ochat into your own workflows:

```console
$ ochat chat-completion \
    -prompt-file prompts/hello.chatmd \
    -output-file .chatmd/smoke.chatmd
```

Open `.chatmd/smoke.chatmd` and you should see something along the lines of:

```xml
<tool_call id="1" name="echo">{"text":"Hello ChatMD"}</tool_call>
<tool_response id="1">{"reply":"Hello ChatMD"}</tool_response>
```

If you do **not** get a reply, check that `OPENAI_API_KEY` is set and
reachable from the shell session.

---

## 2 Basic usage

```console
$ ochat chat-completion [flags]
```

The command reads a ChatMD prompt and appends messages, tool calls and
assistant responses to the output file. The entire conversation stays in a
single `.chatmd` document.

Typical invocation:

```console
$ ochat chat-completion \
    -prompt-file prompts/hello.chatmd \
    -output-file .chatmd/session.chatmd
```

Re-run the command with the same `-output-file` to extend the chat history.

---

## 3 Frequently-used flags

| Flag | Purpose | Default |
|------|---------|---------|
| `-prompt-file` | Append this template before running. Supply it only when initializing a transcript; every invocation with this flag appends it again. | *(none)* |
| `-output-file` | Chat log that *persists* across invocations (created if absent, **appended** otherwise). Use `$(mktemp)` or `/dev/stdout` when you want an *ephemeral* transcript. | `./prompts/default.md` |

---

## 4 Conversation state lives in a file

The file supplied to `-output-file` is the *single* source of truth for the
conversation: tool-calls, reasoning deltas, assistant messages – everything is
captured in ChatMarkdown.

Re-run the command with the *same* output file to extend the history:

```console
# Turn 1
$ ochat chat-completion -prompt-file prompts/hello.chatmd \
    -output-file .chatmd/tech_support.chatmd

# Turn 2 (assistant sees full history)
$ echo '<user>My computer is on fire!</user>' >> .chatmd/tech_support.chatmd
$ ochat chat-completion -output-file .chatmd/tech_support.chatmd
```

Open the result at any time in the interactive UI:

```console
$ dune exec bin/chat_tui.exe -- --no-config --local -file .chatmd/tech_support.chatmd
```

`chat_tui` lets you keep chatting as if the session had always been
interactive.

---

## 5 Ephemeral runs

Nothing prevents you from pointing `-output-file` to a temporary file or
standard output when you only care about the final transcript.

```console
# Linux / macOS – remove the temporary transcript after a successful run
$ tmp=$(mktemp /tmp/ochat.XXXX) \
  && ochat chat-completion -prompt-file prompts/hello.chatmd \
       -output-file "$tmp" \
  && cat "$tmp" \
  && rm "$tmp"

# Portable one-liner (store under /dev/shm when available)
$ ochat chat-completion -prompt-file ask_weather.chatmd \
       -output-file /dev/stdout
```

The first variant removes only the temporary transcript, and only if preceding
commands succeed. Both variants can leave `.chatmd` cache/tool payload files
and provider response logs. They are not zero-artifact or privacy-preserving
modes. See [provider logging](../lib/openai/responses.doc.md); use an isolated
working directory and review its contents before retaining or removing it.
The `/dev/stdout` variant writes ChatMD incrementally, not only a final answer,
and still requires `-prompt-file` as input.

## 6 Root-scoped file reads

The batch runner sets `${workspace}` and `${tool_dir}` to its process launch
directory. This file-backed runner first copies `-prompt-file` into the output
transcript, then parses that transcript. `${prompt_dir}`, root source context,
relative imports and document references therefore use the **output transcript's
directory**, not the original template's directory. Imported files retain their
own source context. A template stored elsewhere does not change the workspace.
Launch the command from the project the agent should read:

```console
$ cd /work/project
$ ochat chat-completion \
    -prompt-file /work/prompts/reviewer.chatmd \
    -output-file .chatmd/review.chatmd
```

In this example `${prompt_dir}` is `/work/project/.chatmd`, not `/work/prompts`.
Arrange relative dependencies beside the transcript, use suitable absolute
references, or use an agent host when root-prompt source identity must be retained.

The prompt may expose one or more roots:

```xml
<tool name="read_file">
  <read id="project" path="${workspace}" description="Repository under review"/>
  <read id="docs" path="${source_dir}/reference" description="Prompt-pack reference files"/>
</tool>
```

Configured roots are validated before the first model request. The generated
tool metadata tells the model each root's resolved absolute path and accepts
`file`, optional `root`, optional `offset`, and optional `line_count`.
Requests remain canonically confined to a declared root. See
[configuring `read_file` roots](../overview/tools.md#configuring-read_file-roots).

## 7 Shell-enabled batch runs

If a prompt declares `<shell_access>`, ochat compiles the canonical manifest,
applies administrative/trust/signature policy, authorizes its exact digest,
and instantiates every referenced runtime before publishing tools or sending
the first model request. Missing authorization fails closed; batch execution
never silently switches to direct spawn.

Preflight without executing commands:

```console
$ ochat shell inspect prompts/ci-agent.chatmd -canonical
```

CI prompts should use pinned noninteractive runtimes with a complete allowlist
and no UI reviewer. A request reaching `ask` without an available reviewer is
denied or returned as a configured error.

Shell output enters the transcript only after bounds, UTF-8 validation,
terminal sanitization, secret redaction, and output interceptors. See
[`ochat shell` runtime management](shell-runtime-management.md) and the
[shell security guide](../guide/chatmd-shell-security.md).
