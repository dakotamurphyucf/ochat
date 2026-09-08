# ochat chat-completion – script-friendly cousin of chat_tui

Host scope: this is the existing file-backed completion/utility CLI. New durable
agent sessions use [ochat-agent-server](../agent-server/README.md), while daemon-free
native TUI uses [the local guide](../agent-server/tutorials/local-tui.md).
`ochat shell` store administration targets legacy sessions, not daemon IDs.

`ochat chat-completion` runs a ChatMD conversation non-interactively and appends
its response to a transcript file. Complete [installation and provider setup](../agent-server/quickstart.md)
first. The commands below assume the repository root and the active opam switch.
The model request requires credentials and incurs provider charges; preparation
alone is offline. Response timing and wording vary. Review the current
[provider transport boundary](../agent-server/permissions-and-security.md).

<a id="130-second-smoke-test"></a>

## 1 Run a batch request

Create a private directory for this example. Copy the tracked, tool-free
[hello prompt](../examples/agent-server/prompts/hello.chatmd) and append a request:

```sh
OCHAT_BATCH=$(mktemp -d /tmp/ochat-batch.XXXXXX)
cp docs-src/examples/agent-server/prompts/hello.chatmd "$OCHAT_BATCH/prompt.chatmd"
printf '%s\n' '<user>Greet a new Ochat user in one sentence.</user>' >> "$OCHAT_BATCH/prompt.chatmd"
```

Keep `OCHAT_BATCH` in this shell. The copied prompt selects its model and declares
no tools, imports, scripts, or other file dependencies. Adjust the model in your
private copy if your configured provider requires it. Then make one request:

```sh
dune exec bin/main.exe -- chat-completion \
  -prompt-file "$OCHAT_BATCH/prompt.chatmd" \
  -output-file "$OCHAT_BATCH/session.chatmd"
```

After a successful run, inspect the transcript:

```sh
cat "$OCHAT_BATCH/session.chatmd"
```

It should contain the prompt, your user message, and an assistant greeting.
No `echo` tool call is expected: this example has no tools. Exact response text,
provider IDs, and optional reasoning output are not fixed test expectations.
If the command fails, read its diagnostics and check the host's provider settings;
do not assume a partially written file means the request completed.

## 2 Basic usage

Once installed, the same executable is named `ochat`:

```sh
ochat chat-completion -prompt-file "$OCHAT_BATCH/prompt.chatmd" \
  -output-file "$OCHAT_BATCH/another-session.chatmd"
```

The driver creates its launch-directory `.chatmd` cache directory. It does not
create arbitrary output parent directories. The private directory above already
exists; create the parent first when choosing another output path. Conversation
text is appended incrementally; tool payloads and provider logs can be stored
separately. A transcript is not a backup of all runtime artifacts.

## 3 Frequently-used flags

| Flag | Purpose | Default |
|------|---------|---------|
| `-prompt-file` | Append the template before this run. Supply it only to initialize a transcript; every invocation with this flag appends it again. | *(none)* |
| `-output-file` | Transcript path, created if absent and appended otherwise. Its parent must exist. | `./prompts/default.md` |

Use a fresh output path for an independent conversation. Use the same output
path and omit `-prompt-file` when continuing one. Running the initial command
twice does not reset the transcript or deduplicate its instructions.

## 4 Conversation state lives in a file

Append another user message and continue the existing example:

```sh
printf '%s\n' '<user>Now describe ChatMD in one sentence.</user>' >> "$OCHAT_BATCH/session.chatmd"
dune exec bin/main.exe -- chat-completion -output-file "$OCHAT_BATCH/session.chatmd"
```

This is another billable request. The previous transcript becomes input;
the template is not appended again. Keep one writer per transcript and wait for
completion before editing it or starting another run.

To open its conversation in the native TUI:

```sh
dune exec bin/chat_tui.exe -- --no-config --local -file "$OCHAT_BATCH/session.chatmd"
```

The TUI starts a separate native host initialized from that file. It does not
attach to the finished batch process, and it does not turn batch storage into a
durable daemon session. See [native TUI persistence](../agent-server/tutorials/local-tui.md).

## 5 Ephemeral runs

The private-directory example keeps its transcript until you deliberately remove
or archive it. For output on stdout, the driver also supports this special path:

```sh
dune exec bin/main.exe -- chat-completion \
  -prompt-file "$OCHAT_BATCH/prompt.chatmd" -output-file /dev/stdout
```

This writes ChatMD incrementally, including the template, and still requires
`-prompt-file` as input. `/dev/stdout` is a Unix device path, not a portable
Windows filename. Relative dependencies use the output source context; use the
tool-free example here rather than a prompt pack with relative imports.

Both file and stdout runs can leave launch-directory `.chatmd` caches/tool
payloads and provider response logs. They are not zero-artifact modes. See
[provider logging](../lib/openai/responses.doc.md). When finished with the private
example, first stop all processes using it, then archive it or remove only the
recorded `OCHAT_BATCH` directory. Inspect other runtime artifacts separately.

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
$ mkdir -p .chatmd
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

Finalized shell output passes through byte bounds, terminal filtering, literal
secret replacement, and output interceptors. Byte-truncated finalized output is
not guaranteed to end on a UTF-8 boundary. Optional sanitized live progress has
a separate incremental UTF-8 and disclosure contract; it does not replace the
finalized transcript result. See
[`ochat shell` runtime management](shell-runtime-management.md) and the
[shell security guide](../guide/chatmd-shell-security.md).

## Checkpoint and next step

You prepared a complete input, chose a fresh output path, inspected the resulting
conversation, and learned how to append a follow-up without repeating the template.
A successful transcript includes your message and an assistant response; a partial
file or a provider error is not success. Check installation/model access first for
request failures and create the output parent before retrying a missing-path error.
Keep one writer per transcript. Follow the cleanup guidance in section 5 after all
processes exit; caches and provider logs can live outside the transcript directory.

Previous: [add a specialist reviewer](../tutorials/specialist.md). Next:
[control completed turns with ChatML](../tutorials/workflow.md), or choose an
advanced host from the [tutorial and example catalog](../examples/README.md).
