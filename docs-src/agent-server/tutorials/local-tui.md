# Run the TUI without a daemon

In this tutorial you will open a small agent in your terminal, send a request,
and return to your shell. No daemon or OCaml programming is needed.

Complete [installation and provider setup](../quickstart.md) first. You need a
built Ochat checkout, the active opam environment, and an API key with access to
the selected model. Sending a request incurs provider charges.

## 1. Read the agent definition

The tracked example at [hello.chatmd](../../examples/agent-server/prompts/hello.chatmd)
is a complete ChatMD agent:

```xml
<config model="gpt-5.6-sol"/>
<developer>You are a concise assistant. Explain your reasoning briefly when helpful.
Do not claim to have run tools: this prompt has no tools.</developer>
```

`config` selects the model and `developer` supplies the instructions. This agent
has no tools: it can answer your message, but cannot inspect your project or
modify files. Change the model in your local example if your account does not
have access to `gpt-5.6-sol`.

## 2. Open the agent

In the terminal where you completed setup, stay in the **Ochat repository root**
and run:

```sh
dune exec bin/chat_tui.exe -- --no-config --local \
  -file docs-src/examples/agent-server/prompts/hello.chatmd
```

`--local` starts a process-bound host inside the terminal application.
`--no-config` prevents saved TUI configuration from changing this tutorial's
mode. `-file` selects the agent definition. The workspace is the directory you
launched from.

Opening the file does not send a model request: this definition contains no
initial user message. You should see the terminal interface waiting for input.

## 3. Send your first request

Press <kbd>i</kbd> to enter Insert mode if you are in Normal mode. Type:

```text
Explain what an AI agent is in two sentences. Do not use tools.
```

Submit with **Meta+Enter**: usually **Option+Enter** on macOS, or **Alt+Enter**
on Linux. Plain Enter in Insert mode adds a line. Terminal key mappings can
vary; see the [keyboard guide](../../guide/chat_tui.md) if submission does not
work. If your terminal does not transmit Meta+Enter, press Esc, type `:w`, and
press Enter to submit the draft from Cmdline mode. `:wq` quits without submitting.
The earlier recorded macOS manual checks used Option+Enter.

A successful request produces a streamed assistant response. The exact text
varies with the model; expect a short explanation, not an identical transcript.
Wait for streaming to end and the busy status to clear. This prompt should not
produce tool activity.

If the request fails, check the key in the launching terminal, model access,
and `API_URL` in [provider setup](../quickstart.md#provider-environment).
Use [troubleshooting](../troubleshooting.md) for connection or host errors.
Do not paste credentials into the agent definition or a bug report.

## 4. Quit cleanly

After the response finishes, press <kbd>Esc</kbd> to leave Insert mode, type
`:q`, and press Enter. You should return to your shell.

Quitting ends this process-bound host and leaves durable records under
`$HOME/.ochat/agent-store`; it does not leave an agent running in the background.
Use `--transient` when you want disposable records. See the persistence details
below to inspect, export or explicitly select a retained session.

## 5. Use an agent in your own project

Create `assistant.chatmd` in your project directory and copy the complete
agent definition above into it. After changing to that directory, use the
installed command in the same configured opam environment:

```sh
chat-tui --no-config --local -file assistant.chatmd
```

Now `${workspace}` and `${tool_dir}` refer to that project directory. Merely
changing the workspace does not grant this tool-free agent access to files.
Continue with [the file-tool tutorial](../../tutorials/file-tool.md) to add a
read-oriented tool, then [a specialist reviewer](../../tutorials/specialist.md).

There is no native local `--workspace` TUI option. `--data-root` selects an
absolute durable storage root, independently of the workspace. In the native
host, `${prompt_dir}` points into the materialized prompt artifact; imported
sources have their own captured `${source_dir}`. Uncaptured neighboring files
are not copied automatically. Use `${workspace}` or explicit tool roots for
project data; see [workspace and file roots](../../guide/chat_tui.md#workspace-and-read_file-roots).

## Persistence and compatibility

Native local records are durable by default under `$HOME/.ochat/agent-store`.
`--data-root /absolute/path` selects another root; `--transient` selects disposable
storage. Quit always ends the host. There is no implicit legacy migration, no cwd
fallback when HOME is absent, and no workspace substitution when reopening.

Use the native catalog explicitly:

```sh
chat-tui --no-config --local --list-sessions --json
chat-tui --no-config --local --session-info SESSION_ID --json
chat-tui --no-config --local --export-session SESSION_ID --out /tmp/session.chatmd
chat-tui --no-config --local --session SESSION_ID
```

The first three commands read without execution selection, including archived
records. Export reuses the authorized session blob/download path. The last
command explicitly selects the exact inspected lifecycle anchor before attaching;
it preserves saved identity, source and workspace. A stopped session stays stopped,
and an archived or restore-gated session needs the supported lifecycle Restore
and Resume commands first. A gate commit alone does not report running.

Use a detached daemon session when work must outlive the terminal.

Older file-backed sessions remain available through compatibility flags, without
explicit `--local`. For example (replace the prompt path):

```sh
chat-tui --no-config -file /absolute/path/to/agent.chatmd --new-session
chat-tui --no-config --list-sessions
```

Use IDs from that legacy listing only with the corresponding legacy commands.
Do not confuse them with daemon session IDs. `--authorize-shell-manifest` without
`--local` also selects that compatibility path. For a reviewed shell-enabled
prompt in the native host, use `--local --authorize-shell-manifest` together.
The flag permits the manifest to load; command approvals and sandbox restrictions
still apply. See [shell host integration](../../guide/chatmd-shell-host-integration.md).

## Config precedence

TUI configuration may come from `OCHAT_CHAT_TUI_CONFIG` or XDG config locations.
It contributes arguments before final mode normalization. `--no-config` makes
these tutorials reproducible. `--print-effective-args` helps diagnose unexpected
mode selection; review output for sensitive local paths before sharing it.
See the [CLI reference](../../bin/chat_tui.doc.md) for exact flags and conflicts.

## Checkpoint and next step

You opened a tool-free agent, observed a completed response, and returned to the
shell. The local host has stopped; its durable session remains available for
inspection and explicit resume. If you used a private prompt copy, keep it available
at its recorded path when resuming. Runtime/provider logs may be separate.
Next, [give the agent a file tool](../../tutorials/file-tool.md).
Use the [example catalog](../../examples/README.md) for the complete hello source.
