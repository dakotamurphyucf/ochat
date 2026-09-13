# Give an agent a useful shell command

An agent needs project facts before it can give useful advice. Give it a command
that reads Lantern's setup tutorial, inside a reusable shell runtime with explicit
read access, no project write access and no network access.

This starts a larger workflow: inspect a tutorial, run its checks, save the
evidence, then ask specialists to improve it. A fixed operation makes the tool
interface and its runtime easy to understand before adding more capabilities.

## Prerequisites and command context

Complete [installation](../quickstart.md) and [the first local agent](local-tui.md).
Use installed Ochat, provider configuration with access to `gpt-6-astra`, and a
supported macOS Seatbelt or Linux bubblewrap backend. Interactive requests use
your provider; the offline verification uses deterministic provider fixtures.

Inspect every file in the **Lantern shell inspection** source reader below.
Download and extract the complete bundle when ready to run it:

```text
narrow-shell/
  agent.chatmd
  runtimes/inspection.chatmd
  sample-project/
    docs/setup.md
    docs/reference.md
    scripts/check-docs.sh
    expected-report.json
  LICENSE.txt
```

Work from the extracted `narrow-shell` directory containing `agent.chatmd`.
That directory becomes `${workspace}`. If you built without installing, use
absolute paths to `_build/default/bin/main.exe` and `_build/default/bin/chat_tui.exe`
in place of `ochat` and `chat-tui`, keeping the bundle as your working directory.

Resource-limit setup is linked into Ochat; no separate helper executable is needed.
The sandbox backend is a separate platform requirement. See
[build troubleshooting](../troubleshooting.md) for backend or installation errors.

## Read the tool and its runtime

The root imports a reusable runtime, then binds a named tool to it:

```xml
<import src="runtimes/inspection.chatmd"/>

<tool name="inspect_setup" type="shell" mode="fixed" runtime="inspection"
      result="structured" description="Read the maintained Lantern setup document.">
  <command program="/bin/cat">
    <path_arg base="workspace" path="sample-project/docs/setup.md"/>
  </command>
  <arguments mode="none"/>
</tool>
```

`inspect_setup` is the model-visible action; `inspection` is its runtime.
The author fixes the executable and file path. The model supplies `{}` and cannot
append arguments or select another file. `result="structured"` returns status,
stdout, stderr and runtime metadata.

Open `runtimes/inspection.chatmd` in the reader:

| Setting | Effect in this example |
| --- | --- |
| Working directory | Commands run in `sample-project`. |
| Read roots | The sample project is readable within supported backend boundaries. |
| Write/network capabilities | No project writes or network access are declared. |
| Policy | Allow `cat`; deny other top-level commands. |
| Environment | Select declared values, including a fixed `PATH`. |
| Limits | Stop after 10 seconds and bound returned output. |
| Confinement | Require the supported OS sandbox. |

The tool interface is narrower than the runtime's read root: this tool exposes
only `docs/setup.md`. Another tool can reuse the runtime with a different fixed
operation. A file reader is also appropriate for reading files; this lesson
teaches the shell binding you can reuse for existing CLI programs and a real checker.

## Inspect without executing

From the extracted directory:

```sh
ochat shell inspect agent.chatmd
ochat shell inspect agent.chatmd -canonical
```

Check the imported runtime, `/bin/cat`, resolved setup path, read roots, working
directory, policy and limits. Inspection compiles the requested authority; it
does not execute the tool. Resolve unexpected paths or errors before authorizing it.

<a id="legacy-local-interactive-authorization"></a>

## Run the agent locally

After reviewing the prompt and inspection output:

```sh
chat-tui --no-config --local -file agent.chatmd --authorize-shell-manifest
```

The flag authorizes loading the compiled shell manifest in this native process.
There is no separate manifest file to create. Without authorization, a fresh
shell-enabled prompt rejects startup. Policy, approvals and confinement still apply.

Send:

> Read Lantern's setup instructions with inspect_setup. Tell me how to run the
> checker and what a reader still needs to know to verify success. Do not run it yet.

Submit with Meta+Enter, or Esc then `:w` and Enter as described in the
[keyboard guide](../../guide/chat_tui.md). Tool activity should show exit 0 and
stdout containing the actual setup instructions and reference link. The tutorial
deliberately lacks a verification section. A model naming that omission without a
tool result does not prove the command ran.

## Check the boundary

The valid call is `{}`. This attempted call is invalid:

```json
{ "arguments": ["../reports/latest.json"] }
```

Extra arguments fail the tool schema before execution. Asking in prose to read
another file does not change the declaration. Command policy and OS confinement
are additional layers: a schema alone is not a sandbox. This checkpoint's allowed
command needs no per-command approval; the next lesson adds one that does.

## Daemon authorization

No daemon is needed here. For that optional host, configure the same bundle as a
prompt with the bundle directory as its workspace, and use an operator grant or
explicit reviewed-prompt authorization policy. The local flag is not a daemon
setting. Follow [host integration](../../guide/chatmd-shell-host-integration.md)
and [private Unix setup](unix-daemon.md) for the exact differences.

The old [pwd source](../../examples/agent-server/shell/pwd.chatmd) remains a minimal
repository example. The URL of this upgraded lesson is unchanged.

## Checkpoint, troubleshooting, and next step

You should have a real setup-file result and an explanation of the missing
verification instructions. For path errors, confirm your working directory and
keep the complete bundle layout. For backend errors, consult
[shell diagnostics](../../guide/chatmd-shell-host-integration.md); do not change
`sandbox="required"` to bypass the problem. Truncated output is incomplete evidence.

Quit with Esc, then `:q` and Enter after work finishes. Native local state ends
with the process; source files remain. No background work was started.

Next, [run checks with separate capabilities and approved report writes](../../tutorials/shell-guardrails.md).
The next bundle includes the same project and all its companion files.
