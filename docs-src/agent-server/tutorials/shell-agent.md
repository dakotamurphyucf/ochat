# A narrow shell agent

Give a model exactly one declared `/bin/pwd` command and inspect its authority before use.

## Prerequisites and command context

Complete [installation](../quickstart.md) and [the first local agent](local-tui.md). Commands below use `dune exec` from the repository root and its active opam environment. Interactive execution uses the legacy local host; the daemon variant needs private example setup. A supported Seatbelt or bubblewrap backend and the resource helper are platform prerequisites. Native `--local` must not be combined with `--authorize-shell-manifest`.

Read the current [provider TLS and permission boundaries](../permissions-and-security.md)
before model work or deployment. [Build troubleshooting](../troubleshooting.md)
includes the Apple Silicon/OpenBLAS setup path.


Use a disposable workspace. This example publishes exactly one fixed command,
`/bin/pwd`, with no model-supplied arguments. The
[tracked prompt](../../examples/agent-server/shell/pwd.chatmd) extends the versioned
read-only workspace profile and defaults shell policy to deny except this command.
Read [host integration](../../guide/chatmd-shell-host-integration.md) first.

## Inspect without executing

From the repository root, with required platform backend/helper installed:

```sh
dune build bin/main.exe bin/chat_tui.exe bin/ochat_shell_resource_runner.exe
dune exec bin/main.exe -- shell inspect docs-src/examples/agent-server/shell/pwd.chatmd
dune exec bin/main.exe -- shell inspect docs-src/examples/agent-server/shell/pwd.chatmd -canonical
```

Inspection must either show the expected authority or a specific configuration/
platform error. Do not weaken a required sandbox to make an example pass. Check
the exact cwd, executable resolution, roots, source/manifest identity and limits.
The helper applies resource limits; Seatbelt/bubblewrap enforce their own supported
OS boundaries. A direct backend does not enforce filesystem/network roots.

## Legacy local interactive authorization

After reviewing the manifest, the supported local CLI bootstrap is:

```sh
dune exec bin/chat_tui.exe -- --no-config \
  -file docs-src/examples/agent-server/shell/pwd.chatmd --authorize-shell-manifest
```

This deliberately selects the legacy local host, not native `--local`. Ask for the
working directory; this is a billable model request. The published tool can only
invoke its fixed argv. Requests for another command cannot be expressed through
this tool. Shell policy denial is separate from the model refusing in prose.

Inspect Shell Security overview/runtime/audit state. Per-command approval appears
only when policy asks; the example's single allowed command need not ask again.
Quit normally and verify terminal restoration. For a bounded automated denial/
approval audit check use the existing shell E2E tier, not a destructive command.

## Daemon authorization

In a separate private tutorial config, point the `hello` prompt path at the same
absolute shell prompt. Choose one explicit bootstrap policy:

- `require_grant`: configure an exact source/manifest operator grant for the
  intended daemon path/principal/workspace context. A standalone inspection digest
  with different runtime variables is not interchangeable.
- `assume_authorized`: an operator's deliberate trust of this reviewed prompt.
  For this disposable example it avoids inventing a grant-creation CLI; shell
  capabilities, policy, backend and administrative checks remain enforced.
- `deny`: useful to verify startup/tool authority rejection, not to execute it.

Validate, start and connect as in [the Unix tutorial](unix-daemon.md). Use daemon
`permission.list/respond`, `grant.list/revoke`, and `audit.read` or the connected
Shell Security page. Do not supply a daemon ID to `ochat shell grants ...`:
those commands target the legacy store.

The generic tool gate delegates shell calls to shell review; do not expect two
approval dialogs for one request. Audit replay is non-executing. A revoked grant
or changed manifest must not silently authorize future different commands.

See [the 17 shell declaration examples](../../guide/chatmd-shell-examples.md)
for structured/chain/raw/script tools, hooks, model reviewers, secrets and backend
failure cases. Those examples have explicit dependencies/placeholders and are not
all safe copy-and-run deployment scripts.

## Checkpoint, troubleshooting, and next step

Success means inspection matches the fixed executable/argv and a requested tool call returns the workspace directory. A model naming the directory without a tool result does not verify execution. For backend/helper errors consult [shell diagnostics](../../guide/chatmd-shell-host-integration.md); keep required confinement enabled. A permission denial is separate from parsing and model prose. Wait for work to finish, quit the TUI with Esc then `:q` and Enter, and stop any daemon before removing its private directory. Legacy caches/store records and provider logs can remain separately; inspect their configured locations. Continue to [durable Unix sessions](unix-daemon.md).
