# Keep a conversation with a specialist

A one-off reviewer can identify a missing verification section. A retained reviewer
can then refine its proposed wording, explain tradeoffs, and answer a follow-up
without starting the investigation again.

This Lantern project exposes two authored agent tools. `quick_review` lets the
model choose a one-off answer or a persistent session. `review_thread` always
creates or continues a persistent specialist. Both use the same maintained
reviewer definition and the same lifecycle tools.

## Start a durable local host

Complete the [one-off specialist lesson](specialist.md). Install Ochat and configure
provider access to `gpt-6-astra`; the specialist requests high reasoning. Download
**Lantern specialist conversations**, inspect its files below, and extract it.
The bundle also contains the next lesson's generated-specialist entry point.

```text
specialist-conversations/
  authored.chatmd
  generated.chatmd
  project-tools.chatmd
  agents/reviewer.chatmd
  generated/
    reviewer.chatmd
    tools.chatmd
    create.json
    validate.json
  server.sexp
  sample-project/
    docs/setup.md
    docs/reference.md
    scripts/check-docs.sh
    expected-report.json
  LICENSE.txt
```

Persistent child sessions need a durable host. Use this bundle's local Unix daemon,
rather than transient `chat-tui --local`. From the extracted directory:

```sh
ochat-agent-server -config "$PWD/server.sexp" -validate-only
ochat-agent-server -config "$PWD/server.sexp"
```

In another terminal, enter the same extracted directory and connect:

```sh
chat-tui --no-config --connect "unix://$PWD/agent.sock" \
  --new-daemon-session --prompt authored --workspace lantern
```

Checkout users can substitute absolute paths to `_build/default/bin/ochat_agent_server.exe`
and `_build/default/bin/chat_tui.exe`. Keep the bundle directory as the working
directory for these commands. Relative paths inside `server.sexp` resolve from
the configuration file's directory.

The host stores private session data in `private-data/`, beside the prompts. Only
`sample-project/` is the workspace. Its file tools read that root; they cannot read
the sibling server configuration or private store. The permission profile allows
the declared tools so child reads can run unattended. This is not a general shell
grant: neither parent nor reviewer has a shell tool. The report is bundled sample
evidence, not proof that this session ran the checker.

## Choose the specialist's lifetime

The parent declares:

```xml
<tool name="quick_review" agent="agents/reviewer.chatmd" local
      persistence="optional"/>
<tool name="review_thread" agent="agents/reviewer.chatmd" local
      persistence="persistent"/>
```

The complete root adds useful descriptions. Ochat appends guidance explaining
creation, continuation, receipts, and the lifecycle tools to those descriptions.
The specialist reads the sample files through its own authored file-tool declaration.
Its instructions and private tool configuration are part of the captured definition.

| Call | Behavior |
| --- | --- |
| `quick_review` with `input` only | A fresh one-off conversation. |
| `quick_review` with `mode: "persistent"`, no session ID | A new retained specialist. |
| `quick_review` with persistent mode and its returned session ID | Continue that same specialist. |
| `review_thread` with `input`, no session ID | A new retained specialist; no mode argument is needed. |
| `review_thread` with its returned session ID | Continue that instance. |

Omitting the ID creates another instance even if an earlier specialist exists.
A session created through `quick_review` is not an instance of `review_thread`:
the named wrapper checks its own declaration and ownership. The shared lifecycle
tools can manage either child when the caller has the recorded relationship.

## Ask for a review and a follow-up

Send the parent:

> Use quick_review for a persistent review of docs/setup.md and expected-report.json.
> Ask what verification guidance a new reader is missing. Keep the specialist's ID
> and response receipt for a follow-up.

The creation call has this shape:

```json
{
  "input": "Review docs/setup.md against docs/reference.md and expected-report.json. What verification guidance is missing?",
  "mode": "persistent"
}
```

The result includes `session_id`, the submission receipt, and a bounded output
page. It may still be pending when the tool returns. Keep the actual returned ID
and `receipt_id`; examples cannot supply usable IDs for your run.

Then ask:

> Continue with that same specialist. Using its earlier diagnosis, propose a short
> Verification section that explains the check command's expected failing result.
> Make clear that the file has not been edited and no fresh check was run.

The model supplies the earlier `session_id` along with `mode: "persistent"` and
the new `input`. The specialist's conversation includes the first exchange. Its
answer should develop that investigation rather than treating the follow-up as an
unrelated review. The model's exact wording is not deterministic.

Try the same first request through `review_thread`. Omit `mode` entirely: that
fixed-persistence tool rejects mode overrides. For a genuinely one-off call,
use `quick_review` without mode or session ID. One-off calls reject a session ID.

## Follow work through the shared lifecycle

| Tool | What to retain or inspect |
| --- | --- |
| `agent_status` | Current child state and pending-permission count. |
| `agent_send` | A new submission receipt for a message to the existing child. |
| `agent_wait` | The chosen receipt's terminal outcome, or a bounded timeout. |
| `agent_read` | Committed assistant output and `next_cursor` for later pages. |
| `agent_stop` | A stop receipt; inspect progress before claiming cleanup finished. |

`agent_send` requires `session_id`, plaintext `message`, and a new
`idempotency_key`. Keep the same key and exact message for an uncertain retry;
use a new key for a new intended message. The named tools manage their internal
creation/send keys from the invocation; their input schema does not accept your
own idempotency key.

Wait with the session ID and the relevant `receipt_id`, then check whether that
receipt succeeded, failed, was cancelled, or was interrupted. An idle child is not
proof that a particular request completed. A timeout from `agent_wait` does not
cancel the child. Read with the same receipt filter and keep the returned cursor
until all output pages have been collected. Large output can arrive as fragments;
the [lifecycle reference](../guide/chatml-authoring-children.md#read-output-and-recover-cursors)
explains reconstruction and cursor recovery.

## Stop, retain, and troubleshoot

Ask the parent to stop the child with `agent_stop`, a new key, and `mode: "graceful"`
after its work is complete. Use `cancel` to request interruption of active work.
Stopping preserves stored conversation and output. Reads remain subject to current
authority and retention. Sending a new message to a stopped child does not restart
it; starting it again requires a separately authorized host operation.

Owned children are cancelled when the parent stops. Merely knowing an ID does not
grant another agent access to that child. If a child waits for permission, its
parent cannot approve the request by reading status. Inspect the child through an
authorized host client when you deliberately change this example's permission policy.

For connection failures, check that the daemon is running and both terminals use
the same extracted directory. For file errors, check that `sample-project/` contains
all bundle files and that tool calls use `root: "project"`. Do not point the
workspace at the entire bundle to make a missing-file error disappear.

When finished, stop children, quit the TUI with Esc then `:q` and Enter, and stop
the daemon with Ctrl-C. Keep the extracted directory to retain its private store;
deleting it removes that local example's saved sessions. Native local transient
mode cannot substitute for this durable setup.

Build the [complete persistent review team](../applications/persistent-review-team.md)
to coordinate several roles, collect their receipt-correlated output with ChatML,
handle a failed reviewer and refine the same conversations with new evidence.

Continue with [create a task-specific specialist](generated-specialist.md) to let
the parent choose a new role and capture its definition while retaining the same
session lifecycle and inherited authority rules.
