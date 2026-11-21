# Recover an earlier decision across local windows

This example saves a per-session handoff, opens a new context window and
recovers the original wording of a decision. The same moderator continues
the existing [Review component](../lifecycle-composition/components/review/src/review.chatml)
after the transition. The [paired recovery module](../../libraries/recovery-rollover/src/recovery_rollover.chatmli)
decides what to do with typed host events, and the project resolves all three
dependencies through ordinary local ChatML project declarations. Follow the
[operator guide](../../../development/operator-recovery-rollover.md) to work with
a session that already uses recovery.

## Set up a private daemon

Run from the Ochat checkout with the [repository toolchain](../../../agent-server/quickstart.md).
This example uses the `first-party-openai-responses` profile and
`gpt-4.1`. For live use, supply the
[provider environment](../../../agent-server/quickstart.md#provider-environment)
and check the provider registry as the same local user who will run the
daemon. If status reports `setup_required`, initialize the registry once.
If the OpenAI API key profile has no credential, explicitly enroll the
environment source. Run enrollment with `OPENAI_API_KEY` already available
in the launching environment.

```sh
dune exec bin/main.exe -- provider status
dune exec bin/main.exe -- provider setup -key recovery-provider-setup
dune exec bin/main.exe -- provider configure-environment -key recovery-provider-enrollment
```

Check that the selected profile is `first-party-openai-responses`. If you
intend to change the host's default, `ochat provider select` takes that
profile, the selection revision from status and a new application key. The
daemon needs the same provider environment for each start. A ready prompt
can still fail at creation with `Target_unavailable` if its provider profile
has no usable credential.

The sample capacity numbers are local policy for that specific target.
Keep the prompt, model and policy aligned. The offline fixture below uses a
deterministic loopback provider and needs no account.

In the [example catalog](../../README.md), find **A recoverable review handoff**
and download its bundle, `context-recovery.tar`. You can also copy the source
tree below from the checkout. The individual
[ChatMD definition](agent.chatmd), [moderator](moderator.chatml),
[project](chatml.project.json), and [daemon configuration](server.sexp) are
available for inspection. Keep the folders together so ChatML can resolve the
paired interfaces and the Review component.

For a download, extract into a short, private directory:

```sh
OCHAT_RECOVERY_DEMO=$(mktemp -d /tmp/ochat-rec.XXXXXX)
tar -xf /path/to/context-recovery.tar -C "$OCHAT_RECOVERY_DEMO" --strip-components=1
chmod 700 "$OCHAT_RECOVERY_DEMO/learning/context-recovery"
```

For a checkout, create the same private layout instead:

```sh
OCHAT_RECOVERY_DEMO=$(mktemp -d /tmp/ochat-rec.XXXXXX)
mkdir -p "$OCHAT_RECOVERY_DEMO/learning/lifecycle-composition/components" "$OCHAT_RECOVERY_DEMO/libraries"
cp -R docs-src/examples/learning/context-recovery "$OCHAT_RECOVERY_DEMO/learning/"
chmod 700 "$OCHAT_RECOVERY_DEMO/learning/context-recovery"
cp -R docs-src/examples/learning/lifecycle-composition/components/review \
  "$OCHAT_RECOVERY_DEMO/learning/lifecycle-composition/components/"
cp -R docs-src/examples/libraries/lifecycle docs-src/examples/libraries/recovery-rollover \
  "$OCHAT_RECOVERY_DEMO/libraries/"
```

The daemon uses this read-only example workspace and keeps session data and
the Unix socket in its directory. The short path fits the operating system's
socket limit. The tutorial's allow policy applies to the small tool selection
in this private example. For deployed prompts, set a permission policy for
the specific tools and principals you need.

```sh
OCHAT_RECOVERY_CONFIG="$OCHAT_RECOVERY_DEMO/learning/context-recovery/server.sexp"
dune exec bin/ochat_agent_server.exe -- -config "$OCHAT_RECOVERY_CONFIG" -validate-only
dune exec bin/ochat_agent_server.exe -- -config "$OCHAT_RECOVERY_CONFIG"
```

In a second terminal, set `OCHAT_RECOVERY_DEMO` to the same directory and
connect over the Unix socket
`$OCHAT_RECOVERY_DEMO/learning/context-recovery/agent.sock`. Use an owned
local identity with create, start, message-send and transcript scopes. Keep
the daemon running while you work. The installed NDJSON gateway also works
with a client that keeps one bidirectional connection open.

```sh
dune exec bin/ochat_agent_stdio.exe -- --connect \
  "unix://$OCHAT_RECOVERY_DEMO/learning/context-recovery/agent.sock"
```

### Create and enroll the conversation

Paste each JSON-RPC request as one line into the gateway, and wait for its
matching response ID. Replace uppercase placeholders with the values you get
from the server. Start by initializing and discovering the configured IDs
for prompt `local-recovery` and workspace `recovery-review`. Keep the
`server_id` from initialization and check that the response selected
`recovery.windows`.

```json
{"jsonrpc":"2.0","id":"init","method":"protocol.initialize","params":{"implementation":{"name":"recovery-tutorial","version":"1"},"protocol_min":{"major":2,"minor":0},"protocol_max":{"major":2,"minor":0},"features":["recovery.windows","recovery.rollover_control","recovery.history_query","recovery.text_range"],"event_encodings":["json"],"max_inbound_event_bytes":16777216}}
{"jsonrpc":"2.0","id":"prompts","method":"prompt.list","params":{"limit":20}}
{"jsonrpc":"2.0","id":"workspaces","method":"workspace.list","params":{"limit":20}}
```

Create a stopped, detached, durable session and request the owner writer on
this connection. Keep `result.session.id`, `result.session.generation` and
`result.attachment.attachment.id` from the reply.

```json
{"jsonrpc":"2.0","id":"create","method":"session.create","params":{"spec":{"execution_host":"daemon","prompt":{"type":"catalog","prompt_id":"PROMPT_ID"},"workspace":{"type":"configured","workspace_id":"WORKSPACE_ID"},"liveness":{"type":"detached"},"persistence":"durable","start_immediately":false,"labels":{}},"requested_mode":"owner_read_write","subscribe":true,"idempotency_key":"recovery-tutorial-create"}}
```

Enable recovery on that session, then start it. Replace the example generation
`0` with the generation from creation. An enable response identifies the
first window. Wait for the session to become idle before sending the first
message.

```json
{"jsonrpc":"2.0","id":"enable","method":"session.recovery.enable","params":{"session":{"server_id":"SERVER_ID","session_id":"SESSION_ID"},"attachment_id":"WRITER_ID","expected_generation":0}}
{"jsonrpc":"2.0","id":"start","method":"session.start","params":{"session_id":"SESSION_ID","attachment_id":"WRITER_ID","queue_if_limited":false,"idempotency_key":"recovery-tutorial-start"}}
```

A subsequent reconnect needs a new current attachment. See the
[stdio client guide](../../../agent-server/tutorials/stdio-client.md#gateway-to-a-detached-daemon)
for connection and event handling and the
[window protocol](../../../agent-server/operations.md#local-recovery-windows)
for other ways to read the lineage.

### Select capabilities in your own definition

The [example definition](agent.chatmd) imports the reviewed guidance with
`<doc src="builtin:context-recovery"/>`. Add native tools as separate
declarations. A simple local note workflow selects `recovery_note_list`,
`recovery_note_write`, and `recovery_note_read`. This example adds
`context_checkpoint` and `context_new_window` for rollover and
`context_list_windows`, `context_search`, and `context_read_text` for
recovery. It also selects `context_remaining` for capacity information.
The host's capacity policy, the per-session enrollment and the current
tool permissions each have their own checks.

The [moderator](moderator.chatml) uses the
[typed recovery interface](../../libraries/recovery-rollover/src/recovery_rollover.chatmli)
and combines its before and after decisions with Review. Copy the
[project declaration](chatml.project.json) with the source tree to retain
ordinary module resolution. See
[window tools and hooks](../../../development/context-window-tools.md#authored-rollover-hooks)
for the phase contract.

## Work through the two windows

Send the first message through the gateway and wait for the reply to finish.
Use a new application key for the second message. A send acknowledgment
confirms disposition. Follow the session events to observe each response.

```json
{"jsonrpc":"2.0","id":"decision","method":"session.send_message","params":{"session_id":"SESSION_ID","attachment_id":"WRITER_ID","content":{"kind":"plain_text","text":"Decision α-lantern belongs to the blue review.","attachments":[]},"idempotency_key":"recovery-tutorial-decision"}}
{"jsonrpc":"2.0","id":"recover","method":"session.send_message","params":{"session_id":"SESSION_ID","attachment_id":"WRITER_ID","content":{"kind":"plain_text","text":"Checkpoint this review, open a new window, and recover the earlier decision.","attachments":[]},"idempotency_key":"recovery-tutorial-recover"}}
```

For a visual transcript, keep the writer gateway open and connect
[the TUI](../../../bin/chat_tui.doc.md) in read-only mode:

```sh
chat-tui --no-config --connect "unix://$OCHAT_RECOVERY_DEMO/learning/context-recovery/agent.sock" \
  --session SESSION_ID --read-only
```

The foreground uses `recovery_note_list` to get the current catalog revision,
creates `handoff/review` with a short local note, and lists again to obtain
its exact ID and revision. It registers that selection with
`context_checkpoint` using the current window and a new key. An `accepted`
checkpoint receipt records the selected revisions and current history cut.
Use it at the next settled batch while the cut still applies.

`context_new_window` requests the transition. Its pending receipt means the
host durably admitted the request. The successor is confirmed by
`context_list_windows`, which shows the sealed predecessor and the new open
window. Before the cut the moderator requires a qualified checkpoint. After
the cut it combines the recovery preference with Review's independently
recorded lifecycle decision. The host schedules the eligible successor after
both phase receipts. Review's state advances in the successor and is stored
with the moderator's normal checkpoint.

In the new window the agent can read the selected local note. Search the sealed
predecessor's canonical user history for `α-lantern`, using limit 1. Use the
returned citation and part with `context_read_text`. For a canonical item set
`preparation_revision` to null. The exact start and stop are half-open UTF-8
byte offsets. The two UTF-8 bytes of `α` must stay together. When a result supplies
`next_offset`, keep the citation, part and stop and use the returned offset
as the next start. The recovered result is the original user text at that
range. The optional note hint is guidance data. To verify the evidence independently, use the [operator text-read API](../../../agent-server/operations.md#read-an-exact-recovery-text-range) with the same citation and range. [Selected history tools](../../../development/recovery-history-tools.md)

## Interpret a missing or interrupted step

- A stale or missing checkpoint leaves the ordinary request incomplete.
  Read back the original checkpoint with the same key and window,
  `mode: "readback"` and `notes: []` if its acknowledgement was lost. Check
  the current cut before choosing a new key and registering again. A
  checkpoint receipt alone leaves the active window unchanged.
- This moderator vetoes an unqualified cut. With a different authorized
  policy, an archive-only cut can still seal the old window and expose
  independently readable history. It has no qualified checkpoint selection.
  The helper's `Allow_archive` policy allows the host to consider that
  route and its after-policy waits for a separate next step.
- A veto or failure before publication leaves the old window active. A failed
  or interrupted after-hook can leave a committed successor waiting for work.
  Inspect the recorded phase and window; current authorization governs
  continuation. The stored phase receipt lets a later reader distinguish
  pending, completed and interrupted work. [Hook semantics](../../../design/local-context-rollover.md#chatml-hooks-and-continuation)
- The note stays local. Sharing a pinned revision requires a separate,
  authorized source-owner grant to the recipient session. The captured
  builtin and ordinary module imports grant only their source content.
  [Explicit note sharing](../../../development/recovery-note-shares.md)

## Compare with other ways to use history

Use [summary compaction](../../../context_compaction/compactor.doc.md) to
condense a working conversation, and the
[TUI's message search](../../../guide/chat_tui.md#search-prompt-mode-entered-via--or--in-normal)
to find a message in the displayed history. Use a recovery query to search
an authorized retained window and obtain an exact citation for a separate read.
The [operator guide](../../../development/operator-recovery-rollover.md#host-lifetime-and-provider-transport)
explains which windows survive local process and daemon exits.

## Run the deterministic fixture

The test loads the checked-in ChatMD and paired modules through the actual
daemon, saves a local note, commits a checkpointed rollover, and reads the
exact original text in bounded chunks. It also reads the persisted before and
after decisions and verifies Review's successor state.

```sh
env -u OPENAI_API_KEY -u OPENAI_KEY dune build @test/agent_server_e2e/agent-e2e-recovery-authored-example
```

The case is `daemon-recovery.authored-example`. The broader
`@test/agent_server_e2e/agent-e2e-recovery` alias runs the supported recovery
workflow cases.
