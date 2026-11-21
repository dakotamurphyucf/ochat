# Recover an earlier decision across local windows

This example saves a per-session handoff, opens a new context window and
recovers the original wording of a decision. The same moderator continues
the existing [Review component](../lifecycle-composition/components/review/src/review.chatml)
after the transition. The [paired recovery module](../../libraries/recovery-rollover/src/recovery_rollover.chatmli)
decides what to do with typed host events, and the project resolves all three
dependencies through ordinary local ChatML project declarations.

## Set up a private daemon

Run from the Ochat checkout with the [repository toolchain](../../../agent-server/quickstart.md).
The default profile uses OpenAI Responses with `gpt-4.1`. For live use, set the
daemon's provider credentials as described in the quickstart. The sample numeric
policy configures this tutorial's target and uses local token bounds. Keep the
prompt and model aligned with that policy. The offline fixture below uses a
deterministic loopback provider and needs no account.

Preserve the relative project paths when you copy the tutorial. The configuration
uses the private copy of the example directory as a read-only workspace and places
session data and the Unix socket beside it. The short temporary path keeps the socket
address within the operating system limit. The daemon requires the socket
parent to be private.

```sh
OCHAT_RECOVERY_DEMO=$(mktemp -d /tmp/ochat-rec.XXXXXX)
mkdir -p "$OCHAT_RECOVERY_DEMO/learning/lifecycle-composition/components" "$OCHAT_RECOVERY_DEMO/libraries"
cp -R docs-src/examples/learning/context-recovery "$OCHAT_RECOVERY_DEMO/learning/"
chmod 700 "$OCHAT_RECOVERY_DEMO/learning/context-recovery"
cp -R docs-src/examples/learning/lifecycle-composition/components/review \
  "$OCHAT_RECOVERY_DEMO/learning/lifecycle-composition/components/"
cp -R docs-src/examples/libraries/lifecycle docs-src/examples/libraries/recovery-rollover \
  "$OCHAT_RECOVERY_DEMO/libraries/"
OCHAT_RECOVERY_CONFIG="$OCHAT_RECOVERY_DEMO/learning/context-recovery/server.sexp"
dune exec bin/ochat_agent_server.exe -- -config "$OCHAT_RECOVERY_CONFIG" -validate-only
dune exec bin/ochat_agent_server.exe -- -config "$OCHAT_RECOVERY_CONFIG"
```

Connect a protocol client over the Unix socket
`$OCHAT_RECOVERY_DEMO/learning/context-recovery/agent.sock`. Use an owned
local identity with create, start, message-send and transcript scopes. Keep
the daemon running while you work. The installed NDJSON gateway also works
with a client that keeps one bidirectional connection open.

```sh
dune exec bin/ochat_agent_stdio.exe -- --connect \
  "unix://$OCHAT_RECOVERY_DEMO/learning/context-recovery/agent.sock"
```

1. Initialize with `recovery.windows`. Discover the catalog IDs for prompt
   `local-recovery` and workspace `recovery-review`.
2. Create a stopped, detached, durable daemon session with that prompt and
   workspace. Request an `owner_read_write` attachment and keep this connection
   open. Use the returned attachment ID, session ID and generation, and the
   initialized server ID.
3. Call `session.recovery.enable` before `session.start`. Set
   `session` to `{"server_id":"…","session_id":"…"}`, `attachment_id` to the
   current writer, and `expected_generation` to the returned generation.
   The response gives the first recovery window. Start and send the messages
   below from the same authorized connection. [Recovery window protocol](../../../agent-server/operator-contracts.md#local-recovery-windows)
4. Optionally negotiate `recovery.rollover_control` on initialization so the
   owner can inspect the exact transition after a disconnect or failed step.
   [Operator status and receipts](../../../development/operator-recovery-rollover.md)

The `<doc src="builtin:context-recovery"/>` import captures guidance as part
of this definition. Each `<tool>` selects a capability separately. The two
host controls are the capacity policy and the per-session opt-in.

## Work through the two windows

Send the first message and wait for its reply.

> Decision α-lantern belongs to the blue review.

Then send the second message.

> Checkpoint this review, open a new window, and recover the earlier decision.

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
byte offsets. The first two bytes contain `α`. When a result supplies
`next_offset`, keep the citation, part and stop and use the returned offset
as the next start. The recovered result is the original user text at that
range. The optional note hint is guidance data. [Selected history tools](../../../development/recovery-history-tools.md)

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

## Run the deterministic fixture

The test loads the checked-in ChatMD and paired modules through the actual
daemon, saves a local note, commits a checkpointed rollover, and reads the
exact original text in bounded chunks. It also reads the persisted before and
after decisions and verifies Review's successor state.

```sh
env -u OPENAI_API_KEY -u OPENAI_KEY dune build @test/agent_server_e2e/agent-e2e-recovery
```

The authored case is `daemon-recovery.authored-example`. The alias also runs
the existing recovery cases.
