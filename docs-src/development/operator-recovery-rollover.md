# Inspect and request a local context rollover

Negotiate `recovery.rollover_control` in `protocol.initialize`. The stock daemon
uses the same commands on HTTP and its other supported RPC transports. Keep
`recovery.windows` selected if the client also needs to enable capture or list
sealed windows. Rollover control does not enable recovery on a session.

To try a complete authored, checkpointed cut and exact original read, use
the [local recovery tutorial](../examples/learning/context-recovery/README.md).
The [window and history operations](../agent-server/operations.md#local-recovery-windows)
give the negotiated methods for enrollment, retained search and exact reads.

The typed client uses `Agent_client.Recovery_windows.inspect_rollover` and
`Agent_client.Recovery_windows.request_rollover`. Both require initialization on
the host in the session reference.

## Inspect the session

Call `session.recovery.status` with a host-qualified session reference.

```json
{"session":{"server_id":"example-host","session_id":"example-session"}}
```

The caller needs current transcript scope and visibility of the local session.
The response reports `state: "unavailable"` for a visible session without a
current recovery window. A successful available response includes:

- `current` contains the exact `scope` and decimal-string session `revision`.
  An optional `previous` contains the predecessor's scope. Follow the existing
  window-list and history APIs to inspect retained windows or items.
- `capacity` names the selected profile, model and configuration revision when
  those labels are available. An observation reports its `freshness` as
  `preparing`, `effective` or `retained`. A retained measurement describes the
  last completed preparation on that basis. It is historical evidence about
  that request. A later preparation can have different inputs.
- `remaining`, when present, is paired with `evidence` of `measured` or
  `estimated`. It contains one of `available`, `exceeded_by`, or
  `excess_out_of_range`. Counts use decimal strings. No count is reported if
  the bound estimate or policy evidence is unavailable. An `unavailable` reason
  can identify disabled policy, no preparation on this process, a stale
  preparation, or missing target evidence. After restart no prior process's
  provider measurement is restored from a rollover receipt.
- `receipt` contains at most one transition. Without a key this is the
  latest transition from or into the current window. With `idempotency_key`
  the receipt belongs to the current principal's exact key. If that key
  has no receipt, the field is omitted. The key and principal are omitted
  from the response.
- `actions` describes whether the caller can submit a fresh `rollover` and
  whether it can request `force`. Supply the current connection's
  `attachment_id` to obtain writer action hints. A fresh cut must be quiescent
  and the session must have local capacity policy selected. An ordinary
  request can still yield `checkpoint_required` if its checkpoint is absent
  or fails verification. The actor checks authority and state again when
  admitting and committing the request.

The status and receipt carry no transcript, note contents, checkpoint recipe,
private provider account, attachment credential, archive location or internal
operation identifier.

## Submit or reconcile one exact request

Use the `current` field from a fresh available status as `expected`. Send it
and the current writer attachment in `session.recovery.rollover`. For example:

```json
{
  "expected": {
    "scope": {
      "session": {"server_id":"example-host","session_id":"example-session"},
      "generation":0,
      "window":"example-window"
    },
    "revision":"17"
  },
  "attachment_id":"example-writer",
  "idempotency_key":"rollover-2026-11-20-1",
  "force":false
}
```

Submit needs current transcript and send scope, and the actual authenticated
owner writer on this connection. An ordinary rollover requires a current
verified checkpoint. `force:true` separately requires `configuration.admin`
on that same writer. With that authority, a cut can produce an archive-only
successor if checkpoint evidence is missing or no longer qualifies. A valid
checkpoint still takes precedence. The service archives the old window at
the verified quiescent cut. It does not launch a new foreground model request.

The receipt reports:

- `pending` after durable admission, while the old window remains active.
- `committed` once the verified archive and successor are committed. Its
  `successor`, exact `cut_revision` and checkpoint `quality` describe the
  published cut.
- `failed` with a body-free reason such as `checkpoint_required` or
  `basis_changed`. A later quiescent revision can close a pending intent as
  `basis_changed` without publishing a different cut in its place.

A new key with an old scope or revision returns `conflict`. An unauthorized
force returns `permission_denied` before admission. Reusing an admitted key
with changed expected scope, revision or force returns `idempotency_conflict`
to its authorized owner.

If a response is lost, keep the original expected scope, revision, force and
key. Reconnect and initialize on the same host under the same principal.
Inspect with that key to read its durable state, including after a restart.
To resubmit, attach as the current writer and use the original arguments with
the new attachment ID. A terminal exact retry returns the original receipt
without admitting another successor. A pending receipt can be completed only
while the original verified cut still applies. Choose a fresh key and current
cut for a genuinely new request.

## Resolve a suspended follow-up

A moderator can return `Wait` after a window is committed. The successor then
exists, but its next model request can remain pending. Negotiate
`recovery.followup_control` on a connection to the owning host and attach as
the owner writer. Use `session.recovery.followup` to inspect the current phase.

```json
{"action":"inspect","session":{"server_id":"<server>","session_id":"<session>"},"attachment_id":"<owner attachment>"}
```

The reply contains `phase` and `basis`. Copy the complete returned `basis`
object into `expected` for an action. Supply the current attachment ID and one
of `continue`, `wait`, `finish` or `cancel` as `action`. The OCaml client
exposes these operations through `Agent_client.Recovery_windows.followup`.

Use `continue` to request a fresh preparation when the reason for a hold has
been addressed. The actor checks current authority, input and capacity before
dispatch. `wait` suspends eligible follow-up. `finish` ends that follow-up,
and `cancel` can also interrupt preparation. A model request already admitted
uses ordinary operation cancellation. If a connection closes before the
reply, reconnect, obtain a current owner attachment and inspect again. A stale
basis returns a conflict. An `idle` phase means the current window has no
foreground follow-up.

## After an edit or configuration change

- For an edited or withdrawn message, run a fresh recovery query. Read the
  citation and part it returns if the message remains visible. Old citations,
  cursors and effective preparations can lose their source basis. A prior
  snippet, byte range or digest grants no continued access.
  [History edit contract](../agent-server/protocol.md#sessions-and-messages)
- Editing a local note advances its revision. That can invalidate checkpoint
  selection and future reads through a share pinned to the former head.
  Recheck the local catalog before recording a new checkpoint. The source
  owner can explicitly grant the intended current revision again.
  [Note revision and share controls](recovery-note-shares.md)
- A model, profile or effective settings change may leave the target outside
  the host's capacity qualification. Inspect fresh status after the next
  preparation. An unmatched target has no numeric policy and cannot trigger
  automatic rollover. The window ledger and authorized retained reads still
  use local session state.
  [Capacity qualification](../agent-server/configuration.md#local-context-capacity)

## Host lifetime and provider transport

| Host | Retained recovery |
| --- | --- |
| Durable detached daemon | A client can disconnect and reattach. After daemon restart, use the same store and current authority. A visible retained window can still be queried without activating a model run. |
| Durable native `chat-tui --local` or `ochat chat-completion` | Enrollment belongs to the selected process-bound conversation. Use `-enable-recovery` on the initial enrollment, then reopen the same absolute storage root and session ID. Supply the local capacity file again for each process that needs the policy. |
| Transient native host, legacy TUI or remote provider conversation | Use a durable native store or a qualified daemon session for local recovery. A provider conversation or response ID cannot reopen the local ledger. |

See [TUI flags](../bin/chat_tui.doc.md#local-context-recovery),
[managed headless use](../cli/chat-completion.md#capture-recoverable-context)
and the [local capacity file](../agent-server/configuration.md#local-recovery-capacity-file).
Provider-independent ownership refers to local windows, citations and
receipts. Token estimates, triggers and model submission still depend on the
qualified adapter and the resolved target. The stock numeric qualification
described here uses OpenAI Responses.

A fresh successor starts from a new local request envelope with its selected
inputs and bounded checkpoint hints when applicable. It does not use a
predecessor's provider continuation ID. SSE and WebSocket loopback tests
exercise this separation. On a still-live qualified WebSocket, an idle
archive-only cut sends a full successor-local input on the same physical
connection. Ordinary prefix reuse can work within each window. An in-flight
successor can own a new binding. This checks request construction and actual
writes, while live provider behavior depends on your configured profile.
