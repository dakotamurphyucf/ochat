# Inspect and request a local context rollover

Negotiate `recovery.rollover_control` in `protocol.initialize`. The stock daemon
uses the same commands on HTTP and its other supported RPC transports. Keep
`recovery.windows` selected if the client also needs to enable capture or list
sealed windows. Rollover control does not enable recovery on a session.

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
