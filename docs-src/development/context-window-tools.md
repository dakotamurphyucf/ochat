# Selected context window tools

An authored session selects each context capability independently. A read-only
workflow can select just `context_remaining`.

```xml
<tool name="context_remaining"/>
<tool name="context_checkpoint"/>
<tool name="context_new_window"/>
```

From the foreground model conversation, call `context_remaining` with `{}`.
On success the result includes the session revision, the current window scope
(or `null`), and capacity status. A current assessment identifies the preparation
it describes. When it includes a numeric count, check whether the evidence is
estimated or measured. A disabled, awaiting, stale or unknown assessment gives
you no numeric basis for deciding to start another window.

To record a checkpoint, first commit the local notes through an authorized
workflow. Select the exact current note IDs and revisions using the local
note tools. Then call `context_checkpoint` from the foreground model before
requesting the transition. It requires both transcript and send authority.

```json
{
  "expected_window": "example-window",
  "key": "handoff-checkpoint-1",
  "mode": "register",
  "notes": [{"id": "1", "revision": "3"}]
}
```

Use a fresh key for a new registration. An exact retry with the same key
reuses the durable receipt. A checkpoint records the selected note revisions
and the current history cut. The tool returns only a status.

- `accepted` confirms the registration was recorded. Rollover still checks the
  original published acknowledgment, the history and the source at settlement.
- `stale` means the expected window, registration or note source no longer
  qualifies, or the key names a different selection.
- `absent` means no receipt for the key was found on readback.
- `uncertain` requires readback after the host recovers its journal. Use
  `"mode": "readback"` and `"notes": []` with the original window and key.

A new user message, unrelated tool effect, note update or source change can
make a previously accepted registration ineligible for rollover. A note write
leaves the checkpoint unregistered. A retry can read the receipt without
creating a fresh checkpoint or claiming that the current history is covered.

To request a transition, pass the string in the returned scope's `window`
field as `expected_window`. The following illustrates the argument shape.
Replace the example ID with the one from the current observation.

```json
{"expected_window":"example-window"}
```

The session needs an enabled local rollover mode. Reading requires the
original caller's transcript scope. Requesting a transition requires its send
scope and a current foreground preparation. On a conflict, inspect the window
again before deciding whether the request still applies. A `pending` receipt
acknowledges durable admission. The old window is still active until the host
commits its archive and successor at a safe boundary. When the checkpoint
is missing or stale, the actor keeps the old window. The host checks the
original invocation authority for every selected operation.
