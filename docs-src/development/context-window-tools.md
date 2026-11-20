# Selected context window tools

In an authored session, select the two capabilities independently. A read-only
workflow can select just the first declaration.

```xml
<tool name="context_remaining"/>
<tool name="context_new_window"/>
```

From the foreground model conversation, call `context_remaining` with `{}`.
On success the result includes the session revision, the current window scope
(or `null`), and capacity status. A current assessment identifies the preparation
it describes. When it includes a numeric count, check whether the evidence is
estimated or measured. A disabled, awaiting, stale or unknown assessment gives
you no numeric basis for deciding to start another window.

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
commits its archive and successor at a safe boundary. Selecting a tool or
reading this guide grants no additional authority.
