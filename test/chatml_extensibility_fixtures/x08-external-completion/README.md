# X08: external completion through a registered data event

This source bundle is qualified with a daemon using the extensibility-v1 host
services. General model-facing availability remains gated by the authoring and
host-integration phases. The offline test runs a fake provider; it does not call
an LLM service.

The agent's `watch` tool creates a subscription and registers an `external.report`
event schema. Its initial Pending acknowledgement exposes the registration ID
and namespace. An external producer sends a JSON object with a string `value`.
The moderator checks the registration, completes the subscription, and publishes
that value as a later notification requesting one model turn.

Keep `agent.chatmd`, `any.json` and `string.json` together. Use the session ID from
session creation and the registration ID from the public `watch` tool output.
Neither ID is a credential: the producer must authenticate as the registered
principal, have `ingress.submit`, and negotiate protocol 1.1. The ordinary endpoint
authentication policy still applies. The fixture explicitly narrows helper
connections to submission permission; stock same-user Unix access is not itself
a per-agent permission boundary.

The existing public stdio gateway is a helper without a new privileged builtin:

```sh
ochat-agent-stdio --connect unix:///private/ochat/agent.sock < requests.ndjson
```

For authenticated HTTP, use the normal `--connect` endpoint and
`--bearer-token-file` option. Keep credentials out of the event payload. The
Unix path above is illustrative; select the actual configured private endpoint.

Prepare `requests.ndjson` with these two requests, substituting the actual session
and registration IDs:

```json
{"jsonrpc":"2.0","id":"init","method":"protocol.initialize","params":{"implementation":{"name":"completion-helper","version":"1"},"protocol_min":{"major":1,"minor":1},"protocol_max":{"major":1,"minor":1},"features":[],"event_encodings":["json"],"max_inbound_event_bytes":1048576}}
{"jsonrpc":"2.0","id":"submit","method":"ingress.submit","params":{"version":1,"session_id":"SESSION_ID","registration_id":"REGISTRATION_ID","namespace":"external.report","idempotency_key":"worker-run-42","payload":{"value":"Build passed"}}}
```

The gateway writes structured protocol responses to stdout and diagnostics to
stderr. Acceptance means the event and its queue frame were saved. The moderator
and model may run later. Retrying the same key and JSON data returns the original
acknowledgement even after subscription completion, provided producer/source/
generation, registration lifetime and revocation checks still pass. Changing the
payload conflicts; changing the key admits different work only while the
subscription and registration remain eligible.

`test/chatml_composition/ingress_socket_tests.ml` compiles these exact files,
creates the session over a real peer-authenticated Unix socket, and obtains the
registration from the attached client's public tool output. It runs the actual
`ochat-agent-stdio` executable in two independent helper processes and checks:

- Submission-only access cannot read the transcript or claim a producer in JSON.
- Wrong namespaces and schema-invalid completions are rejected.
- Native-looking fields remain data and do not resolve permissions.
- A retry across helper reconnect returns the same acknowledgement.
- One queued handler produces one User-role runtime notification and one model
  continuation, with no duplicate completion output.

This demonstrates Unix/helper reconnect. Process-crash interruption, broader host
recovery and authenticated HTTP qualification are tracked separately.
