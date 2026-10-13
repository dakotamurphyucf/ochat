# Managed-session qualification

Run `dune build @agent-managed-qualification` from the repository root to exercise
complete managed-send and authored-pending workflows. These scenarios retain the
former default suite's stdio/HTTP checks, real production named-tool deadlines,
receipt matrices, UTF-8/escaping fragment reconstruction, dispatcher cancellation,
authority revocation, reconnect and daemon restart. Their assertions are unchanged.
The executable's stdout is compared with `expected.output`, preserving the original
expect tests' success evidence.

The ordinary PR suite keeps focused witnesses in
`test/agent_server_managed_send_test.ml` and
`test/agent_server_authored_pending_test.ml`. The stdio witness checks native send,
ChatML retry, independently scoped wait cancellation, native and ChatML output
pages, denied foreign disclosure and stop. The HTTP witness checks concurrent
named children, held-child-read-reservation contention, distinct/coalescing
continuations, cancellation after acceptance, disconnected completion and scoped
reads after reconnect. Both use actual transport connectors and daemon persistence.

HTTP ordinary writers belong to one live connection and session. Disconnect closes
and clears them before closing the connection; reconnect creates fresh writers.
Explicit cancellation handles retain their separate ownership and close behavior.
The two real ten-second named-tool waits remain in the focused HTTP regression.
Component actor, page, cursor and replay tests remain in `test/agent_session`.
