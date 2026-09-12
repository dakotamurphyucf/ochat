# Moderator-handled session helper

This fixture supplies a moderator-handled `manage_agent`
tool accepting the version 1 session-management envelope. The moderator starts
`session_request` as a background job, returns a pending job reference, then
publishes the decoded result as a correlated runtime notification. The helper
connection can close while the created child remains persisted in the daemon.

`request.chatml` converts structured helper stdout into a standalone tool outcome.
This means the notification's completion matches the job's canonical completion,
rather than relabeling an unrelated or transformed result as that job's output.
`moderator.chatml` retains invocation/job correlation and delivery state.

The integration test writes these sources as `helper-request.chatml`,
`helper-moderator.chatml`, and `helper-any.json`, alongside the parent definition.
The parent must also declare a fixed `session_bridge` shell tool and its shell
manifest. Its trusted host explicitly grants that named tool the scoped helper
channel; the ChatMD declaration alone grants no session-management authority.
Normal daemon hosts configure these grants through
[`server.session_helpers`](../../../docs-src/agent-server/configuration.md).
The private `read_file` dependency permits the fixture's child to receive that
existing binding without widening its filesystem scope.

`test/agent_server_helper_test.ml` runs the compiled helper in the required OS
sandbox with native lifecycle registrations absent. It checks asynchronous
creation, direct and one-off idempotent replay, lifecycle operations, readonly and
foreign-parent denial, inherited file restrictions, one notification across
restart, and rejection of changed helper policy on child reactivation. Providers
are deterministic local fixtures.

The sibling [X06 response watcher](../x06-response-watcher/README.md) composes this
helper with polling jobs, timers and correlated notifications. Both backends
exercise active-watch recovery; compiled-source tests cover the deadline and
policy branches described in X06. Complete standalone example packaging remains
separate from these executable integration fixtures.
