# Maintained MCP/OAuth regression tests

Build `test/mcp_oauth_audit/audit_test.exe`, then run the executable directly or
use `dune runtest test/mcp_oauth_audit` when no other Dune invocation is active.

The runner uses injected transport operations, its own stdio child process,
and ephemeral IPv4 loopback listeners. It never contacts an external provider
or starts the interactive PKCE browser/listener flow; PKCE coverage exercises
the token exchange directly. OAuth caches use a private temporary directory,
and modified environment variables are restored before that directory is removed.

Assertions cover pending-RPC drain and late replies; send/wait cancellation;
stdio EOF and invalid JSON; HTTP receiver wakeup; wire versus persisted token
decoding; acquisition, PKCE exchange and both refresh grant paths; discovery
fallback; malformed responses; local timestamp persistence and cache reuse;
failed-refresh reacquisition; cancellation; and stored-secret Authorization.
Additional regressions cover SSE delivery while the body remains open, premature
SSE EOF, HTTP error/malformed-body shutdown, and typed async results during
protected teardown of closed clients or cancelled switches.

This suite does not claim coverage of notification fan-out, live catalog
refresh, interactive browser callbacks, or broader OAuth security design.
