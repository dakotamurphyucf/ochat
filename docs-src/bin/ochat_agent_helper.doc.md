# ochat-agent-helper

This executable transports one JSON request over a host-lent private pipe pair.
It is a foundation for the session-management script/CLI bridge. The current
ChatMD runtime does not yet install this channel automatically; launching the
binary from an ordinary shell reports an unavailable channel. It does not obtain
operator credentials or connect to the daemon's control socket.

Build with `dune build bin/ochat_agent_helper.exe`. Installation provides the
`ochat-agent-helper` command. The command accepts no arguments. It reads a JSON
value from standard input until EOF, compacts it, exchanges one line-framed
request/response on inherited pipe descriptors 3/4, validates the returned JSON,
and prints that JSON to standard output with a trailing newline.

Defaults allow 16 MiB of input and 1 MiB of response. A host channel may impose
smaller limits. Invalid JSON, missing pipes, truncated responses and exceeded
limits produce a fixed diagnostic on standard error and exit status 1. Payloads
and exception details are excluded from these diagnostics. A valid application
failure returned as JSON is still a successful transport exchange (exit 0);
callers must inspect the application result.

## Host integration boundary

`Shell_access.Request_channel.create` accepts bounded frames and a trusted
handler. `Executor.with_request_channel` lends that handler to a single-process
execution. The executor requires built-in verified Seatbelt or bubblewrap
confinement, required sandbox mode, disabled network and privilege changes, and
the trusted [resource runner](ochat_shell_resource_runner.doc.md). Direct,
external, simulated and pipeline execution cannot host this channel.

The host authorizes the final execution context after ordinary authorization and
again before spawn. It must validate the executable, environment and effective
filesystem access, including the backend's implicit system roots, against its
private credentials and control endpoints. The transport cannot infer where
those resources reside. A required sandbox by itself is not proof that a
particular host's credentials are inaccessible.

The resource runner closes unrelated inherited descriptors before loading the
helper. The host checks channel authority before each request and again before
disclosing the handler's response. Invocation cancellation or helper exit cancels
and joins a suspended handler. The handler must implement its own durable
operation semantics: losing a reply does not imply that an admitted operation
was rolled back. Channel traffic counts toward shell activity; the ordinary
invocation wall deadline still applies.

The channel has no session authority of its own. A future session host binding
must supply the current caller's authorized
[session-management adapter](../agent-server/extensibility-foundations.md),
operation grants and revalidation. Establishing a new shell execution scope
clears an existing channel so a different caller cannot inherit its handler.
Helper exit does not itself request a managed child session to stop.

## Qualification

`@test/runtest-request_channel_integration_test` launches the compiled helper
and resource runner through a real platform sandbox. It covers JSON exchange,
large escaped Unicode payloads, unsafe backend rejection, byte limits,
revocation before response, cancellation and process reaping, helper exit during
a suspended handler, and attempts to access a private fixture file, Unix socket,
parent environment marker and non-close-on-exec file descriptor.

This proves the transport against that fixture, not a completed session host
binding. The lifecycle bridge with native lifecycle registrations absent and
authored persistent-agent tools remain separate qualification work. Linux needs
an installed, usable bubblewrap; unavailable confinement fails the test rather
than substituting an unconfined or fake process.
