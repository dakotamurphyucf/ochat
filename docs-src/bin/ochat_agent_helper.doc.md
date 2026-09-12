# ochat-agent-helper

This executable transports one JSON request over a host-lent private pipe pair.
Internally qualified durable hosts can opt named ChatMD shell tools into the
session-management bridge. A one-off ChatML script can call the same shell tool.
The default host grants no channels; launching the binary from an ordinary shell
reports an unavailable channel. It does not obtain operator credentials or connect
to the daemon's control socket. Public configuration-file/CLI exposure remains
gated by the authoring and composition qualification work.

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
linked [child descriptor cleanup](../lib/shell_access/process_spawn.doc.md). Direct,
external, simulated and pipeline execution cannot host this channel.

The host authorizes the final execution context after ordinary authorization and
again before spawn. It must validate the executable, environment and effective
filesystem access, including the backend's implicit system roots, against its
private credentials and control endpoints. The transport cannot infer where
those resources reside. A required sandbox by itself is not proof that a
particular host's credentials are inaccessible.

Linked child setup closes unrelated inherited descriptors before loading the
sandbox backend and helper. The host checks channel authority before each request and again before
disclosing the handler's response. Invocation cancellation or helper exit cancels
and joins a suspended handler. The handler must implement its own durable
operation semantics: losing a reply does not imply that an admitted operation
was rolled back. Channel traffic counts toward shell activity; the ordinary
invocation wall deadline still applies.

The channel has no session authority of its own. The host binding supplies the
current caller's authorized
[session-management adapter](../agent-server/extensibility-foundations.md),
operation grants and revalidation. Establishing a new shell execution scope clears
an existing channel; the actual caller's adapter then lends a new one. Helper exit
does not itself request a managed child session to stop.

## Durable host opt-in

`Agent_session.Session_management_channel.grant` constructs a trusted host grant
with `tool_name`, `policy_revision`, `allowed`, `limits` and `authorize`. The named
tool must already be an admitted shell tool. `allowed` selects from `Create`,
`Send`, `Read`, `Status`, `Wait`, `Stop`, `Reference` and `Validate`; it does not choose
the tools a child inherits. `Reference` and `Validate` are separate readonly
authoring grants, not implied by permission to read a child session. The request
cannot change the grant. An empty operation list or blank
name/revision rejects, and multiple grants matching one invocation reject.

Pass these grants through `Agent_server.Daemon.options.session_helpers` (default
`[]`) for the internally qualified durable host. The factory installs them through
`Script_tool_calls.with_session_helpers`. Native shell dispatch uses the actual
caller's services, approval store and expiring capability borrow. The shell
executor adapter runs after binding those services, never during tool registration.
Direct model calls and selected-tool ChatML calls share this path. No native
`agent_*` lifecycle tool registration is required.

`authorize` receives the final shell context before spawn and is rechecked for
each request/response. It must account for the actual deployment's executable,
normalized roots, environment and private endpoints as described above. The host
must revise `policy_revision` when this callback's policy changes. Grant names,
operation sets, limits and explicit policy revisions contribute to authored tool
resource fingerprints, including resource-only reconstruction. Changed grants
cannot silently reactivate an old delegated binding with wider services. Stored
child status/history can remain readable without reactivating that binding.

Use the version-1 request envelope documented in the shared adapter guide. Replies
are `Agent_protocol.Invocation.outcome` JSON, whose `type` identifies a completed
value or failure. A completed outer shell invocation can also contain the shell
tool's own structured error if process authorization/spawning failed; check that
before interpreting the helper response. These are separate failure boundaries.

## Authoring requests

An admitted helper can use `operation: "reference"` with the complete
[ochat_authoring_context request](../guide/authoring-context-tool.md) as `arguments`,
or `operation: "validate"` with an `ochat_validate` request. The qualified runtime
must have an authoring target configured, and the helper's host grant must include
the chosen operation. Native authoring tools do not need to appear in the agent's
tool list. Neither operation grants child creation, messaging or other session
management capabilities.

```json
{
  "version": 1,
  "operation": "reference",
  "arguments": {
    "version": 1,
    "operation": "prepare",
    "task": "child_agent",
    "query": null,
    "topic_id": null,
    "features": null,
    "cursor": null,
    "max_tokens": null
  }
}
```

The `Complete` outcome's value is the ordinary reference response or validation
report. Check reference pagination and validation's `valid` field: successful
transport and a completed operation do not mean a package is fully read or a
candidate is valid. Both operations use the invoking scope's selected tools and
target. Reference cursors can continue through subsequent helper invocations in
the same session/generation and runtime service; scope, permissions, target or
service replacement invalidates them. Reissue the original query after restart.
Validation checks captured source without executing initializers, tools or models.

## Qualification

`@test/runtest-request_channel_integration_test` launches the compiled helper
and resource runner through a real platform sandbox. It covers JSON exchange,
large escaped Unicode payloads, unsafe backend rejection, byte limits,
revocation before response, cancellation and process reaping, helper exit during
a suspended handler, and attempts to access a private fixture file, Unix socket,
parent environment marker and non-close-on-exec file descriptor.

`@test/runtest-agent_server_helper_test` runs the compiled helper against a real
persisted daemon with native lifecycle registrations absent. A one-off ChatML
script creates a child; the helper exercises all six operations, readonly grants,
foreign-parent denial, duplicate creation, inherited file-read denial and retained
output after daemon restart. It also distinguishes reading stored child state from
reactivating a binding after host helper policy changes. Its provider is local and
offline. This is not yet the complete moderator-handled X07/X06 composition.

Authored persistent-agent tools and the remaining composition matrix retain their
separate qualification work. Linux needs an installed, usable bubblewrap;
unavailable confinement fails the tests rather than substituting an unconfined or
fake process. The current local process evidence uses macOS Seatbelt.
