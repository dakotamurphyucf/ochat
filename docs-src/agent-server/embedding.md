# Embed the agent core in OCaml

Use Core as the standard library and Eio for I/O, switches, fibers and resource
ownership. The installed libraries are `ochat.agent_protocol`, `agent_session`,
`agent_store`, `agent_server`, `agent_client`, `agent_transport_socket`,
`agent_transport_stdio`, `agent_transport_http` and `agent_transport_client`
with the `ochat.` prefix on each public library name.

`Agent_server.Embedded.start` accepts optional `~authoring_package_files` containing
absolute paths to [custom authoring packages](../guide/authoring-context-tool.md).
It validates and captures the complete set before creating a store, using the
same loader and host configuration as a daemon. Package text stays immutable for
the host's lifetime and is visible only through matching selected tool metadata.
This configuration does not grant tools or enable the gated extension rollout.

`~authoring_budget` configures documentation query defaults/ceilings and the total
automatic primer/preload budget. Construct it with
`Chat_response.Authoring_validation.context_budget`; the shared daemon host passes
it to native/helper queries, context insertion and delegated sessions. See the
[budget contract](../guide/authoring-context-tool.md#budgets-and-continuation).

## Client integration

The complete [compiled client](../examples/agent-server/clients/docs_example.ml)
and its [Dune dependencies](../examples/agent-server/clients/dune) demonstrate:

1. Load a bearer-token file through Eio only for HTTP.
2. Parse a Unix/HTTP endpoint with `Agent_transport_client.Endpoint.create`.
3. Connect inside an Eio switch with a bounded notification capacity.
4. Use `Agent_client.Connection.request` for typed commands and
   `next_notification` for asynchronous events, or the shared stdio gateway.
5. Close the connection in `Fun.protect` before leaving the switch.

Initialize explicitly before dispatch. Request/notification handling must not
assume a synchronous next-line response. The client projection/reconnect modules
maintain snapshots, stable identities and event cursors; rendering clients should
keep local drafts separate from server state. Blob downloads verify cursor,
length and digest before atomic installation.

## Embedded session host

`Agent_server.Embedded.start ~sw ~env options` creates the host and initial
session. Options specify absolute prompt/workspace/tool_dir/home context,
optional durable data root, start intent, permission profile, attachment mode and
event capacity. No data root means a private transient root. `data_root = Some`
does not change process-bound liveness into detached daemon liveness.

Trusted embedding applications can also pass `~daemon_options` to install the
shared daemon's provider adapter, policy, reviewer resolvers and runtime options.
The default remains `Daemon.default_options`. Embedded startup derives its host
identity from `data_root` even if those options specify another host, and applies
the configured attachment limit to each in-memory connection. It starts no network
listener. ChatML extensions are enabled by default, with individual tools selected
by the ChatMD definition and constrained by its execution authority. Durable
embedded hosts advertise the same five extension services as the daemon;
transient hosts omit persisted child delegation. An embedding application can
set `qualify_chatml_extensions = false` as a compatibility override, which also
suppresses extension discovery. This override has no CLI/configuration-file flag.

`Embedded.start` initializes the Unix cryptographic RNG before allocating its
transient root or any session IDs. Callers do not need to initialize it first.
Older builds had a [local stdio startup defect](troubleshooting.md#local-stdio-rng-initialization)
when no data root was supplied.

Use `Embedded.session_id`, `attachment`, `connection`, or `connect` as documented
in its interface. Close each extra connection and finally `Embedded.close`;
the switch owns fibers/resources. The [embedded interface](../../lib/agent_server/embedded.mli)
and [offline embedded tests](../../test/agent_server_embedded_test.ml) contain
complete typed lifecycle examples with no real provider calls.

## Daemon embedding

`Daemon.start` owns one durable data root and semantic services; it does not
start transport listeners. Supply validated config, launch `tool_dir`, home and
process identity inside the owning switch. Bind selected adapters to its
dispatcher/registry/authenticator/close callbacks. `Daemon.shutdown` drains and
releases locks; transport owners must close their own connections/listeners too.

`Daemon.options` supports named reviewer, deterministic policy and OAuth
resolvers, as well as a model-stream injection seam. Reviewer implementations
receive redacted invocation data and have immutable security revisions.
Unavailable implementations fail closed; an ID string is not a network endpoint
or downloaded program. OAuth validators return a typed principal with scopes.
Never reuse identity-sensitive state globally across daemon instances.

## Ownership and extension rules

- Session actors serialize state changes and durable commits. Do not mutate a
  store or runtime behind an actor's back.
- Workers execute blocking/model/tool work outside the actor and return results;
  cancellation must propagate through owned fibers and subprocesses.
- Schedulers own quota/start/job/timer capacity and release it on terminal/failure
  paths. A client disconnect is not a detached worker cancellation request.
- Snapshot/replay/live/export visibility uses the principal projection. A new
  adapter must not bypass it or cache unscoped results.
- Store operations return typed errors; preserve corruption versus missing/I/O
  distinctions rather than swallowing all failures as retryable absence.
- Fake provider/clock injection is useful for deterministic tests, not a claim
  that production providers or external MCP servers behave identically.

## API reference and source interfaces

The hosted documentation currently provides integration guides and library
architecture notes. Generated OCaml API pages are deferred for this release.
Use the linked public `.mli` interfaces for exact types, signatures, and lifecycle
contracts; the website's search covers the published guides and library notes.

Start with the [library overview](../lib/README.md) for the wider Ochat library
collection, including ChatMD, ChatML, tools, MCP, and retrieval. For agent hosting,
the map below pairs each architecture guide with a useful interface entry point.
The interface links open the repository source at the website's build revision.

## Library map

See the module inventories, ownership notes, and public interfaces:

- [Protocol](../lib/agent_protocol/architecture.doc.md) — [interface](../../lib/agent_protocol/command.mli).
- [Session actors and runtime](../lib/agent_session/architecture.doc.md) — [interface](../../lib/agent_session/session_actor.mli).
- [Store](../lib/agent_store/architecture.doc.md) — [interface](../../lib/agent_store/session_store.mli).
- [Daemon and authorization](../lib/agent_server/architecture.doc.md) — [interface](../../lib/agent_server/daemon.mli).
- [Client](../lib/agent_client/architecture.doc.md) — [interface](../../lib/agent_client/connection.mli).
- [Unix transport](../lib/agent_transport_socket/architecture.doc.md) — [interface](../../lib/agent_transport_socket/client.mli).
- [Stdio transport](../lib/agent_transport_stdio/architecture.doc.md) — [interface](../../lib/agent_transport_stdio/gateway.mli).
- [HTTP transport](../lib/agent_transport_http/architecture.doc.md) — [interface](../../lib/agent_transport_http/client.mli).
- [Endpoint composition](../lib/agent_transport_client/architecture.doc.md) — [interface](../../lib/agent_transport_client/endpoint.mli).

## Generate API documentation locally

With the project dependencies and `odoc` installed in your active OCaml switch,
run this from the repository root:

```sh
dune build @doc
```

Open `_build/default/_doc/_html/index.html` to browse the generated reference.
This step does not require model credentials. Generation can succeed with
unresolved-reference or markup warnings; local output is not a verified hosted
artifact. The checked-in `docs/` snapshot is historical and does not cover all
current agent libraries.

For the separate installed-package and API-search workflows, see
[the development guide](../../DEVELOPMENT.md). Those workflows have their own
dependencies and, for semantic indexing, provider requirements.
