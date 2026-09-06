# Ochat Agent Server Implementation Specification

Status: implementation specification; see the [current user guides](../agent-server/README.md) for executable commands and deployment instructions.

Date: August 15, 2026

Companion architecture: `ochat-agent-server-spec.md`

Audience: Ochat maintainers and contributors implementing the agent server,
transports, clients, persistence layer, and TUI integration

## 1. Purpose

This document specifies how to implement every requirement in
`ochat-agent-server-spec.md` in the current Ochat codebase.

It defines:

- the target OCaml library and module structure;
- the existing code that must be reused, extracted, wrapped, or replaced;
- concrete public and internal types;
- actor ownership and concurrency rules;
- persistent schemas, journal framing, snapshots, indexes, locks, and
  migrations;
- prompt-revision and workspace construction;
- ChatMD, ChatML, shell, tool, model, compaction, and history integration;
- command, event, HTTP, Unix-socket, stdio, and in-memory implementations;
- daemon and embedded execution paths;
- TUI migration and parity work;
- authentication, authorization, permission, job, schedule, and audit
  services;
- failure handling and recovery algorithms;
- tests, implementation phases, rollout gates, and requirement traceability.

The architecture specification remains authoritative for externally visible
behavior. This document is authoritative for implementation shape unless code
discovery during implementation proves that a named internal hook must move.
Any such change must preserve the stated module ownership and invariants and
must update both specifications.

## 2. Implementation rules

### 2.1 Code is the compatibility baseline

The current source is the behavioral baseline for ChatMD parsing, imported
source provenance, ChatML safe points, history identity, response streaming,
tool execution, shell policy, approvals, compaction, and TUI behavior.

Documentation informs intent, but implementation work must verify behavior in
the source and tests before moving it.

### 2.2 Preserve Jane Street style

All new OCaml follows the repository coding guidelines:

- `Core` is the project's standard library; new code uses `Core` APIs and
  conventions instead of treating the OCaml standard library as the default;
- most modules define one principal type named `t`;
- typed `result` errors are preferred over exceptions;
- exception-raising helpers use an `_exn` suffix;
- functions taking a module-owned value take that value first;
- predicate names describe predicates;
- implementation functions should normally remain below 25 lines;
- public invariants and behavior are documented in `.mli` files;
- implementation files avoid inline comments except for genuinely complex
  algorithms that cannot be made clear by naming and decomposition;
- ignored values are explicitly annotated; and
- tests favor deterministic clocks, fake services, and expectable values.

### 2.3 No big-bang replacement

The server is introduced through compatibility adapters and vertical slices.
The current standalone TUI must remain usable throughout the work.

The migration order is:

1. characterize existing behavior;
2. add protocol and persistence foundations;
3. extract a shared session engine behind an in-memory client;
4. switch standalone TUI execution to that engine;
5. add daemon ownership and durable recovery;
6. add Unix-socket and stdio access;
7. add HTTP and SSE;
8. add connected TUI mode; and
9. harden, migrate, and remove temporary compatibility paths.

### 2.4 Deprecated MCP prompt-server architecture remains isolated

No new agent-server transport/session ownership may be based on:

- MCP protocol types as its public agent-server API;
- `Mcp_server_core`;
- `Mcp_server_router`;
- MCP transport session IDs;
- MCP capability names; or
- MCP global notification registries.

Generic Piaf, SSE, OAuth, bearer-token, NDJSON, and Eio techniques may be
copied or extracted into neutral modules. The legacy MCP prompt-server implementation is not
changed merely to adopt the new server architecture.

This restriction does not apply to the actively maintained MCP client/tool
adapter used by ChatMD declarations. That adapter, including its client/types
dependencies and tool discovery, is part of the supported agent runtime.

## 3. Current codebase baseline

### 3.1 Existing components to preserve

| Existing component | Current role | Implementation decision |
|---|---|---|
| `History_entry` | Canonical application-owned history identity and allocator | Reuse directly; make it the only canonical history identity source |
| `Session` V5 | Standalone conversation, moderator, shell, task, and key/value snapshot | Reuse as a compatibility conversation payload; do not make it the daemon's complete state schema |
| `Session_store` | Single-process snapshot loading, saving, reset, rebuild, and migration | Retain for legacy standalone session commands; do not use as the daemon store |
| `Prompt.Chat_markdown` | ChatMD parsing, import expansion, declarations, and source provenance | Reuse directly through a new prompt-revision builder |
| `Chat_response.Agent_runtime` | ChatMD tool and shell runtime construction | Reuse and extend with tool security metadata |
| `Chat_response.In_memory_stream` | Identity-bearing moderated response loop | Reuse as the foreground turn worker |
| `Chat_response.Chatml_turn_driver` | Turn-start, tool, and turn-end safe points | Reuse directly |
| `Chat_response.Moderator_manager` | Durable ChatML state, overlay, queued events, and wakeups | Reuse behind session-actor ownership |
| `Chat_response.Model_executor` | Process-local `Model.spawn` support | Retain for legacy/embedded compatibility; replace with durable job service in durable daemon sessions |
| `Context_compaction.Compactor` | Canonical history compaction | Reuse through a session-owned compaction worker |
| `Shell_runtime` | Manifest policy, exact grants, approvals, execution, audit, and interrupted requests | Reuse; add actor-backed persistence adapters |
| `Chat_tui.App_runtime` and reducer helpers | Current host session controller mixed with UI state | Extract host-owned behavior into the shared session engine |
| `Chat_tui.Model` and renderer/controller modules | TUI presentation and editor state | Keep client-local; adapt to protocol snapshots and events |
| `Mcp_server_http` | Piaf/SSE/OAuth examples | Copy or extract only neutral low-level techniques |

### 3.2 Existing limitations that must not carry forward

The implementation must eliminate these limitations in the new path:

- `Session_store.save` exits the process when a lock is unavailable.
- The TUI persists mostly at shutdown rather than per accepted durable
  mutation.
- TUI model state and host session state are mixed in one mutable record.
- Operation IDs are process-local integers.
- `Model_executor` jobs are process-local and not restart-safe.
- current ChatML approval suspension retains an in-memory evaluator
  continuation that cannot be serialized.
- MCP HTTP sessions and SSE subscriptions are global and process-local.
- current MCP SSE IDs are per connection and have no replay journal.
- the current daemon-like HTTP code has no per-session actor or durable state.
- runtime construction currently defaults `${tool_dir}` to the TUI process
  current directory.
- ChatML debug/print sinks are process-global.
- remote MCP tool discovery uses a process-global cache keyed only by server
  URI; and
- some legacy OAuth and prompt metadata helpers use process-global mutable
  tables unsuitable as daemon ownership boundaries.

### 3.3 Existing behavior that must be characterized before extraction

Before moving controller logic, add or preserve tests for:

- one foreground stream or compaction at a time;
- deferred user-entry allocation and FIFO adoption;
- no insertion between a tool call and its output;
- turn-start, pre-tool, post-tool, turn-end, and idle safe points;
- moderator overlay revision application;
- follow-up turn count and rate-limit behavior;
- cancellation repair of incomplete history;
- startup and resume event choice;
- shell approval blocking and grant persistence;
- moderator approval routing;
- compaction replacement and post-compaction wakeup ordering;
- streamed output, sourced events, tool progress, and Agent-page metadata;
- startup failure behavior; and
- TUI export and legacy snapshot behavior.

## 4. Target library graph

### 4.1 New libraries

Introduce these wrapped libraries:

```text
ochat.agent_protocol
ochat.agent_store
ochat.agent_session
ochat.agent_server
ochat.agent_client
ochat.agent_transport_socket
ochat.agent_transport_stdio
ochat.agent_transport_http
```

Their dependency direction is:

```text
agent_protocol
    ^
    |
agent_store      chat_response / chatmd / chatml / shell_runtime
    ^                         ^
    |                         |
    +-------- agent_session --+
                    ^
                    |
              agent_server
                    ^
                    |
       +------------+-------------+
       |            |             |
 transport_socket transport_http daemon executable

agent_protocol <--- agent_client <--- TUI / stdio gateway
                       ^
                       |
                 in-memory adapter

transport_socket ----+
                     +--- agent_transport_client ---> TUI / stdio gateway
transport_http ------+
```

`agent_protocol` must not depend on Eio, Piaf, ChatMD parsing, the TUI, or
server runtime modules. It may depend on `Core`, `Jsonaf`, `History_entry`, and
dependency-light shared types needed for snapshots.

`agent_store` depends on `agent_protocol`, Eio/Core Unix, Bin_prot, and
Digestif. It must not depend on the TUI, HTTP, or live Chat_response runtime.

Eio is the mandatory runtime abstraction throughout all new implementation
libraries and executable integration points. Store, session, server, client,
transport, and TUI-adapter modules use `Eio.Path`, `Eio.File`, `Eio.Flow`,
`Eio.Net`, switches, fibers, mutexes, promises, clocks, and bounded streams for
ordinary operations. `Core` is the project standard library, but its blocking
Unix/file/socket APIs are not an alternative runtime substrate.

A narrow Unix backend may access an Eio-owned file descriptor only for host
primitives Eio does not expose directly, such as advisory process locks or
directory `fsync`; those calls run through the Eio Unix system-thread
boundary. New runtime modules must not independently open, read, write,
append, rename, truncate, stat, remove, connect, accept, sleep, or wait using
blocking `Core_unix` or Stdlib APIs. Existing legacy code may be migrated at
an integration seam, but no new agent-server path may introduce blocking IO.
`agent_protocol` is intentionally pure and remains the only new layer that
does not depend on Eio.

`agent_session` depends on the protocol, store interfaces, ChatMD, ChatML,
Chat_response, Shell_runtime, Session, and context compaction. It must not
depend on a network transport or TUI rendering module.

`agent_server` composes configuration, catalogs, registry, scheduler,
authentication, and listeners. Transport implementations depend on the
server's dispatch interface, never on session mutable internals.

`agent_client` depends on protocol types and abstract transport interfaces. It
must not depend on server internals.

### 4.2 New source directories

Create:

```text
lib/agent_protocol/
lib/agent_store/
lib/agent_session/
lib/agent_server/
lib/agent_client/
lib/agent_transport_socket/
lib/agent_transport_stdio/
lib/agent_transport_http/
```

Each directory gets a dedicated `dune` file and public library stanza.

### 4.3 Executables

Add:

```text
bin/ochat_agent_server.ml
bin/ochat_agent_stdio.ml
```

Expose installed commands:

```text
ochat-agent-server
ochat-agent-stdio
```

Also add `agent` subcommands to the existing `ochat` command group after the
new binaries are stable:

```text
ochat agent server
ochat-agent-stdio
ochat agent sessions ...
```

The dedicated executables remain useful for process supervision and editors.
CLI behavior is a frontend to the same library APIs.

### 4.4 TUI dependency change

`ochat.chat_tui` must add a dependency on `ochat.agent_client` and eventually
stop depending on session-engine internals. During migration it may also use
`ochat.agent_session` only to construct an embedded in-memory server adapter.

The final renderer, controllers, editor, and presentation model must depend
only on client snapshots/events and TUI-local types.

## 5. Protocol foundation

### 5.1 Module list

`lib/agent_protocol` contains at least:

```text
version.ml/mli
id.ml/mli
timestamp.ml/mli
error.ml/mli
idempotency_key.ml/mli
mutation_result.ml/mli
blob.ml/mli
history.ml/mli
initialize.ml/mli
ping.ml/mli
health.ml/mli
principal.ml/mli
scope.ml/mli
page.ml/mli
prompt.ml/mli
workspace.ml/mli
permission.ml/mli
job.ml/mli
schedule.ml/mli
operation.ml/mli
session.ml/mli
event.ml/mli
command.ml/mli
snapshot.ml/mli
method_result.ml/mli
envelope.ml/mli
json_codec.ml/mli
```

Avoid a single monolithic protocol type file. Each module owns one coherent
type family and its JSON codec.

### 5.2 Opaque identifiers

Define distinct abstract ID modules for:

- server;
- session;
- attachment;
- operation;
- event cursor;
- transaction;
- job;
- schedule;
- permission;
- grant;
- workspace definition;
- workspace instance;
- prompt definition;
- prompt revision;
- principal;
- blob; and
- idempotency record.

Each ID module exposes:

```ocaml
type t [@@deriving compare, hash, sexp]

val create : unit -> t
val of_string : string -> (t, Error.t) result
val to_string : t -> string
```

IDs created by the server use cryptographically secure random bytes encoded
with a filesystem- and URL-safe alphabet. They must not use `Random.int`.
Session and storage directory IDs are validated against a strict maximum
length and character set before path construction.

Tests may inject deterministic ID generators through a `Generator.t`
interface.

### 5.3 Versions

`Version.t` contains:

```ocaml
type t =
  { major : int
  ; minor : int
  }
[@@deriving compare, equal, sexp]
```

The initial protocol version is `1.0`. `Version.negotiate` accepts client
minimum/maximum versions and server-supported versions, rejects major-version
mismatch, and chooses the highest compatible minor version.

Feature names are stable lowercase dotted strings. Features are additive
within a protocol major version.

Authorization scopes use the stable wire identifiers specified in
architecture section 30.2. `Scope.of_string` rejects unknown values and scope
set decoding rejects duplicate entries.

### 5.4 Errors

`Agent_protocol.Error.t` is the only error shape crossing a transport:

```ocaml
type t =
  { code : code
  ; message : string
  ; retryable : bool
  ; data : Jsonaf.t
  }
```

The `code` variant includes every architecture error plus implementation
errors for:

- incompatible protocol;
- cursor expired;
- blob unavailable;
- lease stale;
- configuration invalid;
- store locked;
- store schema too new;
- migration required;
- journal corrupt;
- server shutting down; and
- command queue full.

Internal exceptions are converted at the owning boundary. Transport code must
not expose OCaml exception strings unless the principal has diagnostic scope
and the server is in an explicit development mode.

### 5.5 JSON coding

Use explicit codecs rather than relying on derived JSON field names as a wire
contract. `Json_codec` provides:

- object-field lookup with duplicate-field rejection;
- bounded integer parsing;
- required and optional field helpers;
- unknown-required-feature rejection;
- enum decoding with structured errors;
- payload depth and size checks; and
- stable canonical JSON generation for hashes and idempotency comparison.

Decoders accept unknown optional object fields for minor-version forward
compatibility. They reject unknown enum values when semantics would be
ambiguous.

Durable counters, revisions, lease generations, and event positions use
nonnegative `int64` values. Their JSON representation is an integer number,
not a quoted decimal string. Decoders reject negative values, fractions,
overflow, and exponent spellings that do not parse as exact OCaml `int64`
integers.

`Idempotency_key.t` validates the Protocol 1.0 syntax from architecture
section 23.3. Command records use this type rather than unvalidated strings.

### 5.6 Command representation

Internally decode methods into a closed variant:

```ocaml
type t =
  | Protocol_initialize of Initialize.Request.t
  | Protocol_ping of Ping.Request.t
  | Server_info
  | Server_health of Health.Request.t
  | Prompt_list of Prompt.List_request.t
  | Prompt_get of Prompt.Get_request.t
  | Workspace_list of Workspace.List_request.t
  | Workspace_get of Workspace.Get_request.t
  | Session_create of Session.Create_request.t
  | Session_list of Session.List_request.t
  | Session_get of Session.Get_request.t
  | Session_attach of Session.Attach_request.t
  | Session_detach of Session.Detach_request.t
  | Session_renew_owner of Session.Renew_owner_request.t
  | Session_start of Session.Start_request.t
  | Session_stop of Session.Stop_request.t
  | Session_cancel_operation of Session.Cancel_operation_request.t
  | Session_send_message of Session.Send_message_request.t
  | Session_compact of Session.Compact_request.t
  | Session_export of Session.Export_request.t
  | Session_reset of Session.Reset_request.t
  | Session_rebuild of Session.Rebuild_request.t
  | Session_upgrade_prompt of Session.Upgrade_prompt_request.t
  | Session_delete of Session.Delete_request.t
  | Blob_read of Blob.Read_request.t
  | Permission_list of Permission.List_request.t
  | Permission_respond of Permission.Respond_request.t
  | Grant_list of Grant.List_request.t
  | Grant_revoke of Grant.Revoke_request.t
  | Audit_read of Audit.Read_request.t
  | Job_list of Job.List_request.t
  | Job_get of Job.Get_request.t
  | Job_cancel of Job.Cancel_request.t
  | Schedule_list of Schedule.List_request.t
  | Schedule_get of Schedule.Get_request.t
  | Schedule_create of Schedule.Create_request.t
  | Schedule_cancel of Schedule.Cancel_request.t
```

Method dispatch must pattern-match on this variant. Session mutation is never
implemented by matching raw method strings inside a transport.

## 6. Configuration implementation

### 6.1 Modules

Add to `agent_server`:

```text
config.ml/mli
config_parser.ml/mli
config_validator.ml/mli
config_diff.ml/mli
config_watcher.ml/mli
```

### 6.2 Parsed and validated forms

Use separate types:

```ocaml
module Raw_config : sig
  type t
end

module Config : sig
  type t
end
```

`Raw_config.t` preserves source locations and unresolved paths.
`Config.t` contains only normalized, validated values.

Every diagnostic contains:

- stable code;
- config path;
- source file and S-expression location when available;
- message; and
- remediation.

### 6.3 Path resolution

Resolve configuration-relative paths relative to the configuration file, not
the daemon process current directory. Expand `~` only in the config parser and
record the resulting absolute path.

Normalize:

- data directory;
- Unix socket path;
- prompt files;
- physical workspace roots;
- temporary workspace roots;
- TLS files if later enabled;
- token/credential files; and
- external reviewer endpoints.

### 6.4 Validation phases

Validation proceeds in this order:

1. syntax and schema version;
2. field-level ranges and enums;
3. duplicate identifier checks;
4. cross-reference checks;
5. path normalization and existence checks;
6. prompt parse and revision preflight;
7. workspace canonical identity checks;
8. permission profile compilation;
9. listener and authentication safety checks;
10. quota consistency checks; and
11. data-directory/store compatibility preflight.

The daemon does not bind listeners or mutate the store until validation
succeeds.

### 6.5 Reload

`Config_watcher` may poll or use a platform watcher, but reload installation is
transactional:

1. read a complete new file;
2. parse and validate it independently;
3. construct a `Config_diff.t`;
4. ask affected services to prepare changes;
5. atomically swap the current immutable config reference;
6. commit service changes; and
7. emit an administrative audit event.

Reload failure leaves the old config active.

Existing sessions retain pinned prompt revision, workspace instance,
permission profile revision, and liveness settings. Lowered quota limits block
new acquisitions but do not forcibly evict existing sessions unless an
explicit administrative command is later added.

## 7. Prompt catalog and revision artifacts

### 7.1 Modules

Add to `agent_session`:

```text
prompt_definition.ml/mli
prompt_catalog.ml/mli
prompt_revision.ml/mli
prompt_revision_builder.ml/mli
prompt_artifact_store.ml/mli
```

### 7.2 Prompt definition

The validated prompt definition contains:

```ocaml
type t =
  { id : Prompt_id.t
  ; root_file : string
  ; allowed_workspaces : Workspace_id.Set.t
  ; permission_profile : string
  ; runtime_policy : Runtime_policy.t
  ; enabled : bool
  ; description : string option
  }
```

### 7.3 Revision builder

`Prompt_revision_builder.build` performs all prompt work before session runtime
construction:

1. load the root source bytes;
2. determine canonical root path and prompt directory;
3. parse through `Prompt.Chat_markdown.parse_chat_inputs` with explicit
   `~source` and `~dir`;
4. retain import-expanded declaration provenance;
5. collect root, imported, ChatML, shell script, and other referenced source
   material;
6. resolve and capture local nested-agent prompt references and local
   document/image inputs required to reconstruct initial prompt history,
   subject to configured file-count and byte limits;
7. compute SHA-256 for every source/resource;
8. compile/validate moderator scripts;
9. call `Chat_response.Agent_runtime.inspect_shell` without authorizing or
   instantiating a runtime;
10. compute the shell manifest digest and built-in versions;
11. validate duplicate tools, script IDs, and moderator declarations;
12. encode a deterministic manifest of the source/resource closure; and
13. derive the immutable revision ID from that manifest.

The builder returns a prepared revision or typed diagnostics. It does not
create shell processes, request approvals, execute tools, or mutate sessions.

### 7.4 Artifact contents

Persist each revision under a content-addressed prompt artifact directory:

```text
<data-dir>/prompt-artifacts/<revision-id>/
  manifest.sexp
  root.chatmd
  sources/<digest>
  source-map.sexp
  tree/
```

The manifest records:

- prompt definition ID;
- revision ID;
- canonical source identity;
- root source digest;
- ordered imported source descriptors;
- declaration source map;
- ChatML source digests and script IDs;
- local nested-agent prompt identities;
- captured initial document/image resource identities and media types;
- parser/runtime schema versions;
- shell manifest digest and built-in profile versions;
- creation timestamp; and
- artifact schema version.

Source-map entries map logical/canonical source names to content-addressed
files. A restored durable session must be able to parse its pinned revision
without depending on the operator's current prompt file contents.

### 7.5 Artifact-backed source loading

Add a source-loading interface to `chatmd`:

```ocaml
module Source_loader : sig
  type t
  type source

  val filesystem : root:Eio.Fs.dir_ty Eio.Path.t -> t
  val root : t -> file:string -> (source, Error.t) result
  val resolve : t -> base:source -> reference:string -> (source, Error.t) result
  val read : t -> source -> (string, Error.t) result
  val file_name : source -> string
  val materialized_dir : source -> Eio.Fs.dir_ty Eio.Path.t
end
```

Thread the loader through ChatMD import expansion and external ChatML/shell
script source loading. The filesystem loader preserves current standalone
behavior. The artifact loader resolves the recorded source graph and serves
captured bytes.

The artifact store creates owner-read-only files in a `tree/` materialization with one stable
directory per captured source node. `Source_ref.file`, `source_dir`, and
`prompt_dir` for daemon runtime point at this materialized tree, while the
manifest separately retains original logical/canonical source identities for
diagnostics and trust comparison.

Directories remain owner-managed for pruning. Before parsing, verify the
materialized inventory and digests against the artifact, rejecting missing,
altered, unexpected and symlinked files. Serve parser source reads from the
verified captured bytes, not a second unchecked filesystem read. This is
load-time integrity verification, not an OS boundary against the daemon account;
keep its data root outside tool write authority. Existing artifacts need not
change permissions to receive these checks.

The loader, not untrusted source text, maps an import or `src` edge to its
captured node. Absolute and `..` references cannot escape the artifact. A
missing edge is `prompt_unavailable` rather than a fallback read from the
daemon cwd.

Only the captured prompt source closure and explicitly referenced script
resources are materialized. Selecting `${source_dir}` as a tool root therefore
exposes the pinned artifact source directory, not an uncaptured copy of every
sibling file from the operator's original directory. Prompts needing project
files should refer to `${workspace}` or another explicitly declared root.

### 7.6 Catalog caching and reload

`Prompt_catalog.t` stores immutable prepared revisions keyed by prompt ID and
revision ID. It may cache parsed prompt elements and compiled moderator
artifacts in memory.

A catalog reload builds new revisions before publishing them. Invalid prompts
become unavailable for new sessions while prior pinned artifact directories
remain usable.

Artifact garbage collection may remove a revision only when no session,
archive, export, or retained audit reference uses it.

Prompt parsing and revision construction must be reentrant. Do not rely on
`Prompt.Metadata`'s process-global table as revision state. Either remove that
side table from the server path or replace it with an explicit parse-local
metadata context before allowing concurrent catalog builds.

## 8. Workspace implementation

### 8.1 Modules

Add:

```text
workspace_definition.ml/mli
workspace_catalog.ml/mli
workspace_instance.ml/mli
workspace_resolver.ml/mli
workspace_cleanup.ml/mli
workspace_lease.ml/mli
```

### 8.2 Definition and instance separation

`Workspace_definition.t` is immutable validated configuration.
`Workspace_instance.t` is persisted session state.

The instance stores:

- instance ID;
- definition ID when catalog-backed;
- source kind;
- configured root;
- canonical root captured at creation;
- conflict domain;
- access mode;
- cleanup policy;
- whether the server created the directory;
- creation timestamp; and
- optional cleanup completion metadata.

### 8.3 Physical resolution

`Workspace_resolver.resolve_physical`:

1. starts from the validated absolute configured path;
2. resolves symlinks to a canonical native path;
3. verifies directory type and required host reachability;
4. computes default conflict domain from canonical path;
5. creates no filesystem content; and
6. returns a persisted instance snapshot.

On restart, the server re-resolves availability but does not silently replace
the captured canonical identity. A changed identity produces
`workspace_unavailable` or an explicit operator migration requirement.

### 8.4 Temporary resolution

For `Session_dir`, create:

```text
<session-dir>/workspace
```

using mode `0700` before runtime construction. For `System_tmp`, use secure
exclusive temporary-directory creation and persist the exact native path and
creation marker.

Managed cleanup requires all checks:

- `server_created = true`;
- instance ID matches session metadata;
- path is below the configured managed root;
- path is not `/`, the data root, a workspace catalog root, or a physical
  workspace;
- no active workspace lease exists; and
- cleanup policy permits the current lifecycle operation.

### 8.5 Runtime paths

Create one `Runtime_paths.t`:

```ocaml
type t =
  { tool_dir : Eio.Fs.dir_ty Eio.Path.t
  ; workspace : Eio.Fs.dir_ty Eio.Path.t
  ; prompt_dir : Eio.Fs.dir_ty Eio.Path.t
  ; session_dir : Eio.Fs.dir_ty Eio.Path.t
  ; cache_dir : Eio.Fs.dir_ty Eio.Path.t
  ; home : Eio.Fs.dir_ty Eio.Path.t
  }
```

In daemon mode, `workspace` is the concrete workspace-instance root, while
`tool_dir` is the captured daemon launch directory. In standalone mode both
default to the captured current directory. Explicit host tool-directory
overrides remain supported; selecting a workspace never changes `tool_dir`.

`source_dir` remains declaration-specific and is supplied by ChatMD source
provenance to `Chat_response.Agent_runtime.host`.

### 8.6 Workspace is not authority

No workspace module exposes a generic `read`, `write`, or shell capability to
the agent. It only constructs paths and concurrency identity. Tool authority
continues to come from ChatMD declarations, shell policy, and host permission
ceilings.

## 9. Quotas and workspace leases

### 9.1 Modules

Add:

```text
quota_key.ml/mli
quota_manager.ml/mli
workspace_lease.ml/mli
start_queue.ml/mli
```

### 9.2 Acquisition key

Root-agent prompt limits use:

```ocaml
type t =
  { conflict_domain : string
  ; prompt_id : Prompt_id.t
  }
```

Exclusive workspace leases use only `conflict_domain`.

### 9.3 Acquisition order

Session start acquisition order is fixed:

1. global running-session capacity;
2. exclusive workspace lease when required;
3. prompt/workspace root-agent slot;
4. per-principal running-session limit; and
5. runtime construction capacity.

Failure releases every earlier acquisition in reverse order.

### 9.4 Queue behavior

Queued starts have a durable queue ticket containing session ID, accepted
command sequence, quota key, and creation time. The queue is FIFO per quota
key with bounded fairness across keys.

A queued session has desired state `Running` and observed state
`Queued_for_slot`. Stop or delete cancels its ticket durably.

Quota release wakes the manager, which offers the slot to the oldest eligible
ticket. The ticket holder must acknowledge acquisition before a timeout or
the offer moves to the next ticket.

### 9.5 Nested work

Nested agents, model jobs, and asynchronous tools do not consume root-agent
slots. They consume separate per-session, prompt, conflict-domain, and global
job semaphores configured by runtime policy.

## 10. Durable session data model

### 10.1 Do not overload `Session.t`

Create `Agent_session_state.t` as the daemon/engine state. Embed the existing
conversation fields that are already stable, but keep server lifecycle and
protocol state outside legacy `Session.t`.

The durable snapshot root is conceptually:

```ocaml
type t =
  { schema_version : int
  ; identity : Identity.t
  ; spec : Spec.t
  ; lifecycle : Lifecycle.t
  ; conversation : Conversation.t
  ; moderator : Moderator_state.t
  ; shell : Session.Shell_state.t
  ; permissions : Permission_state.t
  ; operations : Operation_state.t
  ; jobs : Job_state.t
  ; schedules : Schedule_state.t
  ; owners : Owner_state.t
  ; idempotency : Idempotency_state.t
  ; revisions : Revision_state.t
  ; failure : Failure_state.t option
  }
```

Split these records into modules so no source type becomes an unmaintainable
single record.

### 10.2 Identity

`Identity.t` stores session ID, display name, creating principal, created and
updated timestamps, labels, and current generation.

Generation increments on reset when the session ID is retained. Operation,
job, schedule, permission, and event records include generation where stale
cross-generation identity would be dangerous.

### 10.3 Session specification

`Spec.t` stores:

- execution host policy;
- liveness policy;
- persistence policy;
- pinned prompt definition and revision;
- persisted workspace instance;
- permission-profile ID and immutable revision digest;
- runtime policy;
- quota key;
- cache policy;
- parallel-tool-call policy; and
- operator metadata.

### 10.4 Conversation

`Conversation.t` stores:

```ocaml
type t =
  { canonical_history : History_entry.t list
  ; next_history_sequence : int
  ; tasks : Session.Task.t list
  ; kv_store : (string * string) list
  ; deferred_user_entries : History_entry.t list
  ; initial_prompt_entry_count : int
  ; compaction_generation : int
  }
```

Deferred user entries are canonical immediately and must be persisted. They
remain separate from installed history only until the safe-point adoption
transaction.

### 10.5 Lifecycle

Persist desired and observed states separately. Observed operation states
include stable operation IDs and start timestamps. A snapshot written during
active work records the operation's recovery classification; it never claims
that an Eio switch or provider stream is serializable.

### 10.6 Revisions and sequences

Persist independent monotonic counters:

- session revision;
- durable event sequence;
- transaction sequence;
- history allocator next sequence;
- owner lease generation;
- compaction generation; and
- audit sequence when mirrored from shell state.

Every increment is checked for overflow. Overflow fails the session closed
rather than wrapping.

History ID allocation has an additional durability rule: an ID correlated
with a live provider event must be durably reserved before that event is
published outside the session engine. Gaps are permitted; reuse after an ID
has been externally observed is forbidden.

Implement this with durably reserved ID blocks. The actor advances
`next_history_sequence` by a configured block size in a journal transaction,
then publishes that bounded block to `History_id_source`. Allocating inside an
already committed block requires no journal write. Exhaustion asks the actor
to commit another block before allocation continues. Unused IDs become gaps
after restart and are never reused.

### 10.7 Client snapshot

Define a protocol snapshot projection separate from the persistence snapshot.
Projection code receives a principal visibility policy and redacts:

- secret-bearing arguments;
- unauthorized shell state;
- raw audit payloads;
- hidden prompt source paths;
- grants outside scope; and
- jobs or permissions not visible to the principal.

The projection contains the exact revision and latest durable event sequence
used to build it.

## 11. Store layout and filesystem ownership

### 11.1 Modules

`lib/agent_store` contains:

```text
data_root.ml/mli
lock.ml/mli
durable_file.ml/mli
frame.ml/mli
journal.ml/mli
journal_segment.ml/mli
snapshot.ml/mli
session_store.ml/mli
session_index.ml/mli
idempotency_store.ml/mli
blob_store.ml/mli
prompt_artifact_store.ml/mli
migration.ml/mli
recovery.ml/mli
```

### 11.2 Data layout

Use this initial layout:

```text
<data-dir>/
  schema.sexp
  daemon.lock
  server-id
  indexes/
    sessions.snapshot
    sessions.journal
    sessions.recovery-required  present while missing-index recovery is incomplete
  prompt-artifacts/
    <revision-id>/...
  blobs/
    temporary/
    durable/
  sessions/
    <session-id>/
      metadata.sexp
      ARCHIVED                  present only for an archived session
      actor.lock
      snapshot/
        CURRENT
        snapshot-<transaction-sequence>.bin
      journal/
        CURRENT
        0000000000000001.log
      cache/
        cache.bin
      workspace/
      responses/
      audit/
        shell.jsonl
        server.jsonl
      exports/
      archive/
      idempotency/
  migrations/
  lost-and-found/
```

`CURRENT` files contain only a validated segment or snapshot identifier and
are atomically replaced. Paths are created with restrictive permissions.
Directory syncing must not silently downgrade when an Eio directory
capability lacks a native descriptor. Obtain a syncable Eio-owned read-only
descriptor for the directory, verify its kind, and perform fsync through the
Eio system-thread boundary; otherwise report a typed IO failure.

An absent global session index may be reconstructed from validated installed
durable session layouts. An existing corrupt index must fail closed, not be
replaced with an empty index. Ignore staged/deleted namespaces; reject invalid
active layouts and symlinks. Preserve archival state in an identity-bearing
`ARCHIVED` marker written before the reconstructable index is updated, and
backfill markers from valid older archived index entries.

Before publishing a rebuilt index, durably write
`indexes/sessions.recovery-required`. While this marker exists, recover all
non-archived entries, not merely those whose stale metadata looks runnable.
Only clear the marker after successful actor loading, checked job/schedule
reconciliation and durable actor/index checkpoints. An interrupted recovery
must retain this requirement on the next restart. Store metadata schema and
session-data schema are separate: the session-state codec owns data-schema
compatibility checks during hydration.

### 11.3 Store API

The high-level store exposes typed operations and never calls `exit`:

```ocaml
type error =
  | Locked of Lock.owner option
  | Missing
  | Schema_too_new of int
  | Migration_required of int
  | Corrupt of Corruption.t
  | Io of Error.t

val open_ : env:Eio_unix.Stdenv.base -> root:string -> (t, error) result
val create_session : t -> Initial_state.t -> (Session_handle.t, error) result
val open_session : t -> Session_id.t -> (Session_handle.t, error) result
val list_sessions : t -> Session_index.Entry.t list
val close : t -> unit
```

`Session_handle.t` owns paths and lock state, not live session mutation.

### 11.4 Locks

`Lock.t` records:

- server ID;
- process ID;
- process start identity when available;
- hostname;
- acquisition timestamp; and
- random lock nonce.

The daemon holds one data-root lock for its lifetime. A session actor holds a
session lock while loaded. Lock takeover requires explicit stale-owner
verification. A mere old modification time is insufficient.

The embedded standalone engine may use the legacy store or a separately
selected data root. It must not open a daemon-owned session directory behind
the daemon's lock.

### 11.5 Durable replacement

`Durable_file.replace` implements:

1. create a sibling temporary file exclusively;
2. write all bytes;
3. flush and fsync the file when durability requires it;
4. rename over the target atomically;
5. fsync the parent directory when supported and required; and
6. remove the temporary file on failure.

Blocking system calls that cannot be expressed without blocking an Eio domain
must run through the repository's chosen Eio Unix system-thread boundary.
All ordinary file operations use `Eio.Path`, `Eio.File`, and `Eio.Flow`.

### 11.6 No ambient destructive paths

All destructive store functions receive a `Session_handle.t`, `Blob_handle.t`,
or `Managed_workspace.t`. They do not accept arbitrary strings. This makes it
impossible for ordinary cleanup code to recursively delete the data root,
home directory, or physical workspace.

## 12. Journal format and transaction protocol

### 12.1 Transaction record

Define a versioned Bin_prot transaction:

```ocaml
type t =
  { schema_version : int
  ; session_id : Session_id.t
  ; generation : int
  ; transaction_sequence : int64
  ; previous_transaction_hash : string option
  ; session_revision : int64
  ; first_event_sequence : int64 option
  ; last_event_sequence : int64 option
  ; accepted_at_ns : int64
  ; command : Command_audit.t option
  ; delta : Delta.t
  ; events : Event.Durable.t list
  }
[@@deriving bin_io]
```

`Delta.t` is a closed variant of durable state transitions. Do not journal
arbitrary closures, mutable values, or an untyped replacement snapshot for
every command.

Initial delta variants include:

- session created;
- desired state changed;
- observed state changed;
- canonical entries appended;
- canonical history replaced;
- deferred entries enqueued or adopted;
- task state changed;
- session key/value state changed;
- moderator snapshot replaced;
- shell state replaced;
- permission created or resolved;
- grant changed;
- operation intent, terminal state, or interruption;
- job created or changed;
- schedule created, fired, rescheduled, or cancelled;
- owner lease changed;
- prompt revision upgraded;
- workspace state changed;
- labels or display name changed;
- idempotency result recorded;
- reset generation created; and
- failure state changed.

### 12.2 Frame encoding

Each journal frame is:

```text
magic              8 bytes
frame version      2 bytes
flags              2 bytes
payload length     8 bytes unsigned
payload            payload length bytes
SHA-256             32 bytes over header fields and payload
```

All integer byte order is fixed and documented. The maximum frame length is
configured and validated before allocation.

A frame is committed only when its complete bytes are present and its checksum
is valid. An incomplete final frame is a crash tail and may be truncated.
Checksum failure or sequence discontinuity before the final incomplete frame
is corruption and fails closed.

### 12.3 Hash chain

`previous_transaction_hash` links committed transactions. Recovery verifies
the chain across segment boundaries. The chain detects reordered, removed, or
spliced records; it is integrity detection, not authentication unless a future
configured HMAC is used.

### 12.4 Commit writer

Each loaded session has one `Commit_writer.t` fiber. The session actor submits
an immutable transaction candidate and a promise resolver.

The writer:

1. validates expected next transaction sequence;
2. encodes the frame;
3. appends it to the current segment;
4. performs the configured flush policy;
5. resolves the commit promise with committed sequence/hash; and
6. rotates the segment when thresholds are reached.

For bounded group commit, the writer may collect candidates across a short
interval, append in actor order, perform one flush, then resolve all promises
in order. A transaction is not acknowledged to the actor before the flush
level promised by configuration.

### 12.5 Actor commit algorithm

For every durable mutation, the actor:

1. validates the command against current state;
2. creates a pure candidate next state;
3. computes `Delta.t` and durable events;
4. assigns revision and event sequences in the candidate;
5. submits the immutable transaction to `Commit_writer`;
6. continues servicing only operations that cannot observe the candidate, or
   waits for the commit result;
7. installs the candidate after successful commit;
8. updates indexes;
9. publishes durable events; and
10. resolves the command or worker acknowledgement.

The simplest initial implementation waits for each session commit before
accepting another durable mutation. Group commit still batches across sessions
or closely queued writes inside the writer service. Optimization must not
expose uncommitted state.

### 12.6 Intent and completion records

External work uses at least two durable transitions:

```text
Prepared -> Started/Dispatched -> Succeeded | Failed | Cancelled | Interrupted
```

The intent record stores recovery policy and idempotency identity before the
external action begins. Completion records the result after the external
action returns.

If the daemon dies after dispatch and before completion:

- `Never` retry becomes `Interrupted`;
- `Safe_retry` may return to queued within attempt limits; and
- `Idempotent` may retry only with the same external idempotency key.

### 12.7 Journal rotation

Rotate by configurable size and transaction count. Rotation:

1. seals the old segment with a terminal metadata frame;
2. flushes it;
3. creates the next segment exclusively;
4. atomically updates `journal/CURRENT`; and
5. retains old segments until a snapshot safely covers them and event
   retention permits deletion.

## 13. Snapshots, replay, and recovery

### 13.1 Snapshot contents

`Snapshot.t` contains:

- snapshot schema version;
- complete `Agent_session_state.t`;
- covered transaction sequence and hash;
- covered event sequence;
- creation timestamp;
- prompt artifact reference;
- workspace identity; and
- checksum metadata.

### 13.2 Snapshot creation

The actor captures an immutable state value at a committed revision. A
snapshot worker serializes and writes it outside the actor loop. Completion is
accepted only if it corresponds to a still-valid committed transaction.

Snapshot creation does not delete journals. A later retention pass may delete
segments wholly covered by a validated installed snapshot and no longer
needed for event replay.

### 13.3 Snapshot installation

Write a uniquely named snapshot, validate it by rereading metadata/checksum,
then atomically replace `snapshot/CURRENT`. Keep at least the previous known
good snapshot until the next snapshot is established.

### 13.4 Recovery algorithm

`Recovery.load`:

1. validates session metadata and directory identity;
2. acquires the actor lock;
3. loads the newest valid referenced snapshot;
4. falls back to the previous snapshot if the newest is incomplete;
5. enumerates journal segments in numeric order;
6. verifies frame checksums, sequences, session ID, generation, and hash
   chain;
7. applies deltas after the snapshot transaction;
8. validates history IDs and allocator high-water mark;
9. validates event and revision counters;
10. rebuilds operation, permission, job, schedule, owner, and idempotency
    indexes;
11. classifies unfinished external work;
12. reconciles desired and observed lifecycle; and
13. returns recovered state plus recovery actions.

Recovery actions are pure descriptions such as:

- mark operation interrupted;
- retry idempotent job;
- redeliver completed job event;
- fire overdue schedule;
- expire owner lease;
- recreate temporary workspace availability state; or
- start session runtime.

The session actor commits recovery actions before exposing the loaded session
as ready.

### 13.5 Event replay index

The store maintains an index from durable event sequence ranges to journal
segment offsets. The index is reconstructable and therefore not authoritative.
If missing or invalid, scan committed transactions to rebuild it.

Replay reads only durable event payloads visible to the principal. Live stream
deltas use a separate bounded operation buffer and are not reconstructed from
the durable journal after their retention expires.

### 13.6 Recovery of active foreground operations

Provider streams and arbitrary fibers are non-resumable. Recovery preserves
committed canonical entries and marks the active operation interrupted.

A configured retry may start a new operation only when:

- no uncertain side-effecting tool was dispatched;
- provider retry policy permits it;
- the new operation has a new ID;
- the original remains visibly interrupted; and
- retry count and idempotency policy permit it.

### 13.7 Recovery of permissions

Generic and shell permission records persist. If the blocked executor
continuation no longer exists after restart, the permission is either:

- reconstructed at a durable pre-execution boundary and can be answered; or
- resolved as interrupted because execution cannot safely resume.

Current ChatML `Approval.ask_text` and `Approval.ask_choice` are always marked
`Interrupted_chatml_approval` after process restart until ChatML is redesigned
around event-driven continuation.

## 14. Store indexes and list operations

### 14.1 Session index

The data-root session index contains only list/query metadata:

- session ID;
- display name;
- prompt ID and revision;
- workspace definition and instance;
- desired and observed state;
- creating principal;
- labels;
- created/updated timestamps;
- latest revision and event sequence; and
- runnable/deliverable job count;
- earliest schedule due time;
- unresolved owner-grace deadline; and
- deletion/archive state.

Session state remains authoritative in the session journal. Index entries are
updated after session commit and can be rebuilt by scanning session metadata.

### 14.2 Index writer

Use one data-root index writer fiber to serialize index deltas. A session
command may be acknowledged after its session transaction commits even if the
rebuildable global index is momentarily behind, but the actor must enqueue the
index update before acknowledgement.

`session.get` resolves directly by session ID. `session.list` may use the
index and reports index degradation through health metrics.

### 14.3 Pagination

Server-issued cursors encode or reference:

- server generation;
- principal ID;
- query digest;
- stable sort key;
- final item identity; and
- expiration.

Sign cursors with a server-local integrity key so clients cannot alter filters
or sort position. Cursor decoding must not reveal secret server paths.

## 15. Idempotency implementation

### 15.1 Key identity

The key scope is:

```ocaml
type key =
  { principal_id : Principal_id.t
  ; session_id : Session_id.t option
  ; method_name : string
  ; idempotency_key : string
  }
```

Store a canonical request digest, accepted transaction sequence, and complete
protocol result or error that is safe to replay.

### 15.2 Command path

For mutating commands:

1. validate key length and syntax;
2. compute canonical request digest after authentication-bound fields are
   normalized;
3. look up the key in actor-owned durable state;
4. return stored result when the digest matches;
5. return `idempotency_conflict` when it differs;
6. execute the command when missing; and
7. commit state mutation and idempotency result in the same transaction.

### 15.3 Retention

Idempotency records have a configured minimum retention. Records for
destructive operations, externally dispatched work, or protocol objects still
referenced by active state remain until safe archival. Pruning is a durable
transaction.

## 16. Blob storage

### 16.1 Temporary upload

`Blob_store.begin_upload` returns a server-owned temporary path and streaming
sink. The HTTP adapter writes chunks while computing SHA-256 and enforcing
maximum bytes.

On successful close, persist metadata:

- blob ID;
- creating principal;
- optional target session;
- media type;
- byte length;
- digest;
- allowed use;
- created and expiry times; and
- temporary native path.

### 16.2 Adoption

When `session.send_message` references a blob, the actor authorizes it and the
store adopts it into session-owned durable storage before acknowledging the
message. Adoption and message mutation share one durable intent; crash
recovery can finish or roll back an incomplete move without losing identity.

### 16.3 Download

Downloads resolve an opaque blob ID through metadata and principal scope.
Never concatenate a client-provided blob ID directly into a path without ID
validation and store lookup.

### 16.4 Cleanup

A periodic cleanup job removes expired unreferenced temporary blobs. Durable
blobs follow session archive/deletion policy. Cleanup records metrics and
never treats a missing temporary file as authorization to modify session
state.

## 17. Session actor implementation

### 17.1 Modules

Add to `agent_session`:

```text
mailbox.ml/mli
actor_message.ml/mli
session_state.ml/mli
session_actor.ml/mli
session_command.ml/mli
session_transition.ml/mli
operation_worker.ml/mli
live_event_buffer.ml/mli
subscriber.ml/mli
host_services.ml/mli
```

`Host_services.t` is an injected record of dependency-inverting callbacks for
clock/ID access, quota release, job readiness, schedule changes, audit, index
notification, and daemon health reporting. `agent_session` must not depend on
`agent_server` merely to call these services.

### 17.2 Actor ownership

Only `Session_actor` may mutate live durable session state. It owns:

- state value and counters;
- history allocator;
- runtime instance and its Eio switch;
- current foreground operation;
- deferred canonical queue;
- moderator manager and wakeup subscription;
- permission broker state;
- shell stores and registry;
- owner leases;
- job/schedule references;
- subscriber registry;
- command idempotency lookup;
- commit writer handle; and
- snapshot scheduling state.

The actor owns allocator persistence and issues allocation reservations.
Foreground code may borrow an allocation capability, but it must not advance
the durable high-water mark without actor acknowledgement.

### 17.3 Mailbox

Implement a custom fiber-safe bounded mailbox with priority and normal lanes.
Priority messages include:

- worker terminal outcome;
- persistence result;
- cancellation;
- shutdown;
- permission response;
- owner lease expiry; and
- runtime failure.

Normal messages include accepted client commands, moderator wakeup, job
delivery, schedule due, and configuration notices.

Live provider deltas and tool progress do not enter the actor mailbox. They go
through the operation-scoped live-event buffer.

The mailbox reserves capacity for priority messages. Normal admission returns
`command_queue_full` instead of consuming priority capacity. Coalescible
wakeups use one pending flag rather than enqueueing duplicates.

### 17.4 Actor loop

The actor loop:

1. drains available priority messages;
2. processes a bounded number of normal messages;
3. runs an idle safe-point decision when eligible;
4. schedules snapshot/index maintenance; and
5. blocks until the mailbox becomes nonempty.

Each handler is small and returns a transition description. Persistence and
event publication are centralized in `Session_transition.commit`.

### 17.5 Worker communication

Workers receive immutable operation input and callback capabilities. They
must not receive `Session_state.t ref`.

Synchronous callbacks that require actor state use a request/response message
with an Eio promise, including:

- consume deferred entries at a safe point;
- reserve one or more canonical history IDs;
- commit a canonical history entry;
- checkpoint a moderator transaction and allocator high-water mark;
- request generic permission;
- persist extension snapshot;
- create a durable job; and
- create/cancel a durable schedule.

The actor must remain in its event loop while a worker runs. It never blocks
awaiting the worker's final promise from inside a command handler.

### 17.6 Stale outcomes

Every worker event carries session generation and operation ID. The actor
ignores or audits events that do not match the active generation/operation.
A stale terminal outcome must not clear a newer active operation.

### 17.7 Actor startup

Actor startup states are:

```text
Loading -> Recovering -> Stopped | Starting_runtime -> Idle | Failed
```

The registry does not publish a loaded actor as command-ready until recovery
transactions and required runtime startup complete. Read-only diagnostic
access may be allowed to failed or stopped snapshots.

### 17.8 Actor unloading

A stopped actor may unload when:

- no foreground operation exists;
- no nonterminal owned job requires the actor loaded;
- no due schedule requires immediate delivery;
- no pending permission requires a live continuation;
- all commits and snapshots are complete; and
- no attached subscriber requires live-only data.

Unloading closes runtime resources and session lock but does not change
desired state.

### 17.9 Persistence backend abstraction

Session transitions target a small backend interface so durable and transient
modes share semantics:

```ocaml
module type State_backend = sig
  val commit : t -> Transaction.Candidate.t -> (Transaction.Commit.t, Error.t) result
  val snapshot : t -> Snapshot.t -> (unit, Error.t) result
  val replay : t -> after_sequence:int64 -> (Event.t list, Error.t) result
end
```

`Durable_backend` uses the journaled store. `Memory_backend` retains state and
bounded replay only for the host process lifetime. It still enforces
persist-before-publish relative to its in-memory commit point and uses the
same revisions/events/idempotency rules.

Protocol 1.0 permits daemon-transient sessions only with `Owner_bound`
liveness. Daemon `Detached` sessions require `Durable`, because a detached
session advertised as restart-surviving cannot use process-only state.
Embedded process-bound sessions may use either backend according to CLI
policy.

## 18. Lifecycle transition implementation

### 18.1 Pure transition layer

Implement lifecycle validation in `Session_transition` as pure functions over
immutable state. Side effects are emitted as explicit actions.

```ocaml
type action =
  | Acquire_start_capacity
  | Build_runtime
  | Start_turn of Turn_request.t
  | Start_compaction of Compaction_request.t
  | Cancel_operation of Operation_id.t
  | Stop_runtime
  | Release_start_capacity
  | Cleanup_workspace
  | Write_snapshot
  | Unload_actor
```

The actor executes actions only after the transition's required durable state
has committed.

### 18.2 Create

Creation is coordinated by `Agent_server.Session_registry.create`:

1. authenticate and authorize;
2. resolve prompt and workspace definitions;
3. validate allowed combination and principal limits;
4. build/store the pinned prompt revision;
5. reserve a session ID and create the directory exclusively;
6. materialize the workspace instance;
7. create initial `Agent_session_state.t`;
8. commit the initial snapshot/journal record and index entry;
9. create an actor;
10. optionally create an attachment/owner lease; and
11. submit start when `start_immediately` is true.

Failure before publication removes only server-created incomplete paths.
Failure after the initial durable record leaves a discoverable failed or
stopped session that can be diagnosed and deleted.

### 18.3 Start

`session.start` is idempotent:

- already running returns current state;
- already queued returns the existing ticket;
- starting/recovering returns current operation;
- failed requires an explicit retry flag when the failure is retryable; and
- deleted/deleting returns `invalid_state`.

Starting first commits desired state `Running`. Capacity acquisition and
runtime construction are represented by subsequent observed-state
transactions.

### 18.4 Stop

`session.stop` accepts `graceful` or `cancel`.

Graceful stop commits desired `Stopped`, rejects new work, and waits for the
current safe boundary. It then applies background job stop policies, closes
runtime resources, snapshots, releases quotas, and commits observed
`Stopped`.

Cancelling stop additionally sends cancellation to the operation switch,
denies/cancels pending permissions, records uncertain work, repairs history,
and proceeds through the same teardown.

Repeated stop returns the current stop disposition.

### 18.5 Operation cancellation

Cancellation commits a cancellation request for the target operation before
failing its switch. Completion racing with cancellation is resolved by actor
order:

- if terminal completion committed first, cancellation returns the terminal
  result;
- if cancellation committed first, a later normal completion is stale; and
- cancellation repair commits exactly once.

### 18.6 Reset

Reset requires stopped state unless an explicit option first performs a
cancelling stop. It:

1. archives metadata, current snapshot references, and journal generation;
2. increments session generation;
3. applies explicit keep/drop options for history, tasks, cache, workspace
   contents, grants, labels, and blobs;
4. resets moderator and active runtime state;
5. preserves history allocator monotonicity if history is retained;
6. selects and pins the requested prompt revision;
7. commits a fresh generation root; and
8. optionally starts the new generation.

### 18.7 Rebuild

Rebuild differs from reset by reparsing the selected pinned/current prompt and
reconstructing initial prompt history/runtime metadata. Preparation happens
before the archive/commit boundary. Failure leaves the old generation intact.

The server stages a detached candidate, with no live actor callbacks for history
reservation, grants, permissions, schedule mutation or job claims. Allocation
starts at the captured history reservation high-water mark. Runtime construction
and `post_start_moderator` must both succeed, including initial moderator state
capture, before the actor can commit. Preparation has a private cache/response
directory and a child switch that is cancelled and joined before cleanup.
Public runtime path substitutions retain their real session/workspace meaning.
Executable source helpers or ChatML initializers can cause external tool or
filesystem effects; the session-state transaction does not roll them back.

At commit the actor rechecks writer authority, expected revision, stopped state
and absence of an idle-moderator borrow. It writes an independently retained
pre-change archive before committing the candidate. Preparation, archive or
journal failure leaves the previous pinned revision, moderator, shell and
history authoritative. Successful rebuild installs fresh initial prompt messages
with fresh IDs and a new generation, preserving tasks/key-value data, labels and
workspace. Deferred input, permissions, grants, jobs, schedules and old
moderator/shell state are cleared before initializing the replacement.

Reset, rebuild and upgrade use kind-tagged references in the existing
`compaction_archives` field, with old references defaulting to compaction. These
archives survive ordinary snapshot/journal pruning and appear in
`Snapshot.archived_revisions`; export applies current principal authorization.
Administrative replacement also emits an additive `replacement_snapshot` in
`session.updated`. Each snapshot is bound to its own event's revision/sequence,
not the transaction's final cursor, and principal-filtered on live and replay
delivery. The common reducer replaces all collections, including empty ones.

### 18.8 Prompt upgrade

Upgrade preparation computes:

- old/new source closure diff;
- tool set diff;
- shell manifest diff;
- moderator script identity compatibility;
- runtime policy diff;
- initial history retention plan; and
- required new authorization.

The actor commits the upgrade only after all preparation and required
authorization succeeds. Existing runtime resources are replaced under a new
runtime switch. Failure tears down the prepared runtime without changing the
pinned revision.

### 18.9 Delete

Delete requires a destructive scope and explicit confirmation value tied to
session ID and current revision. It:

1. commits deleting intent;
2. cancels/stops all owned work;
3. closes subscribers with a terminal deletion event;
4. releases quotas and locks;
5. removes the session from the active index;
6. moves the session directory to an archive/trash location or deletes it per
   policy; and
7. cleans only verified server-created temporary workspace/blob paths.

Physical workspaces and shared prompt artifacts are never deleted by this
operation.

## 19. Runtime builder

### 19.1 Modules

Add:

```text
runtime_paths.ml/mli
runtime_builder.ml/mli
runtime_instance.ml/mli
runtime_teardown.ml/mli
tool_metadata.ml/mli
```

### 19.2 Runtime instance

`Runtime_instance.t` contains live, nonserializable values:

- owning Eio switch;
- `Chat_response.Ctx.t`;
- `Chat_response.Agent_runtime.t`;
- provider tool descriptors;
- tool runner table;
- tool security metadata table;
- moderator manager and wrapper;
- shell registry;
- approval broker adapters;
- cache;
- model/job capability adapters;
- moderator wakeup subscription;
- prompt configuration; and
- classifications for client Agent-page projection.

No value of this type is written to disk.

### 19.3 Construction sequence

`Runtime_builder.build` performs the architecture sequence exactly:

1. verify desired state and acquired quotas;
2. verify workspace identity and availability;
3. open/create session cache and response directories;
4. load the pinned prompt artifact source closure;
5. parse the root artifact with stored source provenance;
6. construct explicit `Runtime_paths.t`;
7. create `Chat_response.Ctx` using `prompt_dir` and `tool_dir`;
8. inspect shell declarations and administrative policy;
9. authorize the exact manifest through actor-backed grants/profile;
10. create actor-backed shell approval and extension stores;
11. call `Chat_response.Agent_runtime.host` with all runtime paths;
12. call `Chat_response.Agent_runtime.create`;
13. convert functions with `Ochat_function.functions` and tools with
    `Chat_response.Tool.convert_tools`;
14. create/restore the history allocator;
15. create the moderator from compiled pinned artifact and persisted snapshot;
16. register durable model and scheduling handlers;
17. subscribe moderator wakeups to the actor;
18. run `Session_start` or `Session_resume` and bounded startup drains;
19. commit resulting moderator/session state; and
20. publish the runtime only after every prior step succeeds.

The `fetch_prompt` and nested `run_agent` callbacks supplied to
`Chat_response` resolve local nested-agent references through the pinned
artifact loader first. Static relative declarations are resolved at their own
source directory and captured recursively, including child imports/scripts,
with cycle deduplication and limits of 256 agent source files and 8 MiB captured
source bytes. They must not fall back to daemon cwd or a newly edited catalog
source. Explicit absolute local declarations remain external filesystem
dependencies under operator control, not pinned artifact dependencies. Remote nested prompts retain their existing fetch behavior and
are subject to network/tool policy and caching.

### 19.4 Prompt initial history

For a new generation, derive initial canonical prompt entries using the
existing `Chat_response.Converter` path and allocate `History_entry.Id` values
through the session allocator.

For a resumed generation with nonempty canonical history, preserve that
history. Do not reappend static prompt history.

### 19.5 Startup event choice

Use `Session_resume` when persisted compatible moderator state exists for the
pinned script identity. Use `Session_start` for a fresh moderator generation.
Do not infer resume merely because canonical conversation history exists.

### 19.6 Startup failure cleanup

Every partial runtime component is owned by a preparation switch. Failure
cancels the switch and closes brokers/subscriptions before returning typed
diagnostics. The actor commits `Failed` and releases quota unless configured
to hold capacity for inspection.

### 19.7 Teardown

Teardown order is:

1. stop accepting new worker requests;
2. unsubscribe moderator wakeups;
3. cancel runtime/operation switches;
4. close generic and shell approval brokers;
5. terminate and reap child processes through shell runtime ownership;
6. snapshot moderator and extension state where safe;
7. save cache;
8. close audit sinks;
9. release runtime switch; and
10. notify actor that resources are closed.

### 19.8 Remote MCP tools declared by ChatMD

ChatMD's actively maintained `<tool mcp_server="...">` declaration is supported by the
agent runtime. Refactor `Chat_response.Tool` so MCP discovery cache and
invalidation listeners are owned by the runtime's connected declaration rather
than the process-global `tool_cache`. The initial implementation partitions
strictly: every connected declaration owns a separate cache and listener, even
when endpoints match. The listener shares the owning runtime switch lifetime.

Cache isolation includes endpoint, authenticated client identity, requested
tool-name filter, and security-relevant connection settings. One principal's
discovered tool metadata or credentials must not be reused for another
principal merely because the URI matches.

## 20. Extracting the shared session controller

### 20.1 Source of behavior

The initial shared controller is extracted from:

- `Chat_tui.App_runtime` host fields;
- `Chat_tui.Moderator_session_controller`;
- `Chat_tui.App_submit` scheduling behavior;
- `Chat_tui.App_compaction` operation behavior;
- `Chat_tui.App_reducer` operation terminal and idle-safe-point paths;
- `Chat_tui.App_streaming` worker adapter; and
- `Chat_tui.App_reducer.Cancellation_repair`.

### 20.2 Fields moved out of the TUI

Move these concepts into `agent_session`:

- active foreground operation;
- operation ID allocator;
- queued compaction/user actions that affect session semantics;
- moderator dirty state;
- pending/projected overlay revision as server projection metadata;
- deferred canonical user entries;
- pending turn request;
- follow-up turn count/timestamps;
- halted reason;
- moderator startup state;
- pending generic/moderator/shell input state;
- cancellation-before-worker-start state; and
- durable session reference.

### 20.3 Fields retained by TUI

Keep in `Chat_tui`:

- render caches and materialization state;
- editor draft and cursor;
- scroll/selection/page state;
- type-ahead operation;
- redraw throttling;
- terminal presentation generation;
- TextMate grammar/highlighting state;
- local draft history; and
- TUI-only startup rendering.

### 20.4 Compatibility extraction method

First add a new `Agent_session.Controller` with pure state/decision helpers and
have existing TUI reducer call it while the TUI still owns workers. Once
behavioral tests pass, move workers and persistence behind `Session_actor`.

Do not copy the reducer wholesale. Extract small host-semantic functions and
leave terminal event handling in `chat_tui`.

## 21. Foreground turn worker

### 21.1 Worker input

`Turn_worker.Input.t` is immutable and includes:

- session generation;
- operation ID and start reason;
- canonical history snapshot;
- actor-backed history ID source and read-only allocator identity;
- runtime instance reference valid for the operation switch;
- model/configuration values;
- tool descriptors and runners;
- moderator wrapper;
- parallel tool-call setting;
- response directory;
- prompt cache identity;
- safe-point request capability;
- canonical commit capability;
- generic permission capability; and
- live-event sink.

### 21.2 Existing response engine

Invoke
`Chat_response.In_memory_stream.run_completion_stream_in_memory_entries`.
Do not implement a second response loop in the server.

Extend the response engine and `History_stream_event.Registry` to accept a
history ID source:

```ocaml
module History_id_source : sig
  type t

  val namespace : t -> string
  val next_reserved_sequence : t -> int
  val allocate : t -> (History_entry.Id.t, Error.t) result
  val reserve : t -> count:int -> (History_entry.Id.t list, Error.t) result
  val validate : t -> History_entry.t list -> (unit, Error.t) result
end
```

The existing `History_entry.Allocator.t` constructor remains the standalone
adapter. The daemon adapter allocates only from blocks whose high-water mark
the actor has already committed. A low-water notification asks the actor to
reserve the next block; empty-pool allocation waits for that commit.
`History_stream_event.Registry.observe` must obtain an ID from this source
before invoking external history/live-event callbacks.

Map callbacks as follows:

| Existing callback | New destination |
|---|---|
| `on_sourced_event` | operation live-event buffer |
| `on_history_event` | live projection plus canonical completion correlation |
| `on_tool_execution` | operation live-event buffer |
| `on_history_tool_out` | synchronous actor canonical commit |
| `on_runtime_request` | worker accumulator or actor-safe request |
| final returned history | terminal reconciliation against actor-committed canonical history |

Add an `on_moderator_checkpoint` callback after every successfully committed
moderator handler/drain and before the response loop acts on its surfaced
effects. The callback includes the identity snapshot, overlay revision,
allocator high-water mark, and surfaced outcomes. It synchronously asks the
actor to commit the corresponding durable session transition.

### 21.3 Canonical callback acknowledgement

Canonical entries emitted during a stream must be committed before clients
receive a durable history event. The callback:

1. sends `Commit_canonical_entry` to the actor with operation ID;
2. waits for commit acknowledgement;
3. returns normally only after commit; and
4. raises cancellation or a typed worker failure when the actor rejects it.

This synchronous boundary is permitted because the actor continues processing
messages while the worker runs.

### 21.4 Live event buffer

`Live_event_buffer` assigns monotonic `operation_sequence` values and stores an
operation-local bounded ring (2,048 entries). It publishes:

- sourced provider events;
- text/reasoning deltas;
- tool start/progress/finish;
- activity state;
- nested Agent-page trace; and
- compaction progress.

The buffer records the current committed durable event sequence as
`anchor_sequence`. It never mutates canonical history.

The ring drops its oldest entry when full and is not exposed as a replay API.
Protocol 1.0 attach/reconnect and HTTP session SSE replay durable events only.
On reconnect, clients reconstruct state from durable replay or a replacement
snapshot and resume live delivery. No completed-operation delta retention or
coalescing guarantee is made. Slow subscriber handling remains independent.

`max_events_per_session` controls durable replay capacity and initialization's
advertised `maximum_events`. The historical `completed_stream_ms` config field
only supplies the default for omitted `response_artifact_ms`; it does not
configure this ring or provide completed-stream replay. This explicitly replaces
the earlier retained-delta-reconnect design claim for Protocol 1.0.

### 21.5 Safe-point deferred entry consumption

The `Safe_point_input.consume_entries` callback sends a synchronous actor
request. The actor may return deferred entries only when:

- the request operation is active;
- the response loop has reached the documented safe boundary;
- no incomplete tool-call/output pair would be split; and
- entries have not already been adopted.

The adoption transaction removes entries from the deferred queue and appends
them to installed canonical history with the same IDs. It emits one
`item_appended` moderator event per entry in FIFO order.

### 21.6 Final reconciliation

The driver's final returned history is validated against:

- actor-committed history prefix;
- deferred entries already adopted;
- allocator high-water mark; and
- duplicate IDs.

Any final entries not already committed are committed in order. A mismatch in
an existing ID's payload that is not an explicitly allowed replacement fails
the operation and session invariant check.

### 21.7 Terminal handling

The worker returns one terminal outcome:

```ocaml
type outcome =
  | Completed of Summary.t
  | Cancelled of Cancellation.t
  | Failed of Failure.t
```

The actor commits terminal operation state, moderator snapshot, canonical
history, pending runtime requests, and next scheduling intent before
publishing the terminal durable event.

### 21.8 Allocation and moderator borrowing

During an active turn, the worker holds the exclusive foreground borrow of the
moderator manager. The actor does not run idle drains or resume ChatML input
against that manager until the worker returns the borrow.

The manager reserves moderator-inserted IDs through the same bounded source.
If one moderator transaction requests more IDs than remain, allocation returns
a typed refill requirement before the manager commits. The host commits a
large-enough block and retries the moderator event from its transactional
pre-commit state. Unused reserved IDs are harmless gaps.

The worker never publishes a history-correlated provider event before its
actor-backed ID reservation completes. This prevents reconnect/restart from
reusing an externally observed identity.

## 22. User message submission

### 22.1 Parsing

`session.send_message` accepts plain text or typed content. Plain empty input
after trimming is rejected. Raw ChatMD/XML draft mode is represented by an
explicit content kind and parsed using the existing converter path.

### 22.2 Idle submission

When idle and running:

1. allocate a canonical history ID;
2. construct the user entry;
3. commit it and `operation.started` in one transaction when practical;
4. reset host follow-up counters;
5. launch a `User_submit` turn; and
6. return entry and operation IDs.

### 22.3 Active submission

When a foreground operation is active:

1. allocate a canonical history ID immediately;
2. commit the entry to `deferred_user_entries`;
3. emit `history.message_deferred`;
4. return `deferred` disposition; and
5. let the active worker consume it only through the safe-point callback.

If the active worker passes its last consumption boundary before the entry is
accepted, terminal handling leaves it queued and schedules the next user turn.

### 22.4 Multiple clients

Actor mailbox order determines accepted message order. Idempotency prevents a
client retry from creating a second canonical entry. Every writer receives the
actor-assigned history ID and disposition.

## 23. Compaction worker

### 23.1 Input and execution

Compaction snapshots canonical history at a committed revision and invokes
`Context_compaction.Compactor.compact_entries` under an operation switch.

The worker emits recoverable progress and returns replacement history plus
compaction metadata.

### 23.2 Installation

Implementation: `Session_persistence` writes a checksummed pre-replacement state
file through Eio with file-and-directory flush before journaling the reference
and replacement. The reference contains operation ID, pre-replacement revision
and SHA-256. `session.get` exposes `archived_revisions`; `session.export` accepts
those revisions under normal principal projection and blob authorization.
Archives are independent of ordinary journal/snapshot pruning. See
[archive storage](../lib/agent_session/compaction_archive.doc.md) and
[current compaction behavior](../context_compaction/compactor.doc.md).

The actor validates replacement history and allocator high-water mark. It
commits:

- archived pre-compaction reference;
- replacement canonical history;
- compaction generation;
- moderator-visible item events required by current semantics;
- moderator snapshot changes; and
- `history.replaced` plus terminal operation events.

After commit, clients discard incompatible incremental projection state and
install the replacement snapshot/event.

### 23.3 Error and cancellation

Failure preserves original history. Cancellation does not install partial
history. Both return to the post-operation idle safe point before queued work
starts.

## 24. Cancellation repair

Move `Chat_tui.App_reducer.Cancellation_repair` into
`Agent_session.Cancellation_repair` and expose a typed result.

Repair must:

- preserve committed complete entries;
- remove provider-incomplete trailing reasoning where required;
- synthesize error tool outputs for committed tool calls lacking required
  outputs;
- use the session allocator for synthetic entries;
- preserve call IDs;
- validate final history; and
- state exactly which operation and cancellation caused the repair.

Repair is committed as a durable history mutation before the actor returns to
idle.

## 25. ChatML moderator integration

Newly constructed ChatML instructions use provider `Developer`, including both
legacy and identity-based prepend paths, `Item.system_text`, notice helpers, and
explicit `system` text constructors. Preserve helper/effect names and recognize
both historical system and developer items in compatibility predicates. Raw item
values and existing persisted history retain their original roles.

### 25.1 Ownership

The runtime's `Moderator_manager.t` is live-session state whose lifecycle is
owned by the actor. Only one moderator operation executes at a time. While a
foreground turn is active, the turn worker has an exclusive borrow and must
checkpoint each successful moderator transaction through the actor before
using its effects. While idle, the actor executes drains directly. Approval
resume is routed either to the current foreground borrower or the idle actor,
never both.

### 25.2 Wakeups

Use `Moderator_manager.subscribe_committed_changes`. The callback sets one
atomic/coalesced actor wakeup flag and enqueues `Moderator_wakeup` only when no
wakeup is already pending.

Wakeups received during active work mark dirty state. They do not drain the
manager or refresh visible state immediately.

### 25.3 Safe-point controller

Move the behavior of `Chat_tui.Moderator_session_controller` into a
transport-neutral module. It converts outcomes into:

- refresh required;
- compaction requested;
- turn requested and reason;
- halt reason;
- durable/client notice;
- remaining internal events; and
- events to enqueue.

Preserve precedence: end-session suppresses turn, compaction remains visible,
and pure runtime requests do not force history refresh.

### 25.4 Idle drains

Idle drain is allowed only when:

- no foreground operation is active;
- no blocking permission/approval is pending;
- startup is complete;
- session is not stopping;
- policy does not pause idle drains; and
- the session is not failed.

Use `runtime_policy.budget.max_internal_event_drain`. If events remain, keep
the dirty flag and schedule another fair actor pass rather than recursively
draining without yielding.

### 25.5 Snapshots

After every moderator transaction that changes durable state, obtain the
identity snapshot and include it in the same session transaction as surfaced
host effects. Do not persist only the legacy moderator snapshot in the new
store.

The response-loop integration must expose a checkpoint callback at turn start,
pre-tool, post-tool, turn end, and internal-event drains. A successful
moderator transaction is not considered externally installed until this
callback commits. Checkpoint failure cancels the foreground operation and
fails the durable session closed rather than continuing with unpersisted
moderation decisions.

### 25.6 Halt

A committed moderator halt is durable. The actor rejects new turns and
automatic work while allowing read/export/reset/upgrade/stop operations.

## 26. Follow-up budgets and scheduling

### 26.1 Existing policy

Reuse `Chat_response.Runtime_semantics.policy` and its fixed semantics:

- self-triggered continuation limit inside `In_memory_stream`;
- host follow-up count limit;
- internal event drain limit;
- optional sliding-window turn limit; and
- host pause conditions.

### 26.2 Persisted bookkeeping

Persist host follow-up count and timestamps needed to preserve automation
behavior across daemon restart. User submission resets count according to
existing policy.

### 26.3 Decision order

Automatic follow-up decision order is:

1. halted/stopping/failed state;
2. blocking permission;
3. `Pause_followup_turns`;
4. sliding-window rate limit;
5. maximum follow-up count;
6. foreground/queued user work precedence; and
7. start turn.

Use the existing stable budget notice keys. Notices are durable when they
affect session-visible behavior and deduplicated by key.

## 27. Generic permission gate

### 27.1 Modules

Add:

```text
permission_profile.ml/mli
permission_rule.ml/mli
permission_gate.ml/mli
permission_broker.ml/mli
permission_projection.ml/mli
external_reviewer.ml/mli
```

### 27.2 Tool metadata

Extend `Chat_response.Agent_runtime.t` with a table keyed by exposed tool name:

```ocaml
type tool_metadata =
  { kind : Tool_kind.t
  ; declaration_source : Chatmd_shell_spec.Source_ref.t option
  ; shell_runtime_id : string option
  ; effect_hints : string list
  ; permission_class : string
  }
```

Built-ins, read-file tools, nested agents, MCP tools, shell tools, and legacy
custom commands must each produce metadata.

### 27.3 Response-loop hook

Add an optional authorization callback to the identity-bearing response path
at the point after ChatML pre-tool moderation and before tool execution:

```ocaml
type authorization =
  invocation:Invocation.t -> (Decision.t, Error.t) result
```

The invocation includes effective rewritten name/payload, call ID, tool kind,
metadata, session identity, and operation ID.

Thread the callback through `In_memory_stream`, `Tool_call`, and
`Tool_executor` with no behavior change when absent.

### 27.4 Decision behavior

- `Allow` executes the runner.
- `Deny` returns a canonical denied tool output and continues through normal
  post-tool moderation.
- `Ask` creates a durable permission and blocks the worker.
- `Rewrite` restarts authorization on the rewritten invocation with a bounded
  rewrite count.
- `Delegate` skips generic prompting and lets the authoritative subsystem,
  normally shell runtime, perform its own review.

Hard denials and capability ceilings are evaluated before profile rules.

### 27.5 Durable permission request

The worker asks the actor to create a request. The actor commits the request
and `Waiting_for_permission` state, publishes it to eligible clients, then
returns a promise handle to the worker.

The broker supports timeout through the daemon scheduler. Timeout resolution
is an ordinary compare-and-set permission response using the configured
fallback.

### 27.6 Response race

`permission.respond` checks:

- attachment and approver scope;
- principal eligibility;
- permission is pending;
- decision is one of the offered choices;
- grant scope is allowed; and
- expected permission generation.

The first response commits. Later responses return `already_resolved` with
redacted resolution metadata.

### 27.7 Unattended modes

Profiles compile into deterministic rules. Unattended policy never creates a
human wait unless explicitly configured with an external reviewer. Missing or
failed reviewers follow fail-closed fallback.

### 27.8 ChatML approval

Current ChatML text/choice approval remains a distinct moderator pending input
and is projected through the generic permission event shape only at the
client boundary. Its response calls `resume_ui_request`. It is marked
non-resumable in persisted operation metadata.

Long-term durable ChatML approval requires changing ChatML to commit a request
and later consume an `Approval_resolved` internal event. That redesign is a
separate implementation phase and must not be simulated by serializing an
evaluator continuation.

### 27.9 Parallel tool permissions

Parallel tool execution may create more than one pending permission for one
foreground operation. `Permission_state.t` is therefore a map ordered by
creation sequence, not one optional request.

The lifecycle's `Waiting_for_permission` summary identifies the oldest visible
blocking request, while snapshots/events expose every request the principal is
allowed to see. Resolving one request wakes only its waiting tool fiber. The
operation remains permission-blocked while any required tool permission is
pending.

Profiles may set `serialize_interactive_approvals` to expose one newly created
interactive request at a time while retaining the same durable map. Stop or
operation cancellation resolves every unresolved request with the appropriate
cancelled/denied terminal reason.

### 27.10 Reviewer implementations

Implement reviewer strategies behind one interface:

```ocaml
module type Reviewer = sig
  val review : Request.t -> (Decision.t, Error.t) result
end
```

- Human review leaves the durable request pending for authorized clients.
- Policy review is pure/deterministic and returns immediately.
- Model review runs a configured no-tool reviewer recipe with strict
  structured decision decoding, bounded tokens/time, and audited model
  identity. It uses a durable job when the parent session is durable.
- External review sends a redacted request with permission ID and idempotency
  key to a configured endpoint using bearer, mTLS-through-proxy, or HMAC
  authentication. Retries reuse the same key and obey timeout/backoff.

Reviewer output is advisory within the profile ceiling: it cannot override a
hard deny or request a broader grant scope than offered. Malformed, timed-out,
or unavailable reviewers resolve through the configured fail-closed fallback.

`Catalog_builder` receives an optional reviewer resolver from daemon options.
The resolver is keyed by reviewer kind and configured ID. Catalog compilation
installs a typed unavailable reviewer when no implementation is present so
configuration remains loadable but every attempted review fails closed.
Reviewer callbacks may perform Eio-backed model or network work, must return a
strict typed decision, and must not receive unredacted secrets. Each callback
registration supplies a nonempty security revision included in the compiled
profile digest, so reviewer configuration changes cannot masquerade as the
same pinned policy. The generic
gate and unattended shell-policy adapter use the same compiled reviewer.
Interactive timeout fallback remains denial unless the durable timeout path
has an installed reviewer job; it must never silently broaden to approval.

## 28. Shell runtime adapters

### 28.1 Actor-backed stores

Add callback/backend constructors to Shell_runtime instead of passing a
shared mutable `Session.t ref`:

```ocaml
Approval_store.create
  : load:(unit -> (grant list, error) result)
  -> commit:(Mutation.t -> (grant list, error) result)
  -> bindings:bindings
  -> t

Manifest_grant_store.authorizer
  : lookup:(Manifest_identity.t -> (grant option, error) result)
  -> remember:(grant -> (unit, error) result)
  -> ...
```

Keep existing `session` constructors as legacy adapters over the generic
backend.

### 28.2 Persistence routing

Daemon shell mutations send synchronous actor requests. The actor commits
shell state and server audit event before the store operation returns success.

Extension snapshot persistence uses the same actor-backed path and must not
call `Session_store.save`.

### 28.3 Manifest authorization

Manifest grants remain bound to exact manifest/source/builtin/user/host
identity. Prompt revision pinning supplies source material identity. A changed
manifest cannot reuse an old grant.

Version 1 configuration may include optional operator grants with this exact
shape:

```lisp
(manifest_grants
 (((id GRANT_ID)
   (prompt PROMPT_ID)
   (workspaces (WORKSPACE_ID ...))
   (manifest_sha256 HEX_SHA256)
   (source_sha256 HEX_SHA256)
   (principals (PRINCIPAL_ID ...)))))
```

`principals` is optional and an empty list means every authenticated creating
principal. Validation rejects malformed hashes, opaque principal IDs,
duplicate grant IDs, missing prompts/workspaces, and workspace bindings not
allowed by the prompt. Catalog compilation converts prompt/workspace slugs to
stable opaque IDs. Runtime matching requires exact prompt, workspace,
principal, root-source digest, artifact manifest digest, and requested
manifest digest. A match returns only `Authorize_once`; the actor-backed
manifest store must durably commit the complete session grant before runtime
construction continues. Catalog reload atomically replaces the operator grant
set used for future runtime construction.

### 28.4 Shell command approval

The generic gate returns `Delegate` for shell tools when the shell runtime's
approval policy is authoritative. `Shell_runtime.Approval_broker` is adapted
to create the same durable server permission records and wait on the same
actor compare-and-set resolution path.

There must be one client-visible approval, not one generic request followed by
a second shell request.

### 28.5 Interrupted requests

On startup, derive interrupted shell records from session audit using a new
store-aware adapter. Do not use `Shell_runtime.Interrupted_store.refresh` in a
way that writes through legacy `Session_store`.

Unknown shell side effects remain interrupted and are never automatically
rerun.

## 29. Durable job service

### 29.1 Modules

Add to `agent_server`:

```text
job.ml/mli
job_store.ml/mli
job_scheduler.ml/mli
job_worker.ml/mli
job_recovery.ml/mli
job_delivery.ml/mli
```

These scheduler/service modules live in `agent_server`. Persisted job wire
types live in `agent_protocol`, and journal/store codecs live in
`agent_store`. Session actors communicate through injected `Host_services`
callbacks, avoiding a library dependency cycle.

### 29.2 Job state

Persist:

```ocaml
type t =
  { id : Job_id.t
  ; session_id : Session_id.t
  ; generation : int
  ; kind : Kind.t
  ; payload : Jsonaf.t
  ; status : Status.t
  ; retry_policy : Retry_policy.t
  ; attempt : int
  ; external_idempotency_key : string option
  ; created_at : Timestamp.t
  ; started_at : Timestamp.t option
  ; next_run_at : Timestamp.t option
  ; completed_at : Timestamp.t option
  ; result : Result_payload.t option
  ; delivery : Delivery.t
  }
```

Statuses are queued, running, waiting permission, succeeded, failed,
cancelled, and interrupted.

### 29.3 Scheduler ownership

One daemon scheduler owns global runnable-job selection. Session actors own
session-visible state and event delivery. The scheduler never edits session
state directly.

Job creation is committed by the session actor. The scheduler discovers the
committed job and acquires configured semaphores before dispatch.

### 29.4 Dispatch algorithm

1. select an eligible queued job by due time and fairness;
2. ask the owning actor to compare-and-set it to running;
3. commit running state and increment attempt;
4. launch a worker under a job switch;
5. collect terminal result;
6. ask actor to commit terminal state;
7. enqueue the appropriate ChatML internal event when required; and
8. commit delivery acknowledgement after the manager accepts the event.

### 29.5 `Model.spawn`

Replace durable daemon use of `Chat_response.Model_executor` with a capability
whose recipe creates a durable model job. The ChatML operation returns the job
ID after the queued job transaction commits.

The job worker may reuse `Driver.run_agent` and current recipe logic. It must
use pinned prompt/runtime context, response directory, shell policy, and
session attribution.

On success/failure, encode the same internal event shape expected by current
moderator logic. Delivery is idempotent and separate from execution.

### 29.6 `Model.call`

Synchronous `Model.call` is represented as a job awaited by the active
moderator/operation. It still receives a durable intent and terminal state.
After restart, a non-idempotent in-flight call is interrupted; the ChatML
continuation that awaited it is not claimed to have resumed.

### 29.7 Nested agents and asynchronous tools

Nested agent calls executed synchronously inside a tool remain part of the
foreground operation unless explicitly migrated to durable jobs. Their live
trace is attributed by parent call ID.

Any future asynchronous tool must declare:

- persisted payload codec;
- recovery class;
- external idempotency behavior;
- permission boundary;
- result codec; and
- ChatML/tool-output delivery behavior.

### 29.8 Cancellation

Job cancellation commits requested cancellation before failing the worker
switch. Unknown external side effects are recorded as interrupted. Terminal
jobs return their existing state on repeated cancellation.

### 29.9 Job fairness and limits

Enforce semaphores at:

- daemon total;
- principal;
- prompt;
- workspace conflict domain;
- session;
- kind; and
- nested depth.

Runnable selection uses fair queues by session so one orchestration loop
cannot consume all worker capacity.

## 30. Durable schedules

### 30.1 Schedule state

Persist:

```ocaml
type t =
  { id : Schedule_id.t
  ; session_id : Session_id.t
  ; generation : int
  ; payload : Chatml.Chatml_value_codec.Snapshot.t
  ; created_at : Timestamp.t
  ; next_due_at : Timestamp.t
  ; repeat : Repeat.t option
  ; misfire : Misfire_policy.t
  ; status : Status.t
  ; delivery_count : int
  ; last_delivery_at : Timestamp.t option
  }
```

Use ChatML's snapshot/value codec rather than an arbitrary runtime value in
durable storage.

### 30.2 Timer service

The daemon scheduler maintains a min-heap of next due times rebuilt from
durable state. Timers are wakeup optimizations; persisted schedule state is
authoritative.

Clock access is injected. Tests use a manual clock.

### 30.3 `Schedule.after_ms`

The ChatML host handler validates nonnegative delay, converts to an absolute
server timestamp with checked arithmetic, and asks the actor to commit a
schedule. It returns the durable schedule ID.

### 30.4 Due delivery

When due:

1. scheduler asks actor to claim delivery;
2. actor commits a firing/delivery intent;
3. actor enqueues the payload into moderator internal events;
4. actor snapshots moderator queued state;
5. actor commits delivered/rescheduled terminal state; and
6. actor wakeup/idle logic processes it at a safe point.

Crash between steps is reconciled by delivery identity. A payload is accepted
into the moderator queue at most once per delivery occurrence.

### 30.5 Misfire

On recovery:

- `Deliver_once_immediately` claims one occurrence and advances repeat state;
- `Skip_if_expired` records a skipped terminal occurrence; and
- `Fail` records visible schedule failure.

The default is deliver once immediately.

### 30.6 Repeating schedules

Protocol 1.0 supports one-shot schedules only. Reject nonempty repeat fields
with an unsupported-feature error. The wire type reserves repeat policy for a
later negotiated feature, which must define fixed-rate versus fixed-delay
semantics before implementation.

## 31. Session registry

### 31.1 Modules

Add:

```text
session_registry.ml/mli
actor_handle.ml/mli
actor_loader.ml/mli
actor_cache.ml/mli
```

### 31.2 Registry state

The registry maps session ID to one of:

```ocaml
type entry =
  | Unloaded of Session_index.Entry.t
  | Loading of load_promise
  | Loaded of Actor_handle.t
  | Unloading of unload_promise
  | Deleted
```

All registry transitions are serialized under one small mutex or registry
actor. Long I/O occurs outside the critical section using a published loading
promise so concurrent callers share one load.

### 31.3 Resolve/load

`resolve`:

1. checks authorization against index metadata;
2. returns the loaded handle if present;
3. joins an existing load when loading;
4. marks unloaded entry loading;
5. opens/recover store and starts actor outside lock;
6. installs loaded actor if the registry entry still matches; and
7. returns typed failure otherwise.

### 31.4 Startup recovery

Daemon startup scans the session index and loads:

- desired-running sessions;
- sessions with owner-bound grace state requiring reconciliation;
- sessions with due schedules;
- sessions with runnable/deliverable jobs; and
- sessions requiring recovery transactions.

Stopped inactive sessions remain unloaded.

### 31.5 Shutdown

Registry shutdown marks the server draining, rejects new loads/creates, sends
shutdown to loaded actors, waits to the configured deadline, and reports
actors that failed to checkpoint.

## 32. Attachments and owner leases

### 32.1 Attachment state

Connection attachment state is primarily live and includes:

- attachment ID;
- session ID;
- principal ID;
- granted mode;
- scopes;
- connection ID;
- subscription visibility;
- last acknowledged/replayed event sequence; and
- optional owner lease generation.

Ordinary attachments need not survive daemon restart. Owner-bound liveness
state does survive as lease metadata and is reconciled on restart.

### 32.2 Owner lease

Persist:

```ocaml
type t =
  { attachment_id : Attachment_id.t
  ; principal_id : Principal_id.t
  ; generation : int64
  ; expires_at : Timestamp.t
  ; disconnect_grace_until : Timestamp.t option
  ; reclaim_identity : string option
  }
```

Reclaim tokens, if used, are stored hashed and returned only once. Prefer
authenticated principal identity for local/managed clients.

### 32.3 Renewal

`session.renew_owner` is compare-and-set on lease generation. On success it
increments generation and extends expiry. The response contains the next
renewal deadline.

Implicit renewal is allowed only when negotiated. It applies to authenticated
commands from the same owner attachment and still commits lease state at a
bounded interval rather than on every keystroke/message.

### 32.4 Last owner loss

Detaching or expiring the last owner commits grace state and schedules a
timer. Reclaim clears grace. Timer firing asks the actor to compare current
lease state before committing configured graceful/cancelling stop.

Read/write nonowners and read-only observers never satisfy owner-bound
liveness.

### 32.5 Connection loss

Transport connection closure invokes detach for its live attachments. Failure
to deliver detach is harmless because lease expiry is authoritative.

Detached sessions have no owner requirement and continue unchanged.

## 33. Event implementation

### 33.1 Durable event type

Use a closed internal variant and explicit wire projection:

```ocaml
type t =
  | Session_created of Session.Created.t
  | Session_state_changed of Session.State_change.t
  | Session_updated of Session.Update.t
  | Owner_changed of Owner.Change.t
  | History_message_deferred of History.Deferred.t
  | History_appended of History.Append.t
  | History_replaced of History.Replace.t
  | Moderator_overlay_changed of Moderator.Change.t
  | Moderator_notification of Moderator.Notification.t
  | Permission_requested of Permission.Request.t
  | Permission_resolved of Permission.Resolution.t
  | Grant_revoked of Grant.Revocation.t
  | Operation_started of Operation.Started.t
  | Operation_completed of Operation.Completed.t
  | Operation_failed of Operation.Failed.t
  | Operation_cancelled of Operation.Cancelled.t
  | Operation_interrupted of Operation.Interrupted.t
  | Job_state_changed of Job.Change.t
  | Schedule_changed of Schedule.Change.t
  | Prompt_upgraded of Prompt.Upgrade.t
  | Workspace_state_changed of Workspace.Change.t
  | Session_error of Session.Error_event.t
```

Each event is assigned a durable per-session sequence in the same transaction
as its state mutation.

### 33.2 Visibility projection

`Event.project` takes principal/session authorization and may:

- return a fully projected event;
- return a redacted event preserving sequence; or
- return a sequence-preserving hidden marker.

Clients must be able to advance their replay cursor even when an event payload
is invisible. Therefore filtering must not silently remove a durable sequence
without a replacement envelope indicating an inaccessible event occurred.

### 33.3 Subscriber registry

One subscriber registry belongs to each loaded actor. A subscriber contains a
bounded outgoing queue, visibility policy, attachment ID, last queued durable
sequence, and close callback.

Durable event publication enqueues after commit. Slow subscribers are closed
with their last queued cursor. They never block actor progress.

### 33.4 Initial subscription

Attach is race-free:

1. actor captures latest committed sequence;
2. actor registers subscriber at `latest + 1`;
3. replay service reads requested retained events up to captured latest;
4. transport emits replay;
5. live queue emits events after captured latest; and
6. duplicates are removed by sequence if replay/live overlap.

If the cursor is too old, return snapshot-required before enabling live
delivery, or deliver a snapshot then begin at its latest sequence.

### 33.5 Live operation events

Recoverable live events use `(operation_id, operation_sequence)` and
`anchor_sequence`. They are visible only while retained. They do not consume
durable sequence numbers and are never accepted as `after_sequence` cursors.

### 33.6 Heartbeats

Transport heartbeats are connection events. They are not stored in the
session journal and do not alter session state.

### 33.7 Event retention and pruning

The retention service computes the oldest replayable durable sequence from:

- configured minimum replay age;
- configured maximum event count/bytes;
- the newest validated snapshot sequence;
- journal segment boundaries; and
- active administrative holds.

It may prune a journal segment only when the current snapshot reconstructs all
state after that segment and no retained durable event in it is required.
Pruning event replay never prunes canonical history from the snapshot.

When count/byte caps require dropping older replay events, the oldest
replayable sequence advances and older clients receive `snapshot_required`.
Open subscribers do not pin unlimited history; they must consume within queue
limits or reconnect from their last cursor.

Completed live-operation deltas use a separate age/byte retention window and
may be removed as soon as canonical terminal state is available and the
configured reconnect window expires.

## 34. Protocol dispatcher

### 34.1 Modules

Add to `agent_server`:

```text
connection_context.ml/mli
dispatcher.ml/mli
authorization.ml/mli
command_handler.ml/mli
response.ml/mli
```

### 34.2 Connection context

Contains:

- connection ID;
- selected protocol version/features;
- authenticated principal;
- transport kind;
- peer identity/address;
- payload limits;
- attachment registry; and
- cancellation switch.

### 34.3 Dispatch order

For every request:

1. validate envelope and request ID;
2. enforce initialization/version requirement;
3. decode method and parameters;
4. apply transport/request size limits;
5. authenticate if not already connection-bound;
6. authorize method-level scope;
7. resolve attachment/session when required;
8. submit typed command to catalog/registry/actor;
9. project result for principal visibility; and
10. encode response.

Session authorization is checked again inside the actor against current
attachment and scopes.

### 34.4 Batches

HTTP may accept JSON-RPC batches. Decode and size-check the complete batch,
then execute requests independently with bounded concurrency. Preserve each
request ID; response array order should match input order for predictability,
though clients must correlate by ID.

Do not provide cross-request atomicity for a JSON-RPC batch.

### 34.5 Notifications

Client notifications are accepted only for explicitly notification-safe
methods. Mutating session commands require request IDs so the client receives
accepted/rejected state and idempotency result.

## 35. Exact command-handler behavior

### 35.1 Protocol/server/catalog

- `protocol.initialize` negotiates version/features and binds the connection.
- `protocol.ping` returns payload, server time, and draining/readiness state.
- `server.info` returns nonsecret build, protocol, listener, and limit data.
- `server.health` returns scoped health; detailed store/session failures
  require administrative health scope.
- prompt/workspace list/get use immutable catalog snapshots and opaque
  pagination.

### 35.2 Session query

- `session.list` filters by authorized visibility, lifecycle, prompt,
  workspace, label, and owner.
- `session.get` loads an actor only when live state is required; stopped
  snapshots may be projected from store/index when safe.
- history windows use canonical IDs and preserve structural tool pairs.

### 35.3 Attach/detach

- attach authorizes requested mode and may downgrade it;
- owner mode creates/reclaims a lease;
- attach returns replay or snapshot disposition;
- detach is idempotent and removes subscriber/attachment state; and
- connection teardown detaches every connection-owned attachment.

### 35.4 Session mutations

All mutations require attachment ID except creation and explicitly
administrative operations. Read-only attachments fail before actor mutation.

`send_message`, permission responses, destructive operations, job/schedule
creation, and grant revocation require idempotency keys.

Reset, rebuild, upgrade, and delete require expected session revision.

### 35.5 Export

Export captures a committed revision, renders through existing ChatMD export
logic, writes to a server-owned blob, and returns blob metadata. It never
writes a remote-client-supplied native path.

The export worker stages associated `.chatmd` artifacts using the daemon
equivalents of current `Chat_tui.Attachments.copy_all`: pinned prompt artifact
directory, workspace root, and session/cache directory, in that precedence
order. It enforces symlink, file-count, and byte limits and records skipped
artifacts. It must not substitute the client process cwd for the session
workspace.

Standalone local CLI export may accept a local output path because the local
process owner controls it.

### 35.6 Audit reads

Audit reads use a dedicated redaction/projection layer. They page by durable
audit sequence and never load an unbounded JSONL file into memory.

## 36. Unix-socket transport

### 36.1 Purpose

The Unix-socket transport is the preferred local daemon connection for TUI and
stdio gateway clients. It supports full-duplex NDJSON commands, responses,
and events on one connection.

### 36.2 Framing

Each UTF-8 line is exactly one JSON envelope. Enforce maximum line length with
`Eio.Buf_read` configured to the negotiated/server limit. JSON strings may
contain escaped newlines but not raw framing newlines.

### 36.3 Connection loop

Use separate reader and writer fibers under one connection switch:

- reader parses requests and submits bounded dispatch jobs;
- writer is the only fiber writing the socket;
- responses/events enter a bounded outgoing queue;
- writer preserves each envelope as one complete line; and
- connection cancellation detaches attachments and closes queues.

### 36.4 Peer authentication

When platform APIs expose Unix peer credentials, map UID/GID/PID into a local
principal through configured policy. If unavailable, require a bearer token or
explicit unsafe local mode.

Socket file permissions default to owner-only. Startup removes a stale socket
only after proving no live listener owns it.

The initial implementation uses an Eio-owned accepted socket descriptor and a
narrow cross-platform credential query: `SO_PEERCRED` on Linux and
`getpeereid` on macOS/BSD. It accepts only the daemon effective UID, derives a
stable `pri_` identity from that UID, and records `unix.uid`, `unix.gid`, and
`unix.pid` when available. No credential query owns, closes, reads, or writes
the descriptor outside Eio.

Before binding, validate with Eio that the socket parent is a directory owned
by the daemon UID with no group/other permission bits. If a socket node
already exists, attempt an Eio connection. A successful connection means a
live listener and startup fails. Only a refused connection is stale; re-stat
the device/inode immediately before unlinking to prevent replacement races.
Any other probe failure is fail-closed.

### 36.5 Subscription

`session.attach` with `subscribe=true` starts event delivery on the same
writer. Multiple attached sessions are permitted within configured limits.

## 37. Stdio transport

### 37.1 Standalone host

`ochat-agent-stdio --local` creates:

- an embedded `Agent_session` engine;
- an implicit current workspace captured at startup;
- a local arbitrary prompt revision;
- process-bound or configured transient/durable session state; and
- an in-process dispatcher connected to stdin/stdout framing.

EOF applies process-bound owner stop/cancel policy and waits for bounded
teardown.

### 37.2 Daemon gateway

`ochat-agent-stdio --connect ...` opens an `Agent_client` connection to a Unix
socket or HTTP daemon. It forwards stdin requests and writes daemon responses
and subscribed events to stdout.

It does not instantiate ChatMD, own tools, or persist daemon sessions.

### 37.3 Output discipline

Stdout contains protocol NDJSON only. Diagnostics and logs use stderr. Unlike
the existing MCP stdio client, stderr from child tools/processes must never be
merged into protocol stdout.

### 37.4 Concurrency

Use a reader fiber and one stdout writer fiber. Responses and events may
interleave. Backpressure on stdout eventually closes the gateway connection;
it does not block a detached daemon session indefinitely.

### 37.5 Initialization

The first input request must be `protocol.initialize`. A convenience CLI mode
may synthesize initialization and attach/create commands before forwarding
user input, but the wire protocol remains explicit.

## 38. HTTP transport

### 38.1 Module split

Create:

```text
http_server.ml/mli
http_router.ml/mli
http_body.ml/mli
http_auth.ml/mli
sse.ml/mli
sse_subscription.ml/mli
blob_routes.ml/mli
```

Copy small neutral patterns from MCP where useful, but do not import MCP
registries or messages.

### 38.2 Listener

Use Piaf under a daemon-owned switch. Listener configuration includes address,
port, authentication requirement, request limits, connection limits,
keep-alive intervals, and optional reverse-proxy trust policy.

Non-loopback plaintext listeners require explicit operator configuration and
authentication. TLS termination may initially be delegated to a reverse proxy
under an explicit trusted-proxy policy.

### 38.3 Routes

Implement:

```text
POST /v1/rpc
POST /v1/blobs
GET  /v1/blobs/<blob-id>
GET  /v1/sessions/<session-id>/events
GET  /v1/sessions/<session-id>/snapshot
GET  /v1/health
```

Unknown route returns 404. Unsupported method on a known route returns 405
with an `Allow` header.

### 38.4 RPC body

Stream/read the request with a strict maximum. Reject unsupported media type,
invalid UTF-8, malformed JSON, duplicate fields, oversized batches, and
missing protocol version before dispatch.

HTTP status conveys transport/auth/body outcome. Valid JSON-RPC application
errors normally return an HTTP success status with a JSON-RPC error envelope.

### 38.5 Authentication

Extract bearer tokens case-insensitively and validate through the shared
authenticator. Never use the global process tables in MCP OAuth storage as the
new session/auth store.

Generic OAuth route helpers may be extracted only if their storage and
principal mapping are made server-instance scoped.

### 38.6 SSE

The SSE endpoint:

1. authenticates principal;
2. authorizes session visibility;
3. parses `Last-Event-ID` or query cursor and rejects disagreement;
4. creates/validates an attachment or subscription token;
5. performs race-free replay/subscriber registration;
6. emits `id`, `event`, and one-line JSON `data` fields;
7. sends comment keep-alives without session sequence; and
8. unregisters the subscriber on observed body close or after a bounded
   no-pull watchdog confirms that the downstream writer has stopped consuming
   heartbeat frames.

SSE `id` is the durable session event sequence only for durable events. Live
operation events omit `id` or use a non-cursor event field; they must not
overwrite the browser/client durable replay cursor.

### 38.7 Snapshot endpoint

Returns a principal-projected snapshot with ETag derived from session revision
and latest event sequence. Support conditional GET. History window parameters
are bounded and use protocol cursor encoding.

### 38.8 Blob routes

Uploads stream to `Blob_store`; downloads stream from it. Apply content-length
and chunked-body limits, content-type validation, authorization, and cleanup
on disconnect.

The transport-neutral command set also includes `blob.read`. It accepts a
typed session ID, attachment ID, blob ID, nonnegative offset, and decoded-byte
limit no greater than 1 MiB. Dispatch verifies that the current connection
owns the attachment, authorizes transcript visibility, and opens only the
exact session-owned blob through `Blob_store.open_session`.
`Blob_store.read_range` uses Eio positioned reads and never loads the complete
blob.

The result repeats immutable metadata and carries Base64 data, `offset`,
`next_offset`, and `eof`. It is available over Unix, stdio, embedded, and HTTP
RPC. The HTTP GET route remains the preferred zero-copy streaming path when a
client transport exposes it.

### 38.9 Backpressure

Each HTTP response/SSE connection has bounded queues. Piaf body push failure
closes the subscription. No transport callback is allowed to mutate session
state directly.

Because Piaf does not provide a reliable body-close callback while a streaming
producer is blocked waiting for its next item, session SSE production is
pull-driven. The route records each writer pull, emits an initial comment and
periodic comment heartbeats, and detaches the temporary read-only observer
after three missed heartbeat intervals. The watchdog closes only transport
observer state; it never stops or otherwise mutates the durable session.

### 38.10 WebSocket deferral

Do not implement WebSocket until POST/SSE behavior and common client
reconnection are stable. A later WebSocket adapter must reuse the socket-like
duplex protocol and dispatcher.

## 39. Common client library

### 39.1 Modules

Create:

```text
transport.ml/mli
connection.ml/mli
request_table.ml/mli
session.ml/mli
subscription.ml/mli
projection.ml/mli
reconnect.ml/mli
in_memory.ml/mli
unix_socket.ml/mli
http.ml/mli
```

### 39.2 Transport interface

```ocaml
module type S = sig
  type t

  val send : t -> Agent_protocol.Envelope.Request.t -> (unit, Error.t) result
  val receive : t -> (Agent_protocol.Envelope.Server_message.t, Error.t) result
  val close : t -> unit
end
```

The concrete implementation may use callbacks/streams, but connection logic
must depend on one abstract full-duplex message interface plus HTTP-specific
subscription/upload helpers.

### 39.3 Connection

`Connection.t` owns:

- selected protocol/features;
- request ID generator;
- pending request promise table;
- reader fiber;
- writer serialization;
- connection state;
- authentication configuration;
- reconnect policy; and
- session attachment handles.

Unknown response IDs are logged and ignored or treated as protocol failure by
strict mode. Duplicate terminal responses are protocol errors.

### 39.4 Session handle

`Agent_client.Session.t` stores session/attachment identity, access mode,
owner lease state, last applied durable event sequence, and local projection.
It exposes typed methods rather than raw method names.

Its blob downloader loops over bounded `blob.read` calls, rejects metadata or
cursor changes, streams decoded bytes to an Eio sink, and reports success only
after final byte-length and SHA-256 validation. File-oriented front-ends write
to an exclusive temporary sibling, synchronize it, and rename it into place
only after validation.

### 39.5 Projection reducer

Implement a pure reducer:

```ocaml
val install_snapshot : Snapshot.t -> Projection.t
val apply_event : Projection.t -> Event.t -> (Projection.t, Error.t) result
val apply_live_event : Projection.t -> Live_event.t -> (Projection.t, Error.t) result
```

It validates durable sequence continuity, session revision monotonicity,
operation sequence order, history identities, and replacement events.

Projection state contains no Notty values and can be tested independently.

### 39.6 Reconnect

Reconnect behavior:

1. mark connection unavailable without marking session stopped;
2. reconnect with bounded exponential backoff and jitter;
3. reinitialize protocol;
4. reauthenticate;
5. reattach using last durable sequence and owner reclaim identity;
6. apply replay when available;
7. replace projection on snapshot-required; and
8. resume owner renewal.

Retry only transport-safe/idempotent commands. A mutating request with an
idempotency key may be resent after uncertain response delivery. A request
without such a key fails as outcome-unknown.

### 39.7 Owner renewal

The client starts one renewal fiber per owner attachment. Renewal occurs
before server-advertised deadline. Repeated failure marks ownership at risk
but does not locally claim the session stopped. Successful reconnect/reclaim
updates lease generation.

### 39.8 HTTP client

The HTTP client uses POST for commands, SSE for events, snapshot GET for
replacement, and blob routes for content. It coordinates POST responses and
SSE events through the same projection/request table. Clients without the
streaming-route extension use the ordinary `blob.read` command.

Event-stream EOF, failed event requests, malformed event envelopes, body-reader
failure and notification overflow end the transport's notification source after
queued envelopes drain. Subsequent commands return Interrupted. The transport
must not hide these failures in a private SSE retry loop: the common reconnect
owner reports status changes, opens a new logical connection and reattaches with
its last applied durable cursor. Failed transport cleanup closes owned clients
without waiting for a DELETE through a broken network path. SSE body-copy and
parsing fibers share a scoped Eio switch and close both pipe ends on failure.

Test this boundary with actual HTTP connection interruption, not only explicit
Connection.close. Verify disconnected/reconnecting status, preserved draft,
retained-event replay and expired-cursor snapshot replacement independently.

### 39.9 In-memory client

`Agent_client.In_memory` connects directly to an embedded dispatcher through
bounded Eio streams. It must encode/decode or at least round-trip the same
typed envelopes so standalone mode does not bypass command/event semantics.

### 39.10 Daemon endpoint selection

`ochat.agent_transport_client` composes the Unix-socket and HTTP client
adapters without adding session semantics. Its endpoint parser accepts only
absolute `unix://` paths after optional `~/` expansion or absolute `http://`
and `https://` URIs with a host. HTTP user information, query strings, and
fragments are rejected so credentials cannot be smuggled into endpoint logs.

Bearer tokens are separate opaque endpoint state. TUI and stdio clients load
`--bearer-token-file` through `Eio.Path.load`, trim one trailing/leading
whitespace envelope, reject empty/control/whitespace-bearing tokens, and
never include token contents in endpoint descriptions or protocol errors.
Supplying a bearer token for a Unix endpoint is an error.

## 40. Embedded server mode

### 40.1 Purpose

Embedded mode hosts the same session registry/actor logic inside TUI or stdio
without a daemon listener.

### 40.2 Embedded configuration

Construct an implicit config from CLI arguments:

- arbitrary local prompt path;
- current or explicit workspace root;
- process-bound owner liveness;
- transient or legacy-compatible persistence;
- interactive permission profile;
- local principal with requested scopes; and
- no remote listener.

### 40.3 Persistence choices

Support:

- transient in-memory store;
- new durable agent store at an explicitly selected local data root; and
- legacy `Session_store` import/export adapter for current `chat-tui`
  compatibility.

Do not make the daemon store optional locking rules weaker merely because the
server is embedded.

### 40.4 Process lifetime

The embedded process owns the actor switch. Process exit requests a bounded
graceful/cancelling stop and optional export/persistence. There is no claim
that work outlives the process.

## 41. TUI implementation and migration

### 41.1 New TUI session adapter

Add:

```text
lib/chat_tui/agent_session_client.ml/mli
lib/chat_tui/agent_projection.ml/mli
lib/chat_tui/agent_event_apply.ml/mli
lib/chat_tui/connection_status.ml/mli
```

### 41.2 Change `App.run_chat`

Split current `run_chat` into:

- terminal/presenter/highlighting setup;
- client connection/session selection;
- initial projection-to-model construction;
- client event reader;
- TUI input/controller loop; and
- shutdown/detach/export behavior.

Prompt parsing, runtime construction, model execution, moderator ownership,
shell stores, history allocation, and compaction leave `Chat_tui.App` and move
behind the session client.

### 41.3 TUI command mapping

Map user actions:

| TUI action | Client command |
|---|---|
| submit draft | `session.send_message` |
| cancel active operation | `session.cancel_operation` |
| compact | `session.compact` |
| delete selected canonical occurrence | `session.delete_history` with expected revision; remove matching tool pair; idle/stopped and writable only |
| approval answer | `permission.respond` |
| shell grant revoke | `grant.revoke` |
| reset/rebuild/export | corresponding session command |
| quit local process | stop/detach according to mode |
| quit connected detached session | detach only |

### 41.4 Event-to-model mapping

`Agent_event_apply` converts protocol projection changes into existing
`Model` operations:

- canonical history snapshot/replacement updates `history_items`;
- live sourced events reuse `Stream.handle_event` where possible;
- tool progress updates Agent page;
- permission events activate the existing interaction UI;
- activity events set model activity;
- lifecycle/failure events update status bar; and
- connection events update a client-local disconnected indicator.

Do not use list indexes as stable remote identity. Continue using
`History_entry.Id`, projected message IDs, call IDs, and operation IDs.

Use the sourced channel as the single live-text rendering path; paired
history-correlated notifications do not append text again. Ignore transient
events for IDs already present in canonical history. After a durable revision
rebuilds Chat rows, replay only their needed transient presentation; keep
Agent-page tool event cursors separately by operation sequence so progress is
not appended twice. Retire those cursors when their retained operation events
are removed. The shared tool classifier includes built-in `fork` as a subagent.

Use both revision and durable event sequence when deciding whether to reconcile
Chat rows: history and moderator-view events from one transaction share a
revision. `moderator.overlay_changed` carries `halted` plus optional
`effective_history` and `halt_reason` fields, using omission for absent values.
Replace those projection fields together and retain canonical history separately.
Build effective entries from the committed moderator identity snapshot with
inserted/replaced provenance. Actor transitions append the view notification
after other events whenever the committed effective view or halt state changes.
Foreground moderator checkpoints are fenced by active operation ID; unchanged
snapshots are no-ops and cancelled/stale workers cannot commit them.

Retain the last observed durable terminal operation in the client projection
until another operation starts. Apply its terminal outcome to still-running
Agent-page calls even when coalescing has removed transient tool-finish events.
Use canonical tool output when available. Snapshot installation clears this
transient terminal observation; do not fabricate an outcome from an idle
snapshot. A newly observed active operation clears old transient call state.
Share permission-modal presentation between the terminal controller and action
traces so repeated projections preserve selection and resolution closes the modal.

The Responses queue-to-sequence adapter must be fully lazy: requesting one
sequence node reads one queue element and never waits for the next element.
Test this without networking and also with a provider that pauses after a delta.

When constructing a fork context, keep private allocator/registry/source IDs
and inherited tool authorization, but do not inherit the root moderator,
deferred-input consumer, runtime-request sink or canonical history/tool-output
callbacks. Preserve source-attributed observation and invocation progress;
the parent commits its own call/result pair after the child returns.

### 41.5 Local mode first

First run the TUI through `Agent_client.In_memory` and embedded server while
preserving current CLI defaults. This proves client projection parity before
network behavior is introduced.

### 41.6 Connected mode

Add flags/subcommands:

```text
--local
--connect URI
--session ID
--new-daemon-session
--prompt PROMPT_ID
--workspace WORKSPACE_ID
--detached
--owner-bound
--read-only
```

Existing local `-file`, `--new-session`, export, and legacy session-management
flags remain supported during migration. Reject ambiguous combinations early.

### 41.7 Quit semantics

- local process-bound session: prompt/export/persist and stop;
- connected detached session: detach and leave running;
- connected owner-bound session: detach/release owner, allowing grace policy;
- explicit `:stop`: send stop before detach; and
- lost connection: preserve draft/viewport and attempt replay/reclaim.

### 41.8 Local TUI state

Never send scroll position, selection, terminal size, draft cursor, undo
stack, page choice, highlight caches, or render materialization to the server.

### 41.9 TUI parity gate

Connected mode is not complete until characterization tests show equivalent:

- initial prompt history;
- tool availability and behavior;
- streaming text/reasoning;
- tool and nested-agent progress;
- moderator overlays/notices/halts;
- deferred messages;
- approvals;
- compaction;
- cancellation repair;
- follow-up limits;
- grant management; and
- export semantics.

## 42. Daemon composition

### 42.1 Top-level services

`Agent_server.Daemon.t` owns:

- validated config reference;
- data store;
- prompt and workspace catalogs;
- session registry;
- quota manager;
- job/schedule scheduler;
- authenticator and authorizer;
- listener set;
- blob cleanup service;
- snapshot/retention maintenance;
- structured logger and metrics;
- health registry; and
- root Eio switch.

### 42.2 Startup sequence

1. initialize deterministic-safe cryptographic ID source;
2. parse CLI and configuration;
3. validate configuration and paths;
4. open data root and acquire daemon lock;
5. verify/migrate store schema;
6. load/create stable server ID;
7. load prompt/workspace catalogs;
8. rebuild session/global indexes as needed;
9. start registry, quota, job, schedule, retention, and health services;
10. recover required sessions;
11. bind Unix socket;
12. bind HTTP listener;
13. mark readiness; and
14. await shutdown signal.

Do not report ready before the store and required recovery scan are complete.

### 42.3 Shutdown sequence

1. mark server draining;
2. stop accepting new connections/sessions/mutations;
3. emit `protocol.shutdown` where possible;
4. stop new job/schedule dispatch;
5. ask actors to gracefully checkpoint/cancel within policy;
6. terminate child process groups;
7. flush session journals and indexes;
8. install pending snapshots when time permits;
9. close subscriptions/listeners;
10. close actors and release session locks;
11. release daemon lock; and
12. exit with status reflecting shutdown success.

Forced cancellation at deadline leaves valid intent records for recovery.

### 42.4 Signals

Handle termination signals through an Eio-safe notification path. A first
signal begins graceful shutdown. A second signal may shorten the deadline but
must still avoid unsafe recursive deletion or corrupt writes.

### 42.5 Supervision

Failure of a listener, scheduler, or index writer is reported to the daemon
supervisor. Required-service failure moves health to degraded or initiates
shutdown according to config. A detached child fiber must not fail silently.

## 43. Authentication

### 43.1 Authenticator interface

```ocaml
module type S = sig
  val authenticate
    :  Request_identity.t
    -> Credentials.t
    -> (Principal.t, Error.t) result
end
```

Implement instance-scoped authenticators for:

- static bearer tokens;
- OAuth bearer validation;
- trusted reverse-proxy identity;
- Unix peer credentials; and
- explicit development anonymous mode.

### 43.2 Static tokens

Configuration references token files or hashed token values. Do not log raw
tokens. Compare secret material in constant time where practical. Token
metadata maps to principal ID, scopes, attributes, and optional expiration.

### 43.3 OAuth

If reusing OAuth helpers, move mutable token/client storage into a daemon
instance. Authentication output must be an Ochat principal, not an MCP session.

### 43.4 Reverse proxy

Trust asserted identity headers only when the direct peer matches configured
trusted proxies and the proxy strips client-supplied copies. Otherwise ignore
the headers.

### 43.5 Development mode

Anonymous mode requires an explicit flag and defaults to loopback/owner-only
Unix socket. Server info and logs must state that unsafe development auth is
active.

## 44. Authorization

### 44.1 Scope checks

Implement method-level scope tables and resource-level predicates. Scope
checks include:

- prompt/workspace discovery;
- session creation;
- transcript read;
- event subscription;
- message write;
- owner lease;
- approval response;
- grant/audit access;
- lifecycle management;
- destructive deletion; and
- configuration administration.

### 44.2 Session ACL

Protocol 1.0 authorizes sessions through:

- the creating principal, which receives read, write, approve, own, and manage
  roles;
- configured administrative principals/scopes; and
- validated config policy rules mapping principal IDs/attributes and session
  prompt/workspace/labels to read, write, approve, own, or manage roles.

Persist the creating principal and the creation policy revision for audit.
Reevaluate current policy at attach and mutation time so reload can revoke or
grant future access. Protocol 1.0 has no mutable per-session ACL command; add
one only with a later protocol feature and durable ACL event schema.

### 44.3 Read-only behavior

Read-only attachments cannot:

- send messages;
- start/stop/cancel/reset/rebuild/upgrade/delete;
- respond to approvals;
- create/cancel schedules/jobs;
- revoke grants; or
- alter labels.

Transport code does not implement this rule alone; actor command validation
does.

### 44.4 Event visibility

Authorization projection distinguishes transcript, security state, audit,
tool detail, and administration. A principal may receive redacted sequence
markers to maintain replay continuity.

## 45. Security implementation

### 45.1 Path rules

Create a shared `Native_path` validation module for server-controlled paths.
Normal remote RPC types contain IDs, not native paths.

Every cleanup action verifies ownership metadata immediately before deletion.

### 45.2 Secrets and redaction

Use Shell_runtime secret filters for shell-related payloads. Add protocol/log
redaction for bearer tokens, authorization headers, environment values,
uploaded secret metadata, and reviewer credentials.

Redaction occurs before data enters ordinary event payloads or structured
logs, not only at rendering time.

### 45.3 Prompt trust

Catalog inclusion does not imply shell manifest authorization unless profile
explicitly says so. Store and audit exact prompt revision and manifest digest
for every authorization decision.

### 45.4 Resource confinement

Workspace selection is not sandboxing. The server surfaces this in config
validation/docs and relies on ChatMD tool roots, shell administrative policy,
and configured execution backends for actual confinement.

### 45.5 Denial of service

Apply limits before expensive work:

- HTTP body/NDJSON line size;
- JSON depth/field count;
- batch length;
- concurrent requests per connection/principal;
- attachments/subscribers per session;
- history window size;
- replay event count/bytes;
- blob upload bytes;
- actor command queue;
- pending permission count;
- jobs/schedules; and
- prompt parse/source closure size.

## 46. Observability

### 46.1 Logging interface

Use one structured logging facade with fields:

- timestamp;
- severity;
- component;
- server/session/operation/job/schedule/permission IDs;
- principal/connection IDs when authorized;
- event code; and
- redacted data.

Avoid global ChatML print sinks in a multi-session daemon. Replace global sink
mutation with session-scoped sinks passed through capabilities, or serialize
and route current global hooks until the underlying ChatML interface is made
instance-scoped.

### 46.2 Metrics

Implement counters/gauges/histograms for all metrics named in the architecture
specification plus:

- actor mailbox occupancy;
- commit batch size;
- journal bytes/segment rotations;
- snapshot lag;
- replay index rebuilds;
- owner renewal failures;
- live-event drops/coalescing;
- idempotency hits/conflicts;
- blob bytes/cleanup;
- config reload outcomes; and
- migration duration/failure.

### 46.3 Health registry

Each required service publishes `Ready`, `Degraded`, or `Failed` with a stable
code and last transition time. Overall readiness requires store, registry,
scheduler, and configured listeners ready.

### 46.4 Audit

Server audit records are append-only and integrity chained where configured.
Record security-sensitive actions listed in the architecture. Audit append
failure follows configured fail-open/fail-closed policy, with shell execution
continuing to obey its stricter existing policy.

## 47. Resource limits and fairness

### 47.1 Limit configuration

Define a validated `Limits.t` covering:

- loaded/running sessions;
- root-agent quotas;
- nested depth/concurrency;
- foreground provider concurrency;
- tools and shell processes;
- jobs and schedules;
- commands per connection/session;
- subscribers and queue bytes;
- payload/history/replay/blob sizes;
- cache and event retention; and
- HTTP/socket connections.

### 47.2 Semaphore hierarchy

Acquire shared resources in a fixed global order to avoid deadlock:

1. global class semaphore;
2. principal semaphore;
3. conflict-domain semaphore;
4. prompt semaphore;
5. session semaphore; and
6. tool/job-specific semaphore.

Release in reverse order.

### 47.3 Fair scheduling

Use round-robin or weighted fair queues across sessions for provider turns,
jobs, and queued starts. Priority is reserved for cancellation, terminal
outcomes, owner expiry, and persistence failure.

### 47.4 Backpressure policy

Never let a slow client stop an agent. Coalesce live events, disconnect slow
subscribers, retain durable replay, and fail excessive command producers with
typed resource-limit errors.

## 48. Cache and artifact handling

### 48.1 Session cache

Continue using `Chat_response.Cache` initially, stored under session cache
directory. The actor/runtime builder owns load/save. Cache write failure is a
degradation unless a prompt declares it semantically required.

### 48.2 Concurrency

Do not share one mutable `Chat_response.Cache.t` across unrelated session
actors unless synchronization is added. Each runtime gets a session-scoped
cache.

### 48.3 Reset/rebuild

Reset and rebuild options explicitly retain or remove cache. Removal targets
only the session cache handle, never a client path.

### 48.4 Response artifacts

Raw provider response artifacts are written under
`sessions/<id>/responses/` using operation/job IDs. Their retention and access
are configured and scoped separately from canonical history. The
`response_artifact_ms` retention window is enforced by periodic Eio-only
maintenance that deletes regular files without following symbolic links.

## 49. Legacy session migration

### 49.1 Keep legacy store readable

Do not change `Session_store` into the daemon store. Continue reading V0-V5
legacy snapshots for existing TUI users.

### 49.2 Import command

Add an explicit import path:

```text
ochat agent sessions import-legacy --session LEGACY_ID \
  --prompt PROMPT_ID --workspace WORKSPACE_ID
```

Import:

1. reads/migrates legacy `Session.t` through current `Session_store` readers;
2. validates history allocator and moderator/shell state;
3. resolves configured prompt/workspace;
4. selects the legacy `local_prompt_copy` when present and requested,
   otherwise the recorded prompt file, then constructs a pinned prompt
   revision;
5. maps legacy conversation, tasks, key/value, moderator, and shell state;
6. creates a stopped durable server session;
7. records source legacy ID/path and migration diagnostics; and
8. never modifies the legacy source unless `--move` is explicitly requested.

Legacy `vfs_root` is not silently treated as the new workspace. The importer
requires an explicit target workspace. It records the old value as migration
metadata and may copy a verified legacy server-owned VFS tree into a managed
temporary workspace only under an explicit import option and cleanup policy.

### 49.3 Standalone compatibility adapter

During TUI cutover, an embedded session may load legacy `Session.t`, run
through the new engine, and export the resulting conversation back to a new
legacy V5 snapshot only when no new server-only state would be lost.

Prefer migration to the new store for long-running sessions.

### 49.4 Data-store schema migrations

Implement each new store schema as a numbered migration module:

```text
migration_v1_to_v2.ml
migration_v2_to_v3.ml
```

Never edit old migration semantics after release. Add a new migration.
Migration tests use golden pre-migration stores and crash injection at every
activation step.

## 50. Configuration and protocol compatibility

### 50.1 Independent versions

Track separately:

- server config schema;
- wire protocol;
- session store schema;
- journal frame schema;
- prompt artifact schema;
- moderator snapshot compatibility;
- shell state schema; and
- client projection schema.

### 50.2 Compatibility policy

The daemon refuses unsupported newer persistent schemas. Protocol minor
features are negotiated. Config unknown fields fail unless explicitly marked
extension fields. Prompt artifacts remain pinned and self-contained.

### 50.3 Build information

`server.info` reports build/version and supported schemas/features without
exposing filesystem layout or secret config.

## 51. Failure mapping

### 51.1 Typed internal errors

Each boundary defines a local error variant and one conversion to
`Agent_protocol.Error.t`. Avoid passing arbitrary strings across layers.

### 51.2 Required mappings

| Internal failure | Protocol/session behavior |
|---|---|
| store lock held | daemon startup `store_locked` |
| session lock held | session load `conflict` |
| prompt parse/closure invalid | `prompt_unavailable` with diagnostics |
| workspace missing/identity changed | `workspace_unavailable` |
| shell manifest rejected | `manifest_unauthorized` |
| command mailbox full | retryable `command_queue_full` |
| subscriber queue full | close subscriber, replay permitted |
| journal append/flush failure | fail durable session closed |
| snapshot failure with valid journal | degraded health, continue journal |
| corrupt middle journal | fail session closed |
| provider timeout | fail operation, repair history, session may idle |
| tool runner exception | canonical error output or operation failure per existing response semantics |
| permission denial | canonical denied output and post-tool moderation |
| lost client | detach only or owner grace |
| daemon crash during unknown side effect | interrupted, no automatic replay |
| suspended ChatML approval on restart | explicit interrupted approval |
| stale worker completion | ignore/audit without mutation |

### 51.3 Panic policy

An invariant failure isolated to one session fails that actor/session. A
data-root ownership or systemic persistence invariant failure initiates daemon
shutdown. Transport parse errors never crash the daemon.

## 52. Testing architecture

### 52.1 Test support library

Create `test/agent_server/test_support/` with reusable fakes:

```text
manual_clock.ml
deterministic_ids.ml
fake_provider.ml
fake_tool.ml
fake_reviewer.ml
fake_transport.ml
temporary_store.ml
crash_injector.ml
event_collector.ml
session_fixture.ml
prompt_fixture.ml
workspace_fixture.ml
```

All timeouts, lease expiry, schedule due times, retry backoff, and retention
tests use `Manual_clock` rather than wall-clock sleeps.

### 52.2 Unit tests

Add unit tests for:

- every protocol codec and unknown-field rule;
- version negotiation and feature intersection;
- ID parsing/path safety;
- error conversion;
- config parsing, diagnostics, validation, and reload diff;
- prompt source closure hashing and pinning;
- workspace canonicalization and cleanup guards;
- quota keys, queue fairness, and acquisition rollback;
- lifecycle pure transitions;
- permission rule precedence;
- owner lease compare-and-set and expiry;
- event visibility projection;
- pagination cursor signing/validation;
- idempotency request digests;
- journal frame encode/decode/checksum;
- delta application;
- snapshot validation;
- schedule misfire; and
- recovery classification.

### 52.3 Existing behavior characterization

Before extraction, preserve/add tests around current TUI/response behavior.
After extraction, run the same fixtures against the embedded session engine.

Important existing test suites include history identity, moderation safe
points, response-loop execution, shell runtime phases, type-ahead separation,
message presentation, Agent-page progress, and cancellation repair.

### 52.4 Actor model tests

Test with multiple producer fibers:

- concurrent messages accepted in actor order;
- read-only mutations rejected;
- cancellation races with normal completion;
- first permission response wins;
- owner detach/reclaim/expiry races;
- stale worker events ignored;
- moderator wakeup coalescing;
- deferred message arrival before/after last safe-point drain;
- stop racing with queued start;
- quota slot release after startup failure;
- snapshot completion racing with newer revisions; and
- mailbox saturation retaining priority progress.

Use deterministic barriers/promises, not probabilistic sleeps.

### 52.5 Persistence crash matrix

Inject process-like failure after every persistence step:

- temporary file create/write/fsync/rename;
- journal header, payload, checksum, and flush;
- transaction append before actor install;
- actor install before index update;
- snapshot write before/after `CURRENT` switch;
- journal segment seal/rotation;
- job dispatch intent and completion;
- schedule delivery intent and queue acceptance;
- blob upload/adoption;
- reset generation archive/activation; and
- schema migration activation.

Restart the store and assert either the old committed state or the new
committed state, never an invented mixture.

### 52.6 Recovery tests

Cover:

- clean daemon restart with idle/running desired sessions;
- crash during provider stream;
- crash before/after canonical tool output commit;
- crash during unknown shell side effect;
- durable completed job awaiting delivery;
- retryable/idempotent job;
- overdue schedule under each misfire policy;
- pending generic/shell approval;
- pending ChatML approval interruption;
- expired owner lease;
- missing physical workspace;
- changed prompt catalog with pinned artifact intact;
- missing/corrupt prompt artifact;
- incomplete final journal frame;
- corrupt middle frame/hash chain;
- missing replay index; and
- previous-snapshot fallback.

### 52.7 Protocol contract tests

Run the same command/event scenarios over:

- in-memory transport;
- Unix socket;
- standalone stdio;
- stdio daemon gateway;
- HTTP POST plus SSE; and
- HTTP reconnect with snapshot replacement.

Assert equivalent typed results and durable event ordering.

### 52.8 Multi-client tests

At minimum:

- two writers submit concurrently;
- writer plus read-only observer receive the same ordered durable events;
- read-only approval response fails;
- two approvers race;
- slow subscriber disconnects without slowing the turn;
- client reconnect replays from cursor;
- replay cursor too old requires snapshot;
- one client detaches while another stays;
- detached session continues with zero clients; and
- owner-bound session stops only after final owner lease/grace expiry.

### 52.9 TUI parity tests

Build a renderer-neutral transcript/action trace and compare standalone old
behavior fixtures to new embedded and connected projections. Test:

- messages and stable IDs;
- reasoning/tool call/output rows;
- live text and tool progress;
- deferred steering;
- moderator replacement/deletion/insertion;
- Agent-page nested trace;
- approval interaction;
- compaction replacement;
- cancellation repair;
- reconnect during active turn; and
- detached quit semantics.

Visual renderer tests remain TUI-local and consume the same model state.

### 52.10 Security tests

Cover:

- path traversal in IDs/blob references;
- physical workspace deletion refusal;
- temporary cleanup ownership checks;
- workspace not granting undeclared access;
- manifest digest binding;
- grant principal/host/session binding;
- hard deny preceding approval;
- shell delegation producing one request;
- redaction in events/logs/audit/errors;
- bearer token/header handling;
- reverse-proxy spoof attempts;
- Unix socket permission and peer policy;
- oversized/deep JSON; and
- unauthorized event sequence continuity.

### 52.11 Load and soak tests

Test configured numbers of:

- sessions;
- attached clients;
- SSE streams;
- queued commands;
- live deltas;
- durable journal transactions;
- background jobs/schedules; and
- repeated reconnects.

Run long-lived orchestration sessions across repeated daemon restarts. Measure
memory retention after actor unload and prompt artifact/cache reuse.

### 52.12 Test commands

Each phase adds focused Dune aliases or test executables. The final validation
must include:

```console
dune build
dune runtest
```

plus explicit agent-server integration, crash, and soak aliases that are too
expensive for the default unit suite.

## 53. Implementation phases

### Phase 0: Characterization and seams

Work:

- add missing tests around current TUI host semantics;
- isolate cancellation repair;
- identify all current writes through `Session_store.save`;
- replace `Session_store` process exits with typed/executable-boundary error
  handling;
- identify every global mutable sink used by ChatML/MCP/auth code;
- add small interfaces around clocks and ID generation; and
- document baseline performance and startup behavior.

Exit gate:

- current tests pass;
- extraction behavior is covered;
- no user-visible change.

### Phase 1: Protocol library

Work:

- implement IDs, versions, errors, principal/scopes;
- implement all command/result/event/snapshot types;
- implement explicit JSON codecs and initialization;
- add pagination and idempotency types; and
- add protocol golden tests.

Exit gate:

- every architecture method has a typed request/result;
- protocol round-trips and compatibility tests pass;
- no server runtime yet.

### Phase 2: Durable store foundation

Work:

- data root and locks;
- durable file replacement;
- journal frames, segments, writer, and checksums;
- snapshot install/read;
- session metadata/index;
- idempotency and blob stores;
- recovery and fault injection; and
- V1 store migration framework.

Exit gate:

- crash matrix proves atomic old-or-new state;
- lock contention returns typed errors;
- no library `exit` path.

### Phase 3: Config, prompt, workspace, quota

Work:

- versioned config parser/validator;
- prompt catalog and revision artifacts;
- workspace definitions/instances/cleanup;
- runtime path construction;
- quota manager and queued starts; and
- transactional config reload.

Exit gate:

- pinned prompt restores without live source file;
- physical/temp/current workspace tests pass;
- prompt/workspace limits and exclusive leases pass.

### Phase 4: Shared session engine skeleton

Work:

- session durable state and deltas;
- mailbox and actor;
- lifecycle transitions;
- subscribers/events;
- in-memory dispatcher/client;
- embedded session creation/start/stop; and
- no model execution yet or a fake turn worker.

Exit gate:

- multi-client actor tests pass with fake operations;
- detached/owner-bound lifecycle works in memory.

### Phase 5: Runtime and TUI controller extraction

Work:

- runtime builder;
- foreground turn adapter using `In_memory_stream`;
- canonical callbacks and safe-point deferred entries;
- moderator controller/wakeups;
- compaction and cancellation repair;
- generic permission hook with interactive broker;
- shell actor-backed stores; and
- embedded TUI moved to `Agent_client.In_memory`.

Exit gate:

- standalone TUI parity suite passes;
- existing local CLI remains usable;
- TUI no longer owns canonical history mutation or model turns.

### Phase 6: Durable daemon sessions

Work:

- daemon composition and registry;
- persistent actor recovery;
- snapshot/retention/index maintenance;
- desired-running startup recovery;
- owner lease timers; and
- structured health/shutdown.

Exit gate:

- detached sessions survive client loss and daemon restart;
- active unsafe operations recover as interrupted;
- repeated restart soak passes.

### Phase 7: Durable jobs, schedules, and approvals

Work:

- job scheduler/store/workers;
- durable `Model.spawn` delivery;
- synchronous job intents;
- durable schedules and misfire;
- generic/external/model reviewers;
- shell approval unification; and
- ChatML approval interruption/recovery reporting.

Exit gate:

- orchestration loops continue without clients;
- completed jobs redeliver without rerun;
- schedules survive restart;
- approval modes pass interactive and unattended tests.

### Phase 8: Unix socket and stdio

Work:

- duplex Unix NDJSON transport;
- local peer auth;
- common client socket transport;
- standalone stdio host;
- daemon stdio gateway; and
- CLI commands.

Exit gate:

- protocol contract suite passes over in-memory/socket/stdio;
- EOF semantics match local versus daemon modes.

### Phase 9: HTTP, SSE, and blobs

Work:

- Piaf listener/router;
- bearer/OAuth/reverse-proxy auth;
- RPC POST and batch handling;
- per-session replayable SSE;
- snapshot GET and ETag;
- blob upload/download; and
- backpressure/connection limits.

Exit gate:

- HTTP contract, replay, slow-client, auth, and blob tests pass;
- no legacy MCP server protocol/state dependency; explicitly declared MCP
  tools may use the shared runtime's MCP client adapter.

### Phase 10: Connected TUI

Work:

- Unix/HTTP client selection;
- connected creation/attach modes;
- reconnect/replay/snapshot replacement;
- owner renewal/reclaim;
- status rendering and disconnected draft preservation; and
- connected session management commands.

Exit gate:

- connected TUI parity passes;
- detached quit leaves daemon session running;
- owner-bound quit follows lease policy.

### Phase 11: Migration and hardening

Work:

- legacy session import;
- store migration CLI/dry-run;
- retention/GC;
- corruption diagnostics and recovery tools;
- load/soak/security tests;
- operational documentation; and
- performance tuning without invariant changes.

Exit gate:

- all architecture acceptance criteria and traceability rows pass;
- production defaults are secure and durable;
- the legacy MCP prompt-serving server remains separate and unchanged in semantics;
  MCP-backed ChatMD tools remain actively maintained and supported by the new runner.

## 54. File-by-file change inventory

### 54.1 Existing files to modify

`dune-project`:

- add no new dependency unless implementation proves necessary;
- use existing Digestif, Mirage crypto RNG, Piaf, Base64, Core, Eio, and Jsonaf
  where possible.

`lib/chat_response/agent_runtime.{ml,mli}`:

- expose tool security metadata;
- keep explicit host path variables;
- accept runtime-owned MCP discovery/cache service;
- avoid process-global assumptions.

`lib/chat_response/tool.{ml,mli}`:

- replace process-global MCP metadata cache with an injected, identity-aware
  cache/service;
- bind invalidation listeners to runtime/server switches.

`lib/chat_response/in_memory_stream.{ml,mli}`:

- accept generic invocation authorization callback;
- accept actor-backed history ID allocation source;
- emit mandatory moderator checkpoint callbacks at every committed safe-point
  transaction;
- preserve all current defaults when callback absent;
- expose any additional operation correlation needed by server adapter;
- retain safe-point semantics.

`lib/chat_response/history_stream_event.{ml,mli}`:

- construct registries from a history ID source rather than requiring all
  allocation to happen directly in a process-local allocator;
- reserve identity before publishing correlated live events.

`lib/chat_response/moderator_manager.{ml,mli}` and
`chatml_turn_driver.{ml,mli}`:

- expose transaction checkpoint boundaries needed by the durable host;
- preserve serialized manager execution and existing standalone behavior.

`lib/chat_response/tool_call.{ml,mli}` and
`tool_executor.{ml,mli}`:

- apply effective authorization decision before runner execution;
- preserve tool trace/cancellation behavior.

`lib/chat_response/model_executor.{ml,mli}`:

- retain as embedded/legacy implementation;
- factor recipe payload/result encoding so durable job service can reuse it.

`lib/chat_response/moderation.{ml,mli}` or capability construction sites:

- allow daemon-provided model and schedule handlers;
- keep unconfigured defaults explicit.

`lib/chatml/chatml_debug_log.{ml,mli}` and
`chatml_builtin_spec.{ml,mli}`:

- replace process-global debug/print sink selection in the daemon path with
  session-scoped capability/sink routing;
- retain a compatibility default for single-session CLI tools.

`lib/chatmd/prompt.{ml,mli}`:

- accept an explicit filesystem or artifact source loader;
- make any metadata required by parsing/revision construction parse-local or
  explicitly scoped;
- do not use one global metadata table as concurrent daemon state.

`lib/chatmd/chatmd_import_expansion.{ml,mli}` and
`chatmd_script_declaration.{ml,mli}`:

- resolve imports and external script sources through the injected source
  loader;
- preserve the existing filesystem loader as the default standalone path;
- produce materialized runtime source provenance for artifact-backed prompts.

`lib/shell_runtime/approval_store.{ml,mli}`:

- add generic callback-backed persistence backend;
- express mutation errors as typed values.

`lib/shell_runtime/manifest_grant_store.{ml,mli}`:

- add callback-backed lookup/remember/revoke path;
- retain legacy session adapter.

`lib/shell_runtime/interrupted_store.{ml,mli}`:

- separate audit-derived reconstruction from legacy session persistence.

`lib/chat_tui/app.{ml,mli}`:

- become terminal/client composition;
- remove runtime/model-executor/session-store ownership from connected path.

`lib/chat_tui/app_runtime`, `app_reducer`, `app_submit`, `app_compaction`,
`app_streaming`, `app_stream_apply`, and
`moderator_session_controller`:

- move host-semantic pieces to `agent_session`;
- retain UI-only reducers/adapters;
- keep compatibility wrappers only until parity cutover.

`lib/chat_tui/model.{ml,mli}`:

- add projection installation/status helpers as needed;
- do not add server ownership state.

`lib/session_store.{ml,mli}`:

- remain the legacy snapshot store rather than becoming the daemon journal;
- replace lock-contention `exit` with a typed `save` result;
- provide a clearly named `save_exn` compatibility wrapper where immediate
  migration is impractical; and
- update legacy CLI callers to render errors at executable boundaries.

`bin/chat_tui.ml`:

- add local/connect/create/attach/access/liveness flags;
- preserve legacy local flags.

`bin/main.ml` and `bin/dune`:

- add agent command group and new executables/libraries.

### 54.2 Existing files intentionally not repurposed

- `lib/session_store.{ml,mli}` remains the legacy standalone snapshot store;
  its error behavior is corrected, but its storage format is not repurposed.
- The MCP prompt-serving server remains separate legacy/deprecated code.
  MCP client, types, transports and tool adapters in `lib/mcp/` remain actively
  maintained dependencies of ChatMD MCP-backed tools, not deprecated features.
- renderer/highlighter/type-ahead modules remain TUI-local.

### 54.3 New test directories

```text
test/agent_protocol/
test/agent_store/
test/agent_session/
test/agent_server/
test/agent_transport/
test/agent_client/
test/chat_tui_agent_client/
```

## 55. Operational CLI specification

### 55.1 Server

```console
ochat-agent-server -config FILE
```

Support:

- config validation only;
- print normalized config without secrets;
- store migration dry-run/apply;
- foreground versus service-friendly logging;
- graceful shutdown timeout; and
- unsafe development auth override.

### 55.2 Stdio

```console
ochat-agent-stdio --local --prompt FILE [--workspace DIR]
ochat-agent-stdio --connect unix:///path
ochat-agent-stdio --connect https://host/base --bearer-token-file FILE
ochat-agent-stdio ...
```

### 55.3 Session administration

```console
chat-tui --connect URI --list-sessions
chat-tui --connect URI --session-info ID
chat-tui --connect URI --start-session ID
chat-tui --connect URI --stop-session ID [--cancel]
chat-tui --connect URI --export-session ID --out FILE
chat-tui --connect URI --reset-session ID [--keep-history]
chat-tui --connect URI --rebuild-from-prompt ID
chat-tui --connect URI --delete-session ID [--archive]
ochat-agent-server -config FILE -import-legacy LEGACY_ID -prompt NAME -workspace NAME
```

CLI clients use `Agent_client`; they do not open daemon session files
directly while the daemon owns the store.

### 55.4 Recovery administration

Current executable scope: schema inspection/planning and legacy import are
documented in the [operations guide](../agent-server/operations.md). Schema 1 is
the only supported on-disk schema. There is no stock general-purpose repair CLI
that certifies or repairs all journals/artifacts; the following is the design
constraint for any repair workflow, not an advertised available command.

Provide read-only validation and explicit repair/export commands for corrupt
sessions. Repair never silently skips committed middle-journal corruption. It
copies original material to `lost-and-found` or an operator-selected archive
before producing a new generation.

## 56. Documentation deliverables

Implementation must add/update:

- server setup and config reference;
- prompt/workspace/operator security guide;
- standalone versus daemon TUI guide;
- stdio protocol/client guide;
- HTTP/SSE API guide;
- permission profile and unattended automation guide;
- persistence/recovery guarantees;
- operational monitoring/shutdown/migration guide;
- legacy session import guide;
- module `.mli` documentation; and
- generated `docs-src` pages for new public libraries/binaries.

Documentation must explicitly state that workspace is not an authorization or
sandbox boundary.

## 57. Architecture traceability

| Architecture section | Primary implementation sections |
|---|---|
| 1 Purpose | 1-4, 53 |
| 2 Normative language | 1-2 |
| 3 Goals | 4-56, 58 |
| 4 Non-goals | 2.3-2.4, 13.6-13.7, 30.6, 38.10 |
| 5 Design principles | 4, 11-17, 20-28, 33-41 |
| 6 Terminology | protocol and state types in 5, 7-10, 17, 29-33 |
| 7 System architecture | 4, 17, 31, 34, 42 |
| 8 Execution modes | 36-42, 55 |
| 9 Workspace model | 8-9, 45.1, 47 |
| 10 Prompt catalog/revisions | 7, 19, 49 |
| 11 Server configuration | 6, 42, 50, 55 |
| 12 Session identity/state | 5, 10, 14-18 |
| 13 Session actor | 17, 31-33 |
| 14 Lifecycle operations | 18, 35 |
| 15 Runtime construction | 19 |
| 16 History model | 10.4, 21-24, 33, 39.5 |
| 17 Foreground turns | 21-22, 26-28 |
| 18 ChatML host behavior | 25-26, 29-30 |
| 19 Permissions/approvals | 27-28, 43-45 |
| 20 Durable jobs/schedules | 29-30 |
| 21 Persistence | 11-16, 49-51 |
| 22 Events | 33, 39.5 |
| 23 Commands/protocol | 5, 15, 34-35 |
| 24 Multi-client/ownership | 31-33, 39 |
| 25 HTTP | 38 |
| 26 Stdio | 37 |
| 27 TUI | 41 |
| 28 Common client | 39 |
| 29 Legacy MCP | 2.4, 38.1, 54.2 |
| 30 Authentication | 36.4, 38.5, 43 |
| 31 Security boundaries | 8.6, 27-28, 44-45 |
| 32 Resource limits | 9, 17.3, 29.9, 45.5, 47 |
| 33 Observability/audit | 46 |
| 34 Shutdown | 19.7, 31.5, 42.3-42.5 |
| 35 Failures | 12-13, 18, 21, 24, 29-30, 51 |
| 36 End-to-end examples | phase and transport tests in 52-53 |
| 37 Module organization | 4, 54 |
| 38 Implementation phases | 53 |
| 39 Testing | 52 |
| 40 Acceptance criteria | 58 |
| 41 Required invariants | 58-59 |

## 58. Implementation acceptance checklist

The implementation is complete only when all items are true:

1. One shared session actor/engine powers embedded TUI, embedded stdio, daemon
   Unix/HTTP access, and connected TUI.
2. ChatMD parsing, imports, tools, nested agents, shell declarations, and
   source-relative behavior match the current code.
3. Runtime paths set `${workspace}` to the selected concrete workspace and
   `${tool_dir}` to the captured launch directory unless explicitly overridden.
4. Workspace selection itself grants no filesystem/tool authority.
5. Prompt revisions are content-addressed, pinned, and restore from stored
   artifacts.
6. Physical/current/temporary workspaces and cleanup rules are implemented.
7. Prompt/workspace root-agent limits, exclusive leases, queues, and release
   paths work across restart.
8. Durable detached sessions continue with no clients.
9. Owner-bound sessions use renewable leases and stop only after grace expiry.
10. Process-bound local sessions end with the owning TUI/stdio process.
11. Canonical history uses `History_entry.Id` and survives restart.
12. Deferred messages are canonical, FIFO, and never split tool pairs.
13. At most one foreground turn/compaction runs per session.
14. Existing ChatML safe points, overlay behavior, halt, budgets, and wakeups
    are preserved.
15. Generic, shell, model, external, interactive, and unattended permission
    policies obey hard-denial precedence.
16. First valid approval response wins and read-only clients cannot answer.
17. Durable jobs and schedules survive restart and use idempotent delivery.
18. Unknown side effects are interrupted, never silently replayed.
19. Current nonserializable ChatML approval restart is reported honestly.
20. Every durable mutation is journaled before durable event publication and
    promised acknowledgement.
21. Snapshots, journal replay, frame-tail recovery, corruption failure, and
    migration pass fault injection.
22. Multiple clients receive ordered real-time events and can reconnect.
23. Slow clients are disconnected without blocking agents.
24. Read-only and read/write attachment behavior is enforced by the actor.
25. Unix socket, stdio, HTTP POST/SSE, and in-memory adapters share typed
    protocol semantics.
26. SSE is per session and replayable by durable sequence.
27. Blob input/export is bounded, authenticated, and path-safe.
28. TUI runs locally without a daemon and can create/attach to daemon
    sessions.
29. TUI presentation state remains client-local and drafts survive reconnect.
30. New server transports and session ownership have no dependency on legacy
    MCP server protocol/state. The shared runtime may depend transitively on
    MCP client adapters for explicitly declared remote tools.
31. Legacy `Session_store` and the deprecated MCP prompt server remain separate
    compatibility features. ChatMD MCP-backed tools remain actively maintained.
32. Lock/config/path/auth failures are typed and library code does not exit.
33. Security-sensitive state is redacted, scoped, and audited.
34. Shutdown checkpoints durable state and leaves recoverable intents when
    forced.
35. Full build, unit, integration, crash, security, parity, and soak gates
    pass.

## 59. Non-negotiable implementation invariants

1. Exactly one actor mutates one live session.
2. Exactly one allocator issues canonical history IDs for a session
   generation.
3. Durable state commits before its durable event is published.
4. A command result never claims durability beyond the completed flush level.
5. Provider deltas and UI rows are not canonical transcript state.
6. A stale operation/job/schedule/permission generation cannot mutate current
   state.
7. Client disconnection is not session completion.
8. Detached liveness is independent of all client connections.
9. Owner-bound liveness depends on valid owner leases, not observers.
10. Workspace identity is execution context, not authorization.
11. Prompt and workspace identity are pinned in durable state.
12. Physical workspaces are never deleted by session cleanup.
13. Remote protocol paths are IDs/blob references, not native filesystem
    paths.
14. Hard policy denial cannot be approved at a later layer.
15. Shell delegation creates one authoritative approval flow.
16. Deferred entries retain their original IDs through adoption.
17. Moderator background work applies only at documented safe points.
18. End-session suppresses automatic turns.
19. Current ChatML suspended evaluator state is never represented as durable.
20. Unsafe uncertain side effects are never automatically repeated.
21. Event sequences are monotonic per session and visibility filtering does
    not create cursor ambiguity.
22. Slow clients cannot block the actor or provider worker indefinitely.
23. Lock contention and store corruption never call `exit` from a library.
24. TUI-local rendering/editor state never becomes daemon session state.
25. HTTP, socket, stdio, and in-memory transports do not implement agent
    semantics.
26. Legacy MCP state and protocol remain separate from the agent server.

## 60. Final delivery artifacts

The completed work must deliver:

- all new libraries and executables listed in this specification;
- modified integration points listed in the file inventory;
- versioned server configuration and example configs;
- durable store schema and migration tooling;
- protocol type/API documentation;
- local and connected TUI modes;
- standalone and gateway stdio modes;
- Unix-socket and HTTP/SSE servers;
- permission, job, schedule, auth, audit, and observability services;
- legacy session import and compatibility behavior;
- all test suites and operational docs; and
- updated architecture/implementation specs whenever implementation changes a
  named contract.

No phase is considered complete merely because its types compile. Its exit
gate, failure behavior, persistence behavior, transport behavior, and tests
must all be satisfied.
