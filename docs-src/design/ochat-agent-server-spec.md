# Ochat Agent Server Architecture and Protocol Specification

Status: design specification; see the [current user guides](../agent-server/README.md) for executable commands and deployment instructions.

Date: August 15, 2026

Companion implementation: `ochat-agent-server-implementation-spec.md`

Audience: Ochat maintainers, server implementers, client implementers, ChatMD
prompt authors, and operators

## 1. Purpose

This document specifies the architecture, behavior, persistence model,
configuration, security boundaries, client protocol, and transport behavior of
the Ochat agent server system.

The system extends Ochat from an interactive and batch-oriented toolkit into a
host for durable, multi-client agent sessions. It must preserve the behavior
of ChatMD prompts, ChatML moderator scripts, shell runtimes, tools, history,
compaction, and the existing terminal UI while allowing sessions to run
independently from any individual client connection.

The specification defines one shared session system with multiple execution
and connection modes:

- durable sessions owned by a long-running daemon;
- owner-bound sessions hosted by a daemon;
- standalone TUI sessions hosted in the TUI process;
- standalone stdio sessions hosted in the stdio process;
- TUI clients connected to a daemon;
- stdio gateways connected to a daemon; and
- HTTP clients connected to a daemon.

The TUI, stdio, and HTTP interfaces are clients or adapters around the same
session protocol and session engine. They must not implement independent agent
semantics.

## 2. Normative language

The words **must**, **must not**, **should**, **should not**, and **may** are
normative requirements.

- **Must** identifies behavior required for correctness, security, protocol
  compatibility, or persistence guarantees.
- **Should** identifies the preferred behavior when a documented exceptional
  case may require another implementation.
- **May** identifies optional behavior that must preserve the stated
  invariants when implemented.

## 3. Goals

The server system must provide all of the following:

1. Run a ChatMD root agent in a host-selected workspace.
2. Preserve ChatMD parsing, imports, prompt configuration, tools, nested
   agents, shell runtimes, and ChatML moderation behavior.
3. Maintain durable agent sessions that survive client disconnects.
4. Restore durable session state after daemon restart.
5. Allow explicitly configured background model jobs, schedules, and
   orchestration to continue without a connected client.
6. Support explicit session start, stop, cancellation, reset, rebuild,
   export, and deletion operations.
7. Allow multiple clients to attach to one session and receive real-time
   updates in a consistent order.
8. Support owner, read/write, and read-only client attachments.
9. Allow both interactive approval workflows and unattended automation.
10. Preserve the TUI's agent-visible behavior while keeping terminal-only
    state local to each TUI client.
11. Support standalone operation without a daemon for existing local
    workflows.
12. Expose one common command and event protocol over HTTP, stdio, and local
    in-memory adapters.
13. Reuse low-level HTTP, SSE, OAuth, Eio, and NDJSON techniques from the
    legacy MCP implementation where useful without making the new server
    depend on MCP protocol or state.
14. Fail closed when prompt authority, workspace resolution, permissions, or
    recovery state is uncertain.

## 4. Non-goals

The initial system does not need to provide:

- transparent serialization of arbitrary OCaml fibers;
- transparent continuation of an already-open provider HTTP stream after the
  daemon process has terminated;
- automatic retry of arbitrary side-effecting tools;
- distributed execution across multiple simultaneously active daemon
  processes;
- a replacement for ChatMD tool declarations or shell capability policy;
- workspace isolation merely because a workspace root was selected;
- synchronization of TUI presentation state between clients;
- conversion of the legacy MCP server into the new session server; or
- wire compatibility between the new Ochat protocol and MCP.

These exclusions do not weaken the durable-session requirement. Durable state
and restart-safe work must resume where the system has an explicit checkpoint
and retry contract. Work that cannot be resumed safely must become visibly
interrupted instead of being silently repeated.

## 5. Design principles

### 5.1 One session engine

There must be one implementation of session behavior. The daemon, standalone
TUI, and standalone stdio modes must all instantiate the same engine.

### 5.2 Connections do not own detached sessions

A detached session must not stop because a TUI, stdio gateway, HTTP request,
SSE stream, or network connection closes.

### 5.3 Workspace is a coordinate, not authority

A workspace supplies the logical execution root and path variables. It does
not grant read, write, shell, network, process, or tool authority. ChatMD
declarations request authority, and host policy may restrict it.

### 5.4 One authoritative writer per session

Each live session must have one owning session actor that serializes durable
state mutations. Multiple clients and worker fibers may submit commands and
outcomes, but they must not mutate session state directly.

### 5.5 Persist before publish

Durable state transitions must be recorded before the corresponding durable
events are published to clients.

### 5.6 Eio is the runtime substrate

All new agent-server, session-engine, client, transport, and integration code
uses Eio for filesystem access, networking, clocks, cancellation, structured
concurrency, synchronization, and streams. Core is the project standard
library, but Core or Core_unix blocking file and socket APIs must not be used
for ordinary runtime operations. A narrow backend may use an Eio-owned Unix
descriptor for a host primitive that Eio does not expose, such as advisory
locking or directory `fsync`, and must execute blocking calls through the Eio
Unix system-thread boundary.

`agent_protocol` remains a pure dependency-light library and therefore does
not depend on Eio.

### 5.6 Safe-point semantics remain intact

Background ChatML events, deferred user messages, compaction, and follow-up
turns must preserve the existing safe-point rules. Background completion must
not splice state into the middle of an active provider turn.

### 5.7 Canonical history remains canonical

Provider stream events, UI rows, moderator overlays, progress events, and
transport messages must not become competing sources of transcript truth.
Canonical identity-bearing history remains the durable transcript.

### 5.8 Explicit recovery over accidental replay

The server must distinguish resumable, retryable, and interrupted work. It
must not infer that a side effect is safe to repeat.

### 5.9 Legacy MCP prompt-server isolation

The new server must not import MCP session, routing, registry, or notification
semantics. Generic low-level code may be copied or extracted when doing so
does not require redesigning the deprecated MCP prompt-serving server. MCP-backed
tools declared inside ChatMD remain actively maintained runtime functionality.

## 6. Terminology

### 6.1 Root agent

A root agent is the top-level ChatMD prompt instantiated directly by a
session. Nested `<agent>` tools, prompt-as-tool calls, and `Model.spawn` jobs
are not root agents.

### 6.2 Session

A session is the durable or transient logical execution of one root agent. It
owns canonical history, prompt revision, workspace instance, runtime state,
permissions, jobs, schedules, lifecycle, and event sequence.

### 6.3 Connection

A connection is a transport relationship between a client and a server or
embedded host. A connection may attach to zero or more sessions. Connection
identity is not session identity.

### 6.4 Attachment

An attachment grants one connection a defined relationship to a session. An
attachment has an access mode and may hold an owner lease.

### 6.5 Workspace definition

A workspace definition is configuration describing how workspace instances
are resolved or created and which root-agent concurrency rules apply.

### 6.6 Workspace instance

A workspace instance is one concrete directory used as the logical root for a
session. A physical definition commonly resolves to one stable instance. A
temporary definition may create a new instance for every session.

### 6.7 Prompt definition

A prompt definition is a catalog entry naming a root ChatMD source, allowed
workspace definitions, default policies, and root-agent limits.

### 6.8 Prompt revision

A prompt revision is the immutable identity of the root ChatMD source and its
resolved source closure, including imported files and external ChatML script
sources loaded during prompt parsing.

### 6.9 Session actor

A session actor is the single Eio fiber that owns the mutable runtime state of
one live session and serializes commands, worker outcomes, persistence, and
event publication.

### 6.10 Foreground operation

A foreground operation is work serialized by the host controller, currently
an agent turn or history compaction. At most one foreground operation may be
active in a session.

### 6.11 Background job

A background job is host-owned work that may outlive the foreground turn that
created it. Jobs have durable identity and explicit recovery behavior.

### 6.12 Durable event

A durable event describes an accepted state transition and has a stable
per-session sequence number. It can be replayed after reconnect or restart.

### 6.13 Recoverable stream event

A recoverable stream event is incremental live output, such as a text delta,
that is not a second canonical transcript. In Protocol 1.0, recovery means
reconstructing durable state from events/snapshots and resuming live delivery;
there is no operation-delta replay cursor or completed-stream replay API.
The name does not promise recovery of every intermediate delta.

### 6.14 Safe point

A safe point is a boundary at which the host may apply queued moderator work,
append deferred canonical entries, refresh effective history, or schedule the
next operation without splitting a provider request or tool call/output pair.

## 7. System architecture

The system consists of the following major layers:

```text
Clients
  chat_tui       stdio client       HTTP client
      |               |                 |
      +---------------+-----------------+
                      |
                Ochat client API
                      |
       +--------------+----------------+
       |              |                |
  in-memory       Unix/HTTP        stdio gateway
   adapter          client              |
       |              |                 |
       |        Ochat daemon <-----------+
       |              |
       +--------------+
                      |
                Session registry
                      |
             one session actor per
                loaded session
                      |
       +--------------+---------------------------+
       |              |             |             |
  prompt runtime  persistence   job service   permissions
       |              |             |             |
       +--------------+-------------+-------------+
                      |
         ChatMD / ChatML / Chat_response /
          tools / shell runtime / OpenAI
```

### 7.1 Agent server

The agent server owns global daemon services:

- configuration;
- prompt catalog;
- workspace catalog;
- session registry;
- session storage;
- event retention;
- job scheduler;
- authentication and authorization;
- listener lifecycle;
- health reporting;
- logging and metrics; and
- orderly shutdown.

### 7.2 Session registry

The session registry resolves session IDs, loads durable sessions, creates
session actors, enforces root-agent quotas, and unloads stopped sessions when
safe.

The registry must ensure that only one actor in one daemon process owns a
given session at a time.

### 7.3 Session engine

The session engine contains transport-neutral equivalents of the current TUI
host behavior:

- prompt runtime construction;
- foreground operation state;
- deferred canonical user-message queue;
- moderator dirty state and overlay revision tracking;
- pending turn requests;
- automatic-turn budgets;
- compaction scheduling;
- permission waiting;
- cancellation repair;
- history and allocator management;
- runtime requests; and
- durable checkpoint generation.

### 7.4 Client API

The client API exposes typed commands, responses, snapshots, and events. TUI,
stdio, and HTTP code must consume this API rather than accessing session
internals.

### 7.5 Transport adapters

Transport adapters frame and authenticate protocol messages. They do not own
agent semantics, history semantics, permissions, or lifecycle decisions.

## 8. Execution modes

Execution host, liveness, and persistence are separate properties.

### 8.1 Execution host

```ocaml
type execution_host =
  | Daemon
  | Embedded
```

- `Daemon` means a long-running Ochat daemon owns the session actor.
- `Embedded` means the current TUI or stdio process owns the session actor.

### 8.2 Liveness policy

```ocaml
type liveness =
  | Detached
  | Owner_bound of
      { disconnect_grace_ms : int
      ; stop_mode : stop_mode
      }
  | Process_bound

and stop_mode =
  | Graceful
  | Cancel
```

- A detached session requires no attached owner.
- An owner-bound session requires at least one valid owner lease after its
  grace period expires.
- A process-bound session exists only while its embedded host process is
  running. It is used by standalone TUI and stdio hosting and never denotes a
  daemon-owned session.
- A graceful owner-bound stop rejects new work and stops at the next safe
  boundary.
- A cancelling owner-bound stop cancels active work and performs normal
  cancellation repair.

### 8.3 Persistence policy

```ocaml
type persistence =
  | Durable
  | Transient
```

- Durable sessions write snapshots and journals and may survive daemon or
  client restart.
- Transient sessions may keep only in-memory state and explicitly requested
  exports.

### 8.4 Supported combinations

| Mode | Host | Liveness | Persistence | Intended use |
|---|---|---|---|---|
| Persistent daemon session | Daemon | Detached | Durable | Long-running agents and orchestration |
| Owner-bound daemon session | Daemon | Owner-bound | Durable or transient | Remote agent whose lifetime follows an owner client |
| Standalone TUI | Embedded | Process-bound | Optional | Existing local interactive workflow |
| Standalone stdio | Embedded | Process/stdio-bound | Usually transient | Editor or script-owned subprocess |
| TUI daemon client | Client only | Attachment-defined | Server-defined | Interactive control of a daemon session |
| Stdio daemon gateway | Client only | Attachment-defined | Server-defined | NDJSON access to a daemon session |
| HTTP client | Client only | Attachment-defined | Server-defined | Network API and event subscription |

### 8.5 Standalone compatibility

Standalone TUI and stdio modes must not require a daemon, prompt catalog, or
configured workspace catalog. The local caller may supply an arbitrary prompt
path and use the current working directory as the workspace.

## 9. Workspace model

### 9.1 Meaning of a workspace

A workspace is the logical working root in which a ChatMD root agent is run.
It supplies `${workspace}` and normally supplies the host-selected tool
working directory used as `${tool_dir}`.

Selecting a workspace does not grant authority. A prompt must still declare
the tools, read roots, shell capabilities, and other resources it needs.

### 9.2 Workspace source

```ocaml
type workspace_source =
  | Physical of { path : string }
  | Temporary of temporary_definition

and temporary_definition =
  { location : temporary_location
  ; cleanup : cleanup_policy
  }

and temporary_location =
  | System_tmp
  | Session_dir

and cleanup_policy =
  | On_session_stop
  | On_session_delete
  | Retain
```

### 9.3 Physical workspaces

A physical workspace resolves to an existing directory on the host.

The server must:

1. resolve the configured path to an absolute native path;
2. canonicalize it with symlink awareness;
3. verify that it is a directory;
4. record both configured and canonical identity;
5. never delete it as part of workspace cleanup; and
6. fail session startup if it becomes unavailable.

### 9.4 Temporary workspaces

A temporary workspace definition creates a concrete directory for a workspace
instance.

`Session_dir` temporary workspaces should be created below:

```text
<session-dir>/workspace
```

They are preferred for durable sessions because the path is controlled by
the server and survives ordinary daemon restart.

`System_tmp` workspaces use a securely created unique directory. They should
be limited to transient or explicitly disposable sessions because the
operating system may remove them independently.

The server must record that a temporary path was server-created before it may
delete the path. It must never recursively delete an arbitrary client-supplied
or physical workspace path.

### 9.5 Current workspace

Standalone execution creates an implicit workspace instance of kind
`Current`. Its root is the process working directory at session creation.

The path must be captured at creation. Changing the process working directory
later must not silently change the session workspace.

### 9.6 Workspace definition

```ocaml
type workspace_definition =
  { id : string
  ; source : workspace_source
  ; conflict_domain : string option
  ; prompt_limits : prompt_limit list
  ; default_access : workspace_access
  }

and workspace_access =
  | Read_only
  | Shared_write
  | Exclusive
```

`default_access` is an operational concurrency policy. It is not a filesystem
capability declaration.

### 9.7 Workspace instance

```ocaml
type workspace_instance =
  { id : string
  ; definition_id : string option
  ; configured_root : string
  ; canonical_root : string
  ; kind : workspace_kind
  ; conflict_domain : string
  ; cleanup : cleanup_policy option
  }

and workspace_kind =
  | Physical
  | Temporary
  | Current
```

### 9.8 Conflict domains

A conflict domain identifies workspace instances that should share
concurrency accounting.

- A physical workspace defaults to its canonical path.
- A current workspace defaults to its canonical path.
- A temporary workspace defaults to its unique instance ID.
- An operator may assign an explicit shared conflict-domain string.

Aliases that resolve to the same physical directory must not bypass quotas.

### 9.9 Root-agent prompt limits

```ocaml
type prompt_limit =
  { prompt_id : string
  ; max_root_agents : int
  ; overflow : overflow_policy
  }

and overflow_policy =
  | Reject
  | Queue
```

Limits are evaluated by `(conflict_domain, prompt_id)`.

A root-agent slot is acquired before entering `Starting` and released only
after the session reaches `Stopped` or is deleted. The following states consume
a slot:

- starting;
- recovering;
- idle and running;
- running a turn;
- compacting;
- waiting for permission;
- stopping; and
- failed while still retaining running ownership.

A stopped session does not consume a slot. Nested agents and spawned jobs do
not consume root-agent slots; they use separate nested-job limits.

When overflow policy is `Queue`, queued starts must be ordered fairly by
accepted command sequence. A queued start may be cancelled before it acquires
a slot.

### 9.10 Exclusive workspace access

An exclusive workspace instance permits at most one running root session in
its conflict domain regardless of prompt. The server must acquire the
workspace lease before prompt-specific limits.

Shared-write workspaces permit concurrency but leave file-level conflicts to
the agents and operator configuration.

### 9.11 Runtime path variables

The host constructs explicit runtime paths:

```ocaml
type runtime_paths =
  { tool_dir : string
  ; workspace : string
  ; prompt_dir : string
  ; session_dir : string
  ; cache_dir : string
  ; home : string
  }
```

`source_dir` is declaration-specific and comes from ChatMD source provenance.

The variables have these meanings:

| Variable | Meaning |
|---|---|
| `${workspace}` | Concrete workspace-instance root selected by the host |
| `${tool_dir}` | Captured Ochat launch directory for default relative tool behavior, unless explicitly overridden by the host |
| `${prompt_dir}` | Directory containing the root ChatMD prompt source |
| `${source_dir}` | Directory containing the specific declaration source, including imports |
| `${session_dir}` | Current session's data directory |
| `${cache_dir}` | Cache directory selected for the session |
| `${home}` | Current host user's home directory |

Resolution rules are:

| Execution mode | `${workspace}` | `${tool_dir}` |
|---|---|---|
| Standalone TUI or stdio | Captured process working directory unless explicitly overridden | Captured process working directory unless explicitly overridden |
| Daemon physical workspace | Resolved workspace root | Captured daemon launch directory |
| Daemon temporary workspace | Created workspace root | Captured daemon launch directory |
| Explicit host override | Resolved workspace root | Configured tool directory |

The launch directory is captured once; workspace selection does not change it.
Operators must use `${workspace}` explicitly in tool declarations when tools
should act in the selected workspace. No session changes the process cwd.

### 9.12 Workspace authority rule

Workspace configuration determines where a prompt runs and which
prompt/workspace combinations are accepted. It does not determine what the
prompt can access.

For example, selecting `/work/project` does not expose that directory unless
the ChatMD document declares an appropriate `read_file` root, shell
capability, custom tool, or other access mechanism. Conversely, a prompt that
declares `${home}` or `/` may request authority beyond the workspace. The
operator is responsible for publishing correctly configured prompts, and
host administrative policy may impose a stricter ceiling.

## 10. Prompt catalog and revisions

### 10.1 Prompt definition

```ocaml
type prompt_definition =
  { id : string
  ; path : string
  ; allowed_workspaces : string list
  ; permission_profile : string
  ; runtime_policy : string option
  ; description : string option
  ; enabled : bool
  }
```

The daemon accepts catalog prompt IDs, not arbitrary prompt paths, for normal
remote session creation.

### 10.2 Allowed workspace combinations

`allowed_workspaces` is an operational allowlist. The daemon must reject a
session creation request whose prompt and workspace definition are not an
allowed combination.

This allowlist does not grant tool authority and does not replace ChatMD or
shell security checks.

### 10.3 Prompt loading

Prompt loading must:

1. resolve the root prompt path;
2. read the root source;
3. parse ChatMD with correct root `prompt_dir`;
4. expand and track imports;
5. load referenced ChatML sources;
6. retain declaration source provenance;
7. compute a source-closure identity;
8. validate prompt configuration and scripts;
9. inspect shell declarations under host policy; and
10. produce an immutable prompt revision artifact.

### 10.4 Prompt revision identity

A prompt revision must include at least:

- prompt definition ID;
- root source digest;
- canonical root prompt path;
- imported source paths and digests;
- referenced ChatML source paths and digests;
- relevant built-in profile versions;
- parser/runtime schema version;
- compiled shell manifest digest when present; and
- moderator script ID and source digest when present.

### 10.5 Pinning

A session must pin the prompt revision used at creation or explicit upgrade.
Editing a catalog prompt must not silently change an existing session's
runtime authority or moderator compatibility.

### 10.6 Catalog reload

The daemon may watch or reload prompt definitions. Reload affects discovery
and new session creation. Existing sessions continue using their pinned
revision until explicitly upgraded or rebuilt.

Invalid reloaded prompts must be marked unavailable for new sessions without
stopping existing pinned sessions.

### 10.7 Prompt upgrade

An explicit prompt upgrade must:

1. require the session to be idle or stopped;
2. resolve a new prompt revision;
3. compare authority-sensitive changes;
4. require new manifest authorization when the manifest changes;
5. validate moderator snapshot compatibility;
6. define whether canonical history is retained;
7. journal the old and new revision identities; and
8. fail without modifying the session if preparation fails.

### 10.8 Standalone prompt paths

Standalone embedded modes may accept arbitrary local prompt paths because the
local process owner controls the invocation. They still use the same prompt
revision construction and source-provenance rules.

## 11. Server configuration

### 11.1 Format

The initial server configuration should use a versioned S-expression format
with typed parsing and explicit validation.

### 11.2 Complete conceptual example

```lisp
(version 1)

(server
 ((data_dir "~/.ochat/server")
  (unix_socket "~/.ochat/server.sock")
  (http
   ((enabled true)
    (address "127.0.0.1")
    (port 8787)
    (require_auth true)))
  (shutdown_grace_ms 30000)
  (event_retention
   ((completed_stream_ms 3600000)
    (response_artifact_ms 3600000)
    (max_events_per_session 100000)))
  (durability
   ((journal_flush interval)
    (journal_flush_ms 50)
    (snapshot_every_events 100)
    (snapshot_every_ms 5000)))))

(workspaces
 (((id ochat)
   (source (physical "/Users/dakotamurphy/chatgpt"))
   (access shared_write)
   (prompt_limits
    (((prompt coding-agent)
      (max_root_agents 2)
      (overflow queue)))))
  ((id scratch)
   (source
    (temporary
     ((location session_dir)
      (cleanup on_session_delete))))
   (access exclusive)
   (prompt_limits
    (((prompt coding-agent)
      (max_root_agents 8)
      (overflow reject)))))))

(prompts
 (((id coding-agent)
   (path "/opt/ochat/prompts/coding.chatmd")
   (description "Repository coding agent")
   (allowed_workspaces (ochat scratch))
   (permission_profile interactive)
   (enabled true))))

(permission_profiles
 (((id interactive)
   (tool_default ask)
   (approval_timeout none)
   (approval_fallback deny)
   (manifest_authorization require_grant))
  ((id unattended)
   (tool_default policy)
   (approval_timeout_ms 0)
   (approval_fallback deny)
   (manifest_authorization require_grant))
  ((id trusted-automation)
   (tool_default allow)
   (approval_timeout none)
   (manifest_authorization assume_authorized)))))

(manifest_grants
 (((id coding-agent-ochat-v1)
   (prompt coding-agent)
   (workspaces (ochat))
   (manifest_sha256
    0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef)
   (source_sha256
    fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210)
   (principals (pri_example_operator)))))
```

The `manifest_grants` section is optional. Each entry is an explicit operator
authorization pinned to one prompt definition, one or more workspaces, the
exact root-source SHA-256 digest, and the exact canonical shell-manifest
SHA-256 digest. Omitting `principals` authorizes any authenticated creating
principal; a nonempty list binds the grant to those opaque principal IDs.
Changing prompt source, imports, built-in versions, or shell declarations
requires the operator to publish a new exact grant.

### 11.3 Validation

Configuration validation must reject:

- unknown schema versions;
- duplicate IDs;
- missing prompt or workspace references;
- non-positive limits where positive values are required;
- invalid overflow or cleanup policies;
- nonexistent physical workspace roots;
- nonexistent or invalid prompt files;
- unsafe remote HTTP listeners without required authentication unless the
  operator explicitly enables an unsafe development override;
- temporary cleanup paths outside server-owned roots;
- permission-profile references that do not exist; and
- conflicting listener definitions.

### 11.4 Configuration reload

Reload must be transactional. The daemon prepares and validates the complete
new configuration before installing it.

Reload may:

- add or remove prompt availability for new sessions;
- add workspace definitions;
- change limits for future acquisitions;
- update authentication policy; and
- update event retention and observability settings.

Reload must not silently relocate existing workspace instances, change pinned
prompt revisions, revoke active owner leases, or rewrite existing session
permission profiles. Explicit administrative commands handle such changes.

## 12. Session identity and persisted specification

### 12.1 Session identifier

A daemon session ID must be globally unique within the server data store and
safe for use as a directory component. Clients may request a human-readable
display name, but display names must not replace stable IDs.

### 12.2 Session creation specification

```ocaml
type session_spec =
  { prompt : prompt_ref
  ; workspace : workspace_request
  ; liveness : liveness
  ; persistence : persistence
  ; permission_profile : string option
  ; start_immediately : bool
  ; display_name : string option
  ; labels : (string * string) list
  }
```

For daemon creation, `prompt_ref` normally contains a prompt catalog ID and
`workspace_request` contains a workspace-definition ID. Standalone creation
may contain local paths.

### 12.3 Persisted session metadata

Durable metadata must include:

- session ID and display name;
- creation and update timestamps;
- creating principal when authenticated;
- desired lifecycle state;
- liveness and persistence policies;
- prompt definition and pinned revision;
- workspace-instance snapshot;
- permission-profile identity and revision;
- root-agent quota identity;
- canonical history allocator namespace and next sequence;
- latest session revision;
- latest durable event sequence;
- active or interrupted operation metadata;
- durable job and schedule references;
- labels and operator metadata; and
- data-schema version.

### 12.4 Desired and observed lifecycle

Desired state records operator intent:

```ocaml
type desired_state =
  | Running
  | Stopped
```

Observed state records current runtime state:

```ocaml
type observed_state =
  | Stopped
  | Queued_for_slot
  | Starting
  | Recovering
  | Idle
  | Running_turn of operation_id
  | Compacting of operation_id
  | Waiting_for_permission of permission_id
  | Stopping
  | Failed of failure
```

The desired and observed states must not be collapsed. On daemon startup, a
durable session with desired state `Running` enters `Recovering`; a session
with desired state `Stopped` remains stopped.

### 12.5 Session revision

Every accepted durable mutation increments a monotonic session revision.
Revision is distinct from history sequence and event sequence.

- History sequence identifies canonical history entries within an allocator
  namespace.
- Event sequence orders externally observable events.
- Session revision identifies durable state versions.

### 12.6 Session snapshot

A client-visible snapshot is not the same as the binary persistence snapshot.
The client snapshot must contain enough state to construct a correct client
projection without replaying the entire session lifetime.

It includes:

- session identity and metadata;
- desired and observed lifecycle;
- prompt and workspace summaries;
- canonical history or requested history window;
- effective history and provenance when moderation is active;
- active operation summary;
- pending deferred messages;
- active tool-call summaries;
- pending permissions visible to the principal;
- active Agent-page call summaries;
- jobs and schedules visible to the principal;
- halt and failure state;
- latest session revision; and
- latest durable event sequence.

## 13. Session actor

### 13.1 Ownership

Every loaded session has exactly one session actor. The actor owns:

- mutable session state;
- canonical history;
- history allocator;
- moderator manager;
- agent runtime and tool table;
- shell approval broker and grant store;
- foreground operation state;
- deferred user-message queue;
- pending runtime requests;
- permission state;
- owner leases;
- subscribers;
- session-local jobs and schedules; and
- persistence commit sequencing.

### 13.2 Mailbox

All commands and asynchronous outcomes enter through a bounded actor mailbox.
Message classes include:

- client command;
- connection detached;
- owner lease expired;
- model stream event;
- canonical history event;
- tool execution event;
- foreground operation completion or failure;
- moderator wakeup;
- overlay commit notification;
- job completion;
- schedule due;
- permission response;
- persistence completion or failure;
- configuration administration event; and
- daemon shutdown request.

### 13.3 Single-writer invariant

Worker fibers may perform network requests, tool execution, rendering-neutral
conversion, or persistence I/O, but only the session actor installs durable
state and assigns session revisions and event sequences.

### 13.4 Worker ownership

Foreground and background workers must be owned by explicit Eio switches.
The actor must retain cancellation handles and operation IDs. Completion from
a stale or cancelled operation must be ignored or recorded as stale without
mutating current state.

### 13.5 Bounded queues

Mailboxes and subscriber queues must be bounded. Backpressure policy must
prefer session progress over retaining a slow client connection.

- The actor mailbox must reserve capacity for terminal operation outcomes and
  cancellation.
- Provider deltas may be batched before entering the actor.
- A subscriber that cannot keep up must be disconnected with a resumable
  sequence cursor.
- Durable events must remain recoverable from storage even when a subscriber
  is disconnected.

### 13.6 Loading and unloading

Running, recovering, owner-bound, scheduled, or background-active sessions
remain loaded.

A stopped durable session may be unloaded after all commits finish and no
subscribers require live state. Reloading reconstructs it from snapshot and
journal.

## 14. Lifecycle operations

### 14.1 Create

Creation must:

1. authenticate and authorize the requester;
2. resolve prompt and workspace definitions;
3. validate the allowed combination;
4. reserve a session ID and directory;
5. create or resolve the workspace instance;
6. resolve and pin the prompt revision;
7. select the permission profile;
8. initialize empty persisted state;
9. append `session.created`;
10. create an owner lease when requested; and
11. start immediately only when requested.

Creation must be atomic from the client's perspective. A partially created
session must either be recoverable as creation-in-progress or cleaned up
without becoming discoverable.

### 14.2 Start

Starting sets desired state to `Running`, acquires workspace and prompt quota
slots, and constructs the runtime.

If no quota slot is available:

- `Reject` returns `resource_limit` without changing desired state unless the
  command explicitly requested queued intent.
- `Queue` sets observed state to `Queued_for_slot` and retains desired state
  `Running`.

### 14.3 Stop

A graceful stop:

1. sets desired state to `Stopped`;
2. rejects new user turns and new background work;
3. allows the current foreground operation to reach a safe terminal boundary;
4. prevents automatic follow-up turns;
5. drains or cancels background work according to job policy;
6. persists a final snapshot;
7. releases root-agent and workspace leases; and
8. enters `Stopped`.

A cancelling stop additionally cancels the active foreground switch,
repairs incomplete history, marks non-resumable jobs interrupted or
cancelled, and proceeds to final checkpointing.

### 14.4 Cancel active operation

Cancellation targets an operation ID. It must be idempotent.

Cancellation must:

- fail or cancel the operation switch;
- reject stale completion events;
- preserve already accepted canonical history;
- synthesize tool outputs where required to repair incomplete tool-call pairs;
- remove trailing incomplete reasoning when required by provider invariants;
- emit an operation terminal event; and
- return the session to the next valid state.

Cancellation does not imply desired state `Stopped` unless requested by a
stop operation.

### 14.5 Delete

Deletion is destructive and must require explicit administrative authority.

The server must stop the session, finish or cancel owned work, release leases,
archive or remove the session directory according to policy, and clean up only
server-created temporary workspaces.

Physical workspaces must never be deleted.

### 14.6 Reset

Reset archives the current durable session generation and creates a new
generation under the same session identity or an explicitly chosen new
identity.

Reset options define whether to keep canonical history, tasks, cache,
workspace contents, grants, and labels. Moderator runtime state must be reset
unless an explicit compatibility-safe operation says otherwise.

### 14.7 Rebuild from prompt

Rebuild creates fresh runtime state from the pinned or newly selected prompt
revision. It must archive the previous snapshot, invalidate stale caches, and
not infer authorization from the archived state.

### 14.8 Export

Export produces an inspectable ChatMD artifact from canonical history and
relevant persisted state. Export is read-only and may run while the session is
idle. Export during an active turn must either use an explicit stable revision
or wait for a safe point.

## 15. Runtime construction

### 15.1 Construction sequence

Runtime startup must proceed in this order:

1. verify workspace availability and leases;
2. materialize runtime paths;
3. load the pinned prompt revision;
4. load session cache and durable state;
5. create `Chat_response.Ctx` with prompt directory and tool directory;
6. inspect shell declarations against platform and administrative policy;
7. authorize the exact shell manifest;
8. create shell approval and grant stores;
9. instantiate the agent runtime and tools;
10. create the history allocator from the persisted high-water mark;
11. select canonical history from the session or initial prompt;
12. compile and instantiate the ChatML moderator when present;
13. restore compatible moderator and extension snapshots;
14. register durable model/job capabilities and wakeup callbacks;
15. run `session_start` or `session_resume` moderation;
16. drain startup internal events within configured limits;
17. persist the resulting state; and
18. enter `Idle`, `Waiting_for_permission`, `Stopped`, or `Failed` as
    indicated by the result.

### 15.2 Startup failure

Startup must fail closed if:

- the workspace is missing;
- prompt revision data is missing or invalid;
- source hashes violate pinning policy;
- the shell manifest is unauthorized;
- administrative policy rejects the manifest;
- tool construction fails;
- moderator snapshot identity is incompatible;
- persisted history or allocator state is invalid; or
- required durable stores cannot be opened.

Failure must not consume a quota slot indefinitely. A failed session releases
its slot unless policy explicitly retains it for operator inspection while
desired state remains running.

### 15.3 Runtime teardown

Teardown must unregister wakeups, close approval brokers, cancel owned
switches, stop accepting worker outcomes, flush persistence, close caches,
release leases, and avoid publishing events after the actor is closed.

## 16. History model

### 16.1 Canonical history

Canonical history is the durable ordered list of `History_entry.t` values.
Each occurrence has an application-owned `History_entry.Id`. Provider item IDs
and tool `call_id` values remain payload and correlation metadata.

### 16.2 Effective history

Effective history is canonical history projected through the durable ChatML
moderator overlay. It may contain inserted, replacement, or deleted items with
explicit provenance.

Effective history is used for moderated provider requests and client-visible
moderated transcript projection.

Snapshots reconstruct this view from committed moderator identity state without
executing ChatML. Durable `moderator.overlay_changed` notifications carry a
replacement effective-history window and halt state, not the interpreter's
snapshot or private queued events. Omitted optional fields clear the previous
effective view/halt reason. A change to canonical history must also update an
existing effective view. The view and its notification belong to the same
committed revision; canonical history remains authoritative and unchanged by
overlay insertion, replacement or deletion.

### 16.3 Visible history

Visible history is a client projection of effective history. The daemon may
provide presentation-neutral projected message metadata, but terminal rows,
wrapping, highlighting, scroll coordinates, and selection are client-owned.

### 16.4 History allocator

One allocator must own all new canonical occurrences for a root session. Its
namespace and next unused sequence are durable.

Concurrent producers must reserve IDs through the allocator before a
canonical event is accepted.

### 16.5 Deferred user messages

Messages submitted during an active turn are canonical deferred entries.

The host must:

1. validate and strip the input;
2. allocate its canonical history ID immediately;
3. enqueue it in FIFO order;
4. publish that the message was accepted and deferred;
5. avoid inserting it into an already-issued provider request;
6. wait for pending tool outputs;
7. append deferred entries after the current turn's accepted outputs;
8. emit moderator `item_appended` handling for each entry; and
9. start or request the next user turn.

Deferred messages must not be represented only as transient request text.
They retain the same canonical identity through final history.

Raw XML submission during an active turn may be rejected unless a future
typed deferred-XML contract is defined.

### 16.6 History replacement

Compaction or administrative history replacement must validate IDs and the
allocator high-water mark before installation. Clients receive a
`history.replaced` event and must discard incompatible incremental projection
state.

## 17. Foreground turn behavior

### 17.1 Single active foreground operation

A session permits at most one foreground turn or compaction operation.

User submissions, compaction requests, and host follow-up turns received while
another foreground operation is active are queued or deferred according to
their type.

### 17.2 Turn-start reasons

```ocaml
type turn_start_reason =
  | User_submit
  | Moderator_request
  | Idle_followup
  | Recovery_retry
  | Administrative
```

The reason is included in operation metadata and events. Non-user automatic
turns are governed by follow-up budgets.

### 17.3 Turn start

Starting a turn must:

1. verify desired state is running;
2. verify no blocking permission is pending;
3. allocate an operation ID;
4. persist `operation.started` and observed state;
5. apply the ChatML turn-start safe point;
6. drain allowed queued internal events;
7. compute effective request history;
8. consume canonical deferred entries at the correct boundary;
9. issue the provider request; and
10. stream events back to the actor.

### 17.4 Provider stream events

Provider events are observed in provider order. The server may batch adjacent
high-volume deltas, but it must preserve semantic order and source
attribution.

A received provider event must be available to consumers without waiting for
the following event. TUI consumers render sourced deltas once, not again from
their paired history-correlated notifications. Canonical publication replaces
the matching transient row by identity; unrelated durable revisions must not
duplicate previously applied tool progress.

Callbacks and subscribers must not be able to crash the provider worker.
Cancellation exceptions retain their normal propagation semantics.

### 17.5 Tool calls

Tool calls pass through moderation, generic permission policy, and
tool-specific execution. They emit transient execution events and canonical
tool-call/tool-output entries as appropriate.

Parallel tool calls may execute concurrently, but canonical publication must
preserve the ordering guarantees of the shared response loop.

An in-process `fork` has private child history and does not inherit the root
moderator instance, root deferred-input consumer, or root history-commit
callbacks. Source-attributed live events and progress remain observable. Only
the fork's completed tool result is incorporated into root canonical history.

### 17.6 Tool follow-up

Tool outputs are appended before the next provider request. Post-tool ChatML
handling and internal-event drains run at the post-tool safe point.

### 17.7 Turn end

Turn end must:

1. finish provider item ingestion;
2. append accepted canonical entries;
3. run ChatML `turn_end`;
4. drain bounded internal events;
5. collapse runtime requests;
6. persist canonical and moderator state;
7. publish terminal operation events;
8. append late deferred messages;
9. check idle moderator work before other queued actions; and
10. schedule compaction, user work, or allowed follow-up turns.

### 17.8 Turn failure

On failure, the server must distinguish cancellation, provider timeout,
provider parsing failure, tool failure, permission denial, persistence
failure, and internal invariant failure.

The actor repairs incomplete history, records the failure, and either returns
to idle, waits for operator action, or fails the session depending on whether
state remains trustworthy.

## 18. ChatML host behavior

### 18.1 Moderator ownership

ChatML instruction constructors and prepends emit provider `developer` messages.
Legacy helper names containing `system` remain compatible; instruction predicates
recognize both developer and historical system items. Do not rewrite persisted
history or raw item payloads as part of this constructor behavior.

The moderator manager owns durable script state, queued internal events,
overlay state, halted state, and committed overlay revisions. The session
actor owns wakeup scheduling, visible projection, follow-up turns, compaction,
and lifecycle.

### 18.2 Safe boundaries

The server preserves these safe boundaries:

- session start or resume;
- turn start;
- pre-tool call;
- post-tool response;
- turn end;
- idle internal-event drain;
- compaction completion or failure;
- streaming failure recovery; and
- durable job completion delivery.

### 18.3 Wakeups

A background completion enqueues a moderator internal event before waking the
session actor. A wakeup marks moderator work dirty. It does not immediately
mutate visible state during active foreground work.

### 18.4 Idle drains

The session actor drains queued internal events only while idle and not
blocked on an approval. Drain count is bounded by runtime policy. Remaining
events stay queued and eligible for another wakeup.

### 18.5 Runtime requests

ChatML runtime requests express intent:

- `Request_turn` asks the host to schedule another ordinary turn;
- `Request_compaction` asks the host to compact;
- `End_session reason` asks the host to halt or stop the session.

`End_session` suppresses pending automatic turns. Compaction intent may still
be recorded when required for audit, but no work violating the ended state may
start.

### 18.6 Follow-up budgets

The server preserves separate limits for:

- self-triggered continuation turns within one response loop;
- host-started follow-up turns;
- follow-up sliding-window rate limits;
- internal-event drain counts; and
- spawned background jobs.

User-submitted turns bypass automatic follow-up suppression and reset the
host follow-up count as defined by runtime policy.

### 18.7 Halted sessions

A moderator halt prevents new turns. Canonical and overlay state remain
inspectable. Restart does not clear a durable halt. An explicit reset,
upgrade, or administrative action is required.

## 19. Permissions and approvals

### 19.1 Separate authorization questions

The server must keep these questions distinct:

1. May the principal create or control this session?
2. May this prompt run in this configured workspace?
3. May the exact expanded shell manifest be instantiated?
4. Does the prompt declare the capability required by this invocation?
5. Does host administrative policy permit it?
6. Does ChatML moderation approve, reject, rewrite, or redirect the call?
7. Does server invocation policy allow it automatically or require review?
8. Does the tool-specific executor allow and safely execute it?

Approval at a later layer must not override an earlier hard denial.

### 19.2 Manifest authorization

Shell manifest authorization remains bound to the exact canonical manifest
digest and source identity. The server may use session-backed durable grants,
operator-managed grants, or explicit trusted automation policy.

An operator-managed grant identifies a prompt definition, an allowed
workspace definition, an optional set of creating principals, the exact root
source digest, and the exact canonical manifest digest. A matching operator
grant authorizes one bootstrap decision; before runtime publication the
session actor persists a complete session-scoped exact grant containing the
manifest's built-in and imported-source identities. Removing an operator
grant prevents future runtime construction but does not erase a session grant
that was already durably committed; revocation of that session grant uses the
ordinary grant API.

Opening or cataloging a prompt must not automatically authorize its shell
manifest unless the configured profile explicitly assumes authorization.

### 19.3 Generic tool authorization

The server introduces a generic invocation gate around all callable tools.
The gate receives:

- session and principal identity;
- prompt and workspace identity;
- tool name and kind;
- call ID;
- structured or raw arguments;
- declaration provenance;
- inferred effects when available;
- moderator decision;
- active permission profile; and
- tool-specific security metadata.

The result is:

```ocaml
type authorization_decision =
  | Allow
  | Deny of string
  | Ask of permission_request
  | Rewrite of invocation
  | Delegate
```

`Delegate` is used when the authoritative review remains inside a subsystem,
such as the shell runtime. The generic gate must not produce duplicate shell
approval prompts.

### 19.4 Decision order

Invocation order is:

1. connection/session authorization;
2. workspace/prompt operational checks;
3. host hard deny and capability ceilings;
4. ChatMD declaration validity;
5. ChatML pre-tool moderation;
6. server permission profile;
7. external, human, model, or subsystem reviewer;
8. tool execution;
9. ChatML post-tool handling; and
10. audit and canonical output publication.

### 19.5 Permission profile

```ocaml
type permission_profile =
  { id : string
  ; manifest_authorization : manifest_mode
  ; tool_default : tool_default
  ; rules : permission_rule list
  ; approval_timeout_ms : int option
  ; approval_fallback : approval_fallback
  ; approver_scope : approver_scope
  }

type tool_default =
  | Deny
  | Allow
  | Ask
  | Policy

type approval_fallback =
  | Deny
  | Allow_if_policy
  | Model_reviewer of string
  | External_reviewer of string
```

Reviewer implementations are installed by the daemon host and resolved by
configured name and kind. A missing implementation, exception, timeout, or
malformed decision denies the invocation. Reviewer approval is advisory and
cannot override `Deny`, a capability ceiling, ChatMD policy, or a prior hard
denial. Each installed reviewer declares a security-relevant revision string
that participates in the compiled permission-profile digest. The legacy
`allow` fallback remains a compatibility spelling only;
new unattended profiles should use `allow_if_policy` or an explicit reviewer.

Profiles must support:

- deny-all unattended operation;
- policy-only unattended operation;
- interactive human review;
- interactive review with timeout and fail-closed fallback;
- trusted automation;
- model reviewer fallback;
- external reviewer integration; and
- tool-specific rules.

### 19.6 Permission request

Every pending request has stable identity and includes:

- permission ID;
- session ID;
- operation and call IDs;
- tool/runtime identity;
- redacted invocation display;
- rationale and inferred effects;
- available response choices;
- durable grant scopes when supported;
- creation and expiration times;
- required approver scope; and
- restart behavior.

### 19.7 Permission responses

Only attachments with approval authority may respond. The first valid
response wins. Later responses return `already_resolved`.

Responses must identify the permission ID and may include:

- approve once;
- approve for exact session;
- approve an allowed prefix for the session;
- approve durable exact identity;
- deny with reason; or
- rewrite when the underlying subsystem supports rewrites.

### 19.8 Blocking behavior

While a foreground tool call waits for permission:

- observed state is `Waiting_for_permission`;
- the provider/tool continuation remains blocked;
- ordinary automatic progression is suppressed;
- read/write messages may be accepted as deferred messages but do not answer
  the approval;
- internal events may queue but are not drained through a suspended ChatML
  continuation; and
- all eligible clients receive the pending request.

### 19.9 Restart behavior for approvals

Generic server and shell approval requests must be persisted at a resumable
tool boundary when possible.

The current ChatML `Approval.ask_text` and `Approval.ask_choice` continuation
is not serializable. Until ChatML approval is represented as an event-driven
durable boundary, restart during that suspension must produce an explicit
`Interrupted_chatml_approval` condition. It must not claim that the paused
evaluator frame was restored.

The long-term durable design converts approval resolution into a later
`Approval_resolved` internal event rather than persisting an evaluator stack.

### 19.10 Grant persistence and revocation

Durable grants retain existing identity binding rules. Revocation is a
durable mutation, is visible to all clients, and must be audited. Revocation
does not retroactively cancel an invocation that has already crossed its last
safe authorization boundary unless the subsystem explicitly supports that
behavior.

## 20. Durable jobs and schedules

### 20.1 Job model

```ocaml
type job =
  { id : string
  ; session_id : string
  ; kind : job_kind
  ; status : job_status
  ; payload : Jsonaf.t
  ; retry : retry_policy
  ; attempt : int
  ; created_at : timestamp
  ; next_run_at : timestamp option
  ; result : Jsonaf.t option
  ; delivered : bool
  }
```

Job kinds include:

- spawned model call;
- nested agent call;
- scheduled ChatML event;
- asynchronous tool call;
- shell process metadata;
- compaction job when implemented outside the foreground worker; and
- future host-defined operations.

### 20.2 Job states

```ocaml
type job_status =
  | Queued
  | Running
  | Waiting_permission of string
  | Succeeded
  | Failed of string
  | Cancelled
  | Interrupted of string
```

### 20.3 Retry policy

```ocaml
type retry_policy =
  | Never
  | Safe_retry of
      { max_attempts : int
      ; backoff_ms : int
      }
  | Idempotent of
      { key : string
      ; max_attempts : int
      ; backoff_ms : int
      }
```

Unknown tool and shell side effects default to `Never`.

### 20.4 Model jobs

The durable job service generalizes the current model-executor pattern:

1. allocate a stable job ID;
2. persist the queued job;
3. launch background work;
4. persist terminal result;
5. enqueue `Model_job_succeeded` or `Model_job_failed` into the moderator;
6. mark delivery after the internal event is durably accepted; and
7. wake the session actor.

Delivery must be idempotent. Restart may retry event delivery without
rerunning a completed model call.

### 20.5 Schedules

`Schedule.after_ms` must create a durable scheduled job for durable sessions.
The schedule stores an absolute due time and payload.

After restart, overdue schedules are delivered according to configured
misfire policy:

- deliver once immediately;
- skip if expired; or
- fail the schedule visibly.

The initial default should deliver once immediately. `Schedule.cancel`
durably cancels an undelivered schedule.

### 20.6 Process and shell jobs

The daemon must own child process groups and retain enough metadata to kill
and reap them during cancellation or shutdown. After daemon restart, an
uncertain shell invocation must be marked interrupted. It must not be rerun
unless it has an explicit idempotent policy.

### 20.7 Job limits

Limits apply per session, prompt, workspace conflict domain, and daemon as
configured. Exceeding a limit fails before job creation or queues the job when
explicitly allowed.

## 21. Persistence architecture

### 21.1 Storage layout

```text
<data-dir>/
  daemon.lock
  server-id
  sessions/
    <session-id>/
      metadata.sexp
      snapshot.bin
      journal/
        00000001.events
      prompt/
        root.chatmd
        sources/
      workspace/               optional managed temporary workspace
      cache/
        cache.bin
      audit/
        shell.jsonl
        server.jsonl
      archive/
      lock
  jobs/
  auth/
```

Exact segmentation may change, but session state, event journal, prompt
revision material, cache, workspace, and audit data must remain separately
identifiable.

### 21.2 Journal record

Every durable record contains:

- schema version;
- session ID;
- event sequence or transaction sequence;
- previous sequence;
- session revision;
- timestamp;
- transaction kind;
- state delta or event payload;
- integrity/checksum data; and
- commit marker or framing length.

The reader must detect and safely truncate an incomplete final record. It must
not silently skip corruption in the middle of a committed journal.

### 21.3 Commit protocol

A durable mutation follows:

1. validate the command against current state;
2. compute new state and outgoing durable events without installing them;
3. assign the next session revision and event sequences;
4. append the transaction to the journal;
5. flush according to durability policy;
6. install the new in-memory state;
7. publish events to subscribers;
8. acknowledge the command; and
9. schedule snapshot compaction.

For mutations whose external side effect must precede final state, the journal
must first record an intent/checkpoint and later record completion. This is
required for tool execution, provider calls, and process spawn.

### 21.4 Snapshot

Snapshots contain a complete reconstructable session state at a known journal
sequence. Snapshot creation must use an immutable captured state and atomic
replacement. It must not block the session actor for the entire serialization
duration when an immutable copy can be prepared first.

### 21.5 Durability modes

The daemon may support:

- flush each durable transaction;
- bounded interval group commit; and
- explicitly unsafe development buffering.

Command acknowledgement must document which durability level has been
reached. The production default should use bounded group commit with terminal
operation and permission transitions forced before acknowledgement.

### 21.6 Locks

The daemon holds a global data-store ownership lock and session ownership
locks. Lock contention must return typed startup or load errors. Library code
must never terminate the process with `exit` because a lock is unavailable.

Stale-lock recovery must verify process/server identity before takeover.

### 21.7 Recovery

Recovery performs:

1. load and validate metadata;
2. read the latest valid snapshot;
3. replay committed journal records after the snapshot sequence;
4. validate history IDs and allocator high-water marks;
5. reconcile desired and observed lifecycle;
6. classify incomplete operations and jobs;
7. restore grants, moderator state, queued events, and schedules;
8. rebuild client-visible event replay indexes; and
9. start sessions whose desired state is running.

### 21.8 Restart guarantees

After daemon restart:

- canonical history survives committed writes;
- moderator serializable state and queued internal events survive;
- shell grants, interruption records, and audit state survive;
- desired lifecycle survives;
- prompt revision and workspace identity survive;
- durable completed jobs are redelivered when necessary without rerunning;
- durable schedules are reconciled;
- resumable jobs restart under their retry policy; and
- unsafe uncertain operations become interrupted.

An open provider stream cannot continue at the byte-stream position where the
old daemon stopped. It becomes interrupted or a policy-controlled recovery
retry. Already published canonical state is retained.

### 21.9 Corruption behavior

Middle-of-journal corruption, invalid snapshots, or history invariant failure
must fail the session closed. The server exposes diagnostics and recovery
tools but must not guess a state silently.

### 21.10 Persistence failure while running

If a required durable commit fails, the actor must stop acknowledging durable
mutations. It may continue only operations whose results can be safely held
without violating persist-before-publish. The default is to cancel active
work and enter `Failed persistence_error`.

### 21.11 Storage schema migration

The on-disk data schema, protocol version, prompt-revision schema, and
moderator snapshot format are independent version domains. Compatibility in
one domain must not imply compatibility in another.

The daemon must refuse to open a data store written by an unsupported newer
schema. An older supported schema may be migrated only while holding the
exclusive data-store lock.

A migration must:

1. inspect and validate the complete source store before mutation;
2. create a recoverable backup or new destination generation;
3. record source and destination schema versions;
4. migrate session metadata, snapshots, journals, jobs, schedules, grants,
   replay indexes, and audit references consistently;
5. preserve session IDs, history IDs, event sequences, and idempotency
   records;
6. durably commit the destination before activation;
7. switch the active store atomically; and
8. retain enough metadata to diagnose or roll back a failed migration.

The migration command should support validation-only and dry-run modes.
Automatic destructive downgrade is forbidden. A moderator snapshot or pinned
prompt revision that cannot be migrated must leave the affected session
stopped with an explicit compatibility error rather than silently discarding
state.

## 22. Event model

### 22.1 Envelope

```json
{
  "method": "session.event",
  "params": {
    "session_id": "s_123",
    "sequence": 184,
    "revision": 27,
    "timestamp": "2026-08-15T12:00:00Z",
    "kind": "tool.started",
    "payload": {}
  }
}
```

### 22.2 Ordering

Event sequence is monotonic per session. All subscribers observe durable
events in sequence order. Recoverable stream events also carry ordering
metadata relative to their operation.

Only durable events consume the session's durable `sequence`. A recoverable
stream event carries `operation_id`, monotonic `operation_sequence`, and the
latest committed durable sequence as `anchor_sequence`. Ephemeral telemetry
may carry a connection-local sequence but is never valid as a replay cursor.

The server must not assign a future durable sequence to a delta before the
corresponding durable transaction commits. When an operation reaches a
terminal state, its durable terminal event establishes the canonical replay
boundary. Clients that reconnect render completed work from canonical history
and terminal durable state, and use active-call snapshot summaries for ongoing
work before resuming live delivery. They cannot request missed live deltas.

### 22.3 Event classes

Durable state events include:

- `session.created`;
- `session.state_changed`;
- `session.updated`;
- `attachment.owner_changed`;
- `history.message_deferred`;
- `history.appended`;
- `history.replaced`;
- `moderator.overlay_changed`;
- `moderator.notification` when configured durable;
- `permission.requested`;
- `permission.resolved`;
- `grant.revoked`;
- `operation.started`;
- `operation.completed`;
- `operation.failed`;
- `operation.cancelled`;
- `operation.interrupted`;
- `job.state_changed`;
- `schedule.created`;
- `schedule.cancelled`;
- `prompt.upgraded`;
- `workspace.state_changed`; and
- `session.error`.

Recoverable live events include:

- provider stream event;
- sourced stream event;
- history-correlated stream event;
- tool started, progress, trace, and finished;
- Agent-page call classification and progress;
- activity status; and
- compaction progress.

Ephemeral telemetry includes rendering-neutral timing, diagnostics, and
heartbeats that do not change state.

### 22.4 Replay

Clients attach with `after_sequence`. The server returns all retained events
after that sequence or returns `snapshot_required` when the cursor predates
retention or is unknown.

Clients must de-duplicate by `(session_id, sequence)` and replace projection
state when a new snapshot is installed.

### 22.5 Stream retention

Completed operation deltas may be discarded after canonical terminal state is
available and the configured reconnect window expires. The terminal history
must remain sufficient to render the completed conversation.

### 22.6 Event visibility

Events are filtered by principal scope. Read-only transcript access does not
automatically include secret-bearing environment, raw shell audit, grant
administration, or unredacted tool payloads.

The durable wire projection uses `session.event`. Full events omit the
optional `visibility` field. Redacted events set it to `redacted`. Events whose
payload is completely hidden set it to `hidden` and carry an empty object as
`payload`, preserving the durable sequence without disclosing its contents.

Recoverable operation-scoped events use `session.live_event`. Their parameters
carry `operation_id`, `operation_sequence`, and `anchor_sequence`; they never
carry or consume a durable `sequence`.

The initial projection policy requires `security.read` for permission
details, active-call summaries, unredacted tool entries, and recoverable streams;
`grant.manage` for grants; and `session.message.send` for jobs/schedules. These are
principal scopes, independent of read-only/read/write attachment modes.
Transcript-only clients still receive finalized history with tool-content
placeholders. Export blobs carry the creating principal's scope identity;
downloading an export under reduced scopes is denied rather than exposing its
previously privileged bytes. HTTP snapshot ETags identify the scoped content.

## 23. Command protocol

### 23.1 Framing model

The application protocol uses JSON-RPC-style requests, responses, errors, and
notifications. It is Ochat-specific and is not MCP.

### 23.2 Request

```json
{
  "jsonrpc": "2.0",
  "id": "cmd-42",
  "method": "session.send_message",
  "params": {
    "session_id": "s_123",
    "text": "Run the tests",
    "idempotency_key": "client-a-938"
  }
}
```

### 23.3 Idempotency

Every mutating command supports an idempotency key scoped to principal,
session, and method. Repeating a completed command returns the original
outcome. Reusing a key with different parameters returns
`idempotency_conflict`.

Protocol 1.0 idempotency keys are 1 to 256 characters from
`A-Z`, `a-z`, `0-9`, `-`, `_`, `.`, `:`, and `/`. Clients should generate a
new key for each logical mutation and retain it until the outcome is known.

### 23.4 Expected revision

Administrative replacements may require `expected_revision`. Ordinary
message submission is actor-ordered and need not fail merely because another
client submitted first.

### 23.5 Error shape

Errors contain stable code, human message, retryability, and structured data:

```json
{
  "code": "permission_denied",
  "message": "This attachment is read-only.",
  "retryable": false,
  "data": {}
}
```

Stable error codes include:

- `invalid_request`;
- `method_not_found`;
- `unauthenticated`;
- `permission_denied`;
- `session_not_found`;
- `prompt_not_found`;
- `workspace_not_found`;
- `invalid_state`;
- `already_resolved`;
- `resource_limit`;
- `workspace_unavailable`;
- `prompt_unavailable`;
- `manifest_unauthorized`;
- `approval_required`;
- `idempotency_conflict`;
- `snapshot_required`;
- `operation_not_found`;
- `persistence_error`;
- `interrupted`;
- `conflict`; and
- `internal_error`.

### 23.6 Core methods

Protocol methods:

- `protocol.initialize`
- `protocol.ping`

Server and catalog methods:

- `server.info`
- `server.health`
- `prompt.list`
- `prompt.get`
- `workspace.list`
- `workspace.get`

Session methods:

- `session.create`
- `session.list`
- `session.get`
- `session.attach`
- `session.detach`
- `session.renew_owner`
- `session.start`
- `session.stop`
- `session.cancel_operation`
- `session.send_message`
- `session.compact`
- `session.delete_history`
- `session.export`
- `session.reset`
- `session.rebuild`
- `session.upgrade_prompt`
- `session.delete`

Blob methods:

- `blob.read`

Permission and security methods:

- `permission.list`
- `permission.respond`
- `grant.list`
- `grant.revoke`
- `audit.read`

Job methods:

- `job.list`
- `job.get`
- `job.cancel`

Schedule methods:

- `schedule.list`
- `schedule.get`
- `schedule.create`
- `schedule.cancel`

### 23.7 Attach

`session.attach` requests an access mode, optional owner lease, and replay
cursor. The server authorizes the requested mode and may issue a narrower
attachment.

The response includes attachment ID, granted mode, owner status, session
snapshot or replay disposition, and latest sequence.

### 23.8 Send message

`session.send_message` accepts text and optional typed attachments. When idle,
it appends a canonical user entry and starts a user turn. During an active
turn, it allocates and queues a deferred canonical entry.

The canonical Protocol 1.0 request contains a `content` object with `kind`,
`text`, and `attachments`. Initial `kind` values are `plain_text` and `chatmd`.
Each attachment carries the kind, media type, byte length, digest, optional
display name, and either a server blob ID or bounded inline Base64 content as
required by section 23.12. For migration convenience, a request may instead
provide a top-level `text` string; decoders normalize it to `plain_text`
content with no attachments. A request containing both forms is invalid.

Read-only attachments receive `permission_denied`.

### 23.9 Permission response

`permission.respond` identifies the permission ID and response. It requires
approval scope and is compare-and-set against unresolved state.

### 23.10 Stop and cancellation

Stop changes desired lifecycle. Cancellation targets current work but does not
necessarily stop the session. The protocol must not conflate them.

### 23.11 Protocol initialization and versioning

The protocol has an Ochat protocol name and a semantic wire version with
major and minor components. A major-version mismatch is incompatible. A
minor-version mismatch is compatible only when the server and client use the
intersection of advertised features.

The first request on a stateful stdio or future WebSocket connection must be
`protocol.initialize`. HTTP clients may initialize explicitly and must also
send the selected major version in the HTTP media type or version header. A
server must reject commands received before required initialization.

Initialization input contains:

- client implementation name and version;
- supported protocol major and minor range;
- supported feature identifiers;
- preferred event encodings;
- requested maximum inbound event size; and
- optional client instance ID used for diagnostics, not authorization.

Initialization returns:

- protocol name and selected version;
- server implementation name and version;
- stable server ID;
- enabled protocol features;
- authentication principal summary;
- transport and payload limits;
- event-retention summary;
- heartbeat and owner-lease timing guidance; and
- server time.

Unknown optional response fields must be ignored. Unknown methods, enum
values, or required feature identifiers must produce a typed compatibility
error rather than being guessed.

### 23.12 Common value types

Protocol timestamps use RFC 3339 UTC strings. Durations are non-negative
integer milliseconds. IDs are opaque UTF-8 strings with documented size
limits; clients must not derive meaning from their prefixes.

Every successful response has a `result` object. Every failed response has an
`error` object. A response echoes the request ID exactly. A mutating result
includes the accepted session revision and latest durable event sequence when
the mutation belongs to a session.

Session references use `session_id`. Commands issued through an attachment
also carry `attachment_id`; possession of an attachment ID is not sufficient
authorization without the authenticated principal and transport context.

Typed message attachments must include:

- attachment kind;
- media type;
- byte length;
- digest;
- a server-side blob reference or inline content within configured limits;
  and
- optional display name.

Paths supplied by remote clients are never interpreted as server filesystem
paths. Remote file input must use an authorized upload/blob mechanism or a
tool declared by the prompt.

Server-owned blobs are readable over every transport through bounded
`blob.read` requests. A request identifies the session, current attachment,
blob ID, byte offset, and maximum byte count. The response repeats immutable
blob metadata and returns a Base64 chunk, exact next offset, and end-of-file
flag. Protocol 1.0 limits one decoded chunk to 1 MiB. The server authorizes the
attachment and resolves the blob only beneath the typed session store; a
client-supplied native path is never accepted.

Clients validate metadata stability, cursor continuity, final byte length,
and SHA-256 before installing a downloaded file. HTTP clients may use the
authorized streaming blob route as an optimization, but `blob.read` remains
the transport-neutral correctness path used by Unix sockets, stdio, embedded
connections, and compatible HTTP clients.

### 23.13 Method contracts

The following table defines the minimum parameters and result. Additional
optional fields may be introduced compatibly.

| Method | Required or principal parameters | Result and behavior |
|---|---|---|
| `protocol.initialize` | Client/version/features | Selected protocol and server capabilities |
| `protocol.ping` | Optional opaque payload | Same payload, server time, and readiness summary |
| `server.info` | None | Public server identity, version, limits, and enabled transports |
| `server.health` | None; detail depends on scope | Overall status and scoped subsystem health |
| `prompt.list` | Filters and page request | Authorized prompt summaries and next cursor |
| `prompt.get` | `prompt_id` | Prompt summary, current revision identity, allowed workspaces, and defaults |
| `workspace.list` | Filters and page request | Authorized workspace summaries and next cursor |
| `workspace.get` | `workspace_id` | Workspace definition summary, availability, access mode, and applicable limits |
| `session.create` | Session spec and idempotency key | Session summary, initial revision/sequence, and optional attachment |
| `session.list` | Filters and page request | Authorized session summaries and next cursor |
| `session.get` | `session_id`, optional history window | Snapshot at a stated revision and event sequence |
| `session.attach` | `session_id`, requested mode, replay cursor, optional owner claim | Attachment, granted mode, lease, and replay or snapshot disposition |
| `session.detach` | `session_id`, `attachment_id` | Idempotent detach acknowledgement and owner-loss state if applicable |
| `session.renew_owner` | Session, owner attachment, lease generation | Renewed expiration and lease generation |
| `session.start` | Session and optional queue preference | Desired/observed state, queue position when known, revision, and sequence |
| `session.stop` | Session and stop mode | Accepted stopping state and targeted operation IDs |
| `session.cancel_operation` | Session and operation ID | Existing or newly accepted terminal/cancelling state |
| `session.send_message` | Session, attachment, text or typed content, idempotency key | Canonical history ID, accepted disposition, operation ID if started, revision, and sequence |
| `session.compact` | Session and optional compaction policy | Started, queued, or rejected disposition and operation ID |
| `session.delete_history` | Session, writable attachment, canonical occurrence ID, expected revision and idempotency key | Idle/stopped authoritative deletion, including a matching tool pair; mutation result and history replacement event |
| `session.export` | Session, format, revision/window | Export metadata and blob/download reference |
| `session.reset` | Session, reset options, expected revision | New generation summary and archive reference |
| `session.rebuild` | Session, pinned/current revision choice, expected revision | Rebuilt runtime summary and resulting revision |
| `session.upgrade_prompt` | Session, target prompt revision, migration options, expected revision | Old/new revision identities and upgrade result |
| `session.delete` | Session, deletion policy, confirmation token or equivalent | Deletion receipt and retained archive metadata |
| `blob.read` | Session, attachment, blob ID, offset, bounded maximum bytes | Immutable metadata, Base64 chunk, next offset, and end-of-file state |
| `permission.list` | Session and state filters | Visible permission requests and next cursor |
| `permission.respond` | Session, attachment, permission ID, decision, idempotency key | Committed resolution or `already_resolved` |
| `grant.list` | Session or administrative filter | Visible grants and next cursor |
| `grant.revoke` | Grant ID, reason, idempotency key | Revocation state, revision, and sequence |
| `audit.read` | Authorized filters and page request | Redacted audit records and next cursor |
| `job.list` | Session and state filters | Visible jobs and next cursor |
| `job.get` | Session and job ID | Durable job state, attempts, policy, and visible result |
| `job.cancel` | Session and job ID | Accepted or existing terminal job state |
| `schedule.list` | Session and state filters | Visible schedules and next cursor |
| `schedule.get` | Session and schedule ID | Schedule, next due time, misfire policy, and delivery state |
| `schedule.create` | Session, event payload, due time or delay, misfire/repeat policy | Durable schedule ID, next due time, revision, and sequence |
| `schedule.cancel` | Session and schedule ID | Accepted or existing terminal schedule state |

Commands that return large content must return a server-managed blob reference
or stream descriptor instead of exceeding negotiated payload limits.

### 23.14 Listing, pagination, and history windows

List methods use opaque cursor pagination with a stable sort key. A page
request includes `limit` and optional `cursor`; the response includes `items`
and optional `next_cursor`. Servers may lower an excessive requested limit.

Session history windows are requested by canonical history ID, tail count, or
server-issued window cursor. The response identifies whether the beginning or
end of retained history was reached. History pagination must not split a tool
call from its required tool output unless the response marks the boundary as
structurally incomplete and supplies the cursor needed to complete it.

A cursor is valid only for the principal, query, and server generation for
which it was issued. Invalid or expired cursors return `invalid_request` or
`snapshot_required` as appropriate.

The initial implementation also binds cursors to collection content. Changes
invalidate old cursors with `invalid_request`; clients restart the listing.
No pagination cursor retains a server-side collection. List order is stable
for the same collection. Bounded effective-history requests return the selected
window in `effective_history`; canonical history is explicitly omitted as an
empty incomplete window, never replaced by moderator-inserted entries.

### 23.15 Subscriptions and notifications

An attachment on a stateful transport subscribes the connection to visible
events for that session unless `subscribe` is false. HTTP POST is not itself a
subscription; HTTP event delivery uses the session-scoped SSE endpoint.

Server notifications use JSON-RPC notification envelopes without request IDs.
At minimum, transports may emit:

- `session.event` for ordered session events;
- `server.notice` for scoped operational notices;
- `protocol.heartbeat` for connection liveness; and
- `protocol.shutdown` before orderly server disconnect when possible.

Heartbeats, transport notices, and keep-alives do not consume session event
sequence numbers. Clients must not interpret connection closure as a session
state transition.

## 24. Attachments, ownership, and multi-client behavior

### 24.1 Access modes

```ocaml
type attachment_mode =
  | Owner_read_write
  | Read_write
  | Read_only
```

- `Owner_read_write` may keep an owner-bound session alive and perform
  authorized mutations.
- `Read_write` may perform authorized mutations but does not keep an
  owner-bound session alive.
- `Read_only` receives permitted snapshots and events but cannot mutate the
  session or answer approvals.

The server may define narrower scopes such as approval-only, audit-read, or
session-management authority. The client-requested mode is never trusted
without server authorization.

### 24.2 Owner leases

Owner-bound sessions track owner leases rather than raw TCP connections. A
lease contains attachment identity, principal identity, expiration, and
reclaim token or authenticated owner identity.

When the last owner detaches:

1. emit owner-loss state;
2. start `disconnect_grace_ms`;
3. allow an authorized owner to reclaim during the grace period;
4. cancel the timer when ownership returns; and
5. apply configured graceful or cancelling stop when the timer expires.

Observers and ordinary read/write clients do not satisfy the owner
requirement.

Owner leases use server time and a monotonically increasing lease generation.
The holder renews before the advertised renewal deadline with
`session.renew_owner`; successful authenticated activity may renew implicitly
only when the server declares that feature during initialization. A stale
generation cannot shorten, replace, or reclaim a newer lease.

An SSE stream, TCP connection, or stdio process is not itself proof that an
owner lease is current. Conversely, a temporary transport disconnect does not
immediately destroy ownership while the lease and disconnect grace remain
valid.

### 24.3 Concurrent messages

Concurrent client commands are ordered when accepted by the actor. If two
clients submit while idle, the first accepted message starts the turn and the
second is deferred. Both canonical entries retain actor-assigned identity and
all clients observe the accepted order.

### 24.4 Concurrent approvals

Approval responses are compare-and-set against unresolved state. The first
valid response commits; all later responses receive `already_resolved` and
the committed response metadata permitted by their scope.

### 24.5 Client-local state

The server does not synchronize:

- TUI scroll position;
- selected row;
- terminal dimensions;
- input cursor;
- local draft unless a future draft API is used;
- local command history;
- highlight caches; or
- active TUI page.

## 25. HTTP transport

### 25.1 Independence from MCP

The HTTP server is a new Ochat server. It must not use MCP session IDs,
registries, routing, capability negotiation, or notification names.

The implementation may copy or extract low-level techniques from the legacy
MCP HTTP code, including Piaf startup, body streaming, SSE framing,
keep-alives, bearer extraction, OAuth helpers, and Eio cleanup.

### 25.2 Endpoints

The initial HTTP surface is:

```text
POST /v1/rpc
POST /v1/blobs
GET  /v1/blobs/<blob-id>
GET  /v1/sessions/<session-id>/events
GET  /v1/sessions/<session-id>/snapshot
GET  /v1/health
```

Catalog discovery may be exposed through RPC, dedicated GET endpoints, or
both. RPC remains the normative command behavior.

### 25.3 RPC requests

`POST /v1/rpc` accepts one request or a batch. Authentication and body limits
are checked before dispatch. Notifications that do not require responses may
return an empty success body as defined by the HTTP adapter.

### 25.4 SSE subscriptions

The events endpoint establishes a session-scoped SSE stream. It accepts a
cursor from `Last-Event-ID` or an explicit query parameter. If both exist and
differ, the request is invalid.

SSE frames use durable session sequence where applicable:

```text
id: 482
event: session.event
data: {"session_id":"s_123","sequence":482,...}

```

Keep-alive comments may be sent without consuming session sequence numbers.
The server must reclaim the temporary SSE observer within a bounded number of
missed keep-alive intervals when the downstream writer stops consuming.

### 25.5 Replay and snapshot requirement

If the requested cursor is retained, the server replays events and then
continues live delivery. If not retained, the server returns a structured
snapshot-required response or emits a snapshot-required control event and
closes the stream.

HTTP clients must expose event-stream termination to the shared connection
lifecycle. They must not silently retry a lost logical stream while reporting
connected. Recovery opens a fresh logical connection and reattaches using the
last applied durable cursor, replacing the projection if the cursor has expired.
Transport loss does not mark the daemon session stopped.

### 25.6 Per-session subscriber registry

Subscribers belong to one session actor or session event hub. There must be no
global broadcast list that sends unrelated session events to every client.

### 25.7 Backpressure

Each SSE subscriber has a bounded queue. When it overflows, the server closes
the stream after recording the last successfully queued or sent sequence. The
client reconnects and replays from that cursor.

### 25.8 Authentication

Production remote listeners require authentication. Initial authenticators
may include:

- static bearer token;
- OAuth bearer validation using existing generic OAuth helpers;
- reverse-proxy asserted identity under explicit trust policy; and
- Unix-socket peer credentials for local connections where supported.

Authentication produces an Ochat principal and scopes, not an MCP transport
session.

The default Unix listener accepts only peers whose effective UID matches the
daemon. The principal ID is a stable opaque derivation of that UID, and
transport-safe UID/GID/PID attributes are attached when the platform exposes
them. Platforms without peer credentials fail closed unless a future explicit
socket bearer or unsafe-local policy is configured. The socket parent must be
daemon-owned and inaccessible to group/other users.

### 25.9 WebSockets

WebSocket support may be added later as another framing adapter around the
same commands and events. It must not introduce separate session behavior.
HTTP POST plus SSE is the initial canonical HTTP transport.

### 25.10 Blob transfer

Binary or oversized message input and exports use authenticated blob
transfer. Upload requests are bounded, streamed to server-managed storage,
hashed while reading, and return an opaque blob ID, digest, size, media type,
expiration, and allowed use.

A blob is bound to its creating principal and, when supplied, a target
session. Referencing it from a command performs authorization again. Blob IDs
must not expose native paths, and blob content must never be loaded entirely
into memory solely because the HTTP adapter received it.

Unreferenced temporary blobs expire and are cleaned up. Referenced durable
message content is moved or copied into session-owned storage before command
acknowledgement. Downloads require scope checks and support streaming and
bounded range requests where practical.

## 26. Stdio transport

### 26.1 Framing

Stdio uses newline-delimited UTF-8 JSON. Each line contains exactly one
protocol envelope. Standard output is reserved exclusively for protocol
messages. Diagnostics go to standard error.

### 26.2 Standalone stdio

Standalone stdio hosts an embedded session engine in the stdio process. EOF
releases the owner and stops or cancels the process-bound session. It may
accept an arbitrary local prompt and current workspace.

Conceptual invocation:

```console
$ ochat-agent-stdio --local --prompt ./prompts/coding.chatmd --workspace .
```

### 26.3 Daemon stdio gateway

A gateway forwards commands to a daemon and forwards responses and events to
stdout. It does not own detached session lifetime.

```console
$ ochat-agent-stdio --connect unix://~/.ochat/server.sock
# Select the session through session.list/session.attach after protocol.initialize.
$ ochat-agent-stdio --connect https://agents.example.test \
    --bearer-token-file ./agent.token
```

EOF detaches the gateway. A detached daemon session keeps running. An
owner-bound session follows normal owner-lease behavior.

### 26.4 Asynchronous messages

Responses and events may interleave. Request IDs correlate responses; session
and event sequences correlate events. Client implementations must not assume
one response line immediately follows one request line.

### 26.5 Parse errors

Malformed input produces a structured protocol error on stdout when framing
can continue. Repeated or oversized malformed input may terminate the
connection without affecting daemon-owned sessions.

## 27. TUI integration

### 27.1 TUI modes

The TUI supports:

- standalone embedded session execution;
- attachment to an existing daemon session; and
- creation of a new daemon session followed by attachment.

Conceptual commands:

```console
$ chat-tui --local -file ./prompts/coding.chatmd
$ chat-tui --connect unix://~/.ochat/server.sock --session s_123
$ chat-tui --connect unix://~/.ochat/server.sock \
    --new-daemon-session --prompt coding-agent --workspace ochat --detached
$ chat-tui --connect https://agents.example.test \
    --bearer-token-file ./agent.token --session s_123
```

### 27.2 TUI as projection

In connected mode, the daemon owns:

- canonical and effective history;
- history ID allocation;
- turns and compaction;
- deferred messages;
- tools and shell runtimes;
- approvals;
- ChatML state and wakeups;
- jobs and schedules;
- lifecycle; and
- persistence.

The TUI owns presentation and input state.

### 27.3 Shared client abstraction

Standalone and connected TUI modes use the same client-facing command/event
interface. Standalone mode uses an in-memory adapter; connected mode uses a
daemon transport.

The renderer and controllers should not depend on whether the session is
local or remote.

### 27.4 Reconnection

On connection loss, the TUI:

1. marks the view disconnected;
2. preserves local draft and viewport state;
3. reconnects with the last applied event sequence;
4. reclaims owner lease when applicable;
5. applies replay or a replacement snapshot; and
6. resumes normal event processing.

Connection loss must not be rendered as session completion.

### 27.5 TUI parity

Server-owned parity includes:

- ChatMD parsing and imports;
- model configuration;
- all tool kinds;
- shell manifest and approval behavior;
- ChatML lifecycle and overlays;
- streaming and tool progress;
- deferred canonical messages;
- compaction;
- cancellation repair;
- follow-up budgets;
- export, reset, and rebuild;
- shell grant administration; and
- Agent-page classification metadata.

Client-owned features include rendering, highlighting, scrolling, selection,
keyboard modes, type-ahead, and terminal resize behavior.

## 28. Common client library

The OCaml client library should expose conceptual modules:

```text
Ochat_client.Protocol
Ochat_client.Connection
Ochat_client.Session
Ochat_client.Subscription
Ochat_client.Reconnect
Ochat_client.In_memory
Ochat_client.Http
```

It owns request IDs, response correlation, event ordering checks, replay
cursors, reconnect policy, authentication headers, snapshot replacement, and
bounded blob download validation.

The stdio gateway and TUI must reuse this library rather than implement
independent protocol clients.

## 29. MCP relationship

### 29.1 Deprecated prompt server; maintained tool integration

The old server that exposes ChatMD prompts as agents through MCP is legacy and
deprecated. It is not included in, or an architecture base for, the new Ochat
agent server. Existing legacy-server code may remain for compatibility.

ChatMD tool declarations that connect to external MCP servers are not legacy or
deprecated. They are actively maintained runtime functionality and remain
available to agents hosted by the new server, including discovery, authentication,
tool execution, notification handling, and identity-isolated discovery caching.

### 29.2 No new-server dependency

New server transport/session ownership must not be implemented using:

- `Mcp_server_core`;
- `Mcp_server_router`;
- MCP prompt/tool registries;
- MCP transport sessions;
- MCP progress tokens;
- MCP list-changed notifications; or
- MCP capability negotiation.

This does not prohibit MCP client/types/transport dependencies inside the
maintained runtime tool adapter. It prohibits using the old MCP prompt server's
protocol and state as the new Ochat agent-server architecture.

### 29.3 Reusable mechanics

The new implementation may reuse or copy:

- Piaf listener setup;
- request body parsing;
- streaming body construction;
- SSE framing and cleanup;
- keep-alive behavior;
- bearer-token extraction;
- generic OAuth helpers;
- Eio fiber patterns; and
- NDJSON parsing and writing.

If clean extraction risks legacy behavior, code should be copied into new
protocol-neutral modules. Updating MCP to use those modules is optional.

### 29.4 Future bridge

A future MCP adapter may call daemon sessions through the Ochat client API,
but that adapter is outside the core server and must not force MCP semantics
onto durable sessions.

## 30. Authentication and authorization

### 30.1 Principal

```ocaml
type principal =
  { id : string
  ; authentication_kind : string
  ; scopes : scope list
  ; attributes : (string * string) list
  }
```

### 30.2 Scopes

Scopes may include:

- list prompts and workspaces;
- create sessions;
- view session transcript;
- send messages;
- own sessions;
- answer approvals;
- view security state;
- manage grants;
- read audit;
- stop or delete sessions; and
- administer configuration.

The initial stable wire identifiers are:

| Scope | Wire identifier |
|---|---|
| List prompts | `prompt.list` |
| List workspaces | `workspace.list` |
| Create sessions | `session.create` |
| View session transcript | `session.transcript.read` |
| Send messages | `session.message.send` |
| Own sessions | `session.own` |
| Answer approvals | `permission.respond` |
| View security state | `security.read` |
| Manage grants | `grant.manage` |
| Read audit | `audit.read` |
| Stop sessions | `session.stop` |
| Delete sessions | `session.delete` |
| Administer configuration | `configuration.admin` |
| Read diagnostics | `diagnostics.read` |

Scope identifiers are additive within a protocol major version. Unknown
required scopes must be rejected rather than interpreted as a broader known
scope.

### 30.3 Authorization checks

Authorization occurs at request dispatch and again inside the session actor
against current attachment and session state. This prevents a stale transport
decision from bypassing a revoked attachment.

### 30.4 Local unsafe mode

An unauthenticated loopback development mode may exist but must require an
explicit flag. It must not be the default for non-loopback listeners.

## 31. Security boundaries

### 31.1 Workspace non-isolation

The server must clearly report that workspace selection does not confine a
prompt. Actual confinement comes from ChatMD declarations, shell
administrative policy, and execution backends.

### 31.2 Path handling

Server-controlled prompt, workspace, session, cache, and temporary paths must
be canonicalized and validated. Remote clients must not supply arbitrary
native paths through normal catalog APIs.

### 31.3 Secrets

Secrets must not appear in ordinary events, snapshots, errors, or logs.
Permission displays and audit records use existing shell redaction policy.

### 31.4 Event access

Read-only transcript access does not imply access to raw tool arguments,
unredacted outputs, environment identity, shell grants, or audit trails.

### 31.5 Temporary deletion

Before deleting a temporary workspace, the server verifies that the exact
path was created for the target workspace instance and is beneath the
configured server-owned root. Broad paths, unresolved variables, and physical
workspaces are never deletion targets.

## 32. Resource limits and fairness

The server must support limits for:

- root agents per prompt and workspace conflict domain;
- exclusive workspace leases;
- loaded sessions;
- total running sessions;
- provider turns per session;
- follow-up turns and rate windows;
- internal-event drains;
- nested-agent depth and concurrency;
- spawned jobs;
- schedules;
- tool concurrency;
- subscriber count;
- subscriber queue size;
- request and event payload size;
- retained event bytes;
- cache size; and
- HTTP connection count.

Fair queues should avoid one session monopolizing global model, tool, or
workspace capacity. Cancellation and terminal outcomes must not be starved by
ordinary progress events.

## 33. Observability and audit

### 33.1 Structured logs

Logs include timestamp, component, session ID, operation ID, job ID, principal
ID when appropriate, severity, and structured fields. Secrets and redacted
content follow security policy.

### 33.2 Metrics

Useful metrics include:

- sessions by observed state;
- queued root-agent starts;
- attached clients by mode;
- active turns and tools;
- provider latency and idle timeouts;
- job states and retries;
- permission wait duration;
- event replay and snapshot frequency;
- subscriber disconnects due to backpressure;
- persistence latency and failure count;
- workspace lease utilization; and
- recovery outcomes.

### 33.3 Audit

Security-sensitive actions are durable and attributable:

- session creation and deletion;
- prompt upgrades;
- manifest authorization;
- tool approvals and denials;
- durable grants and revocations;
- shell execution;
- workspace cleanup;
- administrative lifecycle changes; and
- recovery decisions for uncertain side effects.

### 33.4 Health

Health reports listener state, storage writability, scheduler state, session
registry readiness, and degraded subsystems. It must not expose secret
configuration.

## 34. Shutdown behavior

Graceful daemon shutdown must:

1. stop accepting new sessions and mutations;
2. notify clients of server shutdown when possible;
3. mark running foreground operations for cancellation or bounded completion;
4. stop scheduling new background work;
5. terminate and reap owned processes;
6. checkpoint every loaded durable session;
7. persist job and schedule states;
8. close subscriber streams;
9. release session and daemon locks; and
10. exit after the grace deadline.

Forced shutdown may leave intent records without terminal records. Recovery
classifies them using the normal interruption rules.

Standalone TUI and stdio shutdown follows the same embedded-engine teardown
but may additionally prompt for export or persistence according to local UI
policy.

## 35. Failure behavior summary

| Failure | Required behavior |
|---|---|
| Client disconnect from detached session | Detach only; session continues |
| Last owner disconnect | Start grace period, then configured stop |
| Slow subscriber | Disconnect and allow replay |
| Provider idle timeout | Fail turn, repair history, emit error |
| Tool failure | Produce canonical error output when required and continue or fail by policy |
| Permission denial | Produce denied tool result and continue through post-tool semantics |
| Daemon crash during model stream | Recover committed state; mark or retry turn by policy |
| Daemon crash during unknown side effect | Mark interrupted; do not silently rerun |
| Snapshot failure | Retain journal state; report degradation |
| Required journal failure | Stop durable mutations and fail session closed |
| Workspace missing on restore | `Failed workspace_unavailable` |
| Prompt revision unavailable | `Failed prompt_unavailable` |
| Moderator snapshot mismatch | Fail startup pending explicit upgrade/reset |
| Suspended ChatML approval on restart | Explicit interrupted approval state |
| Manifest grant no longer valid | Require authorization before startup |
| Journal corruption | Fail closed and expose recovery diagnostics |

## 36. End-to-end behavior examples

### 36.1 Persistent daemon agent

1. A client creates a durable detached session using prompt `coding-agent`
   and workspace `ochat`.
2. The daemon resolves the physical workspace and prompt revision.
3. It acquires the root-agent slot and starts the runtime.
4. The client attaches as owner/read-write and sends a message.
5. The session streams updates to all subscribers.
6. The client disconnects.
7. The session continues because liveness is detached.
8. ChatML background jobs may wake and request follow-up turns.
9. A later TUI reconnects using the last event sequence.

### 36.2 Owner-bound daemon agent

1. A TUI creates an owner-bound session with a 30-second grace period.
2. The TUI disconnects unexpectedly.
3. The daemon starts the grace timer while the active turn continues or waits
   according to policy.
4. The TUI reconnects and reclaims the owner lease before expiration.
5. The timer is cancelled and the session continues.
6. If no owner returned, the configured graceful or cancelling stop would run.

### 36.3 Multiple writers

1. Two clients attach read/write.
2. Client A submits message A while idle.
3. The actor starts a user turn.
4. Client B submits message B during the turn.
5. The actor allocates a canonical ID and queues B.
6. Tool outputs and turn-end handling complete.
7. B is appended after current outputs and triggers the next user turn.
8. Both clients observe the same event order.

### 36.4 Standalone TUI

1. The TUI captures its current directory as workspace and tool directory.
2. It loads an arbitrary local ChatMD prompt.
3. It creates the shared session engine with an in-memory client adapter.
4. The UI renders events exactly as it would for a daemon session.
5. Exiting tears down the process-bound session after optional export/save.

### 36.5 Standalone and gateway stdio

Standalone stdio hosts the engine and terminates its session on EOF. Gateway
stdio attaches to a daemon; EOF detaches the client and leaves detached
sessions running.

### 36.6 Restart with completed background model job

1. A model job succeeds and its result is persisted.
2. The daemon crashes before marking moderator delivery complete.
3. Recovery sees a succeeded undelivered job.
4. It enqueues the completion event idempotently.
5. The moderator drains it at a safe point.
6. The model call is not rerun.

### 36.7 Restart during shell execution

1. A shell command crosses authorization and starts.
2. Intent and process metadata are durable.
3. The daemon terminates before a terminal audit record.
4. Recovery marks the invocation interrupted and reconciles any remaining
   process when possible.
5. It does not automatically rerun the command.

## 37. Proposed module organization

Names may evolve, but responsibilities should remain separated:

```text
lib/agent_server/
  server.ml
  config.ml
  principal.ml
  auth.ml
  protocol.ml
  command.ml
  event.ml
  error.ml
  prompt_catalog.ml
  prompt_revision.ml
  workspace_catalog.ml
  workspace_instance.ml
  session_registry.ml
  session_actor.ml
  session_state.ml
  session_engine.ml
  session_snapshot.ml
  session_store.ml
  event_journal.ml
  permission_policy.ml
  permission_request.ml
  job_store.ml
  job_scheduler.ml
  http_server.ml
  http_router.ml
  sse_stream.ml
  stdio_server.ml

lib/ochat_client/
  protocol.ml
  connection.ml
  session.ml
  subscription.ml
  reconnect.ml
  in_memory.ml
  http.ml
```

Modules should follow repository coding guidelines: one primary `t` where
appropriate, small functions, typed errors instead of process exits, minimal
opens, interface documentation, and focused tests.

## 38. Implementation phases

### Phase 1: Extract the session engine

Move transport-neutral prompt setup, moderator construction, foreground state,
deferred messages, safe-point behavior, compaction coordination, permission
waiting, and cancellation repair out of `Chat_tui`.

Standalone TUI must continue working through an in-memory adapter.

### Phase 2: Commands, events, and snapshots

Define typed protocol structures, JSON conversion, stable errors, event
sequences, client snapshots, idempotency, and protocol expect tests.

### Phase 3: Durable session storage

Implement typed locking, event journal, snapshots, recovery, prompt pinning,
workspace snapshots, and continuous persistence. Existing `Session.t` and
migrations should be reused or evolved where appropriate.

### Phase 4: Daemon and stdio gateway

Implement global configuration, catalogs, session registry, root-agent
limits, detached and owner-bound lifecycle, Unix/local connection, and stdio
gateway.

### Phase 5: HTTP and SSE

Implement new Ochat routing, authentication, RPC, per-session SSE,
backpressure, replay, and reconnect. Reuse low-level MCP transport logic only
through copying or protocol-neutral extraction.

### Phase 6: Generic permission policy

Add the all-tool gate, interactive and unattended profiles, durable requests,
multi-client resolution, and non-duplicated shell delegation.

### Phase 7: Durable jobs and schedules

Persist model jobs and `Schedule.after_ms`, implement idempotent delivery,
restart reconciliation, process interruption, and job APIs.

### Phase 8: Connected TUI

Add daemon session creation, attachment, replay, owner reclaim, disconnected
rendering, and remote shell approval management to `chat_tui`.

### Phase 9: Hardening and migration

Add stress tests, corruption tests, long-running recovery tests, retention,
operational tooling, schema migrations, and documentation. MCP remains
separate and unchanged except for necessary maintenance.

## 39. Testing requirements

Tests must cover:

- prompt/workspace allowlists;
- physical and temporary workspace resolution;
- symlink and canonical-path quota identity;
- root-agent reject and queue behavior;
- exclusive workspace leases;
- detached client disconnect;
- owner-bound grace and reclaim;
- multiple readers and writers;
- deferred canonical message ordering;
- concurrent approval races;
- read-only enforcement;
- stream batching and event order;
- slow subscriber disconnection and replay;
- snapshot-required behavior;
- cancellation repair;
- compaction and safe-point ordering;
- moderator wakeups during active turns;
- follow-up and drain budgets;
- durable model job redelivery;
- schedule recovery;
- interrupted unsafe tool behavior;
- manifest and grant persistence;
- prompt revision pinning and upgrade;
- persistence failure;
- incomplete journal tail recovery;
- middle-journal corruption failure;
- daemon restart at every operation boundary;
- standalone and connected TUI behavioral parity;
- standalone and gateway stdio semantics;
- HTTP authentication and event filtering; and
- legacy MCP continuing to build and pass its existing tests.

Tests should use deterministic fake provider streams, injectable clocks,
bounded fake tools, temporary data roots, and explicit crash/restart harnesses.

## 40. Acceptance criteria

The first production-capable release is complete when:

1. The same ChatMD prompt produces equivalent agent behavior in standalone
   TUI, standalone stdio, connected TUI, stdio gateway, and HTTP control.
2. A detached daemon session continues after every client disconnects.
3. Multiple clients receive ordered real-time updates and can reconnect using
   event sequence.
4. Read-only clients cannot mutate sessions or answer approvals.
5. Owner-bound sessions stop after owner grace expiry and survive temporary
   disconnect when reclaimed.
6. Physical and managed temporary workspaces resolve correctly and are not
   mistaken for authorization boundaries.
7. Prompt/workspace root-agent limits and exclusive leases are enforced.
8. Session history, moderator state, grants, desired lifecycle, jobs, and
   schedules survive committed daemon restart.
9. Unsafe uncertain side effects are visibly interrupted and never silently
   repeated.
10. HTTP uses per-session replayable SSE rather than MCP global broadcasts.
11. Stdio supports both embedded and daemon-gateway operation.
12. The TUI can run locally without a daemon and attach to a daemon using the
    same client-facing state model.
13. The new server has no dependency on MCP protocol modules.
14. Persistence and lock failures return typed errors and never terminate the
    daemon from library code.
15. Security-sensitive actions are authenticated, authorized, redacted, and
    audited.

## 41. Required invariants summary

The implementation must preserve these invariants at all times:

1. One authoritative session actor mutates one live session.
2. At most one foreground operation runs per session.
3. A detached session is independent from client connection lifetime.
4. An owner-bound session is controlled by owner leases, not observer count.
5. Workspace selection grants no tool or filesystem authority.
6. Workspace and tool-directory coordinates are explicit: `${workspace}` is
   selected independently, while `${tool_dir}` preserves the captured launch
   directory unless overridden. Sessions never change process cwd.
7. Canonical history has stable application-owned identity.
8. Deferred user messages are canonical and cannot split tool call/output
   pairs.
9. Moderator background work is applied only at safe boundaries.
10. Durable state is recorded before durable event publication and command
    acknowledgement at the promised durability level.
11. Event sequences are monotonic per session and replayable within retention.
12. Slow clients cannot block agent execution.
13. Hard denials and capability ceilings cannot be overridden by approval.
14. The first valid approval response wins.
15. Unknown side effects are never automatically replayed after uncertain
    completion.
16. Prompt revisions and workspace identities are pinned for durable sessions.
17. Physical workspaces are never deleted by session cleanup.
18. TUI presentation state remains client-local.
19. HTTP, stdio, and TUI use the same commands, events, and engine semantics.
20. Legacy MCP remains separate from the new server architecture.
