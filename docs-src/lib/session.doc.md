# Session — legacy persistent conversation state

## Overview

This record serves the [file-backed compatibility host](prompt_session.doc.md).
Daemon sessions use [actor-owned storage](../agent-server/operations.md).
Current schema V5 carries canonical history, allocator state, tasks, moderator
state, shell state, prompt metadata and VFS/key-value bookkeeping. Neither a
record nor a snapshot is a running-process continuation.

## Quick API reference (simplified)

See the [exact interface](../../lib/session.mli).
`Session.History.t = History_entry.t list`, not raw provider items.
The public record includes `next_history_sequence`, `moderator_state` and
`shell_state` in addition to history and prompt metadata. Allocate new
occurrences consistently with the session namespace and high-water mark;
`Session.allocator` and `Session.validate` expose that contract.

## Detailed semantics

### Creation

`create ~prompt_file ()` defaults history/tasks/key-value data to empty and
VFS root to `vfs`. Optional arguments seed prompt copy, allocator, moderator
and shell state; consult the interface rather than constructing an old record
shape. Omitted IDs are time/PRNG-derived MD5 strings, not credentials.
Construction performs no file I/O, but default ID generation is not pure.

### Resetting a session

`reset` clears history, moderator and shell state. `reset_keep_history` retains
history but still clears moderator/shell state. Both preserve identity, tasks,
key/value data, VFS root and allocator high-water mark. Optional prompt_file
changes the recorded path. Neither function writes files or executes the prompt.

### Task helpers

Tasks have string IDs, titles and Pending/In_progress/Done states. They are
bookkeeping, not the daemon job scheduler or permission grants.

### File I/O

Direct `Session.Io.File.read` decodes only the current binary shape.
Use `Session_store.read_current_file` for migration-aware V5–V0 loading.
Supported migration is not arbitrary forward/downgrade compatibility.

Direct `Session.Io.File.write` serializes and truncates in place; it is not an
atomic or locked save. For compatibility-host publication, use
`Session_store.save`, which adds exclusive save locking and temporary-file
rename. Both use Eio and requested mode 0600 for new files.

## Examples

### 1. Start a new session and save it

```ocaml
let create_and_save env prompt_file =
  let session = Session.create ~prompt_file () in
  Session_store.save ~env session
```

Handle the returned Result; directory setup may raise.

### 2. Load an old snapshot and begin a fresh chat

```ocaml
let load_and_reset path =
  Core.Result.map (Session_store.read_current_file path)
    ~f:(Session.reset ~prompt_file:"agents/revised.chatmd")
```

This returns new in-memory state without overwriting the original.
Archive/back up before persisting a reset.

## Limitations

The record supplies no concurrency control. Save-time locks do not coordinate
separate live legacy TUIs or merge concurrent histories. History is held in
memory. Binary formats require supported migrations and may include sensitive
payloads. Use daemon sessions for shared, durable background execution.

## Typed shell security state

Shell state includes manifest/command grants, source-bound extension snapshots,
audit sequence and interrupted-request metadata. Older versions migrate with
empty shell trust. History alone never grants approval; reset clears trust and
an in-flight process is not resumed from its snapshot.
