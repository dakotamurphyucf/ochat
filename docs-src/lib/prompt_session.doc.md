# Prompt sessions — compatibility-host state

This overview describes the **legacy file-backed TUI**, not the daemon store.
Native `--local` runs transiently; daemon sessions use actor-owned journals and snapshots. See
[host modes](../agent-server/concepts.md) and
[local TUI choices](../agent-server/tutorials/local-tui.md).

## Rationale & big picture

A ChatMD file defines the initial agent. A `Session.t` records conversation and
moderator/shell state for the compatibility host. Loading a session is not
resuming an OCaml stack, running tool process, or pending network connection.

## Data model

Current schema **V5** contains identity-bearing `History_entry.t` occurrences,
the history allocator high-water mark, prompt path/local copy, tasks, key/value
data, moderator state, shell state and VFS root. It is not just a list of raw
provider items. See the [Session reference](session.doc.md) and
[exact interface](../../lib/session.mli).

The record is public; there are no `Session.history` or `Session.with_history`
accessor functions. Hosts must preserve history IDs and allocator invariants
when updating it. A ChatMD export is a different representation, not a complete
binary-session backup.

## CLI integration

Legacy `--session ID`, `--new-session`, listing, inspection, reset/rebuild and
export flags are described in the [TUI CLI](../bin/chat_tui.doc.md).
Without an explicit ID or new-session request, the store derives an ID from
the prompt path supplied by its caller. New-session mode chooses a fresh
time/PRNG-derived MD5 ID (despite the internal helper's historical uuid_v4 name).
Do not apply those rules to daemon opaque session IDs.

Creating/loading returns an in-memory value; it does not immediately write
`snapshot.bin`. A best-effort `prompt.chatmd` copy may be created.

## Public API walk-through

```ocaml
let save_example env prompt_file =
  let session = Session_store.load_or_create ~env ~prompt_file () in
  match Session_store.save ~env session with
  | Ok () -> ()
  | Error error -> Core.Error.raise error
```

`save` returns `unit Core.Or_error.t`; `save_exn` is the compatibility wrapper.
Directory-creation failures before lock acquisition can still raise.
`load_or_create` raises for an existing unreadable snapshot rather than
silently replacing it. See [store behavior](session_store.doc.md).

TUI `:w` submits the draft; it is not a binary snapshot-save command.
Legacy orderly shutdown handles ChatMD export and binary persistence separately.
`--auto-persist`, `--no-persist` and the default save question control the
shutdown snapshot. Shell/security state changes and pre-compaction saves have
their own persistence paths; these flags do not disable every write. See
[checkpoint behavior](../guide/chat_tui.md#snapshot-saving-on-exit)
and [persistence adapter](chat_tui/persistence.doc.md).

## Persistence format & versioning

The store tries current V5 and supported V4–V0 decoders, validates versions and
migrates supported records to current state. Direct `Session.Io.File.read`
decodes the current shape; it does not perform this migration ladder.
Unrecognized/corrupt snapshots fail rather than silently discarding history.
A schema counter is not a promise that future versions can be read by old code.

## FAQ & troubleshooting

- Missing snapshot: a new in-memory record can be created.
- Corrupt snapshot: preserve it and investigate the error; do not overwrite it
  as an automatic recovery action.
- Lock exists: saves fail; first determine whether another writer is active.
  A crash can leave a stale lock.
- Multiple legacy TUIs: save-time locking does not merge concurrent histories.
  Use the [daemon](../agent-server/README.md) for coordinated multi-client sessions.
