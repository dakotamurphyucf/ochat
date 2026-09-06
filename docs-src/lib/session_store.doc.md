# Session_store — legacy snapshot persistence

This is the file-backed compatibility store under `$HOME/.ochat/sessions`
(or `./.ochat/sessions` without HOME), not
[daemon storage](../agent-server/operations.md). Avoid changing HOME merely to
relocate it; HOME also affects unrelated credentials and configuration.

## Quick reference

The [public interface](../../lib/session_store.mli) defines the exact API.
Important entry points:

- `load_or_create ~env ~prompt_file ?id ?new_session () : Session.t`
- `read_existing ~env ~id : Session.t option`
- `read_current_file path : (Session.t, Core.Error.t) result`
- `save ~env session : unit Core.Or_error.t`
- `save_exn ~env session : unit`
- `list ~env : (string * string) list`

Staged V4 readers distinguish missing, loaded and unreadable snapshots without
modifying them; they serve migration/inspection callers.

## 1. Directory layout & identifier strategy

Each ID has a directory containing `snapshot.bin`, an optional `prompt.chatmd`
copy, and associated runtime data. Selection is: a fresh time/PRNG-derived MD5 ID when
`new_session=true`; otherwise explicit ID; otherwise MD5 of the supplied prompt
path. The library does not canonicalize that path for its caller.

`ensure_dir`/`path` create directories with requested mode 0700. IDs and paths
are trusted local inputs, not daemon-authorized remote path selectors.

## 2. Reading or creating a session – load_or_create

An existing snapshot is decoded with `read_current_file`, which tries V5
through supported legacy shapes and validates/migrates them. Failure raises;
it does not create an empty replacement. Missing snapshots return a fresh
in-memory record and attempt a private prompt copy. Copy failure is ignored.

`read_existing` returns None on missing/unreadable snapshots; `list` skips
unreadable records. These convenience APIs intentionally lose the diagnostic
distinction, unlike the staged reader. Loading does not save the migrated value.

## 3. Saving – save

Exclusive creation of `snapshot.bin.lock` serializes individual save operations.
Lock/persistence errors are returned; `save_exn` raises them. Initial directory
creation happens before this Result boundary and can raise.
The store writes an exclusive private temporary file and renames it into place.
It does not fsync; direct Session.Io.File.write still truncates in place. The save lock
is removed on normal unwinding; process crashes can leave it behind.

This is not lifetime ownership or revision compare-and-set: two processes can
load the same snapshot and later overwrite each other's work in separate,
individually locked saves. Use the daemon for multi-client coordination.

## 4. House-keeping helpers

### 4.1 reset_session

Archives the existing snapshot as
`archive/YYYYMMDD-HHMM.snapshot.bin`, resets history unless
`~keep_history:true`, clears moderator/shell state, and saves. A supplied prompt
is copied and recorded. Missing/unreadable sessions print diagnostics.
Minute-resolution archive names can overwrite an earlier archive from the same
minute; archive-rename errors are ignored. This is **not** the daemon's retained,
checksummed administration archive contract.

### 4.2 rebuild_session

Archives similarly, creates fresh empty state from recorded prompt metadata,
and removes `.chatmd/cache.bin`. Prompt parsing occurs on a subsequent launch.
Reset/rebuild are not rollback-capable transactions and use exception-raising
save internally. Back up important material before maintenance.

## 5. Example – minimal CLI wrapper

```ocaml
let save_session env prompt_file =
  let session = Session_store.load_or_create ~env ~prompt_file () in
  Session_store.save ~env session
```

The caller must inspect the returned Result. See
[TUI checkpoint/export rules](../guide/chat_tui.md#snapshot-saving-on-exit).

## 6. Limitations / future work

Whole-record saves, local trusted paths, save-time-only locks and minute-based
maintenance archives are compatibility constraints, not daemon guarantees.
Binary snapshots and exports may contain sensitive tool/history data.

## Shell-state persistence

V5 includes shell state. Migrating older schemas supplies empty shell security
state. Rebuilding or importing a transcript does not infer fresh authorization.
See [Session](session.doc.md) and [legacy import](../agent-server/operations.md#inspection-migration-and-legacy-import).
