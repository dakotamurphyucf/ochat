# `Prompt sessions` – runtime state for conversational chats

This document complements the inline `odoc` comments of
`session.{mli,ml}` and `session_store.{mli,ml}` with a higher-level
overview, rationale, and concrete examples that do **not** belong in
the terse API reference.

Table of contents
-----------------

1. Rationale & big picture
2. Data model
3. CLI integration
4. Public API walk-through
5. Persistence format & versioning
6. FAQ & troubleshooting

------------------------------------------------------------------------

1  Rationale & big picture
---------------------------------

Historically *ochat* treated prompt files (`*.chatmd`) as **immutable**
specifications of a conversation: you started `chat_tui`, chatted for a
while, and then all messages vanished when the process ended.

`Prompt sessions` promote the **runtime state** to a first-class object
that can be *saved*, *restored*, and migrated across schema versions.
They complement – but never replace – the original prompt file which
remains the authoritative declaration of the *initial* system & user
messages.

In practice this means:

* Start a new session → `snapshot.bin` is created under
  `~/.ochat/sessions/<session-id>/`.
* Quit the TUI gracefully → the session snapshot is updated.
* Restart the TUI with the same `--session` flag (or derived ID) → the
  conversation reappears exactly where you left off.

The mechanism is generic and future-proof: alternative front-ends such
as a web UI can consume the same `Session` API without pulling the TUI
stack.

------------------------------------------------------------------------

2  Data model
-------------

```ocaml
module History = struct
  type t = Openai.Responses.Item.t list [@@deriving bin_io, sexp]
end

module Task = struct
  type state = Pending | In_progress | Done [@@deriving bin_io, sexp]
  type t = { id : string; title : string; state : state } [@@deriving bin_io, sexp]
end

type t = {
  version     : int;                     (** on-disk schema version                *)
  id          : string;                  (** uuid v4 or deterministic hash        *)
  prompt_file : string;                  (** absolute path of the original prompt *)
  history     : History.t;               (** full message history                  *)
  tasks       : Task.t list;             (** auxiliary task list                   *)
  kv_store    : (string * string) list;  (** lightweight key-value store           *)
  vfs_root    : string;                  (** sandboxed directory name              *)
}
[@@deriving bin_io, sexp]
```

Two helper modules complement the record:

* `Session_store` – resolves a session ID to an on-disk directory and
  performs *atomic* snapshot writes.
* `Session.Io` – thin functorised wrapper around
  `Bin_prot_utils_eio.With_file_methods` that provides `read`, `write`,
  `write_atomic`, … out of the box.

> **Invariant:** `version = current_version` for every value produced by
> the running binary.

------------------------------------------------------------------------

3  CLI integration
------------------

The TUI provides a *family* of flags that let you manage session state
directly from the command-line, without launching the interactive UI.

```text
-file <file>               Load the given ChatMarkdown prompt (default: ./prompts/interactive.md)
--session <id>             Resume the existing session <id> (error if absent).
--new-session              Start a *fresh* session even if a matching one exists.
--list-sessions            Enumerate all stored sessions and exit.
--session-info <id>        Print metadata (prompt path, last modified, history length, …) for <id>.
--export-session <id>      Convert the snapshot of <id> to ChatMarkdown – requires `--out`.
--out <file>               Destination path for `--export-session`.
```

`--list-sessions`, `--session-info`, and `--export-session` all **exit
immediately** after completing their task, making them suitable for
shell scripts and CI pipelines.

When neither flag is provided the TUI derives a **deterministic**
session identifier from the absolute path of the prompt file.  This
protects users from “losing” their conversation because a random UUID
changed between runs.

------------------------------------------------------------------------

4  Public API walk-through
-------------------------

### Creating or loading

```ocaml
val Session_store.load_or_create
  :  env:Eio_unix.Stdenv.base
  -> prompt_file:string
  -> ?id:string
  -> ?new_session:bool
  -> unit
  -> Session.t
```

Internally the helper either deserialises `snapshot.bin` *or* delegates
to `Session.create` when the file is missing or corrupted.

### Persisting

```ocaml
val Session_store.save : env:Eio_unix.Stdenv.base -> Session.t -> unit
```

Used by the TUI’s `App.export_session` command which is triggered on
regular exit (Ctrl-D, `:q`, etc.) and on explicit `:w` – *write*
command.

### Accessors & mutators

The `Session` module intentionally exposes only coarse-grained
operations so the internal representation can evolve freely.  The TUI
currently relies on:

```ocaml
val history : t -> History.t
val with_history : t -> History.t -> t
```

Finer-grained helpers (append message, delete task, etc.) can be added
later without breaking callers thanks to the versioning strategy.

------------------------------------------------------------------------

5  Persistence format & versioning
---------------------------------

Snapshots are written with `Bin_prot`, the binary protocol used
throughout *ochat*.  Each on-disk record starts with an explicit
`version` field which enables **forwards-compatible upgrades**:

```text
┌─────────────────────────────────────────────┐
│ version : int       (= 1)                  │
│ id      : string                           │
│ …                                         │
└─────────────────────────────────────────────┘
```

The implementation keeps *immutable* copies of every previous schema in
`Session.Legacy.Vn`.  A dedicated upgrade function converts the legacy
value to `Session.Latest.t`:

```ocaml
let upgrade_v0 : Legacy.V0.t -> Latest.t
```

`Session_store.load_or_create` deserialises the raw bytes, inspects
`version`, and either returns the value directly (latest schema) or
passes it through the upgrade pipeline.

------------------------------------------------------------------------

6  FAQ & troubleshooting
-----------------------

**Q  Why not write the snapshot on every message?**  In practice most
conversations are short enough that saving the entire history on exit
is negligible.  For very long chats we plan to switch to an append-only
bin-list which would bring incremental O(1) writes.

**Q  Is the file format stable across commits?**  Only within the same
`major` branch.  Whenever a breaking change lands the `version` counter
increments and upgrade code is provided.

**Q  `Session.Io.File.read` raises – what now?**  The TUI catches all
exceptions in `load_or_create` and silently falls back to a fresh
session to avoid user disruption.  The corrupted snapshot is left on
disk for manual inspection.

------------------------------------------------------------------------

_End of document._

