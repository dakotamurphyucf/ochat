(** Session persistence helper.

    `Session_store` wires the [`Session`](session.mli) data structure to the
    file-system.  It decides where sessions live (`$HOME/.ochat/sessions/<id>`),
    takes care of *schema migrations* when a stored snapshot was produced by an
    older binary, and provides a couple of convenience functions that the CLI
    surfaces as flags – reset, rebuild, and list.

    The API is deliberately minimal: open a session with {!load_or_create},
    mutate the resulting {!Session.t}, then persist it with {!save}.  All
    helpers require an `Eio_unix.Stdenv.base` capability so they work equally
    well in the main thread or inside a fibre/domain. *)

open! Core

type id = string
type path = Eio.Fs.dir_ty Eio.Path.t

(** [base_dir ()] yields the root directory that stores *all* persisted
    chat sessions.

    Resolution rules:
    • [$HOME] present ‒ returns ["$HOME/.ochat/sessions"].
    • otherwise    ‒ returns ["./.ochat/sessions"].

    Pure helper – it merely constructs the path string and never
    touches the file-system. *)
val base_dir : unit -> string

(** [rel_path id] concatenates {!base_dir} with [id] and returns the
     session directory as a plain string (no capability attached). *)
val rel_path : id -> string

(** [ensure_dir ~env id] returns an [`dir Path.t] rooted at the session
    directory [id], creating the hierarchy recursively with permissions
    [0o700] when necessary. *)
val ensure_dir : env:Eio_unix.Stdenv.base -> id -> path

(* Convenience alias so callers can simply write
   [Session_store.path ~env id]. *)
val path : env:Eio_unix.Stdenv.base -> id -> path

(** [load_or_create ~env ~prompt_file ?id ?new_session ()] restores an
     existing session *or* boot-straps a brand-new {!Session.t} when no
     compatible snapshot is present.

     Identifier selection (highest priority first):
     1. explicit [?id] when [new_session = false];
     2. freshly generated UUID-v4 when [new_session = true];
     3. MD5 digest of [prompt_file] (default).

     If [snapshot.bin] exists under the chosen directory it is loaded and
     migrated to the latest schema via {!Session.Legacy}.  Otherwise a new
     record is created and the original markdown prompt is copied into the
     directory as [prompt.chatmd] to keep the session self-contained.

     The returned value lives purely in memory – call {!save} after making
     changes. *)
val load_or_create
  :  env:Eio_unix.Stdenv.base
  -> prompt_file:string
  -> ?id:id
  -> ?new_session:bool
  -> unit
  -> Session.t

(** [save ~env session] atomically writes [session] to
     [<session-dir>/snapshot.bin] while holding an advisory lock file
     (`snapshot.bin.lock`).  The program exits with status 1 when the
     lock is already taken by another process. *)
val save : env:Eio_unix.Stdenv.base -> Session.t -> unit

(** [list ~env] enumerates *valid* sessions – i.e. directories that
     contain a readable [snapshot.bin].  The function returns
     [(id, prompt_file)] pairs and silently ignores damaged snapshots. *)
val list : env:Eio_unix.Stdenv.base -> (id * string) list

(** [reset_session ~env ~id ?prompt_file ()] archives the current
    snapshot of session [id] and resets its in-memory state.

    Behaviour:
    • The existing [snapshot.bin] is moved to an [archive/] subdirectory
      of the session directory using the timestamp format
      "YYYYMMDD-HHMM.snapshot.bin".
    • The loaded session value is passed through {!Session.reset}, which
      clears the history and (optionally) updates the [prompt_file].
    • When [prompt_file] is provided, the referenced markdown document is
      copied into the session directory as [prompt.chatmd] and recorded
      via [local_prompt_copy].
    • The new snapshot is then written back to disk via {!save}.

    The helper prints a short confirmation message on [stdout].  It exits
    early with an error message if the session cannot be found. *)
val reset_session
  :  env:Eio_unix.Stdenv.base
  -> id:id
  -> ?prompt_file:string
  -> ?keep_history:bool
  -> unit
  -> unit

(** [rebuild_session ~env ~id ()] discards the current [snapshot.bin] of
    session [id] and recreates a fresh one seeded solely by the
    {i current} prompt file.  The helper is intended for the workflow
    where the user edits the copied [prompt.chatmd] inside the session
    directory and wants the persisted session state to reflect those
    changes without starting the interactive TUI.

    Behaviour:
    • The existing snapshot is moved to [archive/] just like
      {!reset_session}.
    • A brand-new {!Session.t} value is created using the same
      [prompt_file] and [local_prompt_copy] recorded in the archived
      snapshot.  The new record therefore has an {b empty} history – the
      interactive UI will re-parse the updated prompt on the next
      launch.
    • The per-session cache located at [<session-dir>/.chatmd/cache.bin]
      is deleted to avoid stale tool outputs.
    • A confirmation summary is printed to [stdout]. *)
val rebuild_session : env:Eio_unix.Stdenv.base -> id:id -> unit -> unit
