(** Standalone session document persistence. The filename remains snapshot.bin,
    but its contents are a complete named-field document. Unsupported beta
    binaries are reported explicitly and never rewritten during reads. *)
open! Core

type id = string
type path = Eio.Fs.dir_ty Eio.Path.t

val base_dir : unit -> string
val rel_path : id -> string
val ensure_dir : env:Eio_unix.Stdenv.base -> id -> path
val path : env:Eio_unix.Stdenv.base -> id -> path

(** Missing snapshots create a session. Existing malformed/unsupported snapshots
    raise their decode error without mutation. *)
val load_or_create
  :  env:Eio_unix.Stdenv.base
  -> prompt_file:string
  -> ?id:id
  -> ?new_session:bool
  -> unit
  -> Session.t

(** Returns None only for missing snapshots; existing invalid data raises. *)
val read_existing : env:Eio_unix.Stdenv.base -> id:id -> Session.t option

(** Bounded document conversion/validation; no fallback to historical layouts. *)
val read_current_file : path -> (Session.t, Error.t) Result.t

(** Preflights the complete document before creating directories or taking an
    exclusive lock. An exclusive temporary file and rename preserve the prior
    snapshot on write failure. No fsync durability is promised. Eio cancellation
    propagates; ordinary filesystem failures are returned. *)
val save : env:Eio_unix.Stdenv.base -> Session.t -> unit Or_error.t

val save_exn : env:Eio_unix.Stdenv.base -> Session.t -> unit
val list : env:Eio_unix.Stdenv.base -> (id * string) list

(** Preflight, archive a copy, then atomically replace while holding the same
    lock. Decode/conversion failures leave the snapshot and archive untouched.
    Reset preserves allocator high-water marks and unknown-field ownership. *)
val reset_session
  :  env:Eio_unix.Stdenv.base
  -> id:id
  -> ?prompt_file:string
  -> ?keep_history:bool
  -> unit
  -> unit

(** Resets the conversation, task list and metadata while preserving the prompt,
    history allocator and preservation context; clears the cache after commit. *)
val rebuild_session : env:Eio_unix.Stdenv.base -> id:id -> unit -> unit
