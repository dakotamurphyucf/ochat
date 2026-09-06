(** Inject process-crash boundaries into real Eio filesystem operations. *)

type boundary =
  | After_create
  | After_bytes of int
  | Before_sync
  | After_sync
  | Before_rename
  | After_rename
  | Before_directory_sync

(** [wrap env ~matches ~boundary ~reached] delegates IO to [env], invoking
    [reached] at the selected operation on matching paths. [After_bytes n]
    writes exactly [n] bytes through the underlying file before invoking the
    callback. Use a nonreturning callback and kill the child externally; no
    exception unwinding or production cleanup runs at the crash boundary.
    Renames retain the native filesystem destination capability.
    [Before_directory_sync] invokes the callback after opening a matching
    directory as a read-only file, verifying its kind and native descriptor,
    and before the caller can obtain its descriptor for sync.
    That callback may return to observe a subsequent successful replacement. *)
val wrap
  :  Eio_unix.Stdenv.base
  -> matches:(string -> bool)
  -> boundary:boundary
  -> reached:(string -> unit)
  -> Eio_unix.Stdenv.base
