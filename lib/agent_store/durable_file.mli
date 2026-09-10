(** Atomic replacement for small authoritative store files. *)

type durability =
  | Flush_file
  | Flush_file_and_directory
[@@deriving compare, equal, sexp]

(** Recognize a basename produced by the atomic writer and return its intended
    target basename. Callers must validate that target in their own namespace.
    Recognition grants no ownership or deletion authority. *)
val temporary_target : string -> string option

(** [replace ~env ~durability ~path contents] atomically replaces [path].
    The target must be an absolute path whose parent already exists. *)
val replace
  :  env:Eio_unix.Stdenv.base
  -> durability:durability
  -> path:string
  -> string
  -> (unit, Store_error.t) result

(** [load ~env ~path] reads the complete file through Eio and reports a typed
    [Missing] error when the path does not exist. *)
val load : env:Eio_unix.Stdenv.base -> path:string -> (string, Store_error.t) result

(** [sync_directory ~env ~path] durably records prior directory-entry changes.
    [path] must be an absolute directory path. Opens an Eio-owned read-only
    file descriptor for the directory and performs fsync in an Eio system
    thread. Providers without a native descriptor fail explicitly; directory
    sync is never silently skipped. *)
val sync_directory
  :  env:Eio_unix.Stdenv.base
  -> path:string
  -> (unit, Store_error.t) result
