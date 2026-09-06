(** Validated ownership handle for the daemon data directory. *)

type t

(** [create ~env ~path] validates an absolute non-root path and creates the
    complete server-owned directory layout with mode [0o700]. *)
val create : env:Eio_unix.Stdenv.base -> path:string -> (t, Store_error.t) result

(** [open_existing ~env ~path] validates an existing data-root directory. *)
val open_existing : env:Eio_unix.Stdenv.base -> path:string -> (t, Store_error.t) result

val path : t -> string
val schema_path : t -> string
val daemon_lock_path : t -> string
val server_id_path : t -> string
val indexes_path : t -> string
val prompt_artifacts_path : t -> string
val temporary_blobs_path : t -> string
val durable_blobs_path : t -> string
val audit_path : t -> string
val sessions_path : t -> string
val migrations_path : t -> string
val lost_and_found_path : t -> string

(** [session_path t id] returns a path beneath the owned sessions directory. *)
val session_path : t -> Agent_protocol.Id.Session.t -> string
